{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Migratable where

import           Data.Migratable.TypeFamilies

import           Control.Applicative
import           Control.Arrow
import           Control.Monad
import qualified Data.Aeson                   as JSON
import qualified Data.Aeson.Types             as JSON
import           Data.ByteString.Lazy
import           Data.Either
import           Data.Kind                    (Type)
import           Data.List                    (nub)
import           Data.Maybe
import           Data.Proxy
import qualified Data.Text                    as Text
import           Data.Void                    (Void)
import           GHC.Generics
import           GHC.Natural
import           GHC.TypeLits
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

-- Create an instance of Versioned for each version of your data type
-- To create a set, pristine should be the same, and the num value should
-- increment for each new version
class Versioned (pristine :: Symbol) (num :: Nat) where
  type num `VersionOf` pristine :: Type

-- Create an instance of Migratable for each new version of your data type,
-- providing it a function that attempts to convert it from the previous
-- version
class Migratable (pristine :: Symbol) (num :: Nat) where
  fromPrevious
    :: (num - 1) `VersionOf` pristine
    -> Maybe (num `VersionOf` pristine)

---

-- A Schema is created for any set of Versioned data types, and provides
-- `decodeVia` for converting any JSON value to it
class Schema
  (pristine :: Symbol)
  (earliest :: Nat)
  (target :: Nat) where
  decodeVia    :: JSON.Value -> Maybe (target `VersionOf` pristine)
  parseJSONVia :: JSON.Value -> JSON.Parser (target `VersionOf` pristine)

instance
  ( jobs ~ ReversePath earliest target
  , DecodeAndMigrate jobs target pristine
  , Head jobs ~ target
  , Last jobs ~ earliest
  )
  => Schema pristine earliest target where
    decodeVia
      = join . JSON.parseMaybe (decodeAndMigrate @jobs @target @pristine)
    parseJSONVia a
      = collapseInner <$> (decodeAndMigrate @ jobs @target @pristine a)
      where
        collapseInner mebs
           = case mebs of
               Just a  -> a

---
-- DecodeAndMigrate attempts to decode a JSON.Value from a list of different
-- versions, starting with the newest.
class DecodeAndMigrate (versions :: [Nat]) (target :: Nat) (pristine :: Symbol) where
  decodeAndMigrate :: JSON.Value -> JSON.Parser (Maybe (target `VersionOf` pristine))

instance
  ( DecodeAndMigrate (y ': xs) target pristine
  , Migrate this target pristine
  , thisVersion ~ (this `VersionOf` pristine)
  , JSON.FromJSON thisVersion
  ) => DecodeAndMigrate (this ': y ': xs) target pristine where
    decodeAndMigrate a
      =   decodeAndMigrate' @this @target @pristine a
      <|> decodeAndMigrate  @(y ': xs) @target @pristine a

instance
  ( Migrate this target pristine
  , thisVersion ~ (this `VersionOf` pristine)
  , JSON.FromJSON thisVersion
  ) => DecodeAndMigrate '[this] target pristine where
    decodeAndMigrate = decodeAndMigrate' @this @target @pristine

decodeAndMigrate'
  :: forall this target pristine thisVersion targetVersion
   . ( Migrate this target pristine
     , thisVersion ~ (this `VersionOf` pristine)
     , targetVersion ~ (target `VersionOf` pristine)
     , JSON.FromJSON thisVersion
     )
  => JSON.Value
  -> JSON.Parser (Maybe targetVersion)
decodeAndMigrate' a
  =   migrate @this @target @pristine
  <$> JSON.parseJSON @thisVersion a

---
-- Migrate turns an earlier version of the data type into a newer version of
-- the datatype, via all the conversion functions along the way
class Migrate (earliest :: Nat) (target :: Nat) (pristine :: Symbol) where
  migrate :: earliest `VersionOf` pristine -> Maybe (target `VersionOf` pristine)

instance
    ( jobs ~ FindPath earliest target
    , Migrate_ jobs pristine
    , Last jobs ~ target
    , Head jobs ~ earliest
    )
    => Migrate earliest target pristine where
  migrate = migrate_ @jobs @pristine

class Migrate_ (versions :: [Nat]) (pristine :: Symbol) where
  migrate_
    :: Head versions `VersionOf` pristine
    -> Maybe (Last versions `VersionOf` pristine)

instance Migrate_ '[n] pristine where
  migrate_ = pure

instance {-# OVERLAPPABLE #-}
      ( x ~ (y - 1)
      , Versioned pristine y
      , Migratable pristine y
      , Migrate_ (y ': xs) pristine
      )
      => Migrate_ (x ': y ': xs) pristine where
  migrate_
    = fromPrevious @pristine @y
      >=> migrate_ @(y ': xs) @pristine

-- | An issue with different schemas like this is that a particular JSON blob
-- | could be accidentally compatible with multiple decoders
-- | Matching allows one to take a piece of JSON and try it against all versions
-- | ideally it would match only one.

class Matching (earliest :: Nat) (latest :: Nat) (pristine :: Symbol) where
  matches :: JSON.Value -> [Integer]

instance
  ( jobs ~ FindPath earliest latest
  , Matching_ jobs pristine
  , Last jobs ~ latest
  , Head jobs ~ earliest
  )
  => Matching earliest latest pristine where
    matches json
      = matches_ @jobs @pristine json []

class Matching_ (versions :: [Nat]) (pristine :: Symbol) where
  matches_ :: JSON.Value -> [Integer] -> [Integer]

instance
  ( x ~ (y - 1)
  , thisVersion ~ (x `VersionOf` pristine)
  , KnownNat x
  , Versioned pristine y
  , Migratable pristine y
  , JSON.FromJSON thisVersion
  , Matching_ (y ': xs) pristine
  )
  => Matching_ (x ': y ': xs) pristine where
    matches_ json vals
      = rest newVal
      where
         newVal
           = if isJust (JSON.parseMaybe (JSON.parseJSON @thisVersion) json)
             then [natVal (Proxy :: Proxy x)] <> vals
             else vals
         rest a
           = matches_ @(y ': xs) @pristine json a

instance
  ( KnownNat x
  , thisVersion ~ (x `VersionOf` pristine)
  , JSON.FromJSON thisVersion
  )
  => Matching_ (x ': '[]) pristine where
    matches_ json vals
      = if isJust (JSON.parseMaybe (JSON.parseJSON @thisVersion) json)
        then [natVal (Proxy :: Proxy x)] <> vals
        else vals

class MatchAll (earliest :: Nat) (latest :: Nat) (pristine :: Symbol) where
  matchAll :: IO (Either [MatchError] [Integer])

instance
  ( jobs ~ FindPath earliest latest
  , MatchAll_ jobs earliest latest pristine
  , Last jobs ~ latest
  , Head jobs ~ earliest
  )
  => MatchAll earliest latest pristine where
    matchAll = do
      matched <- matchAll_ @jobs @earliest @latest @pristine []
      case partitionEithers matched of
          ([], as)    -> pure $ Right as
          (errors, _) -> pure $ Left errors

data MatchError
  = Duplicates Integer [Integer]
  | CouldNotRead Integer
  deriving (Eq, Ord, Show)

class MatchAll_ (versions :: [Nat]) (earliest :: Nat) (latest :: Nat) (pristine :: Symbol) where
  matchAll_ :: [(Either MatchError Integer)] -> IO [(Either MatchError Integer)]

valuesToEither :: Integer -> [Integer] -> Either MatchError Integer
valuesToEither x xs
  | xs == [x]             = Right x
  | Prelude.length xs > 1 = Left (Duplicates x xs)
  | otherwise             = Left (CouldNotRead x)

instance
  ( x ~ (y - 1)
  , KnownNat x
  , jobs ~ FindPath earliest latest
  , Head jobs ~ earliest
  , Last jobs ~ latest
  , thisVersion ~ (x `VersionOf` pristine)
  , Arbitrary thisVersion
  , JSON.ToJSON thisVersion
  , Matching_ jobs pristine
  , MatchAll_ (y ': xs) earliest latest pristine
  )
  => MatchAll_ (x ': y ': xs) earliest latest pristine where
    matchAll_ xs = do
      items <- sample' (arbitrary @thisVersion)
      let unique =  valuesToEither (natVal (Proxy :: Proxy x))
                 $  combineArrays
                 $  matches @earliest @latest @pristine . JSON.toJSON
                <$> items
      matchAll_ @(y ': xs) @earliest @latest @pristine (xs <> [unique])


instance
  ( KnownNat x
  , jobs ~ FindPath earliest latest
  , Head jobs ~ earliest
  , Last jobs ~ latest
  , thisVersion ~ (x `VersionOf` pristine)
  , Arbitrary thisVersion
  , JSON.ToJSON thisVersion
  , Matching_ jobs pristine
  )
  => MatchAll_ (x ': '[]) earliest latest pristine where
    matchAll_ xs = do
      items <- sample' (arbitrary @thisVersion)
      let unique =  valuesToEither (natVal (Proxy :: Proxy x))
                 $  combineArrays
                 $  matches @earliest @latest @pristine . JSON.toJSON
                <$> items
      pure $ xs <> [unique]


combineArrays :: (Eq a) => [[a]] -> [a]
combineArrays = nub . mconcat
