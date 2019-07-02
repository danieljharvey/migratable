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
module Data.Migratable.MatchAll where

import qualified Data.Aeson                   as JSON
import qualified Data.Aeson.Types             as JSON
import           Data.Either
import           Data.List                    (nub)
import           Data.Maybe
import           Data.Migratable.TypeFamilies
import           Data.Migratable.Versioned
import           Data.Proxy
import           GHC.TypeLits
import           Test.QuickCheck.Arbitrary
import           Test.QuickCheck.Gen

-- | An issue with different schemas like this is that a particular JSON blob
-- | could be accidentally compatible with multiple decoders
-- | Matching allows one to take a piece of JSON and try it against all versions
-- | ideally it would match only one.

class Matching (earliest :: Nat) (latest :: Nat) (label :: Symbol) where
  matches :: JSON.Value -> [Integer]

instance
  ( jobs ~ FindPath earliest latest
  , Matching_ jobs label
  , Last jobs ~ latest
  , Head jobs ~ earliest
  )
  => Matching earliest latest label where
    matches json
      = matches_ @jobs @label json []

class Matching_ (versions :: [Nat]) (label :: Symbol) where
  matches_ :: JSON.Value -> [Integer] -> [Integer]

instance
  ( x ~ (y - 1)
  , thisVersion ~ (x `VersionOf` label)
  , KnownNat x
  , Versioned label y
  , Migratable label y
  , JSON.FromJSON thisVersion
  , Matching_ (y ': xs) label
  )
  => Matching_ (x ': y ': xs) label where
    matches_ json vals
      = rest newVal
      where
         newVal
           = if isJust (JSON.parseMaybe (JSON.parseJSON @thisVersion) json)
             then [natVal (Proxy :: Proxy x)] <> vals
             else vals
         rest a
           = matches_ @(y ': xs) @label json a

instance
  ( KnownNat x
  , thisVersion ~ (x `VersionOf` label)
  , JSON.FromJSON thisVersion
  )
  => Matching_ (x ': '[]) label where
    matches_ json vals
      = if isJust (JSON.parseMaybe (JSON.parseJSON @thisVersion) json)
        then [natVal (Proxy :: Proxy x)] <> vals
        else vals

class MatchAll (earliest :: Nat) (latest :: Nat) (label :: Symbol) where
  matchAll :: IO (Either [MatchError] [Integer])

instance
  ( jobs ~ FindPath earliest latest
  , MatchAll_ jobs earliest latest label
  , Last jobs ~ latest
  , Head jobs ~ earliest
  )
  => MatchAll earliest latest label where
    matchAll = do
      matched <- matchAll_ @jobs @earliest @latest @label []
      case partitionEithers matched of
          ([], as)    -> pure $ Right as
          (errors, _) -> pure $ Left errors

data MatchError
  = Duplicates Integer [Integer]
  | CouldNotRead Integer
  deriving (Eq, Ord, Show)

class MatchAll_ (versions :: [Nat]) (earliest :: Nat) (latest :: Nat) (label :: Symbol) where
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
  , thisVersion ~ (x `VersionOf` label)
  , Arbitrary thisVersion
  , JSON.ToJSON thisVersion
  , Matching_ jobs label
  , MatchAll_ (y ': xs) earliest latest label
  )
  => MatchAll_ (x ': y ': xs) earliest latest label where
    matchAll_ xs = do
      items <- sample' (arbitrary @thisVersion)
      let unique =  valuesToEither (natVal (Proxy :: Proxy x))
                 $  combineArrays
                 $  matches @earliest @latest @label . JSON.toJSON
                <$> items
      matchAll_ @(y ': xs) @earliest @latest @label (xs <> [unique])


instance
  ( KnownNat x
  , jobs ~ FindPath earliest latest
  , Head jobs ~ earliest
  , Last jobs ~ latest
  , thisVersion ~ (x `VersionOf` label)
  , Arbitrary thisVersion
  , JSON.ToJSON thisVersion
  , Matching_ jobs label
  )
  => MatchAll_ (x ': '[]) earliest latest label where
    matchAll_ xs = do
      items <- sample' (arbitrary @thisVersion)
      let unique =  valuesToEither (natVal (Proxy :: Proxy x))
                 $  combineArrays
                 $  matches @earliest @latest @label . JSON.toJSON
                <$> items
      pure $ xs <> [unique]

combineArrays :: (Eq a) => [[a]] -> [a]
combineArrays = nub . mconcat
