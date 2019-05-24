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
import           Data.Kind                    (Type)
import qualified Data.Text                    as Text
import           Data.Void                    (Void)
import           GHC.Generics
import           GHC.Natural
import           GHC.TypeLits

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
  , JSON.FromJSON (this `VersionOf` pristine)
  ) => DecodeAndMigrate (this ': y ': xs) target pristine where
    decodeAndMigrate a
      =   decodeAndMigrate' @this @target @pristine a
      <|> decodeAndMigrate  @(y ': xs) @target @pristine a

instance
  ( Migrate this target pristine
  , JSON.FromJSON (this `VersionOf` pristine)
  ) => DecodeAndMigrate '[this] target pristine where
    decodeAndMigrate = decodeAndMigrate' @this @target @pristine

decodeAndMigrate'
  :: forall this target pristine
   . Migrate this target pristine
  => JSON.FromJSON (this `VersionOf` pristine)
  => JSON.Value
  -> JSON.Parser (Maybe (target `VersionOf` pristine))
decodeAndMigrate' a
  =   migrate @this @target @pristine
  <$> JSON.parseJSON @(this `VersionOf` pristine) a

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
