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
module Data.Migratable.DecodeAndMigrate where

import           Data.Migratable.Migrate
import           Data.Migratable.Versioned

import           Control.Applicative       ((<|>))
import qualified Data.Aeson                as JSON
import qualified Data.Aeson.Types          as JSON
import           GHC.TypeLits

-- DecodeAndMigrate attempts to decode a JSON.Value from a list of different
-- versions, starting with the newest.
class DecodeAndMigrate (versions :: [Nat]) (target :: Nat) (label :: Symbol) where
  decodeAndMigrate :: JSON.Value -> JSON.Parser (Maybe (target `VersionOf` label))

instance
  ( DecodeAndMigrate (y ': xs) target label
  , Migrate this target label
  , thisVersion ~ (this `VersionOf` label)
  , JSON.FromJSON thisVersion
  ) => DecodeAndMigrate (this ': y ': xs) target label where
    decodeAndMigrate a
      =   decodeAndMigrate' @this @target @label a
      <|> decodeAndMigrate  @(y ': xs) @target @label a

instance
  ( Migrate this target label
  , thisVersion ~ (this `VersionOf` label)
  , JSON.FromJSON thisVersion
  ) => DecodeAndMigrate '[this] target label where
    decodeAndMigrate = decodeAndMigrate' @this @target @label

decodeAndMigrate'
  :: forall this target label thisVersion targetVersion
   . ( Migrate this target label
     , thisVersion ~ (this `VersionOf` label)
     , targetVersion ~ (target `VersionOf` label)
     , JSON.FromJSON thisVersion
     )
  => JSON.Value
  -> JSON.Parser (Maybe targetVersion)
decodeAndMigrate' a
  =   migrate @this @target @label
  <$> JSON.parseJSON @thisVersion a
