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
module Data.Migratable.Schema where

import           Data.Migratable.DecodeAndMigrate (DecodeAndMigrate (..))
import           Data.Migratable.TypeFamilies
import           Data.Migratable.Versioned

import           Control.Monad
import qualified Data.Aeson                       as JSON
import qualified Data.Aeson.Types                 as JSON
import           GHC.TypeLits

-- A Schema is created for any set of Versioned data types, and provides
-- `decodeVia` for converting any JSON value to it
class Schema
  (label :: Symbol)
  (earliest :: Nat)
  (target :: Nat) where
  decodeVia    :: JSON.Value -> Maybe (target `VersionOf` label)
  parseJSONVia :: JSON.Value -> JSON.Parser (target `VersionOf` label)

instance
  ( jobs ~ ReversePath earliest target
  , DecodeAndMigrate jobs target label
  , Head jobs ~ target
  , Last jobs ~ earliest
  )
  => Schema label earliest target where
    decodeVia
      = join . JSON.parseMaybe (decodeAndMigrate @jobs @target @label)
    parseJSONVia a
      = collapseInner <$> (decodeAndMigrate @ jobs @target @label a)
      where
        collapseInner mebs
           = case mebs of
               Just a  -> a
