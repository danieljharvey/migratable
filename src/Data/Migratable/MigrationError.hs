{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Migratable.MigrationError where

import           Data.Bifunctor
import           Data.Proxy
import           GHC.TypeLits


newtype MigrationLabel
  = MigrationLabel String

newtype LastGoodVersion
  = LastGoodVersion Integer

newtype JSONError
  = JSONError String

data MigrationError
  = DecodeError MigrationLabel JSONError
  | MigrateError MigrationLabel LastGoodVersion
  | ComigrateError MigrationLabel LastGoodVersion

reifyLabel :: forall a. (KnownSymbol a) => MigrationLabel
reifyLabel = MigrationLabel $ symbolVal (Proxy :: Proxy a)

reifyVersion :: forall a. (KnownNat a) => LastGoodVersion
reifyVersion = LastGoodVersion $ natVal (Proxy :: Proxy a)

wrapDecodeError :: MigrationLabel -> Either String a -> Either MigrationError a
wrapDecodeError label item
  = first (\a -> DecodeError label (JSONError a)) item
