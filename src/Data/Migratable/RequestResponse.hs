{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
module Data.Migratable.RequestResponse where

import           Control.Monad
import           Control.Monad.Trans.Except
import           Data.Kind
import           Data.Migratable.Comigrate
import           Data.Migratable.DecodeAndMigrate  ()
import           Data.Migratable.Migrate
import           Data.Migratable.MigrationError
import           Data.Migratable.Versioned
import           GHC.TypeLits
import           Test.QuickCheck.Instances.Natural ()
import           Test.QuickCheck.Instances.Text    ()

-- | RequestResponse allows us to take a matching set of
-- | Request types and Response types
-- | and use migrations to bring data up to date
-- | then roll it into the old data type ready for use
class RequestResponse
    (earliest :: Nat)
    (target :: Nat)
    (request :: Symbol)
    (response :: Symbol)
    (m :: Type -> Type) where
  withMigration
    :: Monad m
    => (target `VersionOf` request -> m (target `VersionOf` response))
    -> earliest `VersionOf` request
    -> m (Either MigrationError (earliest `VersionOf` response))

instance
  ( Monad m
  , Migrate earliest target request
  , Comigrate earliest target response)
  => RequestResponse earliest target request response m where
  withMigration f a
    = runExceptT $ (decode >=> compute >=> reencode) a
    where
      decode
        :: (earliest `VersionOf` request)
        -> ExceptT MigrationError m (target `VersionOf` request)
      decode
        = ExceptT . pure . migrate @earliest @target @request

      compute
        :: (target `VersionOf` request)
        -> ExceptT MigrationError m (target `VersionOf` response)
      compute a'
        = ExceptT $ Right <$> f a'

      reencode
        :: (target `VersionOf` response)
        -> ExceptT MigrationError m (earliest `VersionOf` response)
      reencode
        = ExceptT . pure . comigrate @earliest @target @response
