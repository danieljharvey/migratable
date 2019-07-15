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
import           Control.Monad.Trans.Maybe
import           Data.Kind
import           Data.Migratable.Comigrate
import           Data.Migratable.DecodeAndMigrate  ()
import           Data.Migratable.Migrate
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
  profunctorThing
    :: Monad m
    => (target `VersionOf` request -> m (target `VersionOf` response))
    -> earliest `VersionOf` request
    -> m (Maybe (earliest `VersionOf` response))

instance
  ( Monad m
  , Migrate earliest target request
  , Comigrate earliest target response)
  => RequestResponse earliest target request response m where
  profunctorThing f a
    = runMaybeT $ (decode >=> compute >=> reencode) a
    where
      decode
        :: (earliest `VersionOf` request)
        -> MaybeT m (target `VersionOf` request)
      decode
        = MaybeT . pure . migrate @earliest @target @request

      compute
        :: (target `VersionOf` request)
        -> MaybeT m (target `VersionOf` response)
      compute a'
        = MaybeT $ Just <$> f a'

      reencode
        :: (target `VersionOf` response)
        -> MaybeT m (earliest `VersionOf` response)
      reencode
        = MaybeT . pure . comigrate @earliest @target @response
