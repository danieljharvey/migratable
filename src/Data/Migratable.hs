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
import           Data.ByteString.Lazy
import           Data.Kind                    (Type)
import qualified Data.Text                    as Text

import           Data.Void                    (Void)
import           GHC.Generics
import           GHC.Natural
import           GHC.TypeLits

class Versioned (pristine :: Symbol) (num :: Nat) where
  type num `VersionOf` pristine :: Type

class Upgradable (pristine :: Symbol) (num :: Nat) where
  upgrade
    :: (num - 1) `VersionOf` pristine
    -> (num `VersionOf` pristine)

class MaybeUpgradable (pristine :: Symbol) (num :: Nat) where
  tryUpgrade
    :: (num - 1) `VersionOf` pristine
    -> Maybe (num `VersionOf` pristine)

-- every defined Upgradable gives us a free MaybeUpgradable
instance {-# OVERLAPPABLE #-}
  (Upgradable pristine num)
  => MaybeUpgradable pristine num where
    tryUpgrade = Just . upgrade @pristine @num

---

class Schema
  (pristine :: Symbol)
  (earliest :: Nat)
  (target :: Nat) where
    decodeVia :: ByteString -> Maybe (target `VersionOf` pristine)

instance
  ( jobs ~ ReversePath earliest target
  , Migrate jobs target pristine
  , Head jobs ~ target
  , Last jobs ~ earliest
  )
  => Schema pristine earliest target where
    decodeVia = migrate @jobs @target @pristine

---

class WeakSchema
  (pristine :: Symbol)
  (earliest :: Nat)
  (target :: Nat) where
  tryDecodeVia :: ByteString -> Maybe (target `VersionOf` pristine)

instance
  ( jobs ~ ReversePath earliest target
  , TryMigrate jobs target pristine
  , Head jobs ~ target
  , Last jobs ~ earliest
  )
  => WeakSchema pristine earliest target where
    tryDecodeVia = tryMigrate @jobs @target @pristine

---

class Migrate (versions :: [Nat]) (target :: Nat) (pristine :: Symbol) where
  migrate :: ByteString -> Maybe (target `VersionOf` pristine)

instance
  ( Migrate (y ': xs) target pristine
  , GenerallyUpdate this target pristine
  , JSON.FromJSON (this `VersionOf` pristine)
  ) => Migrate (this ': y ': xs) target pristine where
  migrate a
    =   decodeAndUpdate @this @target @pristine a
    <|> migrate  @(y ': xs) @target @pristine a

instance
  ( GenerallyUpdate this target pristine
  , JSON.FromJSON (this `VersionOf` pristine)
  ) => Migrate '[this] target pristine where
  migrate
    = decodeAndUpdate @this @target @pristine

decodeAndUpdate
  :: forall this target pristine
   . GenerallyUpdate this target pristine
  => JSON.FromJSON (this `VersionOf` pristine)
  => ByteString
  -> Maybe (target `VersionOf` pristine)
decodeAndUpdate a
  =   generallyUpdate @this @target @pristine
  <$> JSON.decode @(this `VersionOf` pristine) a

---

class TryMigrate (versions :: [Nat]) (target :: Nat) (pristine :: Symbol) where
  tryMigrate :: ByteString -> Maybe (target `VersionOf` pristine)

instance
  ( TryMigrate (y ': xs) target pristine
  , MaybeUpdate this target pristine
  , JSON.FromJSON (this `VersionOf` pristine)
  ) => TryMigrate (this ': y ': xs) target pristine where
    tryMigrate a
      =   tryDecodeAndUpdate @this @target @pristine a
      <|> tryMigrate  @(y ': xs) @target @pristine a

instance
  ( MaybeUpdate this target pristine
  , JSON.FromJSON (this `VersionOf` pristine)
  ) => TryMigrate '[this] target pristine where
    tryMigrate = tryDecodeAndUpdate @this @target @pristine

tryDecodeAndUpdate
  :: forall this target pristine
   . MaybeUpdate this target pristine
  => JSON.FromJSON (this `VersionOf` pristine)
  => ByteString
  -> Maybe (target `VersionOf` pristine)
tryDecodeAndUpdate
  =  JSON.decode @(this `VersionOf` pristine)
 >=> maybeUpdate @this @target @pristine

---

class GenerallyUpdate (earliest :: Nat) (target :: Nat) (pristine :: Symbol) where
  generallyUpdate :: earliest `VersionOf` pristine -> (target `VersionOf` pristine)

instance
    ( jobs ~ FindPath earliest target
    , GenerallyUpdate_ jobs pristine
    , Last jobs ~ target
    , Head jobs ~ earliest
    )
    => GenerallyUpdate earliest target pristine where
  generallyUpdate = generallyUpdate_ @jobs @pristine

class GenerallyUpdate_ (versions :: [Nat]) (pristine :: Symbol) where
  generallyUpdate_
    :: Head versions `VersionOf` pristine
    -> (Last versions `VersionOf` pristine)

instance GenerallyUpdate_ '[n] pristine where
  generallyUpdate_ = id

instance {-# OVERLAPPABLE #-}
      ( x ~ (y - 1)
      , Versioned pristine y
      , Upgradable pristine y
      , GenerallyUpdate_ (y ': xs) pristine
      )
      => GenerallyUpdate_ (x ': y ': xs) pristine where
  generallyUpdate_
    = upgrade @pristine @y
    >>> generallyUpdate_ @(y ': xs) @pristine

---

class MaybeUpdate (earliest :: Nat) (target :: Nat) (pristine :: Symbol) where
  maybeUpdate :: earliest `VersionOf` pristine -> Maybe (target `VersionOf` pristine)

instance
    ( jobs ~ FindPath earliest target
    , MaybeUpdate_ jobs pristine
    , Last jobs ~ target
    , Head jobs ~ earliest
    )
    => MaybeUpdate earliest target pristine where
  maybeUpdate = maybeUpdate_ @jobs @pristine

class MaybeUpdate_ (versions :: [Nat]) (pristine :: Symbol) where
  maybeUpdate_
    :: Head versions `VersionOf` pristine
    -> Maybe (Last versions `VersionOf` pristine)

instance MaybeUpdate_ '[n] pristine where
  maybeUpdate_ = pure

instance {-# OVERLAPPABLE #-}
      ( x ~ (y - 1)
      , Versioned pristine y
      , MaybeUpgradable pristine y
      , MaybeUpdate_ (y ': xs) pristine
      )
      => MaybeUpdate_ (x ': y ': xs) pristine where
  maybeUpdate_
    = tryUpgrade @pristine @y
      >=> maybeUpdate_ @(y ': xs) @pristine
