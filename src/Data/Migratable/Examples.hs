{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}
module Data.Migratable.Examples where

import           Data.Migratable

import           GHC.Generics
import           GHC.Natural

import qualified Data.Aeson      as JSON
import qualified Data.Text       as Text
{-

This is an example data schema that has changed over time.

Versions 1-3 all convert all of the time, however the change
between 3 and 4 doesn't always work.

-}

-- Version 1
-- a classic stringly typed data type

data Older
  = Older { olderFirstName :: String
          , olderSurname   :: String
          , olderPet       :: String
          , olderAge       :: Int
          }
          deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

-- this typeclass associates our type (Older) with a version (1) and a label "User"
instance Versioned "User" 1 where
  type 1 `VersionOf` "User" = Older

-- Version 2
-- Turned out we should try and tidy this data up.
-- We only recognise 'cat' or 'dog' from the old pet type, anything else
-- becomes NoPet.

data OldUser
  = OldUser
    { oldFirstName :: Name
    , oldSurname   :: Name
    , oldPet       :: OldPet
    , oldAge       :: Int
    }
    deriving (Generic, JSON.FromJSON)

instance Versioned "User" 2 where
  type 2 `VersionOf` "User" = OldUser

instance Upgradable "User" 2 where
  upgrade = fromOlder

fromOlder :: Older -> OldUser
fromOlder older
  = OldUser { oldFirstName = Name (Text.pack (olderFirstName older))
            , oldSurname   = Name (Text.pack (olderSurname older))
            , oldPet       = readPet (olderPet older)
            , oldAge       = olderAge older
            }
  where
    readPet s
      | s == "dog" = OldDog
      | s == "cat" = OldCat
      | otherwise  = NoPet

data OldPet
  = OldDog
  | OldCat
  | NoPet
  deriving (Generic, JSON.FromJSON)

-- Version 3
-- This is a fair old change - and the first one which might not work.
-- The age is now a natural which must be zero or above so we'll need to
-- convert any negative numbers into 0

data NewUser
  = NewUser
    { firstName :: FirstName
    , surname   :: Surname
    , pet       :: Maybe Pet
    , age       :: Natural
    }
    deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

instance Versioned "User" 3 where
  type 3 `VersionOf` "User" = NewUser

instance Upgradable "User" 3 where
  upgrade = fromOldUser

fromOldUser :: OldUser -> NewUser
fromOldUser old
  = NewUser
      { firstName = FirstName (oldFirstName old)
      , surname   = Surname (oldSurname old)
      , pet       = convertPet (oldPet old)
      , age       = intToNatural (oldAge old)
      }
  where
    convertPet pet
      = case pet of
          OldDog -> Just Dog
          OldCat -> Just Cat
          _      -> Nothing


-- Version 3 subtypes

newtype Name
  = Name { getName :: Text.Text }
  deriving (Show, Eq, Ord, Generic, JSON.FromJSON, JSON.ToJSON)

newtype FirstName
  = FirstName { getFirstName :: Name }
  deriving (Show, Eq, Ord, Generic, JSON.FromJSON, JSON.ToJSON)

newtype Surname
  = Surname { getSurname :: Name }
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

data Pet
  = Dog
  | Cat
  | Horse
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

-- Version 4

data EvenNewerUser
  = EvenNewerUser
    { newerFirstName :: FirstName
    , newerSurname   :: Surname
    , newerPet       :: Pet
    , newerAge       :: Natural
    }
    deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

---
instance Versioned "User" 4 where
  type 4 `VersionOf` "User" = EvenNewerUser

instance MaybeUpgradable "User" 4 where
  tryUpgrade = fromNewUser

fromNewUser :: NewUser -> Maybe EvenNewerUser
fromNewUser NewUser {..}
  = case pet of
      Just aPet -> Just (EvenNewerUser firstName surname aPet age)
      Nothing   -> Nothing
