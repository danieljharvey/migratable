{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeFamilies          #-}
module Data.Migratable.Examples where

import qualified Data.Aeson                        as JSON
import           Data.Migratable.DecodeAndMigrate  ()
import           Data.Migratable.Migrate           ()
import           Data.Migratable.RequestResponse
import           Data.Migratable.Schema
import           Data.Migratable.Versioned
import qualified Data.Text                         as Text
import           GHC.Generics
import           GHC.Natural
import           Test.QuickCheck
import           Test.QuickCheck.Arbitrary.Generic
import           Test.QuickCheck.Instances.Natural ()
import           Test.QuickCheck.Instances.Text    ()

{-

This is an example data schema that has changed over time.

At each change there is the possibility of failure.

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

instance Arbitrary Older where
  arbitrary = genericArbitrary

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
    deriving (Generic, JSON.FromJSON, JSON.ToJSON)

instance Arbitrary OldUser where
  arbitrary = genericArbitrary

instance Versioned "User" 2 where
  type 2 `VersionOf` "User" = OldUser

instance Migratable "User" 2 where
  fromPrevious = fromOlder

fromOlder :: Older -> Maybe OldUser
fromOlder older
  = Just $ OldUser { oldFirstName = Name (Text.pack (olderFirstName older))
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
  deriving (Generic, JSON.FromJSON, JSON.ToJSON)

instance Arbitrary OldPet where
  arbitrary = genericArbitrary

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

instance Migratable "User" 3 where
  fromPrevious = fromOldUser

instance Arbitrary NewUser where
  arbitrary = genericArbitrary

fromOldUser :: OldUser -> Maybe NewUser
fromOldUser old
  = Just $ NewUser
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

instance Arbitrary Name where
  arbitrary = genericArbitrary

newtype FirstName
  = FirstName { getFirstName :: Name }
  deriving (Show, Eq, Ord, Generic, JSON.FromJSON, JSON.ToJSON)

instance Arbitrary FirstName where
  arbitrary = genericArbitrary

newtype Surname
  = Surname { getSurname :: Name }
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

instance Arbitrary Surname where
  arbitrary = genericArbitrary

data Pet
  = Dog
  | Cat
  | Horse
  deriving (Show, Generic, JSON.FromJSON, JSON.ToJSON)

instance Arbitrary Pet where
  arbitrary = genericArbitrary

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

instance Migratable "User" 4 where
  fromPrevious = fromNewUser

instance Arbitrary EvenNewerUser where
  arbitrary = genericArbitrary

fromNewUser :: NewUser -> Maybe EvenNewerUser
fromNewUser NewUser {..}
  = case pet of
      Just aPet -> Just (EvenNewerUser firstName surname aPet age)
      Nothing   -> Nothing

-- now we can create a newtype that tries parsing any version of our data type
-- to whichever is the current working version
newtype APIUser
  = APIUser { getAPIUser :: EvenNewerUser }

instance JSON.FromJSON APIUser where
  parseJSON a
    = APIUser <$> parseJSONVia @"User" @1 @4 a


-- | Now it turns out we also had some sort of very basic output type to match
-- | each of these user versions
-- | Ideally - we'd like to receive requests in the "User" set of values
-- | but also return responses in the corresponding "UserResponse" values
-- | How can we do this?

data OutputOne
  = OutputOne { thing1 :: String }
  deriving (Eq, Ord, Show)

instance Versioned "UserResponse" 1 where
  type 1 `VersionOf` "UserResponse" = OutputOne

instance Comigratable "UserResponse" 1 where
  fromNext a
    = Just $ OutputOne { thing1 = thing2 a }

---

data OutputTwo
  = OutputTwo { thing2 :: String }
  deriving (Eq, Ord, Show)

instance Versioned "UserResponse" 2 where
  type 2 `VersionOf` "UserResponse" = OutputTwo

instance Comigratable "UserResponse" 2 where
  fromNext a
    = Just $ OutputTwo { thing2 = thing3 a }

---

data OutputThree
  = OutputThree { thing3 :: String }
  deriving (Eq, Ord, Show)

instance Versioned "UserResponse" 3 where
  type 3 `VersionOf` "UserResponse" = OutputThree

instance Comigratable "UserResponse" 3 where
  fromNext a
    = Just $ OutputThree { thing3 = thing4 a }

---

data OutputFour
  = OutputFour { thing4 :: String }
  deriving (Eq, Ord, Show)

instance Versioned "UserResponse" 4 where
  type 4 `VersionOf` "UserResponse" = OutputFour

-- | Here assume 'api' is some sort of effectful thing - probably the mechanics of
-- | your app such as a DB call
-- | We can use migrate and comigrate to bring the data up to
-- | whichever version we want to operate on
-- | then push it back down again
-- |
-- | Note that here the type we accept must be known
-- | This means we know which response type we are going to return if this
-- | works
apiVersionOne :: Older -> IO (Maybe OutputOne)
apiVersionOne
  = profunctorThing @1 @4 @"User" @"UserResponse" api
  where
    api :: EvenNewerUser -> IO OutputFour
    api user = pure $ OutputFour (show (newerFirstName user))
