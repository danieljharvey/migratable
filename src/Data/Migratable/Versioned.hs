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
module Data.Migratable.Versioned where

import           Data.Kind    (Type)
import           GHC.TypeLits

-- Create an instance of Versioned for each version of your data type
-- To create a set, label should be the same, and the num value should
-- increment for each new version
class Versioned (label :: Symbol) (num :: Nat) where
  type num `VersionOf` label :: Type

-- Create an instance of Migratable for each new version of your data type,
-- providing it a function that attempts to convert it from the previous
-- version
class Migratable (label :: Symbol) (num :: Nat) where
  fromPrevious
    :: (num - 1) `VersionOf` label
    -> Maybe (num `VersionOf` label)
