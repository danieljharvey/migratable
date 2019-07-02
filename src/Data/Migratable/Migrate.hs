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
module Data.Migratable.Migrate where

import           Control.Monad
import           Data.Migratable.TypeFamilies
import           Data.Migratable.Versioned
import           GHC.TypeLits

-- Migrate turns an earlier version of the data type into a newer version of
-- the datatype, via all the conversion functions along the way
class Migrate (earliest :: Nat) (target :: Nat) (label :: Symbol) where
  migrate :: earliest `VersionOf` label -> Maybe (target `VersionOf` label)

instance
    ( jobs ~ FindPath earliest target
    , Migrate_ jobs label
    , Last jobs ~ target
    , Head jobs ~ earliest
    )
    => Migrate earliest target label where
  migrate = migrate_ @jobs @label

class Migrate_ (versions :: [Nat]) (label :: Symbol) where
  migrate_
    :: Head versions `VersionOf` label
    -> Maybe (Last versions `VersionOf` label)

instance Migrate_ '[n] label where
  migrate_ = pure

instance {-# OVERLAPPABLE #-}
      ( x ~ (y - 1)
      , Versioned label y
      , Migratable label y
      , Migrate_ (y ': xs) label
      )
      => Migrate_ (x ': y ': xs) label where
  migrate_
    = fromPrevious @label @y
      >=> migrate_ @(y ': xs) @label
