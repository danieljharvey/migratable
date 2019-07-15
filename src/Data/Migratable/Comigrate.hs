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
module Data.Migratable.Comigrate where

import           Control.Monad
import           Data.Migratable.TypeFamilies
import           Data.Migratable.Versioned
import           GHC.TypeLits

-- Comigrate turns a version of the data type into an earlier version of
-- the datatype, via all the conversion functions along the way
class Comigrate (target :: Nat) (current :: Nat) (label :: Symbol) where
  comigrate :: current `VersionOf` label -> Maybe (target `VersionOf` label)

instance
    ( jobs ~ ReversePath current target
    , Comigrate_ jobs label
    , Last jobs ~ current
    , Head jobs ~ target
    )
    => Comigrate current target label where
  comigrate = comigrate_ @jobs @label

class Comigrate_ (versions :: [Nat]) (label :: Symbol) where
  comigrate_
    :: Head versions `VersionOf` label
    -> Maybe (Last versions `VersionOf` label)

instance Comigrate_ '[n] label where
  comigrate_ = pure

instance {-# OVERLAPPABLE #-}
      ( x ~ (y + 1)
      , Versioned label y
      , Comigratable label y
      , Comigrate_ (y ': xs) label
      )
      => Comigrate_ (x ': y ': xs) label where
  comigrate_
    = fromNext @label @y
      >=> comigrate_ @(y ': xs) @label
