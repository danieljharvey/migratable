{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Migratable.TypeFamilies where

import           GHC.Natural
import           GHC.TypeLits

type family (xs :: [k]) ++ (ys :: [k]) :: [k] where
  '[] ++ ys = ys
  (x ': xs) ++ ys = x ': (xs ++ ys)

type family Head (xs :: [k]) :: k where
  Head (x ': _) = x

type family Last (xs :: [k]) :: k where
  Last '[x] = x
  Last (_ ': xs) = Last xs

type family ReverseAcc xs acc where
    ReverseAcc '[] acc = acc
    ReverseAcc (x ': xs) acc = ReverseAcc xs (x ': acc)

type family Reverse xs where
    Reverse xs = ReverseAcc xs '[]

type family ReversePath (m :: Nat) (n :: Nat) :: [Nat] where
  ReversePath m n = Reverse (FindPath m n)

type family FindPath (m :: Nat) (n :: Nat) :: [Nat] where
  FindPath m m = '[m]
  FindPath m n = m ': FindPath (m + 1) n
