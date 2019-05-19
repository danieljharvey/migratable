{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE UndecidableInstances  #-}

{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE TypeFamilies          #-}
module MigratableSpec where

import           Data.Aeson
import           Data.Maybe
import           Data.Migratable
import           Data.Migratable.Examples
import           Data.Migratable.TypeFamilies
import           Test.Hspec

-- type equivalence tests

data (x :: k) :~: (y :: k) where
  Refl :: x :~: x

testR :: ReversePath 5 10 :~: '[10, 9, 8, 7, 6, 5]
testR = Refl

testR2 :: ReversePath 0 2 :~: '[2, 1, 0]
testR2 = Refl

test0 :: FindPath 5 10 :~: '[5, 6, 7, 8, 9, 10]
test0 = Refl

test1 :: FindPath 5 10 :~: '[5, 6, 7, 8, 9, 10]
test1 = Refl

test2 :: FindPath 2 2 :~: '[2]
test2 = Refl

spec :: SpecWith ()
spec =
  describe "Schema" $ do
    describe "Decoding" $ do
      it "Decodes and converts Older to NewUser" $ do
        let json = encode (Older "a" "b" "c" 100)
        let tryDecoding = decodeVia @"User" @1 @3 json
        isJust tryDecoding `shouldBe` True

      it "Decodes and converts Older to Older" $ do
        let tryDecoding2 = decodeVia @"User" @1 @1 (encode (Older "bo" "f" "f" 5))
        isJust tryDecoding2 `shouldBe` True

      it "Can decode any Schema with WeakSchema" $ do
        let json = encode (Older "don't" "do" "drugs" 5)
        let tryMaybeDecode = tryDecodeVia @"User" @1 @3 json
        isJust tryMaybeDecode `shouldBe` True

      it "Fails to convert a WeakSchema where the data is invalid" $ do
        let json = encode (Older "ham" "man" "wham" 5)
        isJust (tryDecodeVia @"User" @1 @4 json) `shouldBe` False

      it "Succeeds in converting a WeakSchema" $ do
        let json = encode (Older "Me" "Yes" "dog" 5)
        isJust (tryDecodeVia @"User" @1 @4 json) `shouldBe` True

    describe "Converting" $
      it "Updates an old object to new" $ do
        let older = Older "What" "Sure" "great" 5
        let newUser = generallyUpdate @1 @3 @"User" older
        firstName newUser `shouldBe` FirstName (Name "What")
