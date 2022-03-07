{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Monoid
import Data.Override
import Encode
import GHC.Generics (Generic)
import Test.Hspec

main :: IO ()
main = hspec do
  describe "Override" do
    describe "Semigroup" do
      it "Rec1" testRec1'Semigroup
    describe "Monoid" do
      it "Rec1" testRec1'Monoid
    describe "Eq" do
      it "Rec2" testRec2'Eq
      it "Sum1" testSum1'Eq
    describe "Ord" do
      it "Rec2" testRec2'Ord
    describe "Encode" do
      it "Sum1" testSum1'Encode
      it "Sum2" testSum2'Encode
      it "Sum3" testSum3'Encode

-- | Overriding instances by type.
data Rec1 = Rec1
  { foo :: String
  , bar :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving (Semigroup, Monoid)
      via Override Rec1
            '[ Bool `As` Any
             ]

testRec1'Semigroup :: IO ()
testRec1'Semigroup = do
  Rec1 { foo = "a",  bar = False }
    <> Rec1 { foo = "b",  bar = True }
      `shouldBe` Rec1 { foo = "ab", bar = True }

testRec1'Monoid :: IO ()
testRec1'Monoid = do
  mempty `shouldBe` Rec1 { foo = "", bar = False }
  Rec1 { foo = "a",  bar = False }
    `mappend` Rec1 { foo = "b",  bar = True }
      `shouldBe` Rec1 { foo = "ab", bar = True }

data Rec2 = Rec2 String Int
 deriving stock (Show, Generic)
 deriving (Eq, Ord) via (Override Rec2 '[String `As` ByLength])

newtype ByLength = ByLength { byLength :: String }
  deriving (Show)

instance Eq ByLength where
  ByLength x == ByLength y = length x == length y

instance Ord ByLength where
  ByLength x <= ByLength y = length x <= length y

testRec2'Eq :: IO ()
testRec2'Eq = do
  (Rec2 "foo" 0 == Rec2 "bar" 0) `shouldBe` True
  (Rec2 "foo" 0 == Rec2 "bar" 1) `shouldBe` False
  (Rec2 ""    0 == Rec2 "bar" 0) `shouldBe` False

testRec2'Ord :: IO ()
testRec2'Ord = do
  (Rec2 "foo" 0 <= Rec2 "bar" 0) `shouldBe` True
  (Rec2 "foo" 1 <= Rec2 "bar" 0) `shouldBe` False
  (Rec2 ""    0 >= Rec2 "bar" 0) `shouldBe` False

-- | Override support for sum types.
data Sum1 = Sum1String String | Sum1Int Int
  deriving stock (Show, Generic)
  deriving (Eq) via (Override Sum1 '[String `As` ByLength])
  deriving (Encode) via (Override Sum1 '[String `As` ListOf Char])

testSum1'Eq :: IO ()
testSum1'Eq = do
  (Sum1String "foo" == Sum1String "bar") `shouldBe` True
  (Sum1Int 3 == Sum1Int 3) `shouldBe` True
  (Sum1String "foo" == Sum1String "ba") `shouldBe` False
  (Sum1String "foo" == Sum1Int 3) `shouldBe` False
  (Sum1Int 3 == Sum1Int 2) `shouldBe` False

testSum1'Encode :: IO ()
testSum1'Encode = do
  encode (Sum1String "foo") `shouldBe` "Sum1String:f,o,o"
  encode (Sum1Int 1) `shouldBe` "Sum1Int:1"

-- | Override using 'As' which includes a type variable.
data Sum2 a = Sum2List [a] | Sum2Null
  deriving stock (Show, Eq, Generic)
  deriving (Encode) via (Override (Sum2 a) '[[a] `As` ListOf a])

testSum2'Encode :: IO ()
testSum2'Encode = do
  encode (Sum2List [1, 2, 3 :: Int]) `shouldBe` "Sum2List:1,2,3"
  encode (Sum2Null @Int) `shouldBe` "Sum2Null:"

-- | Override using 'As' which uses a kind of @* -> *@. Convenient
-- so you don't have to apply the type parameter.
data Sum3 a = Sum3List [a] | Sum3Null
  deriving stock (Show, Eq, Generic)
  deriving (Encode) via (Override (Sum3 a) '[[] `As` ListOf])

testSum3'Encode :: IO ()
testSum3'Encode = do
  encode (Sum3List [1, 2, 3 :: Int]) `shouldBe` "Sum3List:1,2,3"
  encode (Sum3Null @Int) `shouldBe` "Sum3Null:"
