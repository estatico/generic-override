{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Monoid
import Data.Override
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
    describe "Ord" do
      it "Rec2" testRec2'Ord

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
