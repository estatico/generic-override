{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Monoid
import Data.Override
import Data.Override.Monoid ()
import GHC.Generics (Generic)
import Test.Hspec

main :: IO ()
main = hspec do
  describe "Override machinery" do
    it "Rec1" testRec1

-- | Overriding instances by type.
data Rec1 = Rec1
  { foo :: String
  , bar :: Bool
  } deriving stock (Show, Eq, Generic)
    deriving (Semigroup, Monoid)
      via Override Rec1
            '[ Bool `As` Any
             ]

testRec1 :: IO ()
testRec1 = do
  mempty `shouldBe` Rec1 { foo = "", bar = False }
  Rec1 { foo = "a",  bar = False }
    <> Rec1 { foo = "b",  bar = True }
      `shouldBe` Rec1 { foo = "ab", bar = True }
