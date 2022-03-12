{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Main where

import Data.Aeson (FromJSON(parseJSON), Result(Success), ToJSON(toJSON), Value, fromJSON)
import Data.Aeson.QQ.Simple (aesonQQ)
import Data.List (reverse)
import Data.Override (Override(Override), As, At, With)
import Data.Override.Aeson (AesonOption(..), WithAesonOptions(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import LispCaseAeson (LispCase(LispCase))
import Test.Hspec
import Text.Read (readMaybe)
import qualified Data.Text as Text

main :: IO ()
main = hspec do
  describe "Override ToJSON machinery" do
    it "Rec1" testRec1
    it "Rec2" testRec2
    it "Rec3" testRec3
    it "Rec4" testRec4
    it "Rec5" testRec5
    it "Rec6" testRec6
    it "Rec7" testRec6
    it "Sum1" testSum1
    it "Options1" testOptions1

newtype Uptext = Uptext { unUptext :: Text }

instance ToJSON Uptext where
  toJSON = toJSON . Text.toUpper . unUptext

newtype Shown a = Shown { unShown :: a }

instance (Show a) => ToJSON (Shown a) where
  toJSON = toJSON . show . unShown
instance (Read a) => FromJSON (Shown a) where
  parseJSON v = do
    s <- parseJSON v
    case readMaybe s of
      Nothing -> fail "read: no parse"
      Just a -> pure $ Shown a

newtype CharArray s = CharArray { unCharArray :: s }
  deriving stock (Functor)

instance ToJSON (CharArray String) where
  toJSON = toJSON . map (:[]) . unCharArray

instance FromJSON (CharArray String) where
  parseJSON v = do
    cs :: [String] <- parseJSON v
    pure $ CharArray $ concat cs

instance ToJSON (CharArray Text) where
  toJSON = toJSON . fmap Text.unpack

-- | Overriding instances by type.
data Rec1 = Rec1
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override Rec1
            '[ Text `As` Uptext
             , Int `As` Shown Int
             ]

testRec1 :: IO ()
testRec1 = do
  toJSON Rec1 { foo = 12, bar = "hi", baz = "bye" }
    `shouldBe` [aesonQQ|
    {
      "foo": "12",
      "bar": "hi",
      "baz": "BYE"
    }
  |]

-- | Overriding instances by field name.
data Rec2 = Rec2
  { foo :: Int
  , bar :: Text
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override Rec2
            '[ "baz" `As` CharArray Text
             , "bar" `As` Uptext
             ]

testRec2 :: IO ()
testRec2 = do
  toJSON Rec2 { foo = 12, bar = "hi", baz = "bye" }
    `shouldBe` [aesonQQ|
    {
      "foo": 12,
      "bar": "HI",
      "baz": ["b", "y", "e"]
    }
  |]

-- | Overriding instances by type and field name.
data Rec3 = Rec3
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override Rec3
            '[ String `As` CharArray String
             , "foo" `As` Shown Int
             , Text `As` Uptext
             ]

testRec3 :: IO ()
testRec3 = do
  toJSON Rec3 { foo = 12, bar = "hi", baz = "bye" }
    `shouldBe` [aesonQQ|
    {
      "foo": "12",
      "bar": ["h", "i"],
      "baz": "BYE"
    }
  |]

-- | Overriding instance by type and field name; first match wins.
-- In this case, 'foo' and 'bar' use 'CharArray Text' and 'baz' uses 'Uptext'.
data Rec4 = Rec4
  { foo :: Text
  , bar :: Text
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override Rec4
            '[ "baz" `As` Uptext
             , Text `As` CharArray Text
             ]

testRec4 :: IO ()
testRec4 = do
  toJSON Rec4 { foo = "go", bar = "hi", baz = "bye" }
    `shouldBe` [aesonQQ|
    {
      "foo": ["g", "o"],
      "bar": ["h", "i"],
      "baz": "BYE"
    }
  |]

-- We can compose an 'Override' with another deriving-via newtype, 'LispCase'.
data Rec5 = Rec5
  { fooBar :: Int
  , baz :: Text
  , quuxSpamEggs :: String
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via LispCase (Override Rec5
            '[ "fooBar" `As` Shown Int
             , Text `As` Uptext
             , "quuxSpamEggs" `As` CharArray String
             ])

testRec5 :: IO ()
testRec5 = do
  toJSON Rec5 { fooBar = 1, baz = "hi", quuxSpamEggs = "bye" }
    `shouldBe` [aesonQQ|
      {
        "foo-bar": "1",
        "baz": "HI",
        "quux-spam-eggs": ["b", "y", "e"]
      }
    |]

-- Test 'Override' for both 'ToJSON' and 'FromJSON'.
data Rec6 = Rec6
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON)
      via Override Rec6
            '[ "foo" `As` Shown Int
             , String `As` CharArray String
             ]

testRec6 :: IO ()
testRec6 = do
  Rec6 { foo = 1, bar = "hi", baz = "bye" }
    `shouldRoundtripAs` [aesonQQ|
      {
        "foo": "1",
        "bar": ["h", "i"],
        "baz": "bye"
      }
    |]

-- Test 'Override' for both 'ToJSON' and 'FromJSON'.
data Rec7 = Rec7
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON, FromJSON)
      via Override Rec7
            '[ "foo" `With` Shown
             , String `With` CharArray
             ]

testRec7 :: IO ()
testRec7 = do
  Rec7 { foo = 1, bar = "hi", baz = "bye" }
    `shouldRoundtripAs` [aesonQQ|
      {
        "foo": "1",
        "bar": ["h", "i"],
        "baz": "bye"
      }
    |]

newtype Reverse a = Reverse [a]

instance (ToJSON a) => ToJSON (Reverse a) where
  toJSON (Reverse xs) = toJSON $ reverse xs

instance (FromJSON a) => FromJSON (Reverse a) where
  parseJSON = fmap (Reverse . reverse) . parseJSON

newtype Not = Not Bool

instance ToJSON Not where
  toJSON (Not b) = toJSON $ not b

instance FromJSON Not where
  parseJSON = fmap (Not . not) . parseJSON

data Sum1 a =
    Sum1List [a]
  | Sum1Trip a Char Bool
  | Sum1Null
  deriving (Show, Eq, Generic)
  deriving (ToJSON, FromJSON)
    via Override (Sum1 a)
          '[ At "Sum1List" 0 (Reverse a)
           , At "Sum1Trip" 2 Not
           ]

testSum1 :: IO ()
testSum1 = do
  Sum1List ['a', 'b'] `shouldRoundtripAs` [aesonQQ|
    {
      "tag": "Sum1List",
      "contents": "ba"
    }
  |]
  Sum1Trip 'a' 'b' True `shouldRoundtripAs` [aesonQQ|
    {
      "tag": "Sum1Trip",
      "contents": ["a", "b", false]
    }
  |]
  Sum1Null @Char `shouldRoundtripAs` [aesonQQ|
    {
      "tag": "Sum1Null"
    }
  |]


data Options1 = Options1
  { foo :: Maybe Int
  , bar :: String
  } deriving (Eq, Show, Generic)
    deriving (FromJSON, ToJSON)
      via Override Options1
            '[ "foo" `As` Maybe (Shown Int) ]
            `WithAesonOptions` '[ 'OmitNothingFields ]

testOptions1 :: IO ()
testOptions1 = do
  Options1 { foo = Nothing, bar = "boo" }
    `shouldRoundtripAs` [aesonQQ| { "bar": "boo" } |]
  Options1 { foo = Just 1, bar = "boo" }
    `shouldRoundtripAs` [aesonQQ| { "foo": "1", "bar": "boo" } |]

shouldRoundtripAs
  :: (ToJSON a, FromJSON a, Eq a, Show a, HasCallStack)
  => a -> Value -> IO ()
shouldRoundtripAs x j = do
  toJSON x `shouldBe` j
  fromJSON j `shouldBe` Success x
