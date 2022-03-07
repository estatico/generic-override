{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Encode where

import Data.Coerce
import Data.List
import Data.Override.Internal
import GHC.Generics

class Encode a where
  encode :: a -> String
  default encode :: (Generic a, GEncode (Rep a)) => a -> String
  encode = gencode . from

instance
  ( Generic a
  , GOverride xs (Rep a)
  , GEncode (Rep (Override a xs))
  ) => Encode (Override a xs)

instance
  ( u ~ Using ms a xs
  , Coercible a u
  , Encode u
  ) => Encode (Overridden ms a xs)
  where
  encode = encode @u . coerce

class GEncode f where
  gencode :: f a -> String

instance (GEncode f) => GEncode (M1 D x f) where
  gencode (M1 f) = gencode f

instance (GEncode f, Constructor c) => GEncode (M1 C c f) where
  gencode m@(M1 f) = conName m <> ":" <> gencode f

instance (GEncode f, GEncode g) => GEncode (f :*: g) where
  gencode (f :*: g) = gencode f <> "," <> gencode g

instance (GEncode f, GEncode g) => GEncode (f :+: g) where
  gencode = \case
    L1 f -> gencode f
    R1 g -> gencode g

instance (GEncode f, Selector s) => GEncode (M1 S s f) where
  gencode m@(M1 f) =
    if null (selName m) then
      gencode f
    else
      selName m <> "=" <> gencode f

instance (Encode a) => GEncode (K1 R a) where
  gencode (K1 a) = encode a

instance GEncode U1 where
  gencode _ = ""

instance Encode Int where
  encode = show

instance Encode Char where
  encode = pure

instance {-# OVERLAPPING #-} Encode String where
  encode = id

instance (Encode a) => Encode [a] where
  encode = intercalate "," . map encode

newtype ListOf a = ListOf { unListOf :: [a] }

instance (Encode a) => Encode (ListOf a) where
  encode = intercalate "," . map encode . unListOf
