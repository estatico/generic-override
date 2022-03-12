{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
module Encode where

import Data.List (intercalate)
import Data.Override (Override)
import GHC.Generics

class Encode a where
  encode :: a -> String
  default encode :: (Generic a, GEncode (Rep a)) => a -> String
  encode = gencode . from

instance
  ( Generic (Override a xs)
  , GEncode (Rep (Override a xs))
  ) => Encode (Override a xs)

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

-- | Create an overlapping instance to verify that overriding with
-- 'ListOf' avoids this instance.
instance {-# OVERLAPPING #-} Encode String where
  encode = id

deriving via (ListOf a) instance (Encode a) => Encode [a]

newtype ListOf a = ListOf { unListOf :: [a] }

instance (Encode a) => Encode (ListOf a) where
  encode = intercalate "," . map encode . unListOf
