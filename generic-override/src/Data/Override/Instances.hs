-- | Instances for some classes from @base@.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Override.Instances where

import Data.Coerce (Coercible, coerce)
import Data.Function (on)
import GHC.Generics (Generic(Rep, from, to))

import Data.Override.Internal

-- The @foo `on` from'@ idiom is taken from @generic-data@ by Li-yao Xia.
from' :: Generic a => a -> Rep a ()
from' = from

to' :: Generic a => Rep a () -> a
to' = to

-- Eq

instance
  ( Generic (Override a xs)
  , Eq (Rep (Override a xs) ())
  ) => Eq (Override a xs)
  where
  (==) = (==) `on` from'

instance
  ( Coercible a (Using ms a xs)
  , Eq (Using ms a xs)
  ) => Eq (Overridden ms a xs)
  where
  x == y = (==) @(Using ms a xs) (coerce x) (coerce y)

-- Ord

instance
  ( Generic (Override a xs)
  , Ord (Rep (Override a xs) ())
  ) => Ord (Override a xs)
  where
  compare = compare `on` from'

instance
  ( Coercible a (Using ms a xs)
  , Ord (Using ms a xs)
  ) => Ord (Overridden ms a xs)
  where
  compare x y = compare @(Using ms a xs) (coerce x) (coerce y)


-- Semigroup

instance
  ( Generic (Override a xs)
  , Semigroup (Rep (Override a xs) ())
  ) => Semigroup (Override a xs)
  where
  x <> y = to (from' x <> from' y)

instance
  ( Coercible a (Using ms a xs)
  , Semigroup (Using ms a xs)
  ) => Semigroup (Overridden ms a xs)
  where
  x <> y = coerce $ (<>) @(Using ms a xs) (coerce x) (coerce y)

-- Monoid

instance
  ( Generic (Override a xs)
  , Monoid (Rep (Override a xs) ())
  ) => Monoid (Override a xs)
  where
  mempty = to' mempty
  x `mappend` y = to (from' x `mappend` from' y)

instance
  ( Coercible a (Using ms a xs)
  , Monoid (Using ms a xs)
  ) => Monoid (Overridden ms a xs)
  where
  mempty = coerce $ mempty @(Using ms a xs)
  x `mappend` y = coerce $ mappend @(Using ms a xs) (coerce x) (coerce y)
