{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Override.Internal where

import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.Generics
import GHC.TypeLits (Symbol)

newtype Override a (xs :: [*]) = Override a
  deriving stock (Show, Eq)

-- | Unwrap an 'Override' value.
unOverride :: Override a xs -> a
unOverride (Override a) = a

-- | Construct an 'Override' using a proxy of overrides.
override :: a -> proxy xs -> Override a xs
override a _ = Override a

data As (o :: k) n

newtype Overridden (ms :: Maybe Symbol) a (xs :: [*]) = Overridden a

-- | Unwrap an 'Overridden' value.
unOverridden :: Overridden ms a xs -> a
unOverridden (Overridden a) = a

overridden
  :: forall a (ms :: Maybe Symbol) (xs :: [*]) proxy0 proxy1.
     a -> proxy0 ms -> proxy1 xs -> Overridden ms a xs
overridden a _ _ = Overridden a

instance (Generic a, GOverride xs (Rep a)) => Generic (Override a xs) where
  type Rep (Override a xs) = OverrideRep xs (Rep a)
  from = overrideFrom @xs . from . unOverride
  to = Override . to . overrideTo @xs

-- | Type class used to build the 'Generic' instance for 'Override'.
class GOverride (xs :: [*]) (f :: * -> *) where
  -- | Analogous to 'Rep'; rewrites the type for a given 'Rep' and injects
  -- 'Overridden' at the leaves.
  type OverrideRep xs f :: * -> *
  overrideFrom :: f x -> OverrideRep xs f x
  overrideTo :: OverrideRep xs f x -> f x

instance (GOverride xs f) => GOverride xs (M1 D c f) where
  type OverrideRep xs (M1 D c f) = M1 D c (OverrideRep xs f)
  overrideFrom (M1 x) = M1 (overrideFrom @xs x)
  overrideTo (M1 x) = M1 (overrideTo @xs x)

instance (GOverride xs f) => GOverride xs (M1 C c f) where
  type OverrideRep xs (M1 C c f) = M1 C c (OverrideRep xs f)
  overrideFrom (M1 x) = M1 (overrideFrom @xs x)
  overrideTo (M1 x) = M1 (overrideTo @xs x)

instance (GOverride xs f, GOverride xs g) => GOverride xs (f :*: g) where
  type OverrideRep xs (f :*: g) = OverrideRep xs f :*: OverrideRep xs g
  overrideFrom (f :*: g) = overrideFrom @xs f :*: overrideFrom @xs g
  overrideTo (f :*: g) = overrideTo @xs f :*: overrideTo @xs g

instance GOverride xs (M1 S ('MetaSel ms su ss ds) (K1 R c)) where
  type OverrideRep xs (M1 S ('MetaSel ms su ss ds) (K1 R c)) =
    M1 S ('MetaSel ms su ss ds) (K1 R (Overridden ms c xs))
  overrideFrom (M1 (K1 x)) = M1 (K1 (Overridden @ms x))
  overrideTo (M1 (K1 (Overridden x))) = M1 (K1 x)

-- | Type family used to determine which override from 'xs'
-- to replace 'x' with, if any. The 'ms' holds the field name
-- for 'x', if applicable.
type family Using (ms :: Maybe Symbol) (x :: *) (xs :: [*]) where
  -- No matching override found.
  Using ms x '[] = x

  -- Override the matching field.
  Using ms x (As (o :: Symbol) n ': xs) =
    If (ms == 'Just o) n (Using ms x xs)

  -- Override the matching type.
  Using ms x (As (o :: *) n ': xs) =
    If (x == o) n (Using ms x xs)
