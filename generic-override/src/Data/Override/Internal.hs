-- | This is the internal generic-override API and should be considered
-- unstable and subject to change. This module is exposed for library integrators
-- (e.g. generic-override-aeson). In general, unless you are integrating
-- some type class with generic-override, you should prefer to use the
-- public, stable API provided by 'Data.Override'.
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Override.Internal where

import GHC.Generics
import GHC.TypeLits

-- | The feature of this library. For use with DerivingVia.
-- Apply it to a type @a@ and supply a type-level list of instance
-- overrides @xs@.
newtype Override a (xs :: [*]) = Override a

-- | Unwrap an 'Override' value.
unOverride :: Override a xs -> a
unOverride (Override a) = a

-- | Construct an 'Override' using a proxy of overrides.
override :: a -> proxy xs -> Override a xs
override a _ = Override a

-- | Used to construct a type-level override. Usually used infix.
-- The @o@ should be either a type (kind '*') or a type-level string
-- (kind 'Symbol').
data As (o :: k) n

-- | Used to wrap a field into a something of kind @* -> *@, for example another newtype.
data With (o :: k) (w :: * -> *)

-- | Used to construct a type-level override for the given constructor
-- name @c@ and parameter index @n@.
data At (c :: Symbol) (p :: Nat) n

-- | Used at the leaf nodes of a generic 'Rep'
newtype Overridden (i :: Inspect) a (xs :: [*]) = Overridden a

-- | Unwrap an 'Overridden' value.
unOverridden :: Overridden ms a xs -> a
unOverridden (Overridden a) = a

-- | Same as 'override' but for 'Overridden' types.
overridden
  :: forall a (i :: Inspect) (xs :: [*]) proxy0 proxy1.
     a -> proxy0 i -> proxy1 xs -> Overridden i a xs
overridden a _ _ = Overridden a

instance
  ( Generic a
  , GOverride xs (Rep a)
  ) => Generic (Override a xs)
  where
  type Rep (Override a xs) = OverrideRep EmptyInspect xs (Rep a)
  from = overrideFrom @EmptyInspect @xs . from . unOverride
  to = Override . to . overrideTo @EmptyInspect @xs

type GOverride = GOverride' EmptyInspect

-- | Type class used to build the 'Generic' instance for 'Override'.
class GOverride' (i :: Inspect) (xs :: [*]) (f :: * -> *) where
  type OverrideRep i xs f :: * -> *
  overrideFrom :: f x -> OverrideRep i xs f x
  overrideTo :: OverrideRep i xs f x -> f x

instance (GOverride' i xs f) => GOverride' i xs (M1 D c f) where
  type OverrideRep i xs (M1 D c f) = M1 D c (OverrideRep i xs f)
  overrideFrom (M1 x) = M1 (overrideFrom @i @xs x)
  overrideTo (M1 x) = M1 (overrideTo @i @xs x)

instance
  ( GOverride' ('Inspect ('Just conName) ms mp) xs f
  ) => GOverride' ('Inspect ignore ms mp) xs
        (M1 C ('MetaCons conName conFixity conIsRecord) f)
  where
  type OverrideRep ('Inspect ignore ms mp) xs
        (M1 C ('MetaCons conName conFixity conIsRecord) f) =
          M1 C
            ('MetaCons conName conFixity conIsRecord)
            (OverrideRep ('Inspect ('Just conName) ms mp) xs f)

  overrideFrom (M1 x) = M1 (overrideFrom @('Inspect ('Just conName) ms mp) @xs x)
  overrideTo (M1 x) = M1 (overrideTo @('Inspect ('Just conName) ms mp) @xs x)

instance
  ( GOverride' ('Inspect mc ms ('Just 0)) xs f
  , GOverride' ('Inspect mc ms ('Just 1)) xs g
  ) => GOverride' ('Inspect mc ms 'Nothing) xs (f :*: g)
  where
  type OverrideRep ('Inspect mc ms 'Nothing) xs (f :*: g) =
    OverrideRep ('Inspect mc ms ('Just 0)) xs f
      :*: OverrideRep ('Inspect mc ms ('Just 1)) xs g

  overrideFrom (f :*: g) =
    overrideFrom @('Inspect mc ms ('Just 0)) @xs f
      :*: overrideFrom @('Inspect mc ms ('Just 1)) @xs g

  overrideTo (f :*: g) =
    overrideTo @('Inspect mc ms ('Just 0)) @xs f
      :*: overrideTo @('Inspect mc ms ('Just 1)) @xs g

instance
  ( GOverride' ('Inspect mc ms ('Just p)) xs f
  , GOverride' ('Inspect mc ms ('Just (p + 1))) xs g
  ) => GOverride' ('Inspect mc ms ('Just p)) xs (f :*: g)
  where
  type OverrideRep ('Inspect mc ms ('Just p)) xs (f :*: g) =
    OverrideRep ('Inspect mc ms ('Just p)) xs f
      :*: OverrideRep ('Inspect mc ms ('Just (p + 1))) xs g

  overrideFrom (f :*: g) =
    overrideFrom @('Inspect mc ms ('Just p)) @xs f
      :*: overrideFrom @('Inspect mc ms ('Just (p + 1))) @xs g

  overrideTo (f :*: g) =
    overrideTo @('Inspect mc ms ('Just p)) @xs f
      :*: overrideTo @('Inspect mc ms ('Just (p + 1))) @xs g

instance
  ( GOverride' i xs f
  , GOverride' i xs g
  ) => GOverride' i xs (f :+: g)
  where
  type OverrideRep i xs (f :+: g) = OverrideRep i xs f :+: OverrideRep i xs g

  overrideFrom = \case
    L1 f -> L1 $ overrideFrom @i @xs f
    R1 g -> R1 $ overrideFrom @i @xs g

  overrideTo = \case
    L1 f -> L1 $ overrideTo @i @xs f
    R1 g -> R1 $ overrideTo @i @xs g

instance
  ( GOverride' ('Inspect mc selName mp) xs f
  ) => GOverride' ('Inspect mc ignore mp) xs (M1 S ('MetaSel selName selSU selSS selDS) f)
  where
  type OverrideRep ('Inspect mc ignore mp) xs (M1 S ('MetaSel selName selSU selSS selDS) f) =
    M1 S ('MetaSel selName selSU selSS selDS) (OverrideRep ('Inspect mc selName mp) xs f)

  overrideFrom (M1 x) = M1 (overrideFrom @('Inspect mc selName mp) @xs x)
  overrideTo (M1 x) = M1 (overrideTo @('Inspect mc selName mp) @xs x)

instance GOverride' ('Inspect mc ms 'Nothing) xs (K1 R a) where
  type OverrideRep ('Inspect mc ms 'Nothing) xs (K1 R a) =
    K1 R (Overridden ('Inspect mc ms ('Just 0)) a xs)

  overrideFrom (K1 a) = K1 (Overridden @('Inspect mc ms ('Just 0)) @a @xs a)

  overrideTo (K1 (Overridden a)) = K1 a

instance GOverride' ('Inspect mc ms ('Just p)) xs (K1 R a) where
  type OverrideRep ('Inspect mc ms ('Just p)) xs (K1 R a) =
    K1 R (Overridden ('Inspect mc ms ('Just p)) a xs)

  overrideFrom (K1 a) = K1 (Overridden @('Inspect mc ms ('Just p)) @a @xs a)

  overrideTo (K1 (Overridden a)) = K1 a

instance GOverride' i xs U1 where
  type OverrideRep i xs U1 = U1
  overrideFrom U1 = U1
  overrideTo U1 = U1

data Inspect =
  Inspect
    (Maybe Symbol) -- ^ Constructor name
    (Maybe Symbol) -- ^ Selector name
    (Maybe Nat)    -- ^ Selector index

type EmptyInspect = 'Inspect 'Nothing 'Nothing 'Nothing

-- | Type family used to determine which override from @xs@
-- to replace @a@ with, if any. The @ms@ holds the field name
-- for @a@, if applicable.
type family Using (i :: Inspect) (a :: *) (xs :: [*]) where
  -- No matching override found.
  Using i a '[] = a

  -- Override the matching field.
  Using ('Inspect mc ('Just o) mp) a (As o n ': xs) = n
  Using ('Inspect mc ('Just o) mp) a (With o w ': xs) = w a

  -- Override the matching type.
  Using i a (With a w ': xs) = w a

  -- Override the matching type (type arity 0-10).
  Using i a (As a n ': xs) = n
  Using i (f a0) (As f g ': xs) = g a0
  Using i (f a0 a1) (As f g ': xs) = g a0 a1
  Using i (f a0 a1 a2) (As f g ': xs) = g a0 a1 a2
  Using i (f a0 a1 a2 a3) (As f g ': xs) = g a0 a1 a2 a3
  Using i (f a0 a1 a2 a3 a4) (As f g ': xs) = g a0 a1 a2 a3 a4
  Using i (f a0 a1 a2 a3 a4 a5) (As f g ': xs) = g a0 a1 a2 a3 a4 a5
  Using i (f a0 a1 a2 a3 a4 a5 a6) (As f g ': xs) = g a0 a1 a2 a3 a4 a5 a6
  Using i (f a0 a1 a2 a3 a4 a5 a6 a7) (As f g ': xs) = g a0 a1 a2 a3 a4 a5 a6 a7
  Using i (f a0 a1 a2 a3 a4 a5 a6 a7 a8) (As f g ': xs) = g a0 a1 a2 a3 a4 a5 a6 a7 a8
  Using i (f a0 a1 a2 a3 a4 a5 a6 a7 a8 a9) (As f g ': xs) = g a0 a1 a2 a3 a4 a5 a6 a7 a8 a9

  -- Override the matching constructor parameter.
  Using ('Inspect ('Just c) ms ('Just p)) a (At c p n ': xs) = n

  -- No match on this override, recurse.
  Using i a (x ': xs) = Using i a xs
