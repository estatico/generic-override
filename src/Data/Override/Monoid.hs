{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Override.Monoid where

import Data.Coerce (Coercible, coerce)
import Data.Override.Internal (Overridden(..), Override, Using)
import GHC.Generics (Generic(Rep))
import Generics.Deriving.Monoid (GMonoid(gmappend, gmempty), GMonoid', gmemptydefault)
import Generics.Deriving.Semigroup (GSemigroup(gsappend), GSemigroup', gsappenddefault)

instance (Generic (Override a xs), GSemigroup' (Rep (Override a xs))) => Semigroup (Override a xs) where
  (<>) = gsappenddefault

instance (Generic (Override a xs), GMonoid' (Rep (Override a xs))) => Monoid (Override a xs) where
  mempty = gmemptydefault

instance (Coercible a (Using ms a xs), Semigroup (Using ms a xs)) => GSemigroup (Overridden ms a xs) where
  x `gsappend` y = coerce $ (<>) @(Using ms a xs) (coerce x) (coerce y)

instance (Coercible a (Using ms a xs), Monoid (Using ms a xs)) => GMonoid (Overridden ms a xs) where
  gmempty = coerce $ mempty @(Using ms a xs)
  gmappend = gsappend
