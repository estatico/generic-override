{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Override.Monoid where

import Data.Coerce (Coercible, coerce)
import Data.Override.Internal (Overridden(..), Override, Using)
import Data.Override.Monoid.Internal (GMonoid(gmappend, gmempty), GMonoid', gmemptydefault)
import Data.Override.Semigroup ()
import Data.Override.Semigroup.Internal (GSemigroup(gsappend))
import GHC.Generics (Generic(Rep))

instance (Generic (Override a xs), GMonoid' (Rep (Override a xs))) => Monoid (Override a xs) where
  mempty = gmemptydefault

instance (Coercible a (Using ms a xs), Monoid (Using ms a xs)) => GMonoid (Overridden ms a xs) where
  gmempty = coerce $ mempty @(Using ms a xs)
  gmappend = gsappend
