{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Override.Semigroup where

import Data.Coerce (Coercible, coerce)
import Data.Override.Internal (Overridden(..), Override, Using)
import Data.Override.Semigroup.Internal (GSemigroup(gsappend), GSemigroup', gsappenddefault)
import GHC.Generics (Generic(Rep))

instance (Generic (Override a xs), GSemigroup' (Rep (Override a xs))) => Semigroup (Override a xs) where
  (<>) = gsappenddefault

instance (Coercible a (Using ms a xs), Semigroup (Using ms a xs)) => GSemigroup (Overridden ms a xs) where
  x `gsappend` y = coerce $ (<>) @(Using ms a xs) (coerce x) (coerce y)
