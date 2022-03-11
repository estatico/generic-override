-- | This module contains only orphan instances. It is only needed to
-- be imported where you are overriding instances for aeson generic derivation.

{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Override.Aeson where

import Data.Coerce (Coercible, coerce)
import Data.Override.Internal (Override, Using)
import GHC.Generics (Generic, Rep)
import qualified Data.Aeson as Aeson

instance
  ( Generic (Override a xs)
  , Aeson.GToJSON Aeson.Zero (Rep (Override a xs))
  , Aeson.GToEncoding Aeson.Zero (Rep (Override a xs))
  ) => Aeson.ToJSON (Override a xs)

instance
  ( Generic (Override a xs)
  , Aeson.GFromJSON Aeson.Zero (Rep (Override a xs))
  ) => Aeson.FromJSON (Override a xs)
