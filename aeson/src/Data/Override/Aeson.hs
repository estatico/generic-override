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
import Data.Override (Override, Overridden(Overridden), Using)
import GHC.Generics (Generic, Rep)
import qualified Data.Aeson as Aeson

instance
  ( Generic (Override a xs)
  , Aeson.GToJSON Aeson.Zero (Rep (Override a xs))
  , Aeson.GToEncoding Aeson.Zero (Rep (Override a xs))
  ) => Aeson.ToJSON (Override a xs)

instance
  ( Coercible a (Using ms a xs)
  , Aeson.ToJSON (Using ms a xs)
  ) => Aeson.ToJSON (Overridden ms a xs)
  where
  toJSON = Aeson.toJSON @(Using ms a xs) . coerce
  toEncoding = Aeson.toEncoding @(Using ms a xs) . coerce

instance
  ( Generic (Override a xs)
  , Aeson.GFromJSON Aeson.Zero (Rep (Override a xs))
  ) => Aeson.FromJSON (Override a xs)

instance
  ( Coercible a (Using ms a xs)
  , Aeson.FromJSON (Using ms a xs)
  ) => Aeson.FromJSON (Overridden ms a xs)
  where
  parseJSON = coerce . Aeson.parseJSON @(Using ms a xs)
