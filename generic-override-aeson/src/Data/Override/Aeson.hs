-- | The public, stable @generic-override-aeson@ API.
-- Provides orphan instances for 'Override' as well as customization
-- for aeson's 'Options' when using @DerivingVia@.
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Override.Aeson
  ( WithAesonOptions(..)
  , AesonOption(..)
  ) where

import Data.Aeson
import Data.Override (Override(..))
import Data.Override.Aeson.Options.Internal (AesonOption(..), WithAesonOptions(..))
import GHC.Generics (Generic, Rep)

instance
  ( Generic (Override a xs)
  , GToJSON Zero (Rep (Override a xs))
  , GToEncoding Zero (Rep (Override a xs))
  ) => ToJSON (Override a xs)

instance
  ( Generic (Override a xs)
  , GFromJSON Zero (Rep (Override a xs))
  ) => FromJSON (Override a xs)
