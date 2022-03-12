{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Override.Aeson
  ( WithAesonOptions(..)
  , AesonOption(..)
  ) where

import Data.Override (Override(..))
import Data.Override.Aeson.Options.Internal (AesonOption(..), WithAesonOptions(..))
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
