{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
module LispCaseAeson where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic, Rep)
import qualified Data.Aeson as Aeson
import qualified Data.Char as Char

newtype LispCase a = LispCase a

viaLispCaseAesonOptions :: Aeson.Options
viaLispCaseAesonOptions =
  Aeson.defaultOptions
    { Aeson.fieldLabelModifier = \s -> do
        c <- s
        if Char.isUpper c then ['-', Char.toLower c] else [c]
    }

instance
  ( Generic a
  , Aeson.GToJSON Aeson.Zero (Rep a)
  , Aeson.GToEncoding Aeson.Zero (Rep a)
  ) => ToJSON (LispCase a)
  where
  toJSON (LispCase a) = Aeson.genericToJSON viaLispCaseAesonOptions a
  toEncoding (LispCase a) = Aeson.genericToEncoding viaLispCaseAesonOptions a

instance
  ( Generic a
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  ) => FromJSON (LispCase a)
  where
  parseJSON v = LispCase <$> Aeson.genericParseJSON viaLispCaseAesonOptions v
