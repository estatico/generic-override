-- | This is the internal generic-override-aeson API and should be considered
-- unstable and subject to change. In general, you should prefer to use the
-- public, stable API provided by "Data.Override.Aeson".
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Override.Aeson.Options.Internal where

import Data.Aeson
import Data.Coerce (coerce)
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Aeson as Aeson

-- | Use with @DerivingVia@ to override Aeson @Options@ with a type-level
-- list of 'AesonOption'.
newtype WithAesonOptions (a :: *) (options :: [AesonOption]) = WithAesonOptions a

instance
  ( ApplyAesonOptions options
  , Generic a
  , Aeson.GToJSON Aeson.Zero (Rep a)
  , Aeson.GToEncoding Aeson.Zero (Rep a)
  ) => ToJSON (WithAesonOptions a options)
  where
  toJSON = coerce $ genericToJSON @a $ applyAesonOptions (Proxy @options) defaultOptions
  toEncoding = coerce $ genericToEncoding @a $ applyAesonOptions (Proxy @options) defaultOptions

instance
  ( ApplyAesonOptions options
  , Generic a
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  ) => FromJSON (WithAesonOptions a options)
  where
  parseJSON = coerce $ genericParseJSON @a $ applyAesonOptions (Proxy @options) defaultOptions

-- | Provides a type-level subset of fields from 'Options'
data AesonOption =
    AllNullaryToStringTag Bool -- ^ Equivalient to @'allNullaryToStringTag' = b@
  | OmitNothingFields -- ^ Equivalient to @'omitNothingFields' = True@
  | SumEncodingTaggedObject Symbol Symbol -- ^ Equivalient to @'sumEncoding' = 'TaggedObject' k v@
  | SumEncodingUntaggedValue -- ^ Equivalient to @'sumEncoding' = 'UntaggedValue'@
  | SumEncodingObjectWithSingleField -- ^ Equivalient to @'sumEncoding' = 'ObjectWithSingleField'@
  | SumEncodingTwoElemArray -- ^ Equivalient to @'sumEncoding' = 'TwoElemArray'@
  | UnwrapUnaryRecords -- ^ Equivalient to @'unwrapUnaryRecords' = True@
  | TagSingleConstructors -- ^ Equivalient to @'tagSingleConstructors' = True@

-- | Updates 'Options' given a type-level list of 'AesonOption'.
class ApplyAesonOptions (options :: [AesonOption]) where
  applyAesonOptions :: Proxy options -> Options -> Options

instance ApplyAesonOptions '[] where
  applyAesonOptions _ = id

instance
  ( ApplyAesonOption option
  , ApplyAesonOptions options
  ) => ApplyAesonOptions (option ': options)
  where
  applyAesonOptions _ =
    applyAesonOption (Proxy @option) . (applyAesonOptions (Proxy @options))

-- | Updates 'Options' given a single type-level 'AesonOption'.
class ApplyAesonOption (option :: AesonOption) where
  applyAesonOption :: Proxy option -> Options -> Options

instance ApplyAesonOption ('AllNullaryToStringTag 'True) where
  applyAesonOption _ o = o { allNullaryToStringTag = True }

instance ApplyAesonOption ('AllNullaryToStringTag 'False) where
  applyAesonOption _ o = o { allNullaryToStringTag = False }

instance ApplyAesonOption 'OmitNothingFields where
  applyAesonOption _ o = o { omitNothingFields = True }

instance (KnownSymbol k, KnownSymbol v) => ApplyAesonOption ('SumEncodingTaggedObject k v) where
  applyAesonOption _ o = o { sumEncoding = TaggedObject (symbolVal (Proxy @k)) (symbolVal (Proxy @v)) }

instance ApplyAesonOption 'SumEncodingUntaggedValue where
  applyAesonOption _ o = o { sumEncoding = UntaggedValue }

instance ApplyAesonOption 'SumEncodingObjectWithSingleField where
  applyAesonOption _ o = o { sumEncoding = ObjectWithSingleField }

instance ApplyAesonOption 'SumEncodingTwoElemArray where
  applyAesonOption _ o = o { sumEncoding = TwoElemArray }

instance ApplyAesonOption 'UnwrapUnaryRecords where
  applyAesonOption _ o = o { unwrapUnaryRecords = True }

instance ApplyAesonOption 'TagSingleConstructors where
  applyAesonOption _ o = o { tagSingleConstructors = True }
