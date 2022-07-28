-- | This is the internal generic-override-aeson API and should be considered
-- unstable and subject to change. In general, you should prefer to use the
-- public, stable API provided by "Data.Override.Aeson".
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Override.Aeson.Options.Internal where

import Data.Aeson
import Data.Coerce (coerce)
import Data.Override.Aeson.Options.StringModifier.Internal (StringModifiers(stringModifiers))
import Data.Proxy (Proxy(..))
import GHC.Generics (Generic, Rep)
import GHC.TypeLits (KnownSymbol, Symbol, symbolVal)

import qualified Data.Aeson as Aeson

-- | Use with @DerivingVia@ to override Aeson @Options@ with a type-level
-- list of 'AesonOption'.
newtype WithAesonOptions (a :: *) (options :: [AesonOption]) = WithAesonOptions a

instance
  ( ApplyAesonOptions a options
  , Generic a
  , Aeson.GToJSON Aeson.Zero (Rep a)
  , Aeson.GToEncoding Aeson.Zero (Rep a)
  ) => ToJSON (WithAesonOptions a options)
  where
  toJSON = coerce $ genericToJSON @a $ applyAesonOptions @a (Proxy @options) defaultOptions
  toEncoding = coerce $ genericToEncoding @a $ applyAesonOptions @a (Proxy @options) defaultOptions

instance
  ( ApplyAesonOptions a options
  , Generic a
  , Aeson.GFromJSON Aeson.Zero (Rep a)
  ) => FromJSON (WithAesonOptions a options)
  where
  parseJSON = coerce $ genericParseJSON @a $ applyAesonOptions @a (Proxy @options) defaultOptions

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
  | FieldLabelModifier [*] -- ^ Equivalient to @'fieldLabelModifier'
  | ConstructorTagModifier [*] -- ^ Equivalient to @'constructorTagModifier'

-- | Updates 'Options' given a type-level list of 'AesonOption'.
class ApplyAesonOptions a (options :: [AesonOption]) where
  applyAesonOptions :: Proxy options -> Options -> Options

instance ApplyAesonOptions a '[] where
  applyAesonOptions _ = id

instance
  ( ApplyAesonOption a option
  , ApplyAesonOptions a options
  ) => ApplyAesonOptions a (option ': options)
  where
  applyAesonOptions _ =
    applyAesonOption @a (Proxy @option) . (applyAesonOptions @a (Proxy @options))

-- | Updates 'Options' given a single type-level 'AesonOption'.
class ApplyAesonOption a (option :: AesonOption) where
  applyAesonOption :: Proxy option -> Options -> Options

instance ApplyAesonOption a ('AllNullaryToStringTag 'True) where
  applyAesonOption _ o = o { allNullaryToStringTag = True }

instance ApplyAesonOption a ('AllNullaryToStringTag 'False) where
  applyAesonOption _ o = o { allNullaryToStringTag = False }

instance ApplyAesonOption a 'OmitNothingFields where
  applyAesonOption _ o = o { omitNothingFields = True }

instance (KnownSymbol k, KnownSymbol v) => ApplyAesonOption a ('SumEncodingTaggedObject k v) where
  applyAesonOption _ o = o { sumEncoding = TaggedObject (symbolVal (Proxy @k)) (symbolVal (Proxy @v)) }

instance ApplyAesonOption a 'SumEncodingUntaggedValue where
  applyAesonOption _ o = o { sumEncoding = UntaggedValue }

instance ApplyAesonOption a 'SumEncodingObjectWithSingleField where
  applyAesonOption _ o = o { sumEncoding = ObjectWithSingleField }

instance ApplyAesonOption a 'SumEncodingTwoElemArray where
  applyAesonOption _ o = o { sumEncoding = TwoElemArray }

instance ApplyAesonOption a 'UnwrapUnaryRecords where
  applyAesonOption _ o = o { unwrapUnaryRecords = True }

instance ApplyAesonOption a 'TagSingleConstructors where
  applyAesonOption _ o = o { tagSingleConstructors = True }

instance (StringModifiers a fs) => ApplyAesonOption a ('FieldLabelModifier fs) where
  applyAesonOption _ o = o { fieldLabelModifier = stringModifiers @a @fs }

instance (StringModifiers a fs) => ApplyAesonOption a ('ConstructorTagModifier fs) where
  applyAesonOption _ o = o { constructorTagModifier = stringModifiers @a @fs }
