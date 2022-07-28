-- | This is an internal API and should be considered
-- unstable and subject to change. In general, you should prefer
-- to use the public, stable API provided by
-- "Data.Override.Aeson.Options.StringModifier".
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Data.Override.Aeson.Options.StringModifier.Internal where

-- | Allows for specifying a type to represent a string modification.
class StringModifier a f where
  stringModifier :: String -> String

-- | Specify a list of types in order to compose many string modifications.
-- Evaluates each function from right-to-left.
class StringModifiers a (fs :: [*]) where
  stringModifiers :: String -> String

instance StringModifiers a '[] where
  stringModifiers s = s

instance
  ( StringModifier a f
  , StringModifiers a fs
  ) => StringModifiers a (f ': fs)
  where
  stringModifiers = stringModifier @a @f . stringModifiers @a @fs
