-- | String modifiers at the type level. Useful in conjunction with
-- 'Data.Override.Aeson.FieldLabelModifier' and 
-- 'Data.Override.Aeson.ConstructorTagModifier'.
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Override.Aeson.Options.StringModifier where

import Data.Char (toLower, toUpper)
import Data.Override.Aeson.Options.StringModifier.Internal
  ( StringModifier(stringModifier), StringModifiers(stringModifiers)
  )
import Data.Proxy (Proxy(..))
import GHC.Generics (Meta(MetaData), D1, Generic, Rep)
import GHC.TypeLits (KnownNat, KnownSymbol, Nat, natVal, symbolVal)

-- | Represents 'toUpper' as a 'StringModifier'.
data ToUpper
instance StringModifier a ToUpper where stringModifier = map toUpper

-- | Represents 'toLower' as a 'StringModifier'.
data ToLower
instance StringModifier a ToLower where stringModifier = map toLower

-- | Represents 'drop' as a 'StringModifier'.
data Drop (n :: Nat)
instance (KnownNat n) => StringModifier a (Drop n) where
  stringModifier = drop $ fromIntegral $ natVal $ Proxy @n

-- | Represents 'take' as a 'StringModifier'.
data Take (n :: Nat)
instance (KnownNat n) => StringModifier a (Take n) where
  stringModifier = take $ fromIntegral $ natVal $ Proxy @n

-- | Represents 'atSubstr' as a 'StringModifier'.
data AtSubstr (i :: Nat) (n :: Nat) (fs :: [*])
instance
  ( KnownNat i
  , KnownNat n
  , StringModifiers a fs
  ) => StringModifier a (AtSubstr i n fs)
  where
  stringModifier =
    atSubstr
      (fromIntegral $ natVal $ Proxy @i)
      (fromIntegral $ natVal $ Proxy @n)
      (stringModifiers @a @fs)

-- | Modify only the substring at the given slice, leaving the remaining part
-- of the string unchanged.
atSubstr :: Int -> Int -> (String -> String) -> String -> String
atSubstr i n f s = before <> f at <> after
  where
  (before, (at, after)) = splitAt n <$> splitAt i s

-- | Special case of 'AtSubstr' which only modifies the head character of a
-- string.
type AtHead = AtSubstr 0 1

-- | Automatically strip the type name (inferred from @a@) from the string.
--
-- Example:
--
-- > data Foo = Foo { fooBar :: Int, fooBaz :: Bool }
-- >   deriving stock (Generic)
-- >   deriving (ToJSON) via Override Foo '[ AutoStripDataPrefix ]
--
-- > > putStrLn $ encode $ Foo 1 True
-- > { "bar": 1, "baz": true }
data AutoStripDataPrefix
instance
  ( Generic a
  , Rep a ~ D1 ('MetaData n m p nt) f
  , KnownSymbol n
  ) => StringModifier a AutoStripDataPrefix
  where
  stringModifier s =
    case splitAt (length prefix) s of
      (p, c : rest) | p == prefix -> toLower c : rest
      _ -> s
    where
    prefix =
      case symbolVal $ Proxy @n of
        c : rest -> toLower c : rest
        [] -> [] -- Impossible but no need to error
