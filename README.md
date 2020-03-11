# generic-override

For the associated blog post describing how this works, see
[Overriding Type Class Instances](http://caryrobbins.com/dev/overriding-type-class-instances-2/).

-------------------------

This library provides the ability to override instances used by generic derivation.

## Example

```haskell
-- Needed for constructing our 'Override' type in the DerivingVia clause.
import Data.Override (Override(Override), As)
-- Provides aeson support for generic-override (lives in generic-override-aeson).
import Data.Override.Aeson ()
-- Basic imports we need for the example.
import Data.Aeson (ToJSON(toJSON))
import Data.Text (Text)
import qualified Data.Text as Text

-- | A simple record type. We'll use generic derivation for the 'ToJSON' instance
-- but override the instances used by derivation for the 'String' and 'baz'
-- fields.
data MyRec = MyRec
  { foo :: Int
  , bar :: String
  , baz :: Text
  } deriving stock (Show, Eq, Generic)
    deriving (ToJSON)
      via Override MyRec
            '[ -- Derive the 'String' field via 'CharArray'.
               String `As` CharArray
               -- Derive the 'baz' field via 'Uptext'.
             , "baz" `As` Uptext
             ]

-- | Newtype wrapper to override 'ToJSON Text' instances with ones that
-- encode the 'Text' as uppercase.
newtype Uptext = Uptext { unUptext :: Text }

instance ToJSON Uptext where
  toJSON = toJSON . Text.toUpper . unUptext

-- | Newtype wrapper to override 'ToJSON String' instances with ones that
-- encode the 'String' as a JSON array of characters.
newtype CharArray = CharArray { unCharArray :: String }

instance ToJSON CharArray where
  toJSON = toJSON . map (:[]) . unCharArray
```

Let's serialize an example `MyRec` to JSON -

```haskell
% ghci
> :{
  Data.ByteString.Lazy.Char8.putStrLn
    $ Data.Aeson.encode
    $ Data.Aeson.toJSON MyRec { foo = 12, bar = "hi", baz = "bye" }
  :}
{"foo":12,"bar":["h","i"],"baz":"BYE"}
```

For more examples, see the test suite in
[generic-override-aeson](./aeson/test/Test.hs).
