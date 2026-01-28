---
metaTitle: "Haskell - Data.Aeson - JSON in Haskell"
description: "Smart Encoding and Decoding using Generics, A quick way to generate a Data.Aeson.Value, Optional Fields"
---

# Data.Aeson - JSON in Haskell




## Smart Encoding and Decoding using Generics


The easiest and quickest way to encode a Haskell data type to JSON with Aeson is using generics.

```hs
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Text
import Data.Aeson
import Data.ByteString.Lazy    

```

First let us create a data type Person:

```hs
data Person = Person { firstName :: Text
                     , lastName  :: Text
                     , age       :: Int 
                     } deriving (Show, Generic)

```

In order to use the `encode` and `decode` function from the `Data.Aeson` package we need to make `Person` an instance of `ToJSON` and `FromJSON`. Since we derive `Generic` for `Person`, we can create empty instances for these classes. The default definitions of the methods are defined in terms of the methods provided by the `Generic` type class.

```hs
instance ToJSON Person
instance FromJSON Person

```

Done! In order to improve the encoding speed we can slightly change the `ToJSON` instance:

```hs
instance ToJSON Person where
    toEncoding = genericToEncoding defaultOptions

```

Now we can use the `encode` function to convert `Person` to a (lazy) Bytestring:

```hs
encodeNewPerson :: Text -> Text -> Int -> ByteString
encodeNewPerson first last age = encode $ Person first last age

```

And to decode we can just use `decode`:

```hs
> encodeNewPerson "Hans" "Wurst" 30
"{\"lastName\":\"Wurst\",\"age\":30,\"firstName\":\"Hans\"}"


> decode $ encodeNewPerson "Hans" "Wurst" 30
Just (Person {firstName = "Hans", lastName = "Wurst", age = 30})

```



## A quick way to generate a Data.Aeson.Value


```hs
{-# LANGUAGE OverloadedStrings #-}
module Main where

import Data.Aeson

main :: IO ()
main = do
  let example = Data.Aeson.object [ "key" .= (5 :: Integer), "somethingElse" .= (2 :: Integer) ] :: Value
  print . encode $ example

```



## Optional Fields


Sometimes, we want some fields in the JSON string to be optional. For example,

```hs
data Person = Person { firstName :: Text
                     , lastName  :: Text
                     , age       :: Maybe Int 
                     }

```

This can be achieved by

```hs
import Data.Aeson.TH

$(deriveJSON defaultOptions{omitNothingFields = True} ''Person)

```

