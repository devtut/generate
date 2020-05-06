---
metaTitle: "Haskell - Data.Text"
description: "Text Literals, Checking if a Text is a substring of another Text, Stripping whitespace, Splitting Text Values, Encoding and Decoding Text, Indexing Text"
---

# Data.Text



## Text Literals


The [`OverloadedStrings`](http://stackoverflow.com/documentation/haskell/369/overloaded-literals/1329/strings#t=201610072315314796807) language extension allows the use of normal string literals to stand for `Text` values.

```hs
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

myText :: T.Text
myText = "overloaded"

```



## Checking if a Text is a substring of another Text


```hs
ghci> :set -XOverloadedStrings
ghci> import Data.Text as T

```

[`isInfixOf :: Text -> Text -> Bool`](https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html#v:isInfixOf) checks whether a `Text` is contained anywhere within another `Text`.

```hs
ghci> "rum" `T.isInfixOf` "crumble"
True

```

[`isPrefixOf :: Text -> Text -> Bool`](https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html#v:isPrefixOf) checks whether a `Text` appears at the beginning of another `Text`.

```hs
ghci> "crumb" `T.isPrefixOf` "crumble"
True

```

[`isSuffixOf :: Text -> Text -> Bool`](https://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text.html#v:isSuffixOf) checks whether a `Text` appears at the end of another `Text`.

```hs
ghci> "rumble" `T.isSuffixOf` "crumble"
True

```



## Stripping whitespace


```hs
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

myText :: T.Text
myText = "\n\r\t   leading and trailing whitespace   \t\r\n"

```

`strip` removes whitespace from the start and end of a `Text` value.

```hs
ghci> T.strip myText
"leading and trailing whitespace"

```

`stripStart` removes whitespace only from the start.

```hs
ghci> T.stripStart myText
"leading and trailing whitespace   \t\r\n"

```

`stripEnd` removes whitespace only from the end.

```hs
ghci> T.stripEnd myText
"\n\r\t   leading and trailing whitespace"

```

`filter` can be used to remove whitespace, or other characters, from the middle.

```hs
ghci> T.filter /=' ' "spaces in the middle of a text string"
"spacesinthemiddleofatextstring"

```



## Splitting Text Values


```hs
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

myText :: T.Text
myText = "mississippi"

```

`splitOn` breaks a `Text` up into a list of `Texts` on occurrences of a substring.

```hs
ghci> T.splitOn "ss" myText
["mi","i","ippi"]

```

`splitOn` is the inverse of `intercalate`.

```hs
ghci> intercalate "ss" (splitOn "ss" "mississippi")
"mississippi"

```

`split` breaks a `Text` value into chunks on characters that satisfy a Boolean predicate.

```hs
ghci> T.split (== 'i') myText
["m","ss","ss","pp",""]

```



## Encoding and Decoding Text


Encoding and decoding functions for a variety of Unicode encodings can be found in the `Data.Text.Encoding` module.

```hs
ghci> import Data.Text.Encoding
ghci> decodeUtf8 (encodeUtf8 "my text")
"my text"

```

Note that `decodeUtf8` will throw an exception on invalid input. If you want to handle invalid UTF-8 yourself, use `decodeUtf8With`.

```hs
ghci> decodeUtf8With (\errorDescription input -> Nothing) messyOutsideData

```



## Indexing Text


```hs
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T

myText :: T.Text

myText = "mississippi"

```

Characters at specific indices can be returned by the `index` function.

```hs
ghci> T.index myText 2
's'

```

The `findIndex` function takes a function of type `(Char -> Bool)` and Text and returns the index of the first occurrence of a given string or Nothing if it doesn't occur.

```hs
ghci> T.findIndex ('s'==) myText
Just 2
ghci> T.findIndex ('c'==) myText
Nothing

```

The `count` function returns the number of times a query `Text` occurs within another `Text`.

```hs
ghci> count ("miss"::T.Text) myText
1

```



#### Remarks


`Text` is a more efficient alternative to Haskell's standard `String` type. `String` is defined as a linked list of characters in the standard Prelude, per [the Haskell Report](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1190006.1.2):

```hs
type String = [Char]

```

`Text` is represented as a packed array of Unicode characters. This is similar to how most other high-level languages represent strings, and gives much better time and space efficiency than the list version.

`Text` should be preferred over `String` for all production usage. A notable exception is depending on a library which has a `String` API, but even in that case there may be a benefit of using `Text` internally and converting to a `String` just before interfacing with the library.

All of the examples in this topic use [the `OverloadedStrings` language extension](http://stackoverflow.com/documentation/haskell/1274/common-ghc-language-extensions/4173/overloadedstrings).

