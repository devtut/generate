---
metaTitle: "Haskell - Attoparsec"
description: "Combinators, Bitmap - Parsing Binary Data"
---

# Attoparsec


Attoparsec is a parsing combinator library that is "aimed particularly at dealing efficiently with network protocols and complicated text/binary file formats".

Attoparsec offers not only speed and efficiency, but backtracking and incremental input.

Its API closely mirrors that of another parser combinator library, Parsec.

There are submodules for compatibility with `ByteString`, `Text` and `Char8`. Use of the `OverloadedStrings` language extension is recommended.



## Combinators


Parsing input is best achieved through larger parser functions that are composed of smaller, single purpose ones.

Let's say we wished to parse the following text which represents working hours:

> 
Monday: 0800 1600.


We could split these into two "tokens": the day name -- "Monday" -- and a time portion "0800" to "1600".

To parse a day name, we could write the following:

To parse the time portion we could write:

Now we have two parsers for our individual parts of the text, we can combine these in a "larger" parser to read an entire day's working hours:

and then run the parser:



## Bitmap - Parsing Binary Data


Attoparsec makes parsing binary data trivial. Assuming these definitions:

```hs
dibP :: Parser DIB
dibP = read . unpack <$> take 2

```

Similarly, the size of the bitmap, the reserved sections and the pixel offset can be read easily too:

which can then be combined into a larger parser function for the entire header:



#### Parameters


|Type|Detail
|---|---|---|---|---|---|---|---|---|---
|`Parser i a`|The core type for representing a parser. `i` is the string type, e.g. `ByteString`.
|[`IResult i r`](https://hackage.haskell.org/package/attoparsec-0.13.1.0/docs/Data-Attoparsec-Internal-Types.html#t:IResult)|The result of a parse, with `Fail i [String] String`, `Partial (i -> IResult i r)` and `Done i r` as constructors.

