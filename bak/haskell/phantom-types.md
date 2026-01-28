---
metaTitle: "Haskell - Phantom types"
description: "Use Case for Phantom Types: Currencies"
---

# Phantom types



## Use Case for Phantom Types: Currencies


Phantom types are useful for dealing with data, that has identical representations but isn't logically of the same type.

A good example is dealing with currencies. If you work with currencies you absolutely never want to e.g. add two amounts of different currencies. What would the result currency of `5.32€ + 2.94$` be? It's not defined and there is no good reason to do this.

A solution to this could look something like this:

```hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

data USD
data EUR

newtype Amount a = Amount Double
                 deriving (Show, Eq, Ord, Num)

```

The `GeneralisedNewtypeDeriving` extension allows us to derive `Num` for the `Amount` type. GHC reuses `Double`'s `Num` instance.

Now if you represent Euro amounts with e.g. `(5.0 :: Amount EUR)` you have solved the problem of keeping double amounts separate at the type level without introducing overhead. Stuff like `(1.13 :: Amount EUR) + (5.30 :: Amount USD)` will result in a type error and require you to deal with currency conversion appropriately.

More comprehensive documentation can be found in the [haskell wiki article](https://wiki.haskell.org/Phantom_type)

