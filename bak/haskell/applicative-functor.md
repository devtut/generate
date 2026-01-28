---
metaTitle: "Haskell - Applicative Functor"
description: "Alternative definition, Common instances of Applicative"
---

# Applicative Functor




## Alternative definition


Since every Applicative Functor is a [Functor](http://stackoverflow.com/documentation/haskell/3800/functor#t=201612131701590752957), `fmap` can always be used on it; thus the essence of Applicative is the pairing of carried contents, as well as the ability to create it:

```hs
class Functor f => PairingFunctor f where
  funit :: f ()                  -- create a context, carrying nothing of import
  fpair :: (f a,f b) -> f (a,b)  -- collapse a pair of contexts into a pair-carrying context

```

This class is isomorphic to `Applicative`.

```hs
pure a = const a <$> funit = a <$ funit  

fa <*> fb = (\(a,b) -> a b) <$> fpair (fa, fb) = uncurry ($) <$> fpair (fa, fb)

```

Conversely,

```hs
funit = pure ()

fpair (fa, fb) = (,) <$> fa <*> fb

```



## Common instances of Applicative


### Maybe

`Maybe` is an applicative functor containing a possibly-absent value.

```hs
instance Applicative Maybe where
    pure = Just
    
    Just f <*> Just x = Just $ f x
    _ <*> _ = Nothing

```

`pure` lifts the given value into `Maybe` by applying `Just` to it. The `(<*>)` function applies a function wrapped in a `Maybe` to a value in a `Maybe`. If both the function and the value are present (constructed with `Just`), the function is applied to the value and the wrapped result is returned. If either is missing, the computation can't proceed and `Nothing` is returned instead.

### Lists

One way for lists to fit the type signature `<*> :: [a -> b] -> [a] -> [b]` is to take the two lists' Cartesian product, pairing up each element of the first list with each element of the second one:

```hs
fs <*> xs = [f x | f <- fs, x <- xs]
         -- = do { f <- fs; x <- xs; return (f x) }

pure x = [x]

```

This is usually interpreted as emulating nondeterminism, with a list of values standing for a nondeterministic value whose possible values range over that list; so a combination of two nondeterministic values ranges over all possible combinations of the values in the two lists:

```hs
ghci> [(+1),(+2)] <*> [3,30,300]
[4,31,301,5,32,302]

```

### Infinite streams and zip-lists

There's a class of `Applicative`s which "zip" their two inputs together. One simple example is that of infinite streams:

```hs
data Stream a = Stream { headS :: a, tailS :: Stream a }

```

`Stream`'s `Applicative` instance applies a stream of functions to a stream of arguments point-wise, pairing up the values in the two streams by position. `pure` returns a constant stream – an infinite list of a single fixed value:

```hs
instance Applicative Stream where
    pure x = let s = Stream x s in s
    Stream f fs <*> Stream x xs = Stream (f x) (fs <*> xs)

```

Lists too admit a "zippy" `Applicative` instance, for which there exists the `ZipList` newtype:

```hs
newtype ZipList a = ZipList { getZipList :: [a] }

instance Applicative ZipList where
    ZipList xs <*> ZipList ys = ZipList $ zipWith ($) xs ys

```

Since `zip` trims its result according to the shortest input, the only implementation of `pure` that satisfies the `Applicative` laws is one which returns an infinite list:

```

   pure a = ZipList (repeat a)   -- ZipList (fix (a:)) = ZipList [a,a,a,a,...

```

For example:

```hs
ghci> getZipList $ ZipList [(+1),(+2)] <*> ZipList [3,30,300]
[4,32]

```

The two possibilities remind us of the outer and the inner product, similar to multiplying a 1-column (`n x 1`) matrix with a 1-row (`1 x m`) one in the first case, getting the `n x m` matrix as a result (but flattened); or multiplying a 1-row and a 1-column matrices (but without the summing up) in the second case.

### Functions

When specialised to functions `(->) r`, the type signatures of `pure` and `<*>` match those of the `K` and `S` combinators, respectively:

```hs
pure :: a -> (r -> a)
<*> :: (r -> (a -> b)) -> (r -> a) -> (r -> b)

```

`pure` must be `const`, and `<*>` takes a pair of functions and applies them each to a fixed argument, applying the two results:

```hs
instance Applicative ((->) r) where
    pure = const
    f <*> g = \x -> f x (g x)

```

Functions are the prototypical "zippy" applicative. For example, since infinite streams are isomorphic to `(->) Nat`, ...

```hs
-- | Index into a stream
to :: Stream a -> (Nat -> a)
to (Stream x xs) Zero = x
to (Stream x xs) (Suc n) = to xs n

-- | List all the return values of the function in order
from :: (Nat -> a) -> Stream a
from f = from' Zero
    where from' n = Stream (f n) (from' (Suc n))

```

... representing streams in a higher-order way produces the zippy `Applicative` instance automatically.



#### Remarks


### Definition

```hs
class Functor f => Applicative f where
    pure  :: a -> f a
    (<*>) :: f (a -> b) -> f a -> f b

```

Note the `Functor` constraint on `f`. The `pure` function returns its argument embedded in the `Applicative` structure. The infix function `<*>` (pronounced "apply") is very similar to `fmap` except with the function embedded in the `Applicative` structure.

A correct instance of `Applicative` should satisfy the **applicative laws**, though these are not enforced by the compiler:

```hs
pure id <*> a = a                              -- identity
pure (.) <*> a <*> b <*> c = a <*> (b <*> c)   -- composition
pure f <*> pure a = pure (f a)                 -- homomorphism
a <*> pure b = pure ($ b) <*> a                -- interchange

```

