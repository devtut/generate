---
metaTitle: "Haskell - Arithmetic"
description: "Basic examples, `Could not deduce (Fractional Int) ...`, Function examples"
---

# Arithmetic


In Haskell, all expressions (which includes numerical constants and functions operating on those) have a decidable type. At compile time, the type-checker infers the type of an expression from the types of the elementary functions that compose it. Since data is immutable by default, there are no "type casting" operations, but there are functions that copy data and generalize or specialize the types within reason.



## Basic examples


```hs
λ> :t 1
1 :: Num t => t

λ> :t pi
pi :: Floating a => a

```

In the examples above, the type-checker infers a type-**class** rather than a concrete type for the two constants. In Haskell, the `Num` class is the most general numerical one (since it encompasses integers and reals), but `pi` must belong to a more specialized class, since it has a nonzero fractional part.

```hs
list0 :: [Integer]
list0 = [1, 2, 3]

list1 :: [Double]
list1 = [1, 2, pi]

```

The concrete types above were inferred by GHC. More general types like `list0 :: Num a => [a]` would have worked, but would have also been harder to preserve (e.g. if one consed a `Double` onto a list of `Num`s), due to the caveats shown above.



## `Could not deduce (Fractional Int) ...`


The error message in the title is a common beginner mistake. Let's see how it arises and how to fix it.

Suppose we need to compute the average value of a list of numbers; the following declaration would seem to do it, but it wouldn't compile:

```hs
averageOfList ll = sum ll / length ll

```

The problem is with the division `(/)` function: its signature is `(/) :: Fractional a => a -> a -> a`, but in the case above the denominator (given by `length :: Foldable t => t a -> Int`) is of type `Int` (and `Int` does not belong to the `Fractional` class) hence the error message.

We can fix the error message with `fromIntegral :: (Num b, Integral a) => a -> b`. One can see that this function accepts values of any `Integral` type and returns corresponding ones in the `Num` class:

```hs
averageOfList' :: (Foldable t, Fractional a) => t a -> a
averageOfList' ll = sum ll / fromIntegral (length ll)

```



## Function examples


What's the type of `(+)` ?

```hs
λ> :t (+)
(+) :: Num a => a -> a -> a

```

What's the type of `sqrt` ?

```hs
λ> :t sqrt
sqrt :: Floating a => a -> a

```

What's the type of `sqrt . fromIntegral` ?

```hs
sqrt . fromIntegral :: (Integral a, Floating c) => a -> c

```



#### Remarks


### The numeric typeclass hierarchy

`Num` sits at the root of the numeric typeclass hierarchy. Its characteristic operations and some common instances are shown below (the ones loaded by default with Prelude plus those of `Data.Complex`):

```hs
λ> :i Num
class Num a where
  (+) :: a -> a -> a
  (-) :: a -> a -> a
  (*) :: a -> a -> a
  negate :: a -> a
  abs :: a -> a
  signum :: a -> a
  fromInteger :: Integer -> a
  {-# MINIMAL (+), (*), abs, signum, fromInteger, (negate | (-)) #-}
      -- Defined in ‘GHC.Num’
instance RealFloat a => Num (Complex a) -- Defined in ‘Data.Complex’
instance Num Word -- Defined in ‘GHC.Num’
instance Num Integer -- Defined in ‘GHC.Num’
instance Num Int -- Defined in ‘GHC.Num’
instance Num Float -- Defined in ‘GHC.Float’
instance Num Double -- Defined in ‘GHC.Float’

```

We have already seen the `Fractional` class, which requires `Num` and introduces the notions of "division" `(/)` and reciprocal of a number:

```hs
λ> :i Fractional
class Num a => Fractional a where
  (/) :: a -> a -> a
  recip :: a -> a
  fromRational :: Rational -> a
  {-# MINIMAL fromRational, (recip | (/)) #-}
      -- Defined in ‘GHC.Real’
instance RealFloat a => Fractional (Complex a) -- Defined in ‘Data.Complex’
instance Fractional Float -- Defined in ‘GHC.Float’
instance Fractional Double -- Defined in ‘GHC.Float’

```

The `Real` class models .. the real numbers. It requires `Num` and `Ord`, therefore it models an ordered numerical field. As a counterexample, Complex numbers are **not** an ordered field (i.e. they do not possess a natural ordering relationship):

```hs
λ> :i Real
class (Num a, Ord a) => Real a where
  toRational :: a -> Rational
  {-# MINIMAL toRational #-}
      -- Defined in ‘GHC.Real’
instance Real Word -- Defined in ‘GHC.Real’
instance Real Integer -- Defined in ‘GHC.Real’
instance Real Int -- Defined in ‘GHC.Real’
instance Real Float -- Defined in ‘GHC.Float’
instance Real Double -- Defined in ‘GHC.Float’

```

`RealFrac` represents numbers that may be rounded

```hs
λ> :i RealFrac
class (Real a, Fractional a) => RealFrac a where
  properFraction :: Integral b => a -> (b, a)
  truncate :: Integral b => a -> b
  round :: Integral b => a -> b
  ceiling :: Integral b => a -> b
  floor :: Integral b => a -> b
  {-# MINIMAL properFraction #-}
      -- Defined in ‘GHC.Real’
instance RealFrac Float -- Defined in ‘GHC.Float’
instance RealFrac Double -- Defined in ‘GHC.Float’

```

`Floating` (which implies `Fractional`) represents constants and operations that may not have a finite decimal expansion.

```hs
λ> :i Floating
class Fractional a => Floating a where
  pi :: a
  exp :: a -> a
  log :: a -> a
  sqrt :: a -> a
  (**) :: a -> a -> a
  logBase :: a -> a -> a
  sin :: a -> a
  cos :: a -> a
  tan :: a -> a
  asin :: a -> a
  acos :: a -> a
  atan :: a -> a
  sinh :: a -> a
  cosh :: a -> a
  tanh :: a -> a
  asinh :: a -> a
  acosh :: a -> a
  atanh :: a -> a
  GHC.Float.log1p :: a -> a
  GHC.Float.expm1 :: a -> a
  GHC.Float.log1pexp :: a -> a
  GHC.Float.log1mexp :: a -> a
  {-# MINIMAL pi, exp, log, sin, cos, asin, acos, atan, sinh, cosh,
              asinh, acosh, atanh #-}
      -- Defined in ‘GHC.Float’
instance RealFloat a => Floating (Complex a) -- Defined in ‘Data.Complex’
instance Floating Float -- Defined in ‘GHC.Float’
instance Floating Double -- Defined in ‘GHC.Float’

```

Caution: while expressions such as `sqrt . negate :: Floating a => a -> a` are perfectly valid, they might return `NaN` ("not-a-number"), which may not be an intended behaviour. In such cases, we might want to work over the Complex field (shown later).

