---
metaTitle: "Haskell - Bifunctor"
description: "Common instances of Bifunctor, Definition of Bifunctor, first and second"
---

# Bifunctor




## Common instances of Bifunctor


### Two-element tuples

`(,)` is an example of a type that has a `Bifunctor` instance.

```hs
instance Bifunctor (,) where
    bimap f g (x, y) = (f x, g y)

```

`bimap` takes a pair of functions and applies them to the tuple's respective components.

```hs
bimap (+ 2) (++ "nie") (3, "john") --> (5,"johnnie")
bimap ceiling length (3.5 :: Double, "john" :: String) --> (4,4)

```

### `Either`

`Either`'s instance of `Bifunctor` selects one of the two functions to apply depending on whether the value is `Left` or `Right`.

```hs
instance Bifunctor Either where
    bimap f g (Left x) = Left (f x)
    bimap f g (Right y) = Right (g y)

```



## Definition of Bifunctor


`Bifunctor` is the class of types with two type parameters (`f :: * -> * -> *`), both of which can be covariantly mapped over simultaneously.

```hs
class Bifunctor f where
    bimap :: (a -> c) -> (b -> d) -> f a b -> f c d

```

`bimap` can be thought of as applying a pair of `fmap` operations to a datatype.

A correct instance of `Bifunctor` for a type `f` must satisfy the **bifunctor laws**, which are analogous to the [**functor laws**](http://stackoverflow.com/documentation/haskell/3800/functor/25370/definition-of-functor#t=201611271637470659956):

```hs
bimap id id = id  -- identity
bimap (f . g) (h . i) = bimap f h . bimap g i  -- composition

```

The `Bifunctor` class is found in the `Data.Bifunctor` module. For GHC versions >7.10, this module is bundled with the compiler; for earlier versions you need to install the `bifunctors` package.



## first and second


If mapping covariantly over only the first argument, or only the second argument, is desired, then `first` or `second` ought to be used (in lieu of `bimap`).

```hs
first :: Bifunctor f => (a -> c) -> f a b -> f c b
first f = bimap f id

second :: Bifunctor f => (b -> d) -> f a b -> f a d
second g = bimap id g

```

For example,

```hs
ghci> second (+ 2) (Right 40)
Right 42
ghci> second (+ 2) (Left "uh oh")
Left "uh oh"

```



#### Syntax


- bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
- first :: (a -> b) -> p a c -> p b c
- second :: (b -> c) -> p a b -> p a c



#### Remarks


A run of the mill `Functor` is covariant in a **single** type parameter. For instance, if `f` is a `Functor`, then given an `f a`, and a function of the form `a -> b`, one can obtain an `f b` (through the use of `fmap`).

A `Bifunctor` is covariant in **two** type parameters. If `f` is a `Bifunctor`, then given an `f a b`, and two functions, one from `a -> c`, and another from `b -> d`, then one can obtain an `f c d` (using `bimap`).

`first` should be thought of as an `fmap` over the first type parameter, `second` as an `fmap` over the second, and `bimap` should be conceived as mapping two functions covariantly over the first and second type parameters, respectively.

