---
metaTitle: "Haskell - Functor"
description: "Class Definition of Functor and Laws, Common instances of Functor, Replacing all elements of a Functor with a single value, Polynomial functors, Functors in Category Theory, Deriving Functor"
---

# Functor




## Class Definition of Functor and Laws


```hs
class Functor f where
    fmap :: (a -> b) -> f a -> f b

```

One way of looking at it is that `fmap` **lifts** a function of values into a function of values in a context `f`.

A correct instance of `Functor` should satisfy the **functor laws**, though these are not enforced by the compiler:

```hs
fmap id = id                    -- identity
fmap f . fmap g = fmap (f . g)  -- composition

```

There's a commonly-used infix alias for `fmap` called `<$>`.

```hs
infixl 4 <$>
(<$>) :: Functor f => (a -> b) -> f a -> f b
(<$>) = fmap

```



## Common instances of Functor


### Maybe

`Maybe` is a `Functor` containing a possibly-absent value:

```hs
instance Functor Maybe where
    fmap f Nothing = Nothing
    fmap f (Just x) = Just (f x)

```

`Maybe`'s instance of `Functor` applies a function to a value wrapped in a `Just`. If the computation has previously failed (so the `Maybe` value is a `Nothing`), then there's no value to apply the function to, so `fmap` is a no-op.

```hs
> fmap (+ 3) (Just 3)
Just 6
> fmap length (Just "mousetrap")
Just 9
> fmap sqrt Nothing
Nothing

```

We can check the functor laws for this instance using equational reasoning. For the identity law,

```hs
fmap id Nothing
Nothing  -- definition of fmap
id Nothing  -- definition of id

fmap id (Just x)
Just (id x)  -- definition of fmap
Just x  -- definition of id
id (Just x)  -- definition of id

```

For the composition law,

```hs
(fmap f . fmap g) Nothing
fmap f (fmap g Nothing)  -- definition of (.)
fmap f Nothing  -- definition of fmap
Nothing  -- definition of fmap
fmap (f . g) Nothing  -- because Nothing = fmap f Nothing, for all f

(fmap f . fmap g) (Just x)
fmap f (fmap g (Just x))  -- definition of (.)
fmap f (Just (g x))  -- definition of fmap
Just (f (g x))  -- definition of fmap
Just ((f . g) x)  -- definition of (.)
fmap (f . g) (Just x)  -- definition of fmap

```

### Lists

Lists' instance of `Functor` applies the function to every value in the list in place.

```hs
instance Functor [] where
    fmap f [] = []
    fmap f (x:xs) = f x : fmap f xs

```

This could alternatively be written as a list comprehension: `fmap f xs = [f x | x <- xs]`.

This example shows that `fmap` generalises `map`. `map` only operates on lists, whereas `fmap` works on an arbitrary `Functor`.

The identity law can be shown to hold by induction:

```hs
-- base case
fmap id []
[]  -- definition of fmap
id []  -- definition of id

-- inductive step
fmap id (x:xs)
id x : fmap id xs  -- definition of fmap
x : fmap id xs  -- definition of id
x : id xs  -- by the inductive hypothesis
x : xs  -- definition of id
id (x : xs)  -- definition of id

```

and similarly, the composition law:

```hs
-- base case
(fmap f . fmap g) []
fmap f (fmap g [])  -- definition of (.)
fmap f []  -- definition of fmap
[]  -- definition of fmap
fmap (f . g) []  -- because [] = fmap f [], for all f

-- inductive step
(fmap f . fmap g) (x:xs)
fmap f (fmap g (x:xs))  -- definition of (.)
fmap f (g x : fmap g xs)  -- definition of fmap
f (g x) : fmap f (fmap g xs)  -- definition of fmap
(f . g) x : fmap f (fmap g xs)  -- definition of (.)
(f . g) x : fmap (f . g) xs  -- by the inductive hypothesis
fmap (f . g) xs  -- definition of fmap

```

### Functions

Not every `Functor` looks like a container. Functions' instance of `Functor` applies a function to the return value of another function.

```hs
instance Functor ((->) r) where
    fmap f g = \x -> f (g x)

```

Note that this definition is equivalent to `fmap = (.)`. So `fmap` generalises function composition.

Once more checking the identity law:

```hs
fmap id g
\x -> id (g x)  -- definition of fmap
\x -> g x  -- definition of id
g  -- eta-reduction
id g  -- definition of id

```

and the composition law:

```hs
(fmap f . fmap g) h
fmap f (fmap g h)  -- definition of (.)
fmap f (\x -> g (h x))  -- definition of fmap
\y -> f ((\x -> g (h x)) y)  -- definition of fmap
\y -> f (g (h y))  -- beta-reduction
\y -> (f . g) (h y)  -- definition of (.)
fmap (f . g) h  -- definition of fmap

```



## Replacing all elements of a Functor with a single value


The `Data.Functor` module contains two combinators, `<$` and `$>`, which ignore all of the values contained in a functor, replacing them all with a single constant value.

```hs
infixl 4 <$, $>

<$ :: Functor f => a -> f b -> f a
(<$) = fmap . const

$> :: Functor f => f a -> b -> f b
($>) = flip (<$)

```

`void` ignores the return value of a computation.

```hs
void :: Functor f => f a -> f ()
void = (() <$)

```



## Polynomial functors


There's a useful set of type combinators for building big `Functor`s out of smaller ones. These are instructive as example instances of `Functor`, and they're also useful as a technique for generic programming, because they can be used to represent a large class of common functors.

### The identity functor

The identity functor simply wraps up its argument. It's a type-level implementation of the `I` combinator from SKI calculus.

```hs
newtype I a = I a

instance Functor I where
    fmap f (I x) = I (f x)

```

`I` can be found, under the name of `Identity`, in [the `Data.Functor.Identity` module](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor-Identity.html).

### The constant functor

The constant functor ignores its second argument, containing only a constant value. It's a type-level analogue of `const`, the `K` combinator from SKI calculus.

```hs
newtype K c a = K c

```

Note that `K c a` doesn't contain any `a`-values; `K ()` is isomorphic to [`Proxy`](http://stackoverflow.com/documentation/haskell/8025/proxies#t=201611271824312213601). This means that `K`'s implementation of `fmap` doesn't do any mapping at all!

```hs
instance Functor (K c) where
    fmap _ (K c) = K c

```

`K` is otherwise known as `Const`, from [`Data.Functor.Const`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor-Const.html).

The remaining functors in this example combine smaller functors into bigger ones.

### Functor products

The functor product takes a pair of functors and packs them up. It's analogous to a tuple, except that while `(,) :: * -> * -> *` operates on `types` `*`, `(:*:) :: (* -> *) -> (* -> *) -> (* -> *)` operates on `functors` `* -> *`.

```hs
infixl 7 :*:
data (f :*: g) a = f a :*: g a

instance (Functor f, Functor g) => Functor (f :*: g) where
    fmap f (fx :*: gy) = fmap f fx :*: fmap f gy

```

This type can be found, under the name `Product`, in [the `Data.Functor.Product` module](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor-Product.html).

### Functor coproducts

Just like `:*:` is analogous to `(,)`, `:+:` is the functor-level analogue of `Either`.

```hs
infixl 6 :+:
data (f :+: g) a = InL (f a) | InR (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
    fmap f (InL fx) = InL (fmap f fx)
    fmap f (InR gy) = InR (fmap f gy)

```

`:+:` can be found under the name `Sum`, in [the `Data.Functor.Sum` module](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor-Sum.html).

### Functor composition

Finally, `:.:` works like a type-level `(.)`, taking the output of one functor and plumbing it into the input of another.

```hs
infixr 9 :.:
newtype (f :.: g) a = Cmp (f (g a))

instance (Functor f, Functor g) => Functor (f :.: g) where
    fmap f (Cmp fgx) = Cmp (fmap (fmap f) fgx)

```

The `Compose` type can be found in [`Data.Functor.Compose`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor-Compose.html)

### Polynomial functors for generic programming

`I`, `K`, `:*:`, `:+:` and `:.:` can be thought of as a kit of building blocks for a certain class of simple datatypes. The kit becomes especially powerful when you combine it with [fixed points](http://stackoverflow.com/documentation/haskell/2984/recursion-schemes/10136/fixed-points#t=201611271723029746936) because datatypes built with these combinators are automatically instances of `Functor`. You use the kit to build a template type, marking recursive points using `I`, and then plug it into `Fix` to get a type that can be used with the standard zoo of recursion schemes.

|Name|As a datatype|Using the functor kit
|---|---|---|---|---|---|---|---|---|---
|Pairs of values|`data Pair a = Pair a a`|`type Pair = I :*: I`
|Two-by-two grids|`type Grid a = Pair (Pair a)`|`type Grid = Pair :.: Pair`
|Natural numbers|`data Nat = Zero | Succ Nat`|`type Nat = Fix (K () :+: I)`
|Lists|`data List a = Nil | Cons a (List a)`|`type List a = Fix (K () :+: K a :*: I)`
|Binary trees|`data Tree a = Leaf | Node (Tree a) a (Tree a)`|`type Tree a = Fix (K () :+: I :*: K a :*: I)`
|Rose trees|`data Rose a = Rose a (List (Rose a))`|`type Rose a = Fix (K a :*: List :.: I)`

This "kit" approach to designing datatypes is the idea behind **generic programming** libraries such as [`generics-sop`](https://hackage.haskell.org/package/generics-sop). The idea is to write generic operations using a kit like the one presented above, and then use a type class to convert arbitrary datatypes to and from their generic representation:

```hs
class Generic a where
    type Rep a  -- a generic representation built using a kit
    to :: a -> Rep a
    from :: Rep a -> a

```



## Functors in Category Theory


A Functor is defined in category theory as a structure-preserving map (a 'homomorphism') between categories. Specifically, (all) objects are mapped to objects, and (all) arrows are mapped to arrows, such that the category laws are preserved.

The category in which objects are Haskell types and morphisms are Haskell functions is called **Hask**. So a functor from **Hask** to **Hask** would consist of a mapping of types to types and a mapping from functions to functions.

The relationship that this category theoretic concept bears to the Haskell programming construct `Functor` is rather direct. The mapping from types to types takes the form of a type `f :: * -> *`, and the mapping from functions to functions takes the form of a function `fmap :: (a -> b) -> (f a -> f b)`. Putting those together in a class,

```hs
class Functor (f :: * -> *) where
    fmap :: (a -> b) -> f a -> f b

```

`fmap` is an operation that takes a function (a type of morphism), `:: a -> b`, and maps it to another function, `:: f a -> f b`. It is assumed (but left to the programmer to ensure) that instances of `Functor` are indeed mathematical functors, preserving **Hask**'s categorical structure:

```hs
fmap (id {- :: a -> a -})  ==  id {- :: f a -> f a -}
fmap (h . g)               ==  fmap h . fmap g

```

`fmap` lifts a function `:: a -> b` into a subcategory of **Hask** in a way that preserves both the existence of any identity arrows, and the associativity of composition.

The `Functor` class only encodes **endo**functors on **Hask**. But in mathematics, functors can map between arbitrary categories. A more faithful encoding of this concept would look like this:

```hs
class Category c where
    id  :: c i i
    (.) :: c j k -> c i j -> c i k

class (Category c1, Category c2) => CFunctor c1 c2 f where
    cfmap :: c1 a b -> c2 (f a) (f b)

```

The standard Functor class is a special case of this class in which the source and target categories are both **Hask**. For example,

```hs
instance Category (->) where        -- Hask
    id    = \x -> x
    f . g = \x -> f (g x)

instance CFunctor (->) (->) [] where
    cfmap = fmap

```



## Deriving Functor


The `DeriveFunctor` language extension allows GHC to generate instances of `Functor` automatically.

```hs
{-# LANGUAGE DeriveFunctor #-}

data List a = Nil | Cons a (List a) deriving Functor

-- instance Functor List where            -- automatically defined
--   fmap f Nil = Nil
--   fmap f (Cons x xs) = Cons (f x) (fmap f xs)

map :: (a -> b) -> List a -> List b
map = fmap

```



#### Remarks


A Functor can be thought of as a container for some value, or a computation context. Examples are `Maybe a` or `[a]`. The [Typeclassopedia](https://wiki.haskell.org/Typeclassopedia#Functor) article has a good write-up of the concepts behind Functors.

To be considered a real Functor, an instance has to respect the 2 following laws:

### Identity

```hs
fmap id == id

```

### Composition

```hs
fmap (f . g) = (fmap f) . (fmap g)

```

