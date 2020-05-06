---
metaTitle: "Haskell - Category Theory"
description: "Category theory as a system for organizing abstraction, Haskell types as a category, Definition of a Category, Product of types in Hask, Coproduct of types in Hask, Haskell Applicative in terms of Category Theory"
---

# Category Theory




## Category theory as a system for organizing abstraction


Category theory is a modern mathematical theory and a branch of abstract algebra focused on the nature of connectedness and relation. It is useful for giving solid foundations and common language to many highly reusable programming abstractions. Haskell uses Category theory as inspiration for some of the core typeclasses available in both the standard library and several popular third-party libraries.

### An example

The `Functor` typeclass says that if a type `F` instantiates `Functor` (for which we write `Functor F`) then we have a generic operation

```hs
fmap :: (a -> b) -> (F a -> F b)

```

which lets us "map" over `F`. The standard (but imperfect) intuition is that `F a` is a container full of values of type `a` and `fmap` lets us apply a transformation to each of these contained elements. An example is `Maybe`

```hs
instance Functor Maybe where
  fmap f Nothing = Nothing     -- if there are no values contained, do nothing
  fmap f (Just a) = Just (f a) -- else, apply our transformation

```

Given this intuition, a common question is "why not call `Functor` something obvious like `Mappable`?".

### A hint of Category Theory

The reason is that Functor fits into a set of common structures in Category theory and therefore by calling `Functor` "Functor" we can see how it connects to this deeper body of knowledge.

In particular, Category Theory is highly concerned with the idea of arrows from one place to another. In Haskell, the most important set of arrows are the function arrows `a -> b`. A common thing to study in Category Theory is how one set of arrows relates to another set. In particular, for any type constructor `F`, the set of arrows of the shape `F a -> F b` are also interesting.

So a Functor is any `F` such that there is a connection between normal Haskell arrows `a -> b` and the `F`-specific arrows `F a -> F b`. The connection is defined by `fmap` and we also recognize a few laws which must hold

```hs
forall (x :: F a) . fmap id x == x

forall (f :: a -> b) (g :: b -> c) . fmap g . fmap f = fmap (g . f)

```

All of these laws arise naturally from the Category Theoretic interpretation of `Functor` and would not be as obviously necessary if we only thought of `Functor` as relating to "mapping over elements".



## Haskell types as a category


### Definition of the category

The Haskell types along with functions between types form (almost†) a category. We have an identity morphism (function) (`id :: a -> a`) for every object (type) `a`; and composition of morphisms (`(.) :: (b -> c) -> (a -> b) -> a -> c`), which obey category laws:

```hs
f . id = f = id . f
h . (g . f) = (h . g) . f 

```

We usually call this category **Hask**.

### Isomorphisms

In category theory, we have an isomorphism when we have a morphism which has an inverse, in other words, there is a morphism which can be composed with it in order to create the identity. In **Hask** this amounts to have a pair of morphisms `f`,`g` such that:

```

f . g == id == g . f

```

If we find a pair of such morphisms between two types, we call them **isomorphic to one another**.

An example of two isomorphic types would be `((),a)` and `a` for some `a`. We can construct the two morphisms:

```hs
f :: ((),a) -> a
f ((),x) = x

g :: a -> ((),a)
g x = ((),x)

```

And we can check that `f . g == id == g . f`.

### Functors

A functor, in category theory, goes from a category to another, mapping objects and morphisms. We are working only on one category, the category **Hask** of Haskell types, so we are going to see only functors from **Hask** to **Hask**, those functors, whose origin and destination category are the same, are called **endofunctors**. Our endofunctors will be the polymorphic types taking a type and returning another:

```hs
F :: * -> *

```

To obey the categorical functor laws (preserve identities and composition) is equivalent to obey the Haskell functor laws:

```hs
fmap (f . g) = (fmap f) . (fmap g)
fmap id = id

```

So, we have, for example, that `[]`, `Maybe` or `(-> r)` are functors in **Hask**.

### Monads

A monad in category theory is a monoid on the **category of endofunctors**. This category has endofunctors as objects `F :: * -> *` and natural transformations (transformations between them `forall a . F a -> G a`) as morphisms.

A monoid object can be defined on a monoidal category, and is a type having two morphisms:

```hs
zero :: () -> M
mappend :: (M,M) -> M

```

We can translate this roughly to the category of Hask endofunctors as:

```hs
return :: a -> m a
join :: m (m a) -> m a 

```

And, to obey the monad laws is equivalent to obey the categorical monoid object laws.

†In fact, the class of all types along with the class of functions between types do **not** strictly form a category in Haskell, due to the existance of `undefined`. Typically this is remedied by simply defining the objects of the **Hask** category as types without bottom values, which excludes non-terminating functions and infinite values (codata). For a detailed discussion of this topic, see [here](https://wiki.haskell.org/Hask).



## Definition of a Category


A category `C` consists of:

- A collection of objects called `Obj(C)` ;
- A collection (called `Hom(C)`) of morphisms between those objects. If `a` and `b` are in `Obj(C)`, then a morphism `f` in `Hom(C)` is typically denoted `f : a -> b`, and the collection of all morphism between `a` and `b` is denoted `hom(a,b)` ;
- A special morphism called the **identity** morphism - for every `a : Obj(C)` there exists a morphism `id : a -> a` ;
- A composition operator (`.`), taking two morphisms `f : a -> b`, `g : b -> c` and producing a morphism `a -> c`

which obey the following laws:

```hs
For all f : a -> x, g : x -> b, then id . f = f and g . id = g

```

```hs
For all f : a -> b, g : b -> c and h : c -> d, then h . (g . f) = (h . g) . f

```

In other words, composition with the identity morphism (on either the left or right) does not change the other morphism, and composition is associative.

In Haskell, the `Category` is defined as a typeclass in [Control.Category](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Category.html):

In this case, `cat :: k -> k -> *` objectifies the morphism relation - there exists a morphism `cat a b` if and only if `cat a b` is inhabited (i.e. has a value). `a`, `b` and `c` are all in `Obj(C)`. `Obj(C)` itself is represented by the **kind** `k` - for example, when `k ~ *`, as is typically the case, objects are types.

The canonical example of a Category in Haskell is the function category:

Another common example is the `Category` of `Kleisli` arrows for a `Monad`:



## Product of types in Hask


### Categorical products

In category theory, the product of two objects **X**, **Y** is another object **Z** with two projections: **π₁  : Z → X** and **π₂ : Z → Y**; such that any other two morphisms from another object decompose uniquely through those projections. In other words, if there exist **f₁  : W → X** and **f₂  : W → Y**, exists a unique morphism **g : W → Z** such that **π₁ ○ g = f₁** and **π₂ ○ g = f₂**.

### Products in Hask

This translates into the **Hask** category of Haskell types as follows, `Z` is product of `A`, `B` when:

```hs
-- if there are two functions
f1 :: W -> A
f2 :: W -> B
-- we can construct a unique function
g  :: W -> Z
-- and we have two projections
p1 :: Z -> A
p2 :: Z -> B
-- such that the other two functions decompose using g
p1 . g == f1
p2 . g == f2

```

The **product type of two types** `A`, `B`, which follows the law stated above, **is the tuple** of the two types `(A,B)`, and the two projections are `fst` and `snd`. We can check that it follows the above rule, if we have two functions `f1 :: W -> A` and `f2 :: W -> B` we can decompose them uniquely as follow:

```hs
decompose :: (W -> A) -> (W -> B) -> (W -> (A,B))
decompose f1 f2 = (\x -> (f1 x, f2 x))

```

And we can check that the decomposition is correct:

```hs
fst . (decompose f1 f2) = f1
snd . (decompose f1 f2) = f2

```

### Uniqueness up to isomorphism

The choice of `(A,B)` as the product of `A` and `B` is not unique. Another logical and equivalent choice would have been:

```hs
data Pair a b = Pair a b

```

Moreover, we could have also chosen `(B,A)` as the product, or even `(B,A,())`, and we could find a decomposition function like the above also following the rules:

```hs
decompose2 :: (W -> A) -> (W -> B) -> (W -> (B,A,()))
decompose2 f1 f2 = (\x -> (f2 x, f1 x, ()))

```

This is because the product is not unique but **unique up to isomorphism**. Every two products of `A` and `B` do not have to be equal, but they should be isomorphic. As an example, the two different products we have just defined, `(A,B)` and `(B,A,())`, are isomorphic:

```hs
iso1 :: (A,B) -> (B,A,())
iso1 (x,y) = (y,x,())

iso2 :: (B,A,()) -> (A,B)
iso2 (y,x,()) = (x,y)

```

### Uniqueness of the decomposition

It is important to remark that also the decomposition function must be unique. There are types which follow all the rules required to be product, but the decomposition is not unique. As an example, we can try to use `(A,(B,Bool))` with projections `fst` `fst . snd` as a product of `A` and `B`:

```hs
decompose3 :: (W -> A) -> (W -> B) -> (W -> (A,(B,Bool)))
decompose3 f1 f2 = (\x -> (f1 x, (f2 x, True)))

```

We can check that it does work:

```hs
fst         . (decompose3 f1 f2) = f1 x
(fst . snd) . (decompose3 f1 f2) = f2 x

```

But the problem here is that we could have written another decomposition, namely:

```hs
decompose3' :: (W -> A) -> (W -> B) -> (W -> (A,(B,Bool)))
decompose3' f1 f2 = (\x -> (f1 x, (f2 x, False)))

```

And, as the decomposition is **not unique**, `(A,(B,Bool))` is **not** the product of `A` and `B` in **Hask**



## Coproduct of types in Hask


### Intuition

The categorical product of two types **A** and **B** should contain the minimal information necessary to contain inside an instance of type **A** or type **B**. We can see now that the intuitive coproduct of two types should be `Either a b`. Other candidates, such as `Either a (b,Bool)`, would contain a part of unnecessary information, and they wouldn't be minimal.

The formal definition is derived from the categorical definition of coproduct.

### Categorical coproducts

A categorical coproduct is the dual notion of a categorical product. It is obtained directly by reversing all the arrows in the definition of the product. The coproduct of two objects **X**,**Y** is another object **Z** with two inclusions: **i_1: X → Z** and **i_2: Y → Z**; such that any other two morphisms from **X** and **Y** to another object decompose uniquely through those inclusions. In other words, if there are two morphisms **f₁ : X → W** and **f₂ : Y → W**, exists a unique morphism **g : Z →  W** such that **g ○ i₁ = f₁** and **g ○ i₂ = f₂**

### Coproducts in Hask

The translation into the **Hask** category is similar to the translation of the product:

```hs
-- if there are two functions
f1 :: A -> W
f2 :: B -> W
-- and we have a coproduct with two inclusions
i1 :: A -> Z
i2 :: B -> Z
-- we can construct a unique function
g  :: Z -> W
-- such that the other two functions decompose using g
g . i1 == f1
g . i2 == f2

```

The coproduct type of two types `A` and `B` in **Hask** is `Either a b` or any other type isomorphic to it:

```hs
-- Coproduct
-- The two inclusions are Left and Right
data Either a b = Left a | Right b

-- If we have those functions, we can decompose them through the coproduct
decompose :: (A -> W) -> (B -> W) -> (Either A B -> W)
decompose f1 f2 (Left x)  = f1 x
decompose f1 f2 (Right y) = f2 y 


```



## Haskell Applicative in terms of Category Theory


A Haskell's `Functor` allows one to map any type `a` (an object of **Hask**) to a type `F a` and also map a function `a -> b` (a morphism of **Hask**) to a function with type `F a -> F b`. This corresponds to a Category Theory definition in a sense that functor preserves basic category structure.

A **monoidal category** is a category that has some **additional** structure:

- A tensor product (see [Product of types in Hask](http://stackoverflow.com/documentation/haskell/2261/category-theory/14649/product-of-types-in-hask#t=201608041651141739494))
- A tensor unit (unit object)

Taking a pair as our product, this definition can be translated to Haskell in the following way:

```hs
class Functor f => Monoidal f where
    mcat :: f a -> f b -> f (a,b)
    munit :: f ()

```

The `Applicative` class is equivalent to this `Monoidal` one and thus can be implemented in terms of it:

```hs
instance Monoidal f => Applicative f where
    pure x = fmap (const x) munit
    f <*> fa = (\(f, a) -> f a) <$> (mcat f fa)

```

