---
metaTitle: "Haskell - Type Families"
description: "Datatype Families, Type Synonym Families, Injectivity"
---

# Type Families




## Datatype Families


Data families can be used to build datatypes that have different implementations based on their type arguments.

### Standalone data families

```hs
{-# LANGUAGE TypeFamilies #-}
data family List a
data instance List Char = Nil | Cons Char (List Char)
data instance List () = UnitList Int

```

In the above declaration, `Nil :: List Char`, and `UnitList :: Int -> List ()`

### Associated data families

Data families can also be associated with typeclasses. This is often useful for types with “helper objects”, which are required for generic typeclass methods but need to contain different information depending on the concrete instance. For instance, indexing locations in a list just requires a single number, whereas in a tree you need a number to indicate the path at each node:

```hs
class Container f where
  data Location f
  get :: Location f -> f a -> Maybe a

instance Container [] where
  data Location [] = ListLoc Int
  get (ListLoc i) xs
    | i < length xs  = Just $ xs!!i
    | otherwise      = Nothing

instance Container Tree where
  data Location Tree = ThisNode | NodePath Int (Location Tree)
  get ThisNode (Node x _) = Just x
  get (NodePath i path) (Node _ sfo) = get path =<< get i sfo

```



## Type Synonym Families


Type synonym families are just type-level functions: they associate parameter types with result types. These come in three different varieties.

### Closed type-synonym families

These work much like ordinary value-level Haskell functions: you specify some clauses, mapping certain types to others:

```hs
{-# LANGUAGE TypeFamilies #-}
type family Vanquisher a where
    Vanquisher Rock = Paper
    Vanquisher Paper = Scissors
    Vanquisher Scissors = Rock

data Rock=Rock; data Paper=Paper; data Scissors=Scissors

```

### Open type-synonym families

These work more like typeclass instances: anybody can add more clauses in other modules.

```hs
type family DoubledSize w

type instance DoubledSize Word16 = Word32
type instance DoubledSize Word32 = Word64
-- Other instances might appear in other modules, but two instances cannot overlap
-- in a way that would produce different results.

```

### Class-associated type synonyms

An open type family can also be combined with an actual class. This is usually done when, like with [associated data families](http://stackoverflow.com/documentation/haskell/2955/type-families/10038/datatype-families), some class method needs additional helper objects, and these helper objects **can** be different for different instances but may possibly also shared. A good example is [`VectorSpace` class](http://hackage.haskell.org/package/vector-space-0.10.2/docs/Data-VectorSpace.html#t:VectorSpace):

```hs
class VectorSpace v where
  type Scalar v :: *
  (*^) :: Scalar v -> v -> v

instance VectorSpace Double where
  type Scalar Double = Double
  μ *^ n = μ * n

instance VectorSpace (Double,Double) where
  type Scalar (Double,Double) = Double
  μ *^ (n,m) = (μ*n, μ*m)
  
instance VectorSpace (Complex Double) where
  type Scalar (Complex Double) = Complex Double
  μ *^ n = μ*n

```

Note how in the first two instances, the implementation of `Scalar` is the same. This would not be possible with an associated data family: data families are [injective](https://en.wikipedia.org/wiki/Injective_function), type-synonym families aren't.

While non-injectivity opens up some possibilities like the above, it also makes type inference more difficult. For instance, the following will not typecheck:

```hs
class Foo a where
  type Bar a :: *
  bar :: a -> Bar a
instance Foo Int where
  type Bar Int = String
  bar = show
instance Foo Double where
  type Bar Double = Bool
  bar = (>0)

main = putStrLn (bar 1)

```

In this case, the compiler can't know what instance to use, because the argument to `bar` is itself just a polymorphic `Num` literal. And the type function `Bar` can't be resolved in “inverse direction”, precisely because it's not injective<sup>†</sup> and hence not invertible (there could be more than one type with `Bar a = String`).

<sup>†</sup><sub>With only these two instances, it **is** actually injective, but the compiler can't know somebody won't add more instances later on and thereby break the behaviour.</sub>



## Injectivity


Type Families are not necessarily injective. Therefore, we cannot infer the parameter from an application. For example, in `servant`, given a type `Server a` we cannot infer the type `a`. To solve this problem, we can use `Proxy`. For example, in `servant`, the `serve` function has type `... Proxy a -> Server a -> ...`. We can infer `a` from `Proxy a` because `Proxy` is defined by `data` which is injective.

