---
metaTitle: "Haskell - Lens"
description: "Lenses for records, Manipulating tuples with Lens, Lens and Prism, Stateful Lenses, Lenses compose, Writing a lens without Template Haskell, Traversals, Classy Lenses, Fields with makeFields"
---

# Lens


[Lens](https://hackage.haskell.org/package/lens) is a library for Haskell that provides lenses, isomorphisms, folds, traversals, getters and setters, which exposes a uniform interface for querying and manipulating arbitrary structures, not unlike Java's accessor and mutator concepts.



## Lenses for records


### Simple record

```hs
{-# LANGUAGE TemplateHaskell #-}
import Control.Lens

data Point = Point {
    _x :: Float,
    _y :: Float
}
makeLenses ''Point

```

Lenses `x` and `y` are created.

```hs
let p = Point 5.0 6.0 
p ^. x     -- returns 5.0
set x 10 p -- returns Point { _x = 10.0, _y = 6.0 }
p & x +~ 1 -- returns Point { _x = 6.0, _y = 6.0 }

```

### Managing records with repeating fields names

```hs
data Person = Person { _personName :: String }
makeFields ''Person

```

Creates a type class `HasName`, lens `name` for `Person`, and makes `Person` an instance of `HasName`. Subsequent records will be added to the class as well:

```hs
data Entity = Entity { _entityName :: String }
makeFields ''Entity

```

The Template Haskell extension is required for `makeFields` to work. Technically, it's entirely possible to create the lenses made this way via other means, e.g. by hand.



## Manipulating tuples with Lens


Getting

```hs
("a", 1) ^. _1 -- returns "a"
("a", 1) ^. _2 -- returns 1

```

Setting

```hs
("a", 1) & _1 .~ "b" -- returns ("b", 1)

```

Modifying

```hs
("a", 1) & _2 %~ (+1) -- returns ("a", 2)

```

`both` Traversal

```hs
(1, 2) & both *~ 2 -- returns (2, 4)

```



## Lens and Prism


A `Lens' s a` means that you can **always** find an `a` within any `s`. A `Prism' s a` means that you can **sometimes** find that `s` actually just **is** `a` but sometimes it's something else.

To be more clear, we have `_1 :: Lens' (a, b) a`  because any tuple **always** has a first element. We have `_Just :: Prism' (Maybe a) a` because **sometimes** `Maybe a` is actually an `a` value wrapped in `Just` but **sometimes** it's `Nothing`.

With this intuition, some standard combinators can be interpreted parallel to one another

- `view :: Lens' s a -> (s -> a)` "gets" the `a` out of the `s`
- `set :: Lens' s a -> (a -> s -> s)` "sets" the `a` slot in `s`
- `review :: Prism' s a -> (a -> s)` "realizes" that an `a` could be an `s`
- `preview :: Prism' s a -> (s -> Maybe a)` "attempts" to turn an `s` into an `a`.

Another way to think about it is that a value of type `Lens' s a` demonstrates that `s` has the same structure as `(r, a)` for some unknown `r`. On the other hand, `Prism' s a` demonstrates that `s` has the same structure as `Either r a` for some `r`. We can write those four functions above with this knowledge:



## Stateful Lenses


Lens operators have useful variants that operate in stateful contexts. They are obtained by replacing `~` with `=` in the operator name.

```hs
(+~) :: Num a => ASetter s t a a -> a -> s -> t
(+=) :: (MonadState s m, Num a) => ASetter' s a -> a -> m ()

```

> 
Note: The stateful variants aren't expected to change the type, so they have the `Lens'` or `Simple Lens'` signatures.


### Getting rid of `&` chains

If lens-ful operations need to be chained, it often looks like this:

```hs
change :: A -> A
change a = a & lensA %~ operationA
             & lensB %~ operationB
             & lensC %~ operationC

```

This works thanks to the associativity of `&`. The stateful version is clearer, though.

```hs
change a = flip execState a $ do
    lensA %= operationA
    lensB %= operationB
    lensC %= operationC

```

If `lensX` is actually `id`, the whole operation can of course be executed directly by just lifting it with `modify`.

### Imperative code with structured state

Assuming this example state:

```hs
data Point = Point { _x :: Float, _y :: Float }
data Entity = Entity { _position :: Point, _direction :: Float }
data World = World { _entities :: [Entity] }

makeLenses ''Point
makeLenses ''Entity
makeLenses ''World

```

We can write code that resembles classic imperative languages, while still allowing us to use benefits of Haskell:

```hs
updateWorld :: MonadState World m => m ()
updateWorld = do
    -- move the first entity
    entities . ix 0 . position . x += 1

    -- do some operation on all of them
    entities . traversed . position %= \p -> p `pointAdd` ...

    -- or only on a subset
    entities . traversed . filtered (\e -> e ^. position.x > 100) %= ...

```



## Lenses compose


If you have a `f :: Lens' a b` and a `g :: Lens' b c` then `f . g` is a `Lens' a c` gotten by following `f` first and then `g`. Notably:

- Lenses compose as functions (really they just **are** functions)
- If you think of the `view` functionality of `Lens`, it seems like data flows "left to right"—this might feel backwards to your normal intuition for function composition. On the other hand, it ought to feel natural if you think of `.`-notation like how it happens in OO languages.

More than just composing `Lens` with `Lens`, `(.)` can be used to compose nearly any "`Lens`-like" type together. It's not always easy to see what the result is since the type becomes tougher to follow, but you can use [the `lens` chart](https://hackage.haskell.org/package/lens) to figure it out. The composition `x . y` has the type of the least-upper-bound of the types of both `x` and `y` in that chart.



## Writing a lens without Template Haskell


To demystify Template Haskell, suppose you have

then

produces (more or less)

There's nothing particularly magical going on, though. You can write these yourself:

Essentially, you want to "visit" your lens' "focus" with the `wrap` function and then rebuild the "entire" type.



## Traversals


A `Traversal' s a` shows that `s` has 0-to-many `a`s inside of it.

```hs
toListOf :: Traversal' s a -> (s -> [a])

```

Any type `t` which is `Traversable` automatically has that `traverse :: Traversal (t a) a`.

We can use a `Traversal` to set or map over all of these `a` values

```hs
> set traverse 1 [1..10]
[1,1,1,1,1,1,1,1,1,1]

> over traverse (+1) [1..10]
[2,3,4,5,6,7,8,9,10,11]

```

A `f :: Lens' s a` says there's exactly one `a` inside of `s`. A `g :: Prism' a b` says there are either 0 or 1 `b`s in `a`. Composing `f . g` gives us a `Traversal' s b` because following `f` and then `g` shows how there there are 0-to-1 `b`s in `s`.



## Classy Lenses


In addition to the standard `makeLenses` function for generating `Lens`es, `Control.Lens.TH` also offers the `makeClassy` function. `makeClassy` has the same type and works in essentially the same way as `makeLenses`, with one key difference. In addition to generating the standard lenses and traversals, if the type has no arguments, it will also create a class describing all the datatypes which possess the type as a field. For example

```hs
data Foo = Foo { _fooX, _fooY :: Int }
  makeClassy ''Foo

```

will create

```hs
class HasFoo t where
   foo :: Simple Lens t Foo

instance HasFoo Foo where foo = id

fooX, fooY :: HasFoo t => Simple Lens t Int

```



## Fields with makeFields


(This example copied from [this StackOverflow answer](http://stackoverflow.com/a/34624414/163177))

Let's say you have a number of different data types that all ought to have a lens with the same name, in this case `capacity`.  The `makeFields` slice will create a class that accomplish this without namespace conflicts.

```hs
{-# LANGUAGE FunctionalDependencies
           , MultiParamTypeClasses
           , TemplateHaskell
  #-}

module Foo
where

import Control.Lens

data Foo
  = Foo { fooCapacity :: Int }
  deriving (Eq, Show)
$(makeFields ''Foo)

data Bar
  = Bar { barCapacity :: Double }
  deriving (Eq, Show)
$(makeFields ''Bar)

```

Then in ghci:

```hs
*Foo
λ let f = Foo 3
|     b = Bar 7
| 
b :: Bar
f :: Foo

*Foo
λ fooCapacity f
3
it :: Int

*Foo
λ barCapacity b
7.0
it :: Double

*Foo
λ f ^. capacity
3
it :: Int

*Foo
λ b ^. capacity
7.0
it :: Double

λ :info HasCapacity 
class HasCapacity s a | s -> a where
  capacity :: Lens' s a
    -- Defined at Foo.hs:14:3
instance HasCapacity Foo Int -- Defined at Foo.hs:14:3
instance HasCapacity Bar Double -- Defined at Foo.hs:19:3

```

So what it's actually done is declared a class `HasCapacity s a`, where capacity is a `Lens'` from `s` to `a` (`a` is fixed once s is known). It figured out the name "capacity" by stripping off the (lowercased) name of the data type from the field; I find it pleasant not to have to use an underscore on either the field name or the lens name, since sometimes record syntax is actually what you want. You can use makeFieldsWith and the various lensRules to have some different options for calculating the lens names.

In case it helps, using ghci -ddump-splices Foo.hs:

```hs
[1 of 1] Compiling Foo              ( Foo.hs, interpreted )
Foo.hs:14:3-18: Splicing declarations
    makeFields ''Foo
  ======>
    class HasCapacity s a | s -> a where
      capacity :: Lens' s a
    instance HasCapacity Foo Int where
      {-# INLINE capacity #-}
      capacity = iso (\ (Foo x_a7fG) -> x_a7fG) Foo
Foo.hs:19:3-18: Splicing declarations
    makeFields ''Bar
  ======>
    instance HasCapacity Bar Double where
      {-# INLINE capacity #-}
      capacity = iso (\ (Bar x_a7ne) -> x_a7ne) Bar
Ok, modules loaded: Foo.

```

So the first splice made the class `HasCapcity` and added an instance for Foo; the second used the existing class and made an instance for Bar.

This also works if you import the `HasCapcity` class from another module; `makeFields` can add more instances to the existing class and spread your types out across multiple modules. But if you use it again in another module where you haven't imported the class, it'll make a new class (with the same name), and you'll have two separate overloaded capacity lenses that are not compatible.



#### Remarks


### What is a Lens?

Lenses (and other optics) allow us to separate describing **how** we want to access some data from **what** we want to do with it. It is important to distinguish between the abstract notion of a lens and the concrete implementation. Understanding abstractly makes programming with `lens` much easier in the long run. There are many isomorphic representations of lenses so for this discussion we will avoid
any concrete implementation discussion and instead give a high-level overview of the concepts.

### Focusing

An important concept in understanding abstractly is the notion of **focusing**. Important optics **focus** on a specific part of a larger data structure without forgetting about the larger context. For example, the lens `_1` focuses on the first
element of a tuple but doesn't forget about what was in the second field.

Once we have focus, we can then talk about which operations we are allowed to perform with a lens. Given a `Lens s a` which when given a datatype of type `s` focuses on a specific `a`, we can either

1. Extract the `a` by forgetting about the additional context or
1. Replace the `a` by providing a new value

These correspond to the well-known `get` and `set` operations which are usually used to characterise a lens.

### Other Optics

We can talk about other optics in a similar fashion.

|Optic|Focuses on...
|---|---|---|---|---|---|---|---|---|---
|Lens|One part of a product
|Prism|One part of a sum
|Traversal|Zero or more parts of a data structure
|Isomorphism|...

Each optic focuses in a different way, as such, depending on which type of optic
we have we can perform different operations.

### Composition

What's more, we can compose any of the two optics we have so-far discussed in order
to specify complex data accesses. The four types of optics we have discussed form a lattice, the result of composing two optics together is their upper bound.

[<img src="https://i.stack.imgur.com/nPIlo.png" alt="enter image description here" />](https://i.stack.imgur.com/nPIlo.png)

For example, if we compose together a lens and a prism, we get a traversal. The reason for this is that by their (vertical) composition, we first focus on one part of a product and then on one part of a sum. The result being an optic which focuses on precisely zero or one parts of our data which is a special case of a traversal. (This is also sometimes called an affine traversal).

### In Haskell

The reason for the popularity in Haskell is that there is a very succinct representation of optics. All optics are just functions of a certain form which can
be composed together using function composition. This leads to a very light-weight
embedding which makes it easy to integrate optics into your programs. Further to this, due to the particulars of the encoding, function composition also automatically computes the upper bound of two optics we compose. This means that
we can reuse the same combinators for different optics without explicit casting.

