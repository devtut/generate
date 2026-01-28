---
metaTitle: "Haskell - Profunctor"
description: "(->) Profunctor"
---

# Profunctor


`Profunctor` is a typeclass provided by the `profunctors` package in [`Data.Profunctor`](http://hackage.haskell.org/package/profunctors-5.2/docs/Data-Profunctor.html).

See the "Remarks" section for a full explanation.



## (->) Profunctor


(->) is a simple example of a profunctor: the left argument is the input to a function, and the right argument is the same as the reader functor instance.

```hs
instance Profunctor (->) where
    lmap f g = g . f
    rmap f g = g . g

```



#### Syntax


- dimap :: Profunctor p => (a -> b) -> (c -> d) -> p b c -> p a d
- lmap :: Profunctor p => (a -> b) -> p b c -> p a c
- rmap :: Profunctor p => (b -> c) -> p a b -> p a c
- dimap id id = id
- lmap id = id
- rmap id = id
- dimap f g = lmap f . rmap g
- lmap f = dimap f id
- rmap f = dimap id f



#### Remarks


Profunctors are, as described by the docs on Hackage, "a bifunctor where the first argument is contravariant and the second argument is covariant."

So what does this mean? Well, a bifunctor is like a normal functor, except that it has two parameters instead of one, each with its own `fmap`-like function to map on it.

Being "covariant" means that the second argument to a profunctor is just like a normal functor: its mapping function (`rmap`) has a type signature of `Profunctor p => (b -> c) -> p a b -> p a c`.  It just maps the function on the second argument.

Being "contravariant" makes the first argument a little weirder.  Instead of mapping like a normal functor, its mapping function (`lmap`) has a type signature of `Profunctor p => (a -> b) -> p b c -> p a c`.  This seemingly backward mapping makes most sense for inputs to a function: you would run `a -> b` on the input, and then your other function, leaving the new input as `a`.

**Note:** The naming for the normal, one argument functors is a little misleading: the [`Functor` typeclass](http://hackage.haskell.org/package/base-4.9.1.0/docs/Data-Functor.html#t:Functor) implements "covariant" functors, while "contravariant" functors are implemented in the [`Contravariant` typeclass in `Data.Functor.Contravariant`](https://hackage.haskell.org/package/contravariant-1.4/docs/Data-Functor-Contravariant.html#t:Contravariant), and previously the (misleadingly named) [`Cofunctor` typeclass in `Data.Cofunctor`](https://hackage.haskell.org/package/cofunctor-0.1.0.1/docs/Data-Cofunctor.html#t:Cofunctor).

