---
metaTitle: "Haskell - Containers - Data.Map"
description: "Monoid instance, Constructing, Checking If Empty, Finding Values, Inserting Elements, Deleting Elements, Importing the Module"
---

# Containers - Data.Map



## Monoid instance


`Map k v` provides a [Monoid](https://stackoverflow.com/documentation/haskell/1879/type-classes/7940/monoid#t=201608211304123263501) instance with the following semantics:

- `mempty` is the empty `Map`, i.e. the same as [`Map.empty`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:empty)
- `m1 <> m2` is the left-biased union of `m1` and `m2`, i.e. if any key is present both in `m1` and `m2`, then the value from `m1` is picked for `m1 <> m2`. This operation is also available outside the `Monoid` instance as [`Map.union`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:union).



## Constructing


We can create a Map from a list of tuples like this:

A Map can also be constructed with a single value:

There is also the `empty` function.

Data.Map also supports typical set operations such as [`union`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:union), [`difference`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:difference) and [`intersection`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:intersection).



## Checking If Empty


We use the `null` function to check if a given Map is empty:



## Finding Values


There are [many](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#g:4) querying operations on maps.

`member :: Ord k => k -> Map k a -> Bool` yields `True` if the key of type `k` is in `Map k a`:

`notMember` is similar:

You can also use [`findWithDefault :: Ord k => a -> k -> Map k a -> a`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#v:findWithDefault) to yield a default value if the key isn't present:



## Inserting Elements


[Inserting](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Map-Lazy.html#g:6) elements is simple:



## Deleting Elements




## Importing the Module


The `Data.Map` module in the [`containers` package](https://hackage.haskell.org/package/containers) provides a `Map` structure that has both strict and lazy implementations.

When using `Data.Map`, one usually imports it qualified to avoid clashes with functions already defined in Prelude:

```hs
Map.empty -- give me an empty Map

```

