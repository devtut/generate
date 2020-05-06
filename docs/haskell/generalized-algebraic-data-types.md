---
metaTitle: "Haskell - Generalized Algebraic Data Types"
description: "Basic Usage"
---

# Generalized Algebraic Data Types



## Basic Usage


When the `GADTs` extension is enabled, besides regular data declarations, you can also declare generalized algebraic datatypes as follows:

```hs
data DataType a where
    Constr1 :: Int -> a -> Foo a -> DataType a
    Constr2 :: Show a => a -> DataType a
    Constr3 :: DataType Int

```

A GADT declaration lists the types of all constructors a datatype has, explicitly. Unlike regular datatype declarations, the type of a constructor can be any N-ary (including nullary) function that ultimately results in the datatype applied to some arguments.

In this case we've declared that the type `DataType` has three constructors: `Constr1`, `Constr2` and `Constr3`.

The `Constr1` constructor is no different from one declared using a regular data declaration: `data DataType a = Constr1 Int a (Foo a) | ...`

`Constr2` however requires that `a` has an instance of `Show`, and so when using the constructor the instance would need to exist. On the other hand, when pattern-matching on it, the fact that `a` is an instance of `Show` comes into scope, so you can write:

```hs
foo :: DataType a -> String
foo val = case val of
    Constr2 x -> show x
    ...

```

Note that the `Show a` constraint doesn't appear in the type of the function, and is only visible in the code to the right of `->`.

`Constr3` has type `DataType Int`, which means that whenever a value of type `DataType a` is a `Constr3`, it is known that `a ~ Int`. This information, too, can be recovered with a pattern match.

