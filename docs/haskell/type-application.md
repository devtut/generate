---
metaTitle: "Haskell - Type Application"
description: "Avoiding type annotations, Type applications in other languages, Order of parameters, Interaction with ambiguous types"
---

# Type Application


`TypeApplications` are an alternative to type **annotations** when the compiler struggles to infer types for a given expression.

This series of examples will explain the purpose of the `TypeApplications` extension and how to use it

Don't forget to enable the extension by placing `{-# LANGUAGE TypeApplications #-}` at the top of your source file.



## Avoiding type annotations


We use type annotations to avoid ambiguity. Type applications can be used for the same purpose. For example

```hs
x :: Num a => a
x = 5

main :: IO ()
main = print x

```

This code has an ambiguity error. We know that `a` has a `Num` instance, and in order to print it we know it needs a `Show` instance. This could work if `a` was, for example, an `Int`, so to fix the error we can add a type annotation

```hs
main = print (x :: Int)

```

Another solution using type applications would look like this

```hs
main = print @Int x

```

To understand what this means we need to look at the type signature of `print`.

```hs
print :: Show a => a -> IO ()

```

The function takes one parameter of type `a`, but another way to look at it is that it actually takes two parameters. The first one is a **type** parameter, the second one is a value whose type is the first parameter.

The main difference between value parameters and the type parameters is that the latter ones are implicitly provided to functions when we call them. Who provides them? The type inference algorithm! What `TypeApplications` let us do is give those type parameters explicitly. This is especially useful when the type inference can't determine the correct type.

So to break down the above example

```hs
print :: Show a => a -> IO ()
print @Int :: Int -> IO ()
print @Int x :: IO ()

```



## Type applications in other languages


If you're familiar with languages like Java, C# or C++ and the concept of generics/templates then this comparison might be useful for you.

Say we have a generic function in C#

```hs
public static T DoNothing<T>(T in) { return in; }

```

To call this function with a `float` we can do `DoNothing(5.0f)` or if we want to be explicit we can say `DoNothing<float>(5.0f)`. That part inside of the angle brackets is the type application.

In Haskell it's the same, except that the type parameters are not only implicit at call sites but also at definition sites.

```hs
doNothing :: a -> a
doNothing x = x

```

This can also be made explicit using either `ScopedTypeVariables`, `Rank2Types` or `RankNTypes` extensions like this.

```hs
doNothing :: forall a. a -> a
doNothing x = x

```

Then at the call site we can again either write `doNothing 5.0` or `doNothing @Float 5.0`



## Order of parameters


The problem with type arguments being implicit becomes obvious once we have more than one. Which order do they come in?

```hs
const :: a -> b -> a

```

Does writing `const @Int` mean `a` is equal to `Int`, or is it `b`?
In case we explicitly state the type parameters using a `forall` like `const :: forall a b. a -> b -> a` then the order is as written: `a`, then `b`.

If we don't, then the order of variables is from left to right. The first variable to be mentioned is the first type parameter, the second is the second type parameter and so on.

What if we want to specify the second type variable, but not the first? We can use a wildcard for the first variable like this

```hs
const @_ @Int

```

The type of this expression is

```hs
const @_ @Int :: a -> Int -> a

```



## Interaction with ambiguous types


Say you're introducing a class of types that have a size in bytes.

```hs
class SizeOf a where
    sizeOf :: a -> Int

```

The problem is that the size should be constant for every value of that type. We don't actually want the `sizeOf` function to depend on `a`, but only on it's type.

Without type applications, the best solution we had was the `Proxy` type defined like this

```hs
data Proxy a = Proxy

```

The purpose of this type is to carry type information, but no value information. Then our class could look like this

```hs
class SizeOf a where
    sizeOf :: Proxy a -> Int

```

Now you might be wondering, why not drop the first argument altogether? The type of our function would then just be `sizeOf :: Int` or, to be more precise because it is a method of a class, `sizeOf :: SizeOf a => Int` or to be even more explicit `sizeOf :: forall a. SizeOf a => Int`.

The problem is type inference. If I write `sizeOf` somewhere, the inference algorithm only knows that I expect an `Int`. It has no idea what type I want to substitute for `a`. Because of this, the definition gets rejected by the compiler **unless** you have the `{-# LANGUAGE AllowAmbiguousTypes #-}` extension enabled. In that case the definition compiles,it just can't be used anywhere without an ambiguity error.

Luckily, the introduction of type applications saves the day! Now we can write `sizeOf @Int`, explicitly saying that `a` is `Int`. Type applications allow us to provide a type parameter, even if it doesn't appear in the **actual parameters of the function**!

