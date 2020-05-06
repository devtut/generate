---
metaTitle: "Haskell - Proxies"
description: "Using Proxy, The polymorphic proxy idiom, Proxy is like ()"
---

# Proxies




## Using Proxy


The `Proxy :: k -> *` type, found in [`Data.Proxy`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Proxy.html), is used when you need to give the compiler some type information - eg, to pick a type class instance - which is nonetheless irrelevant at runtime.

```hs
{-# LANGUAGE PolyKinds #-}

data Proxy a = Proxy

```

Functions which use a `Proxy` typically use `ScopedTypeVariables` to pick a type class instance based on the `a` type.

For example, the classic example of an ambiguous function,

```hs
showread :: String -> String
showread = show . read

```

which results in a type error because the elaborator doesn't know which instance of `Show` or `Read` to use, can be resolved using `Proxy`:

```hs
{-# LANGUAGE ScopedTypeVariables #-}

import Data.Proxy

showread :: forall a. (Show a, Read a) => Proxy a -> String -> String
showread _ = (show :: a -> String) . read

```

When calling a function with `Proxy`, you need to use a type annotation to declare which `a` you meant.

```hs
ghci> showread (Proxy :: Proxy Int) "3"
"3"
ghci> showread (Proxy :: Proxy Bool) "'m'"  -- attempt to parse a char literal as a Bool
"*** Exception: Prelude.read: no parse

```



## The "polymorphic proxy" idiom


Since `Proxy` contains no runtime information, there is never a need to pattern-match on the `Proxy` constructor. So a common idiom is to abstract over the `Proxy` datatype using a type variable.

```hs
showread :: forall proxy a. (Show a, Read a) => proxy a -> String -> String
showread _ = (show :: a -> String) . read

```

Now, if you happen to have an `f a` in scope for some `f`, you don't need to write out `Proxy :: Proxy a` when calling `f`.

```hs
ghci> let chars = "foo"  -- chars :: [Char]
ghci> showread chars "'a'"
"'a'"

```



## Proxy is like ()


Since `Proxy` contains no runtime information, you can always write a natural transformation `f a -> Proxy a` for any `f`.

```hs
proxy :: f a -> Proxy a
proxy _ = Proxy

```

This is just like how any given value can always be erased to `()`:

```hs
unit :: a -> ()
unit _ = ()

```

Technically, `Proxy` is the terminal object in the category of functors, just like `()` is the terminal object in the category of values.

