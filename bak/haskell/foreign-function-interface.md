---
metaTitle: "Haskell - Foreign Function Interface"
description: "Calling C from Haskell, Passing Haskell functions as callbacks to C code."
---

# Foreign Function Interface



## Calling C from Haskell


For performance reasons, or due to the existence of mature C libraries, you may want to call C code from a Haskell program. Here is a simple example of how you can pass data to a C library and get an answer back.

foo.c:

```hs
#include <inttypes.h>

int32_t foo(int32_t a) {
  return a+1;
}

```

Foo.hs:

```hs
import Data.Int

main :: IO ()
main = print =<< hFoo 41

foreign import ccall unsafe "foo" hFoo :: Int32 -> IO Int32

```

The `unsafe` keyword generates a more efficient call than 'safe', but requires that the C code never makes a callback to the Haskell system. Since `foo` is completely in C and will never call Haskell, we can use `unsafe`.

We also need to instruct cabal to compile and link in C source.

foo.cabal:

```hs
name:                foo
version:             0.0.0.1
build-type:          Simple
extra-source-files:  *.c
cabal-version:       >= 1.10

executable foo
  default-language: Haskell2010
  main-is:       Foo.hs
  C-sources:     foo.c
  build-depends: base

```

Then you can run:

```hs
> cabal configure
> cabal build foo
> ./dist/build/foo/foo
42

```



## Passing Haskell functions as callbacks to C code.


It is very common for C functions to accept pointers to other functions as arguments. Most popular example is setting an action to be executed when a button is clicked in some GUI toolkit library. It is possible to pass Haskell functions as C callbacks.

To call this C function:

```hs
void event_callback_add (Object *obj, Object_Event_Cb func, const void *data)

```

we first import it to Haskell code:

```hs
foreign import ccall "header.h event_callback_add"
    callbackAdd :: Ptr () -> FunPtr Callback -> Ptr () -> IO ()

```

Now looking at how `Object_Event_Cb` is defined in C header, define what `Callback` is in Haskell:

```hs
type Callback = Ptr () -> Ptr () -> IO ()

```

Finally, create a special function that would wrap Haskell function of type `Callback` into a pointer `FunPtr Callback`:

```hs
foreign import ccall "wrapper"
    mkCallback :: Callback -> IO (FunPtr Callback)

```

Now we can register callback with C code:

```hs
cbPtr <- mkCallback $ \objPtr dataPtr -> do
    -- callback code
    return ()
callbackAdd cpPtr

```

It is important to free allocated `FunPtr` once you unregister the callback:

```hs
freeHaskellFunPtr cbPtr

```



#### Syntax


- foreign import ccall unsafe "foo" hFoo :: Int32 -> IO Int32 {- Imports a function named `foo` in some object file, and defines the symbol `hFoo` which can be called with Haskell code. -}



#### Remarks


While cabal has support for including a C and C++ libraries in a Haskell package, there are a few bugs. First, if you have data (rather than a function) defined in `b.o` that is used in `a.o`, and list the `C-sources: a.c, b.c`, then cabal will be unable to find the data. This is documented in [#12152](https://ghc.haskell.org/trac/ghc/ticket/12152). A workaround when using cabal is to reorder the `C-sources` list to be `C-sources: b.c, a.c`. This may not work when using stack, because stack always links the `C-sources` alphabetically, regardless of the order in which you list them.

Another issues is that you must surround any C++ code in header (.h) files with `#ifdef __cplusplus` guards. This is because GHC doesn't understand C++ code in header files. You can still write C++ code in header files, but you must surround it with guards.

`ccall` refers to the **calling convention**; currently `ccall` and `stdcall` (Pascal convention) are supported. The `unsafe` keyword is optional; this reduces overhead for simple functions but may cause deadlocks if the foreign function blocks indefinitely or has insufficient permission to execute[1](https://wiki.haskell.org/Foreign_Function_Interface).

