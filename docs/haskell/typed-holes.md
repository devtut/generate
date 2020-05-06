---
metaTitle: "Haskell - Typed holes"
description: "Syntax of typed holes, Semantics of typed holes, Using typed holes to define a class instance"
---

# Typed holes



## Syntax of typed holes


A typed hole is a single underscore (`_`) or a valid Haskell identifier which is not in scope, in an expression context. Before the existance of typed holes, both of these things would trigger an error, so the new syntax does not interfere with any old syntax.

### Controlling behaviour of typed holes

The default behaviour of typed holes is to produce a compile-time error when encountering a typed hole. However, there are several flags to fine-tune their behaviour. These flags are summarized as follows ([GHC trac](https://ghc.haskell.org/trac/ghc/ticket/9497)):

> 
<p>By default GHC has typed holes enabled and produces a compile error
when it encounters a typed hole.</p>
<p>When `-fdefer-type-errors` **or** `-fdefer-typed-holes` is enabled, hole
errors are converted to warnings and result in runtime errors when
evaluated.</p>
<p>The warning flag `-fwarn-typed-holes` is on by default. Without
`-fdefer-type-errors` or `-fdefer-typed-holes` this flag is a no-op, since typed holes are an error under these conditions. If either of the
defer flags are enabled (converting typed hole errors into warnings)
the `-fno-warn-typed-holes` flag disables the warnings. This means
compilation silently succeeds and evaluating a hole will produce a
runtime error.</p>




## Semantics of typed holes


The value of a type hole can simply said to be `undefined`, although a typed hole triggers a compile-time error, so it is not strictly necessary to assign it a value. However, a typed hole (when they are enabled) produces a compile time error (or warning with deferred type errors) which states the name of the typed hole, its inferred **most general** type, and the types of any local bindings. For example:

```hs
Prelude> \x -> _var + length (drop 1 x)

<interactive>:19:7: Warning:
    Found hole `_var' with type: Int
    Relevant bindings include
      x :: [a] (bound at <interactive>:19:2)
      it :: [a] -> Int (bound at <interactive>:19:1)
    In the first argument of `(+)', namely `_var'
    In the expression: _var + length (drop 1 x)
    In the expression: \ x -> _var + length (drop 1 x)

```

Note that in the case of typed holes in expressions entered into the GHCi repl (as above), the type of the expression entered also reported, as `it` (here of type `[a] -> Int`).



## Using typed holes to define a class instance


Typed holes can make it easier to define functions, through an interactive process.

Say you want to define a class instance `Foo Bar` (for your custom `Bar` type, in order to use it with some polymorphic library function that requires a `Foo` instance). You would now traditionally look up the documentation of `Foo`, figure out which methods you need to define, scrutinise their types etc. – but with typed holes, you can actually skip that!

First just define a dummy instance:

```hs
instance Foo Bar where

```

The compiler will now complain

```hs
Bar.hs:13:10: Warning:
No explicit implementation for
  ‘foom’ and ‘quun’
In the instance declaration for ‘Foo Bar’

```

Ok, so we need to define `foom` for `Bar`. But what **is** that even supposed to be? Again we're too lazy to look in the documentation, and just ask the compiler:

```hs
instance Foo Bar where
  foom = _

```

Here we've used a typed hole as a simple “documentation query”. The compiler outputs

```hs
Bar.hs:14:10:
    Found hole ‘_’ with type: Bar -> Gronk Bar
    Relevant bindings include
      foom :: Bar -> Gronk Bar (bound at Foo.hs:4:28)
    In the expression: _
    In an equation for ‘foom’: foom = _
    In the instance declaration for ‘Foo Bar’

```

Note how the compiler has already filled the class type variable with the concrete type `Bar` that we want to instantiate it for. This can make the signature a lot easier to understand than the polymorphic one found in the class documentation, especially if you're dealing with a more complicated method of e.g. a multi-parameter type class.

But what the hell is `Gronk`? At this point, it is probably a good idea to ask [Hayoo](http://hayoo.fh-wedel.de/?query=Gronk). However we may still get away without that: as a blind guess, we assume that this is not only a type constructor but also the single value constructor, i.e. it can be used as a function that will somehow produce a `Gronk a` value. So we try

```hs
instance Foo Bar where
  foom bar = _ Gronk

```

If we're lucky, `Gronk` is actually a value, and the compiler will now say

```

   Found hole ‘_’
      with type: (Int -> [(Int, b0)] -> Gronk b0) -> Gronk Bar
    Where: ‘b0’ is an ambiguous type variable

```

Ok, that's ugly – at first just note that `Gronk` has two arguments, so we can refine our attempt:

```hs
instance Foo Bar where
  foom bar = Gronk _ _

```

And this now is pretty clear:

```

   Found hole ‘_’ with type: [(Int, Bar)]
    Relevant bindings include
      bar :: Bar (bound at Bar.hs:14:29)
      foom :: Bar -> Gronk Bar (bound at Foo.hs:15:24)
    In the second argument of ‘Gronk’, namely ‘_’
    In the expression: Gronk _ _
    In an equation for ‘foom’: foom bar = Gronk _ _

```

You can now further progress by e.g. deconstructing the `bar` value (the components will then show up, with types, in the `Relevant bindings` section). Often, it is at some point completely obvious what the correct definition will be, because you you see all avaliable arguments and the types fit together like a jigsaw puzzle. Or alternatively, you may see that the definition is **impossible** and why.

All of this works best in an editor with interactive compilation, e.g. Emacs with haskell-mode. You can then use typed holes much like mouse-over value queries in an IDE for an interpreted dynamic imperative language, but without all the limitations.



#### Remarks


One of the strengths of Haskell is the ability to leverage the type system to model parts of your problem domain in the type system. In doing so, one often encounters very complex types. When writing programs with these types (i.e. with values having these types) it occasionally becomes nearly unmangeable to 'juggle' all of the types. As of GHC 7.8, there is a new syntactic feature called typed holes. Typed holes do not change the semantics of the core language; they are intended purely as an aid for writing programs.

For an in-depth explanation of typed holes, as well as a discussion of the design of typed holes, see the [Haskell wiki](https://wiki.haskell.org/GHC/Typed_holes).

Section of the GHC user guide on [typed holes](https://downloads.haskell.org/%7Eghc/7.8.4/docs/html/users_guide/typed-holes.html).

