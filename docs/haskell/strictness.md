---
metaTitle: "Haskell - Strictness"
description: "Bang Patterns, Lazy patterns, Normal forms, Strict fields"
---

# Strictness




## Bang Patterns


Patterns annotated with a bang (`!`) are evaluated strictly instead of lazily.

```hs
foo (!x, y) !z = [x, y, z] 

```

In this example, `x` and `z` will both be evaluated to weak head normal form before returning the list. It's equivalent to:

```hs
foo (x, y) z = x `seq` z `seq` [x, y, z]

```

Bang patterns are enabled using the Haskell 2010 `BangPatterns` language extension.



## Lazy patterns


Lazy, or **irrefutable**, patterns (denoted with the syntax `~pat`) are patterns that always match, without even looking at the matched value. This means lazy patterns will match even bottom values. However, subsequent uses of variables bound in sub-patterns of an irrefutable pattern will force the pattern matching to occur, evaluating to bottom unless the match succeeds.

The following function is lazy in its argument:

```hs
f1 :: Either e Int -> Int
f1 ~(Right 1) = 42

```

and so we get

```hs
λ» f1 (Right 1)
42
λ» f1 (Right 2)
42
λ» f1 (Left "foo")
42
λ» f1 (error "oops!")
42
λ» f1 "oops!"
*** type mismatch ***

```

The following function is written with a lazy pattern but is in fact using the pattern's variable which forces the match, so will fail for `Left` arguments:

```hs
f2 :: Either e Int -> Int
f2 ~(Right x) = x + 1

λ» f2 (Right 1)
2
λ» f2 (Right 2)
3
λ» f2 (Right (error "oops!"))
*** Exception: oops!
λ» f2 (Left "foo")
*** Exception: lazypat.hs:5:1-21: Irrefutable pattern failed for pattern (Right x)
λ» f2 (error "oops!")
*** Exception: oops! 

```

`let` bindings are lazy, behave as irrefutable patterns:

```hs
act1 :: IO ()
act1 = do
    ss <- readLn
    let [s1, s2] = ss :: [String]
    putStrLn "Done"

act2 :: IO ()
act2 = do
    ss <- readLn
    let [s1, s2] = ss
    putStrLn s1

```

Here `act1` works on inputs that parse to any list of strings, whereas in `act2` the `putStrLn s1` needs the value of `s1` which forces the pattern matching for `[s1, s2]`, so it works only for lists of exactly two strings:

```hs
λ» act1
> ["foo"]
Done
λ» act2
> ["foo"]
*** readIO: no parse ***

```



## Normal forms


This example provides a brief overview - for a more in-depth explanation of **normal forms** and examples, see [this question](http://stackoverflow.com/questions/6872898/haskell-what-is-weak-head-normal-form).

### Reduced normal form

The reduced normal form (or just normal form, when the context is clear) of an expression is the result of evaluating all reducible subexpressions in the given expression. Due to the non-strict semantics of Haskell (typically called **laziness**), a subexpression is not reducible if it is under a binder (i.e. a lambda abstraction - `\x -> ..`). The normal form of an expression has the property that if it exists, it is unique.

In other words, it does not matter (in terms of denotational semantics) in which order you reduce subexpressions. However, the key to writing performant Haskell programs is often ensuring that the right expression is evaluated at the right time, i.e, the understanding the operational semantics.

An expression whose normal form is itself is said to be **in normal form**.

Some expressions, e.g. `let x = 1:x in x`, have no normal form, but are still  productive. The example expression still has a **value**, if one admits infinite values, which here is the list `[1,1, ...]`. Other expressions, such as `let y = 1+y in y`, have no value, or their value is `undefined`.

### Weak head normal form

The RNF corresponds to fully evaluating an expression  - likewise, the weak head normal form (WHNF) corresponds to evaluating to the **head** of the expression. The head of an expression `e` is fully evaluated if `e` is an application `Con e1 e2 .. en` and `Con` is a constructor; or an abstraction `\x -> e1`; or a partial application `f e1 e2 .. en`, where partial application means `f` takes more than `n` arguments (or equivalently, the type of `e` is a function type). In any case, the subexpressions `e1..en` can be evaluated or unevaluated for the expression to be in  WHNF - they can even be `undefined`.

The evaluation semantics of Haskell can be described in terms of the WHNF - to evaluate an expression `e`, first evaluate it to WHNF, then recursively evaluate all of its subexpressions from left to right.

The primitive `seq` function is used to evaluate an expression to WHNF. `seq x y` is denotationally equal to `y` (the value of `seq x y` is precisely `y`); furthermore `x` is evaluated to WHNF when `y` is evaluated to WHNF. An expression can also be evaluated to WHNF with a bang pattern (enabled by the `-XBangPatterns` extension), whose syntax is as follows:

```hs
f !x y = ... 

```

In which `x` will be evaluated to WHNF when `f` is evaluated, while `y` is not (necessarily) evaluated. A bang pattern can also appear in a constructor, e.g.

```hs
data X = Con A !B C .. N

```

in which case the constructor `Con` is said to be strict in the `B` field, which means the `B` field is evaluated to WHNF when the constructor is applied to sufficient (here, two) arguments.



## Strict fields


In a `data` declaration, prefixing a type with a bang (`!`) makes the field a **strict field**. When the data constructor is applied, those fields will be evaluated to weak head normal form, so the data in the fields is guaranteed to always be in weak head normal form.

Strict fields can be used in both record and non-record types:

```hs
data User = User
    { identifier :: !Int
    , firstName :: !Text
    , lastName :: !Text
    }

data T = MkT !Int !Int

```

