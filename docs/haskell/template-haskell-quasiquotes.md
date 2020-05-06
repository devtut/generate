---
metaTitle: "Haskell - Template Haskell & QuasiQuotes"
description: "Syntax of Template Haskell and Quasiquotes, The Q type, An n-arity curry"
---

# Template Haskell & QuasiQuotes



## Syntax of Template Haskell and Quasiquotes


Template Haskell is enabled by the `-XTemplateHaskell` GHC extension. This extension enables all the syntactic features further detailed in this section. The full details on Template Haskell are given by the [user guide](https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide/glasgow_exts.html#template-haskell).

### Splices

<li>
A splice is a new syntactic entity enabled by Template Haskell, written as `$(...)`, where `(...)` is some expression.
</li>
<li>
There must not be a space between `$` and the first character of the expression; and Template Haskell overrides the parsing of the `$` operator - e.g. `f$g` is normally parsed as `($) f g` whereas with Template Haskell enabled, it is parsed as a splice.
</li>
<li>
When a splice appears at the top level, the `$` may be omitted. In this case, the spliced expression is the entire line.
</li>
<li>
A splice represents code which is run at compile time to produce a Haskell AST, and that AST is compiled as Haskell code and inserted into the program
</li>
<li>
Splices can appear in place of: expressions, patterns, types, and top-level declarations. The type of the spliced expression, in each case respectively, is `Q Exp`, `Q Pat`, `Q Type`, `Q [Decl]`. Note that declaration splices may **only** appear at the top level, whereas the others may be inside other expressions, patterns, or types, respectively.
</li>

### Expression quotations (note: **not** a QuasiQuotation)

<li>
An expression quotation is a new syntactic entity written as one of:
<ul>
- `[e|..|]` or `[|..|]` - `..` is an expression and the quotation has type `Q Exp`;
- `[p|..|]` - `..` is a pattern and the quotation has type `Q Pat`;
- `[t|..|]` - `..` is a type and the quotation has type `Q Type`;
- `[d|..|]` - `..` is a list of declarations and the quotation has type `Q [Dec]`.

An expression quotation takes a compile time program and produces the AST represented by that program.

The use of a value in a quotation (e.g. `\x -> [| x |]`) without a splice corresponds to syntactic sugar for `\x -> [| $(lift x) |]`, where `lift :: Lift t => t -> Q Exp` comes from the class

### Typed splices and quotations

<li>
Typed splices are similair to previously mentioned (untyped) splices, and are written as `$$(..)` where `(..)` is an expression.
</li>
<li>
If `e` has type `Q (TExp a)` then `$$e` has type `a`.
</li>
<li>
Typed quotations take the form `[||..||]` where `..` is an expression of type `a`; the resulting quotation has type `Q (TExp a)`.
</li>
<li>
Typed expression can be converted to untyped ones: `unType :: TExp a -> Exp`.
</li>

### QuasiQuotes

<li>
QuasiQuotes generalize expression quotations - previously, the parser used by the expression quotation is one of a fixed set (`e,p,t,d`), but QuasiQuotes allow a custom parser to be defined and used to produce code at compile time. Quasi-quotations can appear in all the same contexts as regular quotations.
</li>
<li>
A quasi-quotation is written as `[iden|...|]`, where `iden` is an identifier of type `Language.Haskell.TH.Quote.QuasiQuoter`.
</li>
<li>
A `QuasiQuoter` is simply composed of four parsers, one for each of the different contexts in which quotations can appear:
</li>

### Names

<li>
Haskell identifiers are represented by the type `Language.Haskell.TH.Syntax.Name`. Names form the leaves of abstract syntax trees representing Haskell programs in Template Haskell.
</li>
<li>
An identifier which is currently in scope may be turned into a name with either: `'e` or `'T`. In the first case, `e` is interpreted in the expression scope, while in the second case `T` is in the type scope (recalling that types and value constructors may share the name without amiguity in Haskell).
</li>



## The Q type


The `Q :: * -> *` type constructor defined in `Language.Haskell.TH.Syntax` is an abstract type representing computations which have access to the compile-time environment of the module in which the computation is run. The `Q` type also handles variable substituion, called **name capture** by TH (and discussed [here](https://hackage.haskell.org/package/template-haskell-2.10.0.0/docs/Language-Haskell-TH-Syntax.html#namecapture).) All splices have type `Q X` for some `X`.

The compile-time environment includes:

<li>in-scope identifiers and information about said identifiers,
<ul>
- types of functions
- types and source data types of constructors
- full specification of type declarations (classes, type families)

The `Q` type also has the ability to generate fresh names, with the function `newName :: String -> Q Name`. Note that the name is not bound anywhere implicitly, so the user must bind it themselves, and so making sure the resulting use of the name is well-scoped is the responsibility of the user.

`Q` has instances for `Functor,Monad,Applicative` and this is the main interface for manipulating `Q` values, along with the combinators provided in `Language.Haskell.TH.Lib`, which define a helper function for every constructor of the TH ast of the form:

```hs
LitE :: Lit -> Exp
litE :: Lit -> ExpQ

AppE :: Exp -> Exp -> Exp 
appE :: ExpQ -> ExpQ -> ExpQ

```

Note that `ExpQ`, `TypeQ`, `DecsQ` and `PatQ` are synonyms for the AST types which are typically stored inside the `Q` type.

The TH library provides a function `runQ :: Quasi m => Q a -> m a`, and there is an instance `Quasi IO`, so it would seem that the `Q` type is just a fancy `IO`. However, the use of `runQ :: Q a -> IO a` produces an `IO` action which does **not** have access to any compile-time environment - this is only available in the actual `Q` type. Such `IO` actions will fail at runtime if trying to access said environment.



## An n-arity curry


The familiar

```hs
curry :: ((a,b) -> c) -> a -> b -> c
curry = \f a b -> f (a,b)

```

function can be generalized to tuples of arbitrary arity, for example:

```hs
curry3 :: ((a, b, c) -> d) -> a -> b -> c -> d
curry4 :: ((a, b, c, d) -> e) -> a -> b -> c -> d -> e 

```

However, writing such functions for tuples of arity 2 to (e.g.) 20 by hand would be tedious (and ignoring the fact that the presence of 20 tuples in your program almost certainly signal design issues which should be fixed with records).

We can use Template Haskell to produce such `curryN` functions for arbitrary `n`:

```hs
{-# LANGUAGE TemplateHaskell #-}
import Control.Monad (replicateM) 
import Language.Haskell.TH (ExpQ, newName, Exp(..), Pat(..))
import Numeric.Natural (Natural) 

curryN :: Natural -> Q Exp

```

The `curryN` function takes a natural number, and produces the curry function of that arity, as a Haskell AST.

```hs
curryN n = do
  f  <- newName "f"
  xs <- replicateM (fromIntegral n) (newName "x")

```

First we produces **fresh** type variables for each of the arguments of the function - one for the input function, and one for each of the arguments to said function.

```

 let args = map VarP (f:xs)

```

The expression `args` represents the pattern `f x1 x2 .. xn`. Note that a pattern is seperate syntactic entity - we could take this same pattern and place it in a lambda, or a function binding, or even the LHS of a let binding (which would be an error).

```

     ntup = TupE (map VarE xs)

```

The function must build the argument tuple from the sequence of arguments, which is what
we've done here. Note the distinction between pattern variables (`VarP`) and expression variables (`VarE`).

```

 return $ LamE args (AppE (VarE f) ntup)

```

Finally, the value which we produce is the AST `\f x1 x2 .. xn -> f (x1, x2, .. , xn)`.

We could have also written this function using quotations and 'lifted' constructors:

```hs
...
import Language.Haskell.TH.Lib  

curryN' :: Natural -> ExpQ
curryN' n = do
  f  <- newName "f"
  xs <- replicateM (fromIntegral n) (newName "x")
  lamE (map varP (f:xs)) 
        [| $(varE f) $(tupE (map varE xs)) |]

```

Note that quotations must be syntactically valid, so `[| \ $(map varP (f:xs)) -> .. |]` is invalid, because there is no way in regular Haskell to declare a 'list' of patterns - the above is interpreted as `\ var -> ..` and the spliced expression is expected to have type `PatQ`, i.e. a single pattern, not a list of patterns.

Finally, we can load this TH function in GHCi:

```hs
>:set -XTemplateHaskell
>:t $(curryN 5)
$(curryN 5)
  :: ((t1, t2, t3, t4, t5) -> t) -> t1 -> t2 -> t3 -> t4 -> t5 -> t
>$(curryN 5) (\(a,b,c,d,e) -> a+b+c+d+e) 1 2 3 4 5
15

```

This example is adapted primarily from [here](https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial).



#### Remarks


### What is Template Haskell?

Template Haskell refers to the template meta-programming facilities built into GHC Haskell. The paper describing the original implementation can be found [here](http://research.microsoft.com/en-us/um/people/simonpj/papers/meta-haskell/).

### What are stages? (Or, what is the stage restriction?)

Stages refer to **when** code is executed. Normally, code is exectued only at runtime, but with Template Haskell, code can be executed at compile time. "Normal" code is stage 0 and compile-time code is stage 1.

The stage restriction refers to the fact that a stage 0 program may not be executed at stage 1 - this would be equivalent to being able to run any **regular** program (not just meta-program) at compile time.

By convention (and for the sake of implementation simplicity), code in the current module is always stage 0 and code imported from all other modules is stage 1. For this reason, only expressions from other modules may be spliced.

Note that a stage 1 program is a stage 0 expression of type `Q Exp`, `Q Type`, etc; but the converse is not true - not every value (stage 0 program) of type `Q Exp` is a stage 1 program,

Futhermore, since splices can be nested, identifiers can have stages greater than 1. The stage restriction can then be generalized - a stage **n** program may not be executed in any stage **m>n**. For example, one can see references to such stages greater than 1 in certain error messages:

```hs
>:t [| \x -> $x |]

<interactive>:1:10: error:
    * Stage error: `x' is bound at stage 2 but used at stage 1
    * In the untyped splice: $x
      In the Template Haskell quotation [| \ x -> $x |]

```

### Using Template Haskell causes not-in-scope errors from unrelated identifiers?

Normally, all the declarations in a single Haskell module can be thought of as all being mutually recursive. In other words, every top-level declaration is in the scope of every other in a single module. When Template Haskell is enabled, the scoping rules change - the module is instead broken into groups of code seperated by TH splices, and each group is mutually recursive, and each group in the scope of all further groups.

