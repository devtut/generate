---
metaTitle: "Haskell - Common GHC Language Extensions"
description: "RankNTypes, OverloadedStrings, BinaryLiterals, ExistentialQuantification, LambdaCase, FunctionalDependencies, FlexibleInstances, GADTs, TupleSections, OverloadedLists, MultiParamTypeClasses, UnicodeSyntax, PatternSynonyms, ScopedTypeVariables, RecordWildCards"
---

# Common GHC Language Extensions




## RankNTypes


Imagine the following situation:

```hs
foo :: Show a => (a -> String) -> String -> Int -> IO ()
foo show' string int = do
   putStrLn (show' string)
   putStrLn (show' int)

```

Here, we want to pass in a function that converts a value into a String, apply that function to both a string parameter and and int parameter and print them both. In my mind, there is no reason this should fail! We have a function that works on both types of the parameters we're passing in.

Unfortunately, this won't type check! GHC infers the `a` type based off of its first occurrence in the function body. That is, as soon as we hit:

```hs
putStrLn (show' string)

```

GHC will infer that `show' :: String -> String`, since `string` is a `String`. It will proceed to blow up while trying to `show' int`.

`RankNTypes` lets you instead write the type signature as follows, quantifying over all functions that satisfy the `show'` type:

```hs
foo :: (forall a. Show a => (a -> String)) -> String -> Int -> IO ()

```

This is rank 2 polymorphism: We are asserting that the `show'` function must work for all `a`s **within** our function, and the previous implementation now works.

The `RankNTypes` extension allows arbitrary nesting of `forall ...` blocks in type signatures. In other words, it allows rank N polymorphism.



## OverloadedStrings


Normally, string literals in Haskell have a type of `String` (which is a type alias for `[Char]`). While this isn't a problem for smaller, educational programs, real-world applications often require more efficient storage such as `Text` or `ByteString`.

`OverloadedStrings` simply changes the type of literals to

```hs
"test" :: Data.String.IsString a => a

```

Allowing them to be directly passed to functions expecting such a type. Many libraries implement this interface for their string-like types including [Data.Text](http://stackoverflow.com/documentation/haskell/3406/text) and [Data.ByteString](https://hackage.haskell.org/package/bytestring-0.10.8.1/docs/Data-ByteString.html) which both provide certain time and space advantages over `[Char]`.

There are also some unique uses of `OverloadedStrings` like those from the [Postgresql-simple](http://hackage.haskell.org/package/postgresql-simple) library which allows SQL queries to be written in double quotes like a normal string, but provides protections against improper concatenation, a notorious source of SQL injection attacks.

To create a instance of the `IsString` class you need to impliment the `fromString` function.  Example<sup>†</sup>:

```hs
data Foo = A | B | Other String deriving Show

instance IsString Foo where
  fromString "A" = A
  fromString "B" = B
  fromString xs  = Other xs

tests :: [ Foo ]
tests = [ "A", "B", "Testing" ]

```

<sup>†</sup> This example courtesy of Lyndon Maydwell (`sordina` on GitHub) found [here](https://gist.github.com/sordina/5714390).



## BinaryLiterals


Standard Haskell allows you to write integer literals in decimal (without any prefix), hexadecimal (preceded by `0x` or `0X`), and octal (preceded by `0o` or `0O`). The `BinaryLiterals` extension adds the option of binary (preceded by `0b` or `0B`).

```hs
0b1111 == 15     -- evaluates to: True

```



## ExistentialQuantification


This is a type system extension that allows types that are existentially quantified, or, in other words, have type variables that only get instantiated at runtime<sup>†</sup>.

A value of existential type is similar to an abstract-base-class reference in OO languages: you don't know the exact type in contains, but you can constrain the **class** of types.

```hs
data S = forall a. Show a => S a

```

or equivalently, with GADT syntax:

```hs
{-# LANGUAGE GADTs #-}
data S where
   S :: Show a => a -> S

```

Existential types open the door to things like almost-heterogenous containers: as said above, there can actually be various types in an `S` value, but all of them can be `show`n, hence you can also do

```hs
instance Show S where
    show (S a) = show a   -- we rely on (Show a) from the above

```

Now we can create a collection of such objects:

```hs
ss = [S 5, S "test", S 3.0]

```

Which also allows us to use the polymorphic behaviour:

```hs
mapM_ print ss

```

Existentials can be very powerful, but note that they are actually not necessary very often in Haskell. In the example above, all you can actually do with the `Show` instance is show (duh!) the values, i.e. create a string representation. The entire `S` type therefore contains exactly as much information as the string you get when showing it. Therefore, it is usually better to simply store that string right away, especially since Haskell is lazy and therefore the string will at first only be an unevaluated thunk anyway.

On the other hand, existentials cause some unique problems. For instance, the way the type information is “hidden” in an existential. If you pattern-match on an `S` value, you will have the contained type in scope (more precisely, its `Show` instance), but this information can never escape its scope, which therefore becomes a bit of a “secret society”: the compiler doesn't let **anything** escape the scope except values whose type is already known from the outside. This can lead to strange errors [like `Couldn't match type ‘a0’ with ‘()’ ‘a0’ is untouchable`](http://stackoverflow.com/questions/28582210/type-inference-with-gadts-a0-is-untouchable).

<sup>†</sup><sub>Contrast this with ordinary parametric polymorphism, which is generally resolved at compile time (allowing full type erasure).</sub>

Existential types are different from Rank-N types – these extensions are, roughly speaking, dual to each other: to actually use values of an existential type, you need a (possibly constrained-) polymorphic function, like `show` in the example. A polymorphic function is **universally** quantified, i.e. it works **for any** type in a given class, whereas existential quantification means it works **for some** particular type which is a priori unknown. If you have a polymorphic function, that's sufficient, however to pass polymorphic functions as such as arguments, you need `{-# LANGUAGE Rank2Types #-}`:

```hs
genShowSs :: (∀ x . Show x => x -> String) -> [S] -> [String]
genShowSs f = map (\(S a) -> f a)

```



## LambdaCase


A syntactic extension that lets you write `\case` in place of `\arg -> case arg of`.

Consider the following function definition:

```hs
dayOfTheWeek :: Int -> String
dayOfTheWeek 0 = "Sunday"
dayOfTheWeek 1 = "Monday"
dayOfTheWeek 2 = "Tuesday"
dayOfTheWeek 3 = "Wednesday"
dayOfTheWeek 4 = "Thursday"
dayOfTheWeek 5 = "Friday"
dayOfTheWeek 6 = "Saturday"

```

If you want to avoid repeating the function name, you might write something like:

```hs
dayOfTheWeek :: Int -> String
dayOfTheWeek i = case i of
    0 -> "Sunday"
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"

```

Using the LambdaCase extension, you can write that as a function expression, without having to name the argument:

```hs
{-# LANGUAGE LambdaCase #-}

dayOfTheWeek :: Int -> String
dayOfTheWeek = \case
    0 -> "Sunday"
    1 -> "Monday"
    2 -> "Tuesday"
    3 -> "Wednesday"
    4 -> "Thursday"
    5 -> "Friday"
    6 -> "Saturday"

```



## FunctionalDependencies


If you have a multi-parameter type-class with arguments a, b, c, and x, this extension lets you express that the type x can be uniquely identified from a, b, and c:

```hs
class SomeClass a b c x | a b c -> x where ...

```

When declaring an instance of such class, it will be checked against all other instances to make sure that the functional dependency holds, that is, no other instance with same `a b c` but different `x` exists.

You can specify multiple dependencies in a comma-separated list:

```hs
class OtherClass a b c d | a b -> c d, a d -> b where ...

```

For example in MTL we can see:

```hs
class MonadReader r m| m -> r where ...
instance MonadReader r ((->) r) where ...

```

Now, if you have a value of type `MonadReader a ((->) Foo) => a`, the compiler can infer that `a ~ Foo`, since the second argument completely determines the first, and will simplify the type accordingly.

The `SomeClass` class can be thought of as a function of the arguments `a b c` that results in `x`. Such classes can be used to do computations in the typesystem.



## FlexibleInstances


Regular instances require:

```hs
All instance types must be of the form (T a1 ... an)
where a1 ... an are *distinct type variables*,
and each type variable appears at most once in the instance head.

```

That means that, for example, while you can create an instance for `[a]` you can't create an instance for specifically `[Int]`.; `FlexibleInstances` relaxes that:

```hs
class C a where

-- works out of the box
instance C [a] where

-- requires FlexibleInstances
instance C [Int] where

```



## GADTs


Conventional algebraic datatypes are parametric in their type variables. For example, if we define an ADT like

```hs
data Expr a = IntLit Int 
            | BoolLit Bool 
            | If (Expr Bool) (Expr a) (Expr a)

```

with the hope that this will statically rule out non-well-typed conditionals, this will not behave as expected since the type of `IntLit :: Int -> Expr a` is universially quantified: for **any** choice of `a`, it produces a value of type `Expr a`. In particular, for `a ~ Bool`, we have `IntLit :: Int -> Expr Bool`, allowing us to construct something like `If (IntLit 1) e1 e2` which is what the type of the `If` constructor was trying to rule out.

Generalised Algebraic Data Types allows us to control the resulting type of a data constructor so that they are not merely parametric. We can rewrite our `Expr` type as a GADT like this:

```hs
data Expr a where
  IntLit :: Int -> Expr Int
  BoolLit :: Bool -> Expr Bool
  If :: Expr Bool -> Expr a -> Expr a -> Expr a

```

Here, the type of the constructor `IntLit` is `Int -> Expr Int`, and so `IntLit 1 :: Expr Bool` will not typecheck.

Pattern matching on a GADT value causes refinement of the type of the term returned. For example, it is possible to write an evaluator for `Expr a` like this:

```hs
crazyEval :: Expr a -> a
crazyEval (IntLit x) = 
   -- Here we can use `(+)` because x :: Int
   x + 1 
crazyEval (BoolLit b) = 
   -- Here we can use `not` because b :: Bool
   not b
crazyEval (If b thn els) = 
  -- Because b :: Expr Bool, we can use `crazyEval b :: Bool`.
  -- Also, because thn :: Expr a and els :: Expr a, we can pass either to 
  -- the recursive call to `crazyEval` and get an a back
  crazyEval $ if crazyEval b then thn else els 

```

Note that we are able to use `(+)` in the above definitions because when e.g. `IntLit x` is pattern matched, we also learn that `a ~ Int` (and likewise for `not` and `if_then_else_` when `a ~ Bool`).



## TupleSections


A syntactic extension that allows applying the tuple constructor (which is an operator) in a section way:

```hs
(a,b) == (,) a b

-- With TupleSections
(a,b) == (,) a b == (a,) b == (,b) a

```

### N-tuples

It also works for tuples with arity greater than two

```hs
(,2,) 1 3 == (1,2,3)

```

### Mapping

This can be useful in other places where sections are used:

```hs
map (,"tag") [1,2,3] == [(1,"tag"), (2, "tag"), (3, "tag")]

```

The above example without this extension would look like this:

```hs
map (\a -> (a, "tag")) [1,2,3]

```



## OverloadedLists


**added in GHC 7.8**.

OverloadedLists, similar to [OverloadedStrings](http://stackoverflow.com/documentation/haskell/1274/common-language-extensions/4173/overloadedstrings), allows list literals to be desugared as follows:

```hs
[]          -- fromListN 0 []
[x]         -- fromListN 1 (x : [])
[x .. ]     -- fromList (enumFrom x)

```

This comes handy when dealing with types such as `Set`, `Vector` and `Map`s.

```hs
['0' .. '9']             :: Set Char
[1 .. 10]                :: Vector Int
[("default",0), (k1,v1)] :: Map String Int
['a' .. 'z']             :: Text

```

[`IsList`](https://hackage.haskell.org/package/base-4.9.0.0/docs/GHC-Exts.html#t:IsList) class in `GHC.Exts` is intended to be used with this extension.

`IsList` is equipped with one type function, `Item`, and three functions, `fromList :: [Item l] -> l`, `toList :: l -> [Item l]` and `fromListN :: Int -> [Item l] -> l` where `fromListN` is optional. Typical implementations are:

```hs
instance IsList [a] where
  type Item [a] = a
  fromList = id
  toList   = id

instance (Ord a) => IsList (Set a) where
  type Item (Set a) = a
  fromList = Set.fromList
  toList   = Set.toList

```

**Examples taken from [OverloadedLists – GHC](https://ghc.haskell.org/trac/ghc/wiki/OverloadedLists)**.



## MultiParamTypeClasses


It's a very common extension that allows type classes with multiple type parameters. You can think of MPTC as a relation between types.

```hs
{-# LANGUAGE MultiParamTypeClasses #-}

class Convertable a b where
    convert :: a -> b

instance Convertable Int Float where
    convert i = fromIntegral i

```

The order of parameters matters.

MPTCs can sometimes be replaced with type families. 



## UnicodeSyntax


An extension that allows you to use Unicode characters in lieu of certain built-in operators and names.

|ASCII|Unicode|Use(s)
|---|---|---|---|---|---|---|---|---|---
|`::`|`∷`|has type
|`->`|`→`|function types, lambdas, `case` branches, etc.
|`=>`|`⇒`|class constraints
|`forall`|`∀`|explicit polymorphism
|`<-`|`←`|`do` notation
|`*`|`★`|the type (or kind) of types (e.g., `Int :: ★`)
|`>-`|`⤚`|[`proc` notation](https://downloads.haskell.org/%7Eghc/8.0.1/docs/html/users_guide/glasgow_exts.html#arrow-notation) for [`Arrows`](https://downloads.haskell.org/%7Eghc/latest/docs/html/libraries/base-4.9.0.0/Control-Arrow.html)
|`-<`|`⤙`|[`proc` notation](https://downloads.haskell.org/%7Eghc/8.0.1/docs/html/users_guide/glasgow_exts.html#arrow-notation) for [`Arrows`](https://downloads.haskell.org/%7Eghc/latest/docs/html/libraries/base-4.9.0.0/Control-Arrow.html)
|`>>-`|`⤜`|[`proc` notation](https://downloads.haskell.org/%7Eghc/8.0.1/docs/html/users_guide/glasgow_exts.html#arrow-notation) for [`Arrows`](https://downloads.haskell.org/%7Eghc/latest/docs/html/libraries/base-4.9.0.0/Control-Arrow.html)
|`-<<`|`⤛`|[`proc` notation](https://downloads.haskell.org/%7Eghc/8.0.1/docs/html/users_guide/glasgow_exts.html#arrow-notation) for [`Arrows`](https://downloads.haskell.org/%7Eghc/latest/docs/html/libraries/base-4.9.0.0/Control-Arrow.html)

For example:

```hs
runST :: (forall s. ST s a) -> a

```

would become

```hs
runST ∷ (∀ s. ST s a) → a

```

Note that the `*` vs. `★` example is slightly different: since `*` isn't reserved, `★` also works the same way as `*` for multiplication, or any other function named `(*)`, and vice-versa.  For example:

```hs
ghci> 2 ★ 3
6
ghci> let (*) = (+) in 2 ★ 3
5
ghci> let (★) = (-) in 2 * 3
-1

```



## PatternSynonyms


[Pattern synonyms](https://ghc.haskell.org/trac/ghc/wiki/PatternSynonyms) are abstractions of patterns similar to how functions are abstractions of expressions.

For this example, let's look at the interface [`Data.Sequence`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Sequence.html) exposes, and let's see how it can be improved with pattern synonyms. The [`Seq`](https://hackage.haskell.org/package/containers-0.5.7.1/docs/Data-Sequence.html#t:Seq) type is a data type that, internally, uses a [complicated representation](http://staff.city.ac.uk/%7Eross/papers/FingerTree.html) to achieve good asymptotic complexity for various operations, most notably both O(1) (un)consing and (un)snocing.

But this representation is unwieldy and some of its invariants cannot be expressed in Haskell's type system. Because of this, the `Seq` type is exposed to users as an abstract type, along with invariant-preserving accessor and constructor functions, among them:

```hs
empty :: Seq a

(<|) :: a -> Seq a -> Seq a
data ViewL a = EmptyL | a :< (Seq a)
viewl :: Seq a -> ViewL a

(|>) :: Seq a -> a -> Seq a 
data ViewR a = EmptyR | (Seq a) :> a 
viewr :: Seq a -> ViewR a

```

But using this interface can be a bit cumbersome:

```hs
uncons :: Seq a -> Maybe (a, Seq a)
uncons xs = case viewl xs of
    x :< xs' -> Just (x, xs')
    EmptyL -> Nothing

```

We can use [view patterns](https://ghc.haskell.org/trac/ghc/wiki/ViewPatterns) to clean it up somewhat:

```hs
{-# LANGUAGE ViewPatterns #-}

uncons :: Seq a -> Maybe (a, Seq a)
uncons (viewl -> x :< xs) = Just (x, xs)
uncons _ = Nothing

```

Using the `PatternSynonyms` language extension, we can give an even nicer interface by allowing pattern matching to pretend that we have a cons- or a snoc-list:

```hs
{-# LANGUAGE PatternSynonyms #-}
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq

pattern Empty :: Seq a
pattern Empty <- (Seq.viewl -> Seq.EmptyL)

pattern (:<) :: a -> Seq a -> Seq a
pattern x :< xs <- (Seq.viewl -> x Seq.:< xs)

pattern (:>) :: Seq a -> a -> Seq a
pattern xs :> x <- (Seq.viewr -> xs Seq.:> x)

```

This allows us to write `uncons` in a very natural style:

```hs
uncons :: Seq a -> Maybe (a, Seq a)
uncons (x :< xs) = Just (x, xs)
uncons _ = Nothing

```



## ScopedTypeVariables


`ScopedTypeVariables` let you refer to universally quantified types inside of a declaration. To be more explicit:

```hs
import Data.Monoid

foo :: forall a b c. (Monoid b, Monoid c) => (a, b, c) -> (b, c) -> (a, b, c)
foo (a, b, c) (b', c') = (a :: a, b'', c'')
    where (b'', c'') = (b <> b', c <> c') :: (b, c)

```

The important thing is that we can use `a`, `b` and `c` to instruct the compiler in subexpressions of the declaration (the tuple in the `where` clause and the first `a` in the final result). In practice, `ScopedTypeVariables` assist in writing complex functions as a sum of parts, allowing the programmer to add type signatures to intermediate values that don't have concrete types.



## RecordWildCards


See [RecordWildCards](http://stackoverflow.com/documentation/haskell/1950/record-syntax/13072/recordwildcards#t=201607291417389498572)



#### Remarks


These language extensions are typically available when using the Glasgow Haskell Compiler (GHC) as they are not part of the approved [Haskell 2010 language Report](https://www.haskell.org/onlinereport/haskell2010/). To use these extensions, one must either inform the compiler using a [flag](https://downloads.haskell.org/%7Eghc/7.2.2/docs/html/users_guide/flag-reference.html) or place [a `LANGUAGE` programa](https://downloads.haskell.org/%7Eghc/7.2.2/docs/html/users_guide/pragmas.html) before the `module` keyword in a file. The official documentation can be found in [section 7](https://downloads.haskell.org/%7Eghc/7.2.2/docs/html/users_guide/ghc-language-features.html) of the GCH users guide.

The format of the `LANGUAGE` programa is `{-# LANGUAGE ExtensionOne, ExtensionTwo ... #-}`.  That is the literal `{-#` followed by `LANGUAGE` followed by a comma separated list of extensions, and finally the closing `#-}`. Multiple `LANGUAGE` programas may be placed in one file.

