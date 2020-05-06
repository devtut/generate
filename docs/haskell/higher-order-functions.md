---
metaTitle: "Haskell - Higher-order functions"
description: "Basics of Higher Order Functions, Lambda Expressions, Currying"
---

# Higher-order functions



## Basics of Higher Order Functions


Review [Partial Application](http://stackoverflow.com/documentation/haskell/1954/partial-application#t=201607290030340619796) before proceeding.

In Haskell, a function that can take other functions as arguments or return functions is called a **higher-order function**.

The following are all **higher-order functions**:

```hs
map       :: (a -> b) -> [a] -> [b]
filter    :: (a -> Bool) -> [a] -> [a]
takeWhile :: (a -> Bool) -> [a] -> [a]
dropWhile :: (a -> Bool) -> [a] -> [a]
iterate   :: (a -> a) -> a -> [a]
zipWith   :: (a -> b -> c) -> [a] -> [b] -> [c]
scanr     :: (a -> b -> b) -> b -> [a] -> [b]
scanl     :: (b -> a -> b) -> b -> [a] -> [b]

```

These are particularly useful in that they allow us to create new functions on top of the ones we already have, by passing functions as arguments to other functions. Hence the name, **higher-order functions**.

Consider:

```hs
Prelude> :t (map (+3))
(map (+3)) :: Num b => [b] -> [b]

Prelude> :t (map (=='c'))
(map (=='c')) :: [Char] -> [Bool]

Prelude> :t (map zipWith)
(map zipWith) :: [a -> b -> c] -> [[a] -> [b] -> [c]]

```

This ability to easily create functions (like e.g. by partial application as used here) is one of the features that makes functional programming particularly powerful and allows us to derive short, elegant solutions that would otherwise take dozens of lines in other languages. For example, the following function gives us the number of aligned elements in two lists.

```hs
aligned :: [a] ->  [a] -> Int
aligned xs ys = length (filter id (zipWith (==) xs ys))

```



## Lambda Expressions


**Lambda expressions** are similar to **anonymous functions** in other languages.

Lambda expressions are [open formulas](https://en.wikipedia.org/wiki/Open_formula) which also specify variables which are to be bound. Evaluation (finding the value of a function call) is then achieved by [substituting](https://en.wikipedia.org/wiki/Lambda_calculus#Substitution) the [bound variables](https://en.wikipedia.org/wiki/Bound_variable) in the lambda expression's body, with the user supplied arguments. Put simply, lambda expressions allow us to express functions by way of variable binding and [substitution](https://en.wikipedia.org/wiki/Substitution_(logic)).

Lambda expressions look like

```hs
\x -> let {y = ...x...} in y

```

Within a lambda expression, the variables on the left-hand side of the arrow are considered bound in the right-hand side, i.e. the function's body.

Consider the mathematical function

```hs
f(x) = x^2

```

As a Haskell definition it is

```hs
f    x =  x^2

f = \x -> x^2

```

which means that the function `f` is equivalent to the lambda expression `\x -> x^2`.

Consider the parameter of the higher-order function `map`, that is a function of type `a -> b`. In case it is used only once in a call to `map` and nowhere else in the program, it is convenient to specify it as a lambda expression instead of naming such a throwaway function. Written as a lambda expression,

```hs
\x -> let {y = ...x...} in y

```

`x` holds a value of type `a`, `...x...` is a Haskell expression that refers to the variable `x`, and `y` holds a value of type `b`. So, for example, we could write the following

```hs
map (\x -> x + 3)

map (\(x,y) -> x * y)

map (\xs -> 'c':xs) ["apples", "oranges", "mangos"]

map (\f -> zipWith f [1..5] [1..5]) [(+), (*), (-)]

```



## Currying


In Haskell, all functions are considered curried: that is, all functions in Haskell take just **one** argument.

Let's take the function `div`:

If we call this function with 6 and 2 we unsurprisingly get 3:

However, this doesn't quite behave in the way we might think. First `div 6` is evaluated and **returns a function** of type `Int -> Int`. This resulting function is then applied to the value 2 which yields 3.

When we look at the type signature of a function, we can shift our thinking from "takes two arguments of type `Int`" to "takes one `Int` and returns a function that takes an `Int`". This is reaffirmed if we consider that arrows in the type notation associate **to the right**, so `div` can in fact be read thus:

In general, most programmers can ignore this behaviour at least while they're learning the language. From a [theoretical point of view](https://wiki.haskell.org/Currying), "formal proofs are easier when all functions are treated uniformly (one argument in, one result out)."



#### Remarks


Higher Order Functions are functions that take functions as parameters and/or return functions as their return values.

