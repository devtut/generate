---
metaTitle: "Haskell - List Comprehensions"
description: "Basic List Comprehensions, Patterns in Generator Expressions, Guards, Parallel Comprehensions, Do Notation, Nested Generators, Local Bindings"
---

# List Comprehensions




## Basic List Comprehensions


Haskell has [list comprehensions](https://www.haskell.org/onlinereport/haskell2010/haskellch3.html#x8-420003.11), which are a lot like set comprehensions in math and similar implementations in  imperative languages such as Python and JavaScript. At their most basic, list comprehensions take the following form.

```hs
[ x | x <- someList ]

```

For example

```hs
[ x | x <- [1..4] ]    -- [1,2,3,4]

```

Functions can be directly applied to x as well:

```hs
[ f x | x <- someList ]

```

This is equivalent to:

```hs
map f someList

```

Example:

```hs
[ x+1 | x <- [1..4]]    -- [2,3,4,5]

```



## Patterns in Generator Expressions


However, `x` in the generator expression is not just variable, but can be any pattern. In cases of pattern mismatch the generated element is skipped over, and processing of the list continues with the next element, thus acting like a filter:

```hs
[x | Just x <- [Just 1, Nothing, Just 3]]     -- [1, 3]

```

A generator with a variable `x` in its pattern creates new scope containing all the expressions on its right, where `x` is defined to be the generated element.

This means that guards can be coded as

```hs
[ x | x <- [1..4], even x] ==
[ x | x <- [1..4], () <- [() | even x]] ==
[ x | x <- [1..4], () <- if even x then [()] else []]

```



## Guards


Another feature of list comprehensions is guards, which also act as filters. Guards are Boolean expressions and appear on the right side of the bar in a list comprehension.

Their most basic use is

```hs
[x    | p x]   ===   if p x then [x] else []

```

Any variable used in a guard must appear on its left in the comprehension, or otherwise be in scope. So,

```hs
[ f x | x <- list, pred1 x y, pred2 x]     -- `y` must be defined in outer scope

```

which is equivalent to

```hs
map f (filter pred2 (filter (\x -> pred1 x y) list))          -- or,

-- ($ list) (filter (`pred1` y) >>> filter pred2 >>> map f)     

-- list >>= (\x-> [x | pred1 x y]) >>= (\x-> [x | pred2 x]) >>= (\x -> [f x])

```

(the `>>=` operator is `infixl 1`, i.e. it associates (is parenthesized) to the left). Examples:

```hs
[ x       | x <- [1..4], even x]           -- [2,4]

[ x^2 + 1 | x <- [1..100], even x ]        -- map (\x -> x^2 + 1) (filter even [1..100])

```



## Parallel Comprehensions


With [Parallel List Comprehensions](https://downloads.haskell.org/%7Eghc/latest/docs/html/users_guide/glasgow_exts.html#parallel-list-comprehensions) language extension,

```hs
[(x,y) | x <- xs | y <- ys]

```

is equivalent to

```hs
zip xs ys

```

Example:

```hs
[(x,y) | x <- [1,2,3] | y <- [10,20]] 

-- [(1,10),(2,20)]

```



## Do Notation


Any list comprehension can be correspondingly coded with [list monad's](http://stackoverflow.com/documentation/haskell/2968/monads/14170/list-monad#t=201608030840447565994) [`do` notation](http://stackoverflow.com/documentation/haskell/2968/monads/15585/do-notation#t=201608030840447565994).

```hs
[f x | x <- xs]                 f  <$> xs         do { x <- xs ; return (f x) }

[f x | f <- fs, x <- xs]        fs <*> xs         do { f <- fs ; x <- xs ; return (f x) }

[y   | x <- xs, y <- f x]       f  =<< xs         do { x <- xs ; y <- f x ; return y }

```

The [guards](https://stackoverflow.com/documentation/haskell/4970/list-comprehensions/17541/guards) can be handled using [`Control.Monad.guard`](https://hackage.haskell.org/package/base/docs/Control-Monad.html#v:guard):

```hs
[x   | x <- xs, even x]                           do { x <- xs ; guard (even x) ; return x }

```



## Nested Generators


List comprehensions can also draw elements from multiple lists, in which case the result will be the list of every possible combination of the two elements, as if the two lists were processed in the **nested** fashion. For example,

```hs
[ (a,b) | a <- [1,2,3], b <- ['a','b'] ]

-- [(1,'a'), (1,'b'), (2,'a'), (2,'b'), (3,'a'), (3,'b')]

```



## Local Bindings


List comprehensions can introduce local bindings for variables to hold some interim values:

```hs
[(x,y) | x <- [1..4], let y=x*x+1, even y]    -- [(1,2),(3,10)]

```

Same effect can be achieved with a trick,

```hs
[(x,y) | x <- [1..4], y <- [x*x+1], even y]   -- [(1,2),(3,10)]

```

The `let` in list comprehensions is recursive, as usual. But generator bindings are not, which enables **shadowing**:

```hs
[x | x <- [1..4], x <- [x*x+1], even x]       -- [2,10]

```

