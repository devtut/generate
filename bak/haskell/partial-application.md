---
metaTitle: "Haskell - Partial Application"
description: "Sections, Partially Applied Adding Function, Returning a Partially Applied Function"
---

# Partial Application



## Sections


Sectioning is a concise way to partially apply arguments to infix operators.

For example, if we want to write a function which adds "ing" to the end of a word we can use a section to succinctly define a function.

```hs
> (++ "ing") "laugh"
"laughing"

```

Notice how we have partially applied the second argument. Normally, we can only partially apply the arguments in the specified order.

We can also use left sectioning to partially apply the first argument.

```hs
> ("re" ++) "do"
"redo"

```

We could equivalently write this using normal prefix partial application:

```hs
> ((++) "re") "do"
"redo"

```

### A Note on Subtraction

Beginners often incorrectly section negation.

```hs
> map (-1) [1,2,3]
***error: Could not deduce...

```

This does not work as `-1` is parsed as the literal `-1` rather than the sectioned operator `-` applied to `1`. The `subtract` function exists to circumvent this issue.

```hs
> map (subtract 1) [1,2,3]
[0,1,2]

```



## Partially Applied Adding Function


We can use **partial application** to "lock" the first argument. After applying one argument we are left with a function which expects one more argument before returning the result.

```hs
(+) :: Int -> Int -> Int

addOne :: Int -> Int
addOne = (+) 1

```

We can then use `addOne` in order to add one to an `Int`.

```hs
> addOne 5
6
> map addOne [1,2,3]
[2,3,4]

```



## Returning a Partially Applied Function


Returning partially applied functions is one technique to write concise code.

```hs
add :: Int -> Int -> Int
add x = (+x)

add 5 2

```

In this example (+x) is a partially applied function. Notice that the second parameter to the add function does not need to be specified in the function definition.

The result of calling `add 5 2` is seven.



#### Remarks


Let's clear up some misconceptions that beginners might make.

You may have encountered functions such as:

```hs
max :: (Ord a) => a -> a -> a  
max m n  
  | m >= n = m  
  | otherwise = n  

```

Beginners will typically view `max :: (Ord a) => a -> a -> a` as function that takes two arguments (values) of type `a` and returns a value of type `a`. However, what is really happening, is that `max` is **taking one argument** of type `a` and **returning a function** of type `a -> a`. This function then takes an argument of type `a` and returns a final value of type `a`.

Indeed, `max` can be written as `max :: (Ord a) => a -> (a -> a)`

Consider the type signature of `max`:

```hs
Prelude> :t max  
max :: Ord a => a -> a -> a  

Prelude> :t (max 75)  
(max 75) :: (Num a, Ord a) => a -> a  

Prelude> :t (max "Fury Road")  
(max "Fury Road") :: [Char] -> [Char]  

Prelude> :t (max "Fury Road" "Furiosa")  
(max "Fury Road" "Furiosa") :: [Char]  

```

`max 75` and `max "Fury Road"` may not **look** like functions, but in actuality, they are.

The confusion stems from the fact that in mathematics and many, other, common programming languages, we are allowed to have functions that take multiple arguments. However, in Haskell, **functions can only take one argument** and they can return either values such as `a`, or functions such as `a -> a`.

