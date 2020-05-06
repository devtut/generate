---
metaTitle: "Haskell - Function composition"
description: "Right-to-left composition, Composition with binary function, Left-to-right composition"
---

# Function composition



## Right-to-left composition


`(.)` lets us compose two functions, feeding output of one as an input to the other:

```hs
(f . g) x = f (g x)

```

For example, if we want to square the successor of an input number, we can write

```hs
((^2) . succ) 1        --    4

```

There is also `(<<<)` which is an alias to `(.)`. So,

```hs
(+ 1) <<< sqrt $ 25    --    6

```



## Composition with binary function


The regular composition works for unary functions. In the case of binary, we can define

```hs
(f .: g) x y = f (g x y)          -- which is also
             = f ((g x) y)
             = (f . g x) y        -- by definition of (.)
             = (f .) (g x) y
             = ((f .) . g) x y   

```

Thus, `(f .: g) = ((f .) . g)` by eta-contraction, and furthermore,

```hs
(.:) f g    = ((f .) . g)
            = (.) (f .) g
            = (.) ((.) f) g
            = ((.) . (.)) f g

```

so `(.:) = ((.) . (.))`, a semi-famous definition.

Examples:

```hs
(map (+1) .: filter) even [1..5]      --  [3,5]
(length   .: filter) even [1..5]      --  2

```



## Left-to-right composition


`Control.Category` defines [`(>>>)`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Category.html#v:-62--62--62-), which, when specialized to functions, is

```hs
-- (>>>) :: Category cat => cat a b -> cat b c -> cat a c  
-- (>>>) :: (->) a b -> (->) b c -> (->) a c 
-- (>>>) :: (a -> b) -> (b -> c) -> (a -> c) 
( f >>> g ) x = g (f x)

```

Example:

```hs
sqrt >>> (+ 1) $ 25    --    6.0

```



#### Remarks


Function composition operator `(.)` is defined as

```hs
(.) :: (b -> c) -> (a -> b) ->  (a -> c)
(.)       f           g          x =  f (g x)     -- or, equivalently,  

(.)       f           g     =   \x -> f (g x)     
(.)       f     =    \g     ->  \x -> f (g x)      
(.) =    \f     ->   \g     ->  \x -> f (g x)      
(.) =    \f     ->  (\g     -> (\x -> f (g x) ) ) 

```

The type `(b -> c) -> (a -> b) -> (a -> c)` can be written as `(b -> c) -> (a -> b) -> a -> c`  because the `->` in type signatures "associates" to the right, corresponding to the function  application associating to the left,

```

f g x y z ...    ==    (((f g) x) y) z ...

```

So the "dataflow" is from the right to the left: `x` "goes" into `g`, whose result goes into `f`, producing the final result:

```hs
(.)       f           g          x =  r
                                      where r = f (g x)  
-- g :: a -> b
-- f ::      b -> c
-- x :: a      
-- r ::           c   

(.)       f           g     =    q
                                 where q = \x -> f (g x) 
-- g :: a -> b
-- f ::      b -> c
-- q :: a      -> c

....

```

Syntactically, the following are all the same:

```hs
(.) f g x  =  (f . g) x  =  (f .) g x  =  (. g) f x 

```

which is easy to grasp as the "three rules of [operator sections](http://stackoverflow.com/documentation/haskell/1954/partial-application/15674/sections#t=201607282218413296604)", where the "missing argument" just goes into the empty slot near the operator:

```hs
(.) f g    =  (f . g)    =  (f .) g    =  (. g) f   
--         1             2             3  

```

The `x`, being present on both sides of the equation, can be omitted. This is known as eta-contraction. Thus, the simple way to write down the definition for function composition is just

```hs
(f . g) x   =   f (g x)

```

This of course refers to the "argument" `x`; whenever we write just `(f . g)` without the `x` it is known as point-free style.

