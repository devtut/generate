---
metaTitle: "Haskell - Lists"
description: "Processing lists, List basics, Ranges, List Literals, List Concatenation, Accessing elements in lists, Basic Functions on Lists, foldl, foldr, Transforming with `map`, Filtering with `filter`, Zipping and Unzipping Lists"
---

# Lists



## Processing lists


To process lists, we can simply pattern match on the constructors of the list type:

```hs
listSum :: [Int] -> Int
listSum []          = 0
listSum (x:xs) = x + listSum xs

```

We can match more values by specifying a more elaborate pattern:

```hs
sumTwoPer :: [Int] -> Int
sumTwoPer [] = 0
sumTwoPer (x1:x2:xs) = x1 + x2 + sumTwoPer xs
sumTwoPer (x:xs) = x + sumTwoPer xs

```

Note that in the above example, we had to provide a more exhaustive pattern match to handle cases where an odd length list is given as an argument.

The Haskell Prelude defines many built-ins for handling lists, like `map`, `filter`, etc.. Where possible, you should use these instead of writing your own recursive functions.



## List basics


The type constructor for lists in the Haskell Prelude is `[]`. The type declaration for a list holding values of type `Int` is written as follows:

```hs
xs :: [Int]    -- or equivalently, but less conveniently,
xs :: [] Int

```

Lists in Haskell are **homogeneous [sequences](http://en.wikipedia.org/wiki/Sequence)**, which is to say that all elements must be of the same type. Unlike tuples, list type is not affected by length:

```hs
[1,2,3]   :: [Int]
[1,2,3,4] :: [Int]

```

Lists are constructed using [two constructors](https://www.haskell.org/onlinereport/haskell2010/haskellch6.html#x13-1200006.1.3):

<li>
`[]` constructs an empty list.
</li>
<li>
`(:)`, pronounced "cons", prepends elements to a list. Consing `x` (a value of type `a`)  onto `xs` (a list of values of the same type `a`) creates a new list, whose **head** (the first element) is `x`, and **tail** (the rest of the elements) is `xs`.
</li>

We can define simple lists as follows:

```hs
ys :: [a]
ys = []

xs :: [Int]
xs = 12 : (99 : (37 : []))   
-- or  = 12 : 99 : 37 : []     -- ((:) is right-associative)
-- or  = [12, 99, 37]          -- (syntactic sugar for lists)

```

Note that `(++)`, which can be used to build lists is defined recursively in terms of `(:)` and `[]`.



## Ranges


Creating a list from 1 to 10 is simple using range notation:

```hs
[1..10]    -- [1,2,3,4,5,6,7,8,9,10]

```

To specify a step, add a comma and the next element after the start element:

```hs
[1,3..10]  -- [1,3,5,7,9]

```

Note that Haskell always takes the step as the arithmetic difference between terms, and that you cannot specify more than the first two elements and the upper bound:

```hs
[1,3,5..10] -- error
[1,3,9..20] -- error

```

To generate a range in descending order, always specify the negative step:

```hs
[5..1]     -- []

[5,4..1]   -- [5,4,3,2,1]

```

Because Haskell is non-strict, the elements of the list are evaluated only if they are needed, which allows us to use infinite lists. `[1..]` is an infinite list starting from 1. This list can be bound to a variable or passed as a function argument:

```hs
take 5 [1..]   -- returns [1,2,3,4,5] even though [1..] is infinite

```

Be careful when using ranges with floating-point values, because it accepts spill-overs up to half-delta, to fend off rounding issues:

```hs
[1.0,1.5..2.4]    -- [1.0,1.5,2.0,2.5] , though 2.5 > 2.4

[1.0,1.1..1.2]    -- [1.0,1.1,1.2000000000000002] , though 1.2000000000000002 > 1.2

```

Ranges work not just with numbers but with any type that implements `Enum` typeclass. Given some enumerable variables `a`, `b`, `c`, the range syntax is equivalent to calling these `Enum` methods:

```hs
[a..]    == enumFrom a
[a..c]   == enumFromTo a c
[a,b..]  == enumFromThen a b
[a,b..c] == enumFromThenTo a b c

```

For example, with `Bool` it's

```

[False ..]      -- [False,True]

```

Notice the space after `False`, to prevent this to be parsed as a module name qualification (i.e. `False..` would be parsed as `.` from a module `False`).



## List Literals


```hs
emptyList     = []

singletonList = [0]               -- = 0 : []

listOfNums    = [1, 2, 3]         -- = 1 : 2 : [3]

listOfStrings = ["A", "B", "C"]

```



## List Concatenation


```hs
listA      = [1, 2, 3]

listB      = [4, 5, 6]

listAThenB = listA ++ listB       -- [1, 2, 3, 4, 5, 6]

(++) xs     [] = xs
(++) []     ys = ys
(++) (x:xs) ys = x : (xs ++ ys)

```



## Accessing elements in lists


Access the **n**th element of a list (zero-based):

```hs
list = [1 .. 10]

firstElement = list !! 0           -- 1

```

Note that `!!` is a partial function, so certain inputs produce errors:

```hs
list !! (-1)     -- *** Exception: Prelude.!!: negative index  

list !! 1000     -- *** Exception: Prelude.!!: index too large

```

There's also `Data.List.genericIndex`, an overloaded version of `!!`, which accepts any `Integral` value as the index.

```hs
import Data.List (genericIndex)

list `genericIndex` 4              -- 5

```

When implemented as singly-linked lists, these operations take **O(n)** time. If you frequently access elements by index, it's probably better to use `Data.Vector` (from the [vector](https://hackage.haskell.org/package/vector) package)  or other data structures.



## Basic Functions on Lists


```hs
head [1..10]       --    1

last [1..20]       --    20

tail [1..5]        --    [2, 3, 4, 5]

init [1..5]        --    [1, 2, 3, 4]

length [1 .. 10]   --    10

reverse [1 .. 10]  --    [10, 9 .. 1]

take 5 [1, 2 .. ]  --    [1, 2, 3, 4, 5]

drop 5 [1 .. 10]   --    [6, 7, 8, 9, 10]

concat [[1,2], [], [4]]   --    [1,2,4]

```



## foldl


This is how the left fold is implemented. Notice how the order of the arguments in the step function is flipped compared to `foldr` (the right fold):

```hs
foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc []     =  acc
foldl f acc (x:xs) =  foldl f (f acc x) xs         -- = foldl f (acc `f` x) xs  

```

The left fold, `foldl`, associates to the left. That is:

```hs
foldl (+) 0 [1, 2, 3]     -- is equivalent to ((0 + 1) + 2) + 3

```

The reason is that `foldl` is evaluated like this (look at `foldl`'s inductive step):

```hs
foldl (+) 0 [1, 2, 3]                        --  foldl (+)    0   [ 1,   2,   3 ]
foldl (+) ((+) 0 1) [2, 3]                   --  foldl (+)   (0 + 1)   [ 2,   3 ]
foldl (+) ((+) ((+) 0 1) 2) [3]              --  foldl (+)  ((0 + 1) + 2)   [ 3 ]
foldl (+) ((+) ((+) ((+) 0 1) 2) 3) []       --  foldl (+) (((0 + 1) + 2) + 3) []
((+) ((+) ((+) 0 1) 2) 3)                    --            (((0 + 1) + 2) + 3)

```

The last line is equivalent to `((0 + 1) + 2) + 3`. This is because `(f a b)` is the same as `(a `f` b)` in general, and so `((+) 0 1)` is the same as `(0 + 1)` in particular.



## foldr


This is how the right fold is implemented:

```hs
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f z []     = z
foldr f z (x:xs) = f x (foldr f z xs)              -- = x `f` foldr f z xs

```

The right fold, `foldr`, associates to the right. That is:

```hs
foldr (+) 0 [1, 2, 3]      -- is equivalent to 1 + (2 + (3 + 0))

```

The reason is that `foldr` is evaluated like this (look at the inductive step of `foldr`):

```hs
foldr (+) 0 [1, 2, 3]                        --          foldr (+) 0  [1,2,3]
(+) 1 (foldr (+) 0 [2, 3])                   -- 1 +        foldr (+) 0  [2,3]
(+) 1 ((+) 2 (foldr (+) 0 [3]))              -- 1 + (2 +     foldr (+) 0  [3])
(+) 1 ((+) 2 ((+) 3 (foldr (+) 0 [])))       -- 1 + (2 + (3 +  foldr (+) 0 []))
(+) 1 ((+) 2 ((+) 3 0))                      -- 1 + (2 + (3 +            0   ))

```

The last line is equivalent to `1 + (2 + (3 + 0))`, because `((+) 3 0)` is the same as `(3 + 0)`.



## Transforming with `map`


Often we wish to convert, or transform the contents of a collection (a list, or something traversable). In Haskell we use `map`:

```

-- Simple add 1
 map (+ 1) [1,2,3]
 [2,3,4]
 
 map odd [1,2,3]
 [True,False,True]
 
 data Gender = Male | Female deriving Show
 data Person = Person String Gender Int deriving Show

 -- Extract just the age from a list of people
 map (\(Person n g a) -> a) [(Person "Alex" Male 31),(Person "Ellie" Female 29)]
 [31,29]

```



## Filtering with `filter`


Given a list:

```

li = [1,2,3,4,5]

```

we can filter a list with a predicate using `filter :: (a -> Bool) -> [a] -> [a]`:

```

filter (== 1) li       -- [1]
 
 filter (even) li       -- [2,4]
 
 filter (odd) li        -- [1,3,5]
 
 -- Something slightly more complicated
 comfy i = notTooLarge && isEven
   where 
      notTooLarge = (i + 1) < 5
      isEven = even i
 
 filter comfy li        -- [2]

```

Of course it's not just about numbers:

```

data Gender = Male | Female deriving Show
 data Person = Person String Gender Int deriving Show
 
 onlyLadies :: [Person] -> Person
 onlyLadies x = filter isFemale x
   where 
     isFemale (Person _ Female _) = True
     isFemale _ = False
 
 onlyLadies [(Person "Alex" Male 31),(Person "Ellie" Female 29)]
 -- [Person "Ellie" Female 29]

```



## Zipping and Unzipping Lists


zip takes two lists and returns a list of corresponding pairs:

```hs
zip []     _      = []
zip _      []     = []
zip (a:as) (b:bs) = (a,b) : zip as bs

> zip [1,3,5] [2,4,6]
> [(1,2),(3,4),(5,6)]

```

Zipping two lists with a function:

```hs
zipWith f  []     _      = []
zipWith f  _      []     = []
zipWith f  (a:as) (b:bs) = f a b : zipWith f as bs

> zipWith (+) [1,3,5] [2,4,6]
> [3,7,11]

```

Unzipping a list:

```hs
unzip = foldr (\(a,b) ~(as,bs) -> (a:as,b:bs)) ([],[])

> unzip [(1,2),(3,4),(5,6)]
> ([1,3,5],[2,4,6])

```



#### Syntax


<li>
empty list constructor
`[] :: [a]`
</li>
<li>
non-empty list constructor
`(:) :: a -> [a] -> [a]`
</li>
<li>
head - returns the first value of a list
`head :: [a] -> a`
</li>
<li>
last - returns the last value of a list
`last :: [a] -> a`
</li>
<li>
tail - returns a list without the first item
`tail :: [a] -> [a]`
</li>
<li>
init - returns a list without the last item
`init :: [a] -> [a]`
</li>
<li>
xs !! i - return the element at an index i in list xs
`(!!) :: Int -> [a] -> a`
</li>
<li>
take n xs - return new list containing n first elements of the list xs
`take :: Int -> [a] -> [a]`
</li>
<li>
map    :: (a -> b) -> [a] -> [b]
</li>
<li>
filter :: (a -> Bool) -> [a] -> [a]
</li>
<li>
(++) :: [a] -> [a]
</li>
<li>
concat :: [[a]] -> [a]
</li>



#### Remarks


1. The type `[a]` is equivalent to `[] a`.
1. `[]` constructs the empty list.
1. `[]` in a function definition LHS, e.g. `f [] = ...`, is the empty list pattern.
1. `x:xs`  constructs a list where an element `x` is prepended to the list `xs`
1. `f (x:xs) = ...` is a pattern match for a non-empty list where `x` is the head and `xs` is the tail.
1. `f (a:b:cs) = ...` and `f (a:(b:cs)) = ...` are the same. They are a pattern match for a list of at least two elements where the first element is `a`, the second element is `b`, and the rest of the list is `cs`.
1. `f ((a:as):bs) = ...` is NOT the same as `f (a:(as:bs)) = ...`. The former is a pattern match for a non-empty list of lists, where `a` is the head of the head, `as` is the tail of the head, and `bs` is the tail.
1. `f (x:[]) = ...` and `f [x] = ...` are the same. They are a pattern match for a list of exactly one element.
1. `f (a:b:[]) = ...` and `f [a,b] = ...` are the same. They are a pattern match for a list of exactly two elements.
1. `f [a:b] = ...` is a pattern match for a list of exactly one element where the element is also a list. `a` is the head of the element and `b` is the tail of the element.
1. `[a,b,c]` is the same as `(a:b:c:[])`. Standard list notation is just syntactic sugar for the `(:)` and `[]` constructors.
1. You can use `all@(x:y:ys)` in order to refer to the whole list as `all` (or any other name you choose) instead of repeating `(x:y:ys)` again.

