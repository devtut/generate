---
metaTitle: "Haskell - Sorting Algorithms"
description: "Insertion Sort, Permutation Sort, Merge Sort, Quicksort, Bubble sort, Selection sort"
---

# Sorting Algorithms



## Insertion Sort


```hs
insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys) | x < y     = x:y:ys
                | otherwise = y:(insert x ys)

isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)

```

**Example use:**

```hs
> isort [5,4,3,2,1]

```

**Result:**

```hs
[1,2,3,4,5]

```



## Permutation Sort


Also known as [bogosort](https://en.wikipedia.org/wiki/Bogosort).

```hs
import Data.List (permutations)

sorted :: Ord a => [a] -> Bool
sorted (x:y:xs) = x <= y && sorted (y:xs)
sorted _        = True

psort :: Ord a => [a] -> [a]
psort = head . filter sorted . permutations

```

Extremely inefficient (on today's computers).



## Merge Sort


**Ordered merging of two ordered lists**

Preserving the duplicates:

```hs
merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y    = x:merge xs (y:ys)
                    | otherwise = y:merge (x:xs) ys

```

**Top-down version:**

```hs
msort :: Ord a => [a] -> [a]
msort [] = []
msort [a] = [a]
msort xs = merge (msort (firstHalf xs)) (msort (secondHalf xs))

firstHalf  xs = let { n = length xs } in take (div n 2) xs
secondHalf xs = let { n = length xs } in drop (div n 2) xs

```

It is defined this way for clarity, not for efficiency.

**Example use:**

```hs
> msort [3,1,4,5,2]

```

**Result:**

```hs
[1,2,3,4,5]

```

**Bottom-up version:**

```hs
msort [] = []
msort xs = go [[x] | x <- xs]
    where
    go [a] = a
    go xs = go (pairs xs)
    pairs (a:b:t) = merge a b : pairs t
    pairs t = t

```



## Quicksort


```hs
qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [a | a <- xs, a < x]
                      ++ [x] ++
               qsort [b | b <- xs, b >= x]

```



## Bubble sort


```hs
bsort :: Ord a => [a] -> [a]
bsort s = case bsort' s of
               t | t == s    -> t
                 | otherwise -> bsort t
  where bsort' (x:x2:xs) | x > x2    = x2:(bsort' (x:xs))
                         | otherwise = x:(bsort' (x2:xs))
        bsort' s = s

```



## Selection sort


[Selection sort](https://en.wikipedia.org/wiki/Selection_sort) selects the minimum element, repeatedly, until the list is empty.

```hs
import Data.List (minimum, delete)

ssort :: Ord t => [t] -> [t]
ssort [] = []
ssort xs = let { x = minimum xs } 
           in  x : ssort (delete x xs)

```

