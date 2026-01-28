---
metaTitle: "Haskell - Foldable"
description: "An instance of Foldable for a binary tree, Definition of Foldable, Counting the elements of a Foldable structure, Folding a structure in reverse, Flattening a Foldable structure into a list, Performing a side-effect for each element of a Foldable structure, Flattening a Foldable structure into a Monoid, Checking if a Foldable structure is empty"
---

# Foldable


`Foldable` is the class of types `t :: * -> *` which admit a **folding** operation. A fold aggregates the elements of a structure in a well-defined order, using a combining function.



## An instance of Foldable for a binary tree


To instantiate `Foldable` you need to provide a definition for at least `foldMap` or `foldr`.

```hs
data Tree a = Leaf
            | Node (Tree a) a (Tree a)

instance Foldable Tree where
    foldMap f Leaf = mempty
    foldMap f (Node l x r) = foldMap f l `mappend` f x `mappend` foldMap f r
    
    foldr f acc Leaf = acc
    foldr f acc (Node l x r) = foldr f (f x (foldr f acc r)) l

```

This implementation performs an [in-order traversal](https://en.wikipedia.org/wiki/Tree_traversal#In-order) of the tree.

```hs
ghci> let myTree = Node (Node Leaf 'a' Leaf) 'b' (Node Leaf 'c' Leaf)

--    +--'b'--+
--    |       |
-- +-'a'-+ +-'c'-+
-- |     | |     |
-- *     * *     *

ghci> toList myTree
"abc"

```

The `DeriveFoldable` extension allows GHC to generate `Foldable` instances based on the structure of the type. We can vary the order of the machine-written traversal by adjusting the layout of the `Node` constructor.

```hs
data Inorder a = ILeaf
               | INode (Inorder a) a (Inorder a)  -- as before
               deriving Foldable

data Preorder a = PrLeaf
                | PrNode a (Preorder a) (Preorder a)
                deriving Foldable

data Postorder a = PoLeaf
                 | PoNode (Postorder a) (Postorder a) a
                 deriving Foldable

-- injections from the earlier Tree type
inorder :: Tree a -> Inorder a
inorder Leaf = ILeaf
inorder (Node l x r) = INode (inorder l) x (inorder r)

preorder :: Tree a -> Preorder a
preorder Leaf = PrLeaf
preorder (Node l x r) = PrNode x (preorder l) (preorder r)

postorder :: Tree a -> Postorder a
postorder Leaf = PoLeaf
postorder (Node l x r) = PoNode (postorder l) (postorder r) x

ghci> toList (inorder myTree)
"abc"
ghci> toList (preorder myTree)
"bac"
ghci> toList (postorder myTree)
"acb"

```



## Definition of Foldable


```hs
class Foldable t where
    {-# MINIMAL foldMap | foldr #-}

    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f = foldr (mappend . f) mempty

    foldr :: (a -> b -> b) -> b -> t a -> b
    foldr f z t = appEndo (foldMap (Endo #. f) t) z

    -- and a number of optional methods

```

Intuitively (though not technically), `Foldable` structures are containers of elements `a` which allow access to their elements in a well-defined order. The `foldMap` operation maps each element of the container to a [`Monoid`](http://stackoverflow.com/documentation/haskell/2211/monoid#t=201607261731381300206) and collapses them using the `Monoid` structure.



## Counting the elements of a Foldable structure


`length` counts the occurences of elements `a` in a foldable structure `t a`.

```hs
ghci> length [7, 2, 9]  -- t ~ []
3
ghci> length (Right 'a')  -- t ~ Either e
1  -- 'Either e a' may contain zero or one 'a'
ghci> length (Left "foo")  -- t ~ Either String
0
ghci> length (3, True)  -- t ~ (,) Int
1  -- '(c, a)' always contains exactly one 'a'

```

`length` is defined as being equivalent to:

```hs
class Foldable t where
    -- ...
    length :: t a -> Int
    length = foldl' (\c _ -> c+1) 0

```

Note that this return type `Int` restricts the operations that can be performed on   values obtained by calls to the `length` function. `fromIntegral`is a useful function that allows us to deal with this problem.



## Folding a structure in reverse


Any fold can be run in the opposite direction with the help of [the `Dual` monoid](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Monoid.html#t:Dual), which flips an existing monoid so that aggregation goes backwards.

```hs
newtype Dual a = Dual { getDual :: a }

instance Monoid m => Monoid (Dual m) where
    mempty = Dual mempty
    (Dual x) `mappend` (Dual y) = Dual (y `mappend` x)

```

When the underlying monoid of a `foldMap` call is flipped with `Dual`, the fold runs backwards; the following `Reverse` type is defined in [`Data.Functor.Reverse`](https://hackage.haskell.org/package/transformers-0.5.2.0/docs/Data-Functor-Reverse.html):

```hs
newtype Reverse t a = Reverse { getReverse :: t a }

instance Foldable t => Foldable (Reverse t) where
    foldMap f = getDual . foldMap (Dual . f) . getReverse

```

We can use this machinery to write a terse `reverse` for lists:

```hs
reverse :: [a] -> [a]
reverse = toList . Reverse

```



## Flattening a Foldable structure into a list


`toList` flattens a `Foldable` structure `t a` into a list of `a`s.

```hs
ghci> toList [7, 2, 9]  -- t ~ []
[7, 2, 9]
ghci> toList (Right 'a')  -- t ~ Either e
"a"
ghci> toList (Left "foo")  -- t ~ Either String
[]
ghci> toList (3, True)  -- t ~ (,) Int
[True]

```

`toList` is defined as being equivalent to:

```hs
class Foldable t where
    -- ...
    toList :: t a -> [a]
    toList = foldr (:) []

```



## Performing a side-effect for each element of a Foldable structure


`traverse_` executes an `Applicative` action for every element in a `Foldable` structure. It ignores the action's result, keeping only the side-effects. (For a version which doesn't discard results, use [`Traversable`](http://stackoverflow.com/documentation/haskell/754/traversable#t=201607211339101859173).)

```hs
-- using the Writer applicative functor (and the Sum monoid)
ghci> runWriter $ traverse_ (\x -> tell (Sum x)) [1,2,3]
((),Sum {getSum = 6})
-- using the IO applicative functor
ghci> traverse_ putStrLn (Right "traversing")
traversing
ghci> traverse_ putStrLn (Left False)
-- nothing printed

```

`for_` is `traverse_` with the arguments flipped. It resembles a `foreach` loop in an imperative language.

```hs
ghci> let greetings = ["Hello", "Bonjour", "Hola"]
ghci> :{
ghci|     for_ greetings $ \greeting -> do
ghci|         print (greeting ++ " Stack Overflow!")
ghci| :}
"Hello Stack Overflow!"
"Bonjour Stack Overflow!"
"Hola Stack Overflow!"

```

`sequenceA_` collapses a `Foldable` full of `Applicative` actions into a single action, ignoring the result.

```hs
ghci> let actions = [putStrLn "one", putStLn "two"]
ghci> sequenceA_ actions
one
two

```

`traverse_` is defined as being equivalent to:

```hs
traverse_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
traverse_ f = foldr (\x action -> f x *> action) (pure ())

```

`sequenceA_` is defined as:

```hs
sequenceA_ :: (Foldable t, Applicative f) -> t (f a) -> f ()
sequenceA_ = traverse_ id

```

Moreover, when the `Foldable` is also a `Functor`, `traverse_` and `sequenceA_` have the following relationship:

```hs
traverse_ f = sequenceA_ . fmap f

```



## Flattening a Foldable structure into a Monoid


`foldMap` maps each element of the Foldable structure to a [`Monoid`](http://stackoverflow.com/documentation/haskell/2211/monoid#t=201607241100239861394), and then combines them into a single value.

`foldMap` and `foldr` can be defined in terms of one another, which means that instances of `Foldable` need only give a definition for one of them.

```hs
class Foldable t where
    foldMap :: Monoid m => (a -> m) -> t a -> m
    foldMap f = foldr (mappend . f) mempty

```

Example usage with [the `Product` monoid](http://stackoverflow.com/documentation/haskell/2211/monoid/7233/numeric-monoids):

```hs
product :: (Num n, Foldable t) => t n -> n
product = getProduct . foldMap Product

```



## Checking if a Foldable structure is empty


`null` returns `True` if there are no elements `a` in a foldable structure `t a`, and `False` if there is one or more. Structures for which `null` is `True` have a [`length`](http://stackoverflow.com/documentation/haskell/753/foldable/2554/counting-the-elements-of-a-foldable-structure#t=201608081807124789244) of 0.

```hs
ghci> null []
True
ghci> null [14, 29]
False
ghci> null Nothing
True
ghci> null (Right 'a')
False
ghci> null ('x', 3)
False

```

`null` is defined as being equivalent to:

```hs
class Foldable t where
    -- ...
    null :: t a -> Bool
    null = foldr (\_ _ -> False) True

```



#### Remarks


If `t` is `Foldable` it means that for any value `t a` we know how to access all of the elements of `a` from "inside" of `t a` in a fixed linear order. This is the meaning of `foldMap :: Monoid m => (a -> m) -> (t a -> m)`: we "visit" each element with a summary function and smash all the summaries together. `Monoid`s respect order (but are invariant to different groupings).

