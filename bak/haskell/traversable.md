---
metaTitle: "Haskell - Traversable"
description: "Traversing a structure in reverse, Definition of Traversable, An instance of Traversable for a binary tree, Instantiating Functor and Foldable for a Traversable structure, Traversable structures as shapes with contents, Transforming a Traversable structure with the aid of an accumulating parameter, Transposing a list of lists"
---

# Traversable


The `Traversable` class generalises the function formerly known as `mapM :: Monad m => (a -> m b) -> [a] -> m [b]` to work with `Applicative` effects over structures other than lists.



## Traversing a structure in reverse


A traversal can be run in the opposite direction with the help of the [`Backwards` applicative functor](https://hackage.haskell.org/package/transformers-0.5.2.0/docs/Control-Applicative-Backwards.html), which flips an existing applicative so that composed effects take place in reversed order.

```hs
newtype Backwards f a = Backwards { forwards :: f a }

instance Applicative f => Applicative (Backwards f) where
    pure = Backwards . pure
    Backwards ff <*> Backwards fx = Backwards ((\x f -> f x) <$> fx <*> ff)

```

`Backwards` can be put to use in a "reversed `traverse`". When the underlying applicative of a `traverse` call is flipped with `Backwards`, the resulting effect happens in reverse order.

```hs
newtype Reverse t a = Reverse { getReverse :: t a }

instance Traversable t => Traversable (Reverse t) where
    traverse f = fmap Reverse . forwards . traverse (Backwards . f) . getReverse

ghci> traverse print (Reverse "abc")
'c'
'b'
'a'

```

The `Reverse` newtype is found under Data.Functor.Reverse.



## Definition of Traversable


```hs
class (Functor t, Foldable t) => Traversable t where
    {-# MINIMAL traverse | sequenceA #-}
    
    traverse :: Applicative f => (a -> f b) -> t a -> f (t b)
    traverse f = sequenceA . fmap f
    
    sequenceA :: Applicative f => t (f a) -> f (t a)
    sequenceA = traverse id
    
    mapM :: Monad m => (a -> m b) -> t a -> m (t b)
    mapM = traverse
    
    sequence :: Monad m => t (m a) -> m (t a)
    sequence = sequenceA

```

`Traversable` structures `t` are [finitary containers](http://stackoverflow.com/a/32821966/1523776) of elements `a` which can be operated on with an effectful "visitor" operation. The visitor function `f :: a -> f b` performs a side-effect on each element of the structure and `traverse` composes those side-effects using `Applicative`. Another way of looking at it is that `sequenceA` says `Traversable` structures commute with `Applicative`s.



## An instance of Traversable for a binary tree


Implementations of `traverse` usually look like an implementation of `fmap` lifted into an `Applicative` context.

```hs
data Tree a = Leaf
            | Node (Tree a) a (Tree a)

instance Traversable Tree where
    traverse f Leaf = pure Leaf
    traverse f (Node l x r) = Node <$> traverse f l <*> f x <*> traverse f r

```

This implementation performs an [in-order traversal](https://en.wikipedia.org/wiki/Tree_traversal#In-order) of the tree.

```hs
ghci> let myTree = Node (Node Leaf 'a' Leaf) 'b' (Node Leaf 'c' Leaf)

--    +--'b'--+
--    |       |
-- +-'a'-+ +-'c'-+
-- |     | |     |
-- *     * *     *

ghci> traverse print myTree
'a'
'b'
'c'

```

The `DeriveTraversable` extension allows GHC to generate `Traversable` instances based on the structure of the type. We can vary the order of the machine-written traversal by adjusting the layout of the `Node` constructor.

```hs
data Inorder a = ILeaf
               | INode (Inorder a) a (Inorder a)  -- as before
               deriving (Functor, Foldable, Traversable)  -- also using DeriveFunctor and DeriveFoldable

data Preorder a = PrLeaf
                | PrNode a (Preorder a) (Preorder a)
                deriving (Functor, Foldable, Traversable)

data Postorder a = PoLeaf
                 | PoNode (Postorder a) (Postorder a) a
                 deriving (Functor, Foldable, Traversable)

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

ghci> traverse print (inorder myTree)
'a'
'b'
'c'
ghci> traverse print (preorder myTree)
'b'
'a'
'c'
ghci> traverse print (postorder myTree)
'a'
'c'
'b'

```



## Instantiating Functor and Foldable for a Traversable structure


```hs
import Data.Traversable as Traversable

data MyType a =  -- ...
instance Traversable MyType where
    traverse = -- ...

```

Every `Traversable` structure can be made a `Foldable` `Functor` using the [`fmapDefault`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Traversable.html#v:fmapDefault) and [`foldMapDefault`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Traversable.html#v:foldMapDefault) functions found in `Data.Traversable`.

```hs
instance Functor MyType where
    fmap = Traversable.fmapDefault

instance Foldable MyType where
    foldMap = Traversable.foldMapDefault

```

`fmapDefault` is defined by running `traverse` in the [`Identity`](https://hackage.haskell.org/package/transformers-0.2.2.1/docs/Data-Functor-Identity.html) applicative functor.

```hs
newtype Identity a = Identity { runIdentity :: a }

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity x = Identity (f x)

fmapDefault :: Traversable t => (a -> b) -> t a -> t b
fmapDefault f = runIdentity . traverse (Identity . f)

```

`foldMapDefault` is defined using the [`Const`](https://hackage.haskell.org/package/base-4.9.0.0/docs/Data-Functor-Const.html) applicative functor, which ignores its parameter while accumulating a monoidal value.

```hs
newtype Const c a = Const { getConst :: c }

instance Monoid m => Applicative (Const m) where
    pure _ = Const mempty
    Const x <*> Const y = Const (x `mappend` y)

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = getConst . traverse (Const . f)

```



## Traversable structures as shapes with contents


If a type `t` is `Traversable` then values of `t a` can be split into two pieces: their "shape" and their "contents":

```hs
data Traversed t a = Traversed { shape :: t (), contents :: [a] }

```

where the "contents" are the same as what you'd "visit" using a `Foldable` instance.

Going one direction, from `t a` to `Traversed t a` doesn't require anything but `Functor` and `Foldable`

```hs
break :: (Functor t, Foldable t) => t a -> Traversed t a 
break ta = Traversed (fmap (const ()) ta) (toList ta)

```

but going back uses the `traverse` function crucially

```hs
import Control.Monad.State

-- invariant: state is non-empty
pop :: State [a] a
pop = state $ \(a:as) -> (a, as)

recombine :: Traversable t => Traversed t a -> t a
recombine (Traversed s c) = evalState (traverse (const pop) s) c

```

The `Traversable` laws require that `break . recombine` and `recombine . break` are both identity. Notably, this means that there are exactly the right number elements in `contents` to fill `shape` completely with no left-overs.

`Traversed t` is `Traversable` itself. The implementation of `traverse` works by visiting the elements using the list's instance of `Traversable` and then reattaching the inert shape to the result.

```hs
instance Traversable (Traversed t) where
    traverse f (Traversed s c) = fmap (Traversed s) (traverse f c)

```



## Transforming a Traversable structure with the aid of an accumulating parameter


The two `mapAccum` functions combine the operations of folding and mapping.

```hs
--                                                       A Traversable structure
--                                                                   |
--                                                     A seed value  |
--                                                             |     |
--                                                            |-|  |---|
mapAccumL, mapAccumR :: Traversable t => (a -> b -> (a, c)) -> a -> t b -> (a, t c)
--                                      |---|---|---|---|---|---|---|---|---|---------------|              |---|---|---|---|---|---|---|---|---|-----|
--                                               |                            |
--                       A folding function which produces a new mapped       |
--                         element 'c' and a new accumulator value 'a'        |
--                                                                            |
--                                                            Final accumulator value
--                                                             and mapped structure

```

These functions generalise `fmap` in that they allow the mapped values to depend on what has happened earlier in the fold. They generalise `foldl`/`foldr` in that they map the structure in place as well as reducing it to a value.

For example, `tails` can be implemented using `mapAccumR` and its sister `inits` can be implemented using `mapAccumL`.

```hs
tails, inits :: [a] -> [[a]]
tails = uncurry (:) . mapAccumR (\xs x -> (x:xs, xs)) []
inits = uncurry snoc . mapAccumL (\xs x -> (x `snoc` xs, xs)) []
    where snoc x xs = xs ++ [x]

ghci> tails "abc"
["abc", "bc", "c", ""]
ghci> inits "abc"
["", "a", "ab", "abc"]

```

`mapAccumL` is implemented by traversing in the `State` applicative functor.

```hs
{-# LANGUAGE DeriveFunctor #-}

newtype State s a = State { runState :: s -> (s, a) } deriving Functor
instance Applicative (State s) where
    pure x = State $ \s -> (s, x)
    State ff <*> State fx = State $ \s -> let (t, f) = ff s
                                              (u, x) = fx t
                                          in (u, f x)

mapAccumL f z t = runState (traverse (State . flip f) t) z

```

`mapAccumR` works by running `mapAccumL` [in reverse](http://stackoverflow.com/documentation/haskell/754/traversable/2560/traversing-a-structure-in-reverse).

```hs
mapAccumR f z = fmap getReverse . mapAccumL f z . Reverse

```



## Transposing a list of lists


Noting that `zip` transposes a tuple of lists into a list of tuples,

```hs
ghci> uncurry zip ([1,2],[3,4])
[(1,3), (2,4)]

```

and the similarity between the types of `transpose` and `sequenceA`,

```hs
-- transpose exchanges the inner list with the outer list
--           +---+-->--+-+
--           |   |     | |
transpose :: [[a]] -> [[a]]
--            | |     |   |
--            +-+-->--+---+

-- sequenceA exchanges the inner Applicative with the outer Traversable
--                                             +------>------+
--                                             |             |
sequenceA :: (Traversable t, Applicative f) => t (f a) -> f (t a)
--                                                |       |
--                                                +--->---+

```

the idea is to use `[]`'s `Traversable` and `Applicative` structure to deploy `sequenceA` as a sort of **n-ary `zip`**, zipping together all the inner lists together pointwise.

`[]`'s default "prioritised choice" `Applicative` instance is not appropriate for our use - we need a "zippy" `Applicative`. For this we use the `ZipList` newtype, found in `Control.Applicative`.

```hs
newtype ZipList a = ZipList { getZipList :: [a] }

instance Applicative ZipList where
    pure x = ZipList (repeat x)
    ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

```

Now we get `transpose` for free, by traversing in the `ZipList` `Applicative`.

```hs
transpose :: [[a]] -> [[a]]
transpose = getZipList . traverse ZipList

ghci> let myMatrix = [[1,2,3],[4,5,6],[7,8,9]]
ghci> transpose myMatrix
[[1,4,7],[2,5,8],[3,6,9]]

```

