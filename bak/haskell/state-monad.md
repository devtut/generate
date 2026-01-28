---
metaTitle: "Haskell - State Monad"
description: "Numbering the nodes of a tree with a counter"
---

# State Monad




## Numbering the nodes of a tree with a counter


We have a tree data type like this:

```hs
data Tree a = Tree a [Tree a] deriving Show

```

And we wish to write a function that assigns a number to each node of the tree, from an incrementing counter:

```hs
tag :: Tree a -> Tree (a, Int)

```

### The long way

First we'll do it the long way around, since it illustrates the `State` monad's low-level mechanics quite nicely.

```hs
import Control.Monad.State

-- Function that numbers the nodes of a `Tree`.
tag :: Tree a -> Tree (a, Int)
tag tree = 
    -- tagStep is where the action happens.  This just gets the ball
    -- rolling, with `0` as the initial counter value.
    evalState (tagStep tree) 0

-- This is one monadic "step" of the calculation.  It assumes that
-- it has access to the current counter value implicitly.
tagStep :: Tree a -> State Int (Tree (a, Int))
tagStep (Tree a subtrees) = do
    -- The `get :: State s s` action accesses the implicit state
    -- parameter of the State monad.  Here we bind that value to
    -- the variable `counter`.
    counter <- get 

    -- The `put :: s -> State s ()` sets the implicit state parameter
    -- of the `State` monad.  The next `get` that we execute will see
    -- the value of `counter + 1` (assuming no other puts in between).
    put (counter + 1)

    -- Recurse into the subtrees.  `mapM` is a utility function
    -- for executing a monadic actions (like `tagStep`) on a list of
    -- elements, and producing the list of results.  Each execution of 
    -- `tagStep` will be executed with the counter value that resulted
    -- from the previous list element's execution.
    subtrees' <- mapM tagStep subtrees  

    return $ Tree (a, counter) subtrees'

```

### Refactoring

### Split out the counter into a postIncrement action

The bit where we are `get`ting the current counter and then `put`ting counter + 1 can be split off into a `postIncrement` action, similar to what many C-style languages provide:

```hs
postIncrement :: Enum s => State s s
postIncrement = do
    result <- get
    modify succ
    return result

```

### Split out the tree walk into a higher-order function

The tree walk logic can be split out into its own function, like this:

```hs
mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM action (Tree a subtrees) = do
    a' <- action a
    subtrees' <- mapM (mapTreeM action) subtrees
    return $ Tree a' subtrees'

```

With this and the `postIncrement` function we can rewrite `tagStep`:

```hs
tagStep :: Tree a -> State Int (Tree (a, Int))
tagStep = mapTreeM step
    where step :: a -> State Int (a, Int)
          step a = do 
              counter <- postIncrement
              return (a, counter)

```

### Use the `Traversable` class

The `mapTreeM` solution above can be easily rewritten into an instance of [the `Traversable` class](http://stackoverflow.com/documentation/haskell/754/traversable#t=201609012048577636313):

```hs
instance Traversable Tree where
    traverse action (Tree a subtrees) = 
        Tree <$> action a <*> traverse action subtrees

```

Note that this required us to use `Applicative` (the `<*>` operator) instead of `Monad`.

With that, now we can write `tag` like a pro:

```hs
tag :: Traversable t => t a -> t (a, Int)
tag init t = evalState (traverse step t) 0
    where step a = do tag <- postIncrement
                      return (a, tag)

```

Note that this works for any `Traversable` type, not just our `Tree` type!

### Getting rid of the `Traversable` boilerplate

GHC has a `DeriveTraversable` extension that eliminates the need for writing the instance above:

```hs
{-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

data Tree a = Tree a [Tree a]
            deriving (Show, Functor, Foldable, Traversable)

```



#### Remarks


Newcomers to Haskell often shy away from the `State` monad and treat it like a taboo—like the claimed benefit of functional programming is the avoidance of state, so don't you lose that when you use `State`?  A more nuanced view is that:

- State can be useful in **small, controlled doses**;
- The `State` type provides the ability to control the dose very precisely.

The reasons being that if you have `action :: State s a`, this tells you that:

- `action` is special because it depends on a state;
- The state has type `s`, so `action` cannot be influenced by any old value in your program—only an `s` or some value reachable from some `s`;
- The `runState :: State s a -> s -> (a, s)` puts a "barrier" around the stateful action, so that its effectfulness **cannot** be observed from outside that barrier.

So this is a good set of criteria for whether to use `State` in particular scenario.  You want to see that your code is **minimizing the scope of the state**, both by choosing a narrow type for `s` and by putting `runState` as close to "the bottom" as possible, (so that your actions can be influenced by as few thing as possible.

