---
metaTitle: "Haskell - Concurrency"
description: "Spawning Threads with `forkIO`, Communicating between Threads with `MVar`, Atomic Blocks with Software Transactional Memory"
---

# Concurrency



## Spawning Threads with `forkIO`


Haskell supports many forms of concurrency and the most obvious being forking a thread using `forkIO`.

The function `forkIO :: IO () -> IO ThreadId` takes an `IO` action and returns its `ThreadId`, meanwhile the action will be run in the background.

We can demonstrate this quite succinctly using `ghci`:

```hs
Prelude Control.Concurrent> forkIO $ (print . sum) [1..100000000]
ThreadId 290
Prelude Control.Concurrent> forkIO $ print "hi!"
"hi!"
-- some time later....
Prelude Control.Concurrent> 50000005000000

```

Both actions will run in the background, and the second is almost guaranteed to finish before the last!



## Communicating between Threads with `MVar`


It is very easy to pass information between threads using the `MVar a` type and its accompanying functions in `Control.Concurrent`:

- `newEmptyMVar :: IO (MVar a)` -- creates a new `MVar a`
- `newMVar :: a -> IO (MVar a)` -- creates a new `MVar` with the given value
- `takeMVar :: MVar a -> IO a` -- retrieves the value from the given `MVar`, or **blocks** until one is available
- `putMVar :: MVar a -> a -> IO ()` -- puts the given value in the `MVar`, or **blocks** until it's empty

Let's sum the numbers from 1 to 100 million in a thread and wait on the result:

```hs
import Control.Concurrent
main = do
  m <- newEmptyMVar
  forkIO $ putMVar m $ sum [1..10000000]
  print =<< takeMVar m  -- takeMVar will block 'til m is non-empty!

```

A more complex demonstration might be to take user input and sum in the background while waiting for more input:

```hs
main2 = loop
  where 
    loop = do
        m <- newEmptyMVar
        n <- getLine
        putStrLn "Calculating. Please wait"
        -- In another thread, parse the user input and sum
        forkIO $ putMVar m $ sum [1..(read n :: Int)]
        -- In another thread, wait 'til the sum's complete then print it
        forkIO $ print =<< takeMVar m
        loop

```

As stated earlier, if you call `takeMVar` and the `MVar` is empty, it blocks until another thread puts something into the `MVar`, which could result in a [Dining Philosophers Problem](https://en.wikipedia.org/wiki/Dining_philosophers_problem). The same thing happens with `putMVar`: if it's full, it'll block 'til it's empty!

Take the following function:

```hs
concurrent ma mb = do
  a <- takeMVar ma
  b <- takeMVar mb
  putMVar ma a
  putMVar mb b

```

We run the the two functions with some `MVar`s

```hs
concurrent ma mb     -- new thread 1 
concurrent mb ma     -- new thread 2

```

What could happen is that:

1. Thread 1 reads `ma` and blocks `ma`
1. Thread 2 reads `mb` and thus blocks `mb`

Now Thread 1 cannot read `mb` as Thread 2 has blocked it, and Thread 2 cannot read `ma` as Thread 1 has blocked it. A classic deadlock!



## Atomic Blocks with Software Transactional Memory


Another powerful & mature concurrency tool in Haskell is Software Transactional Memory, which allows for multiple threads to write to a single variable of type `TVar a` in an atomic manner.

`TVar a` is the main type associated with the [`STM`](https://hackage.haskell.org/package/stm-2.4.4.1/docs/Control-Monad-STM.html) monad and stands for transactional variable. They're used much like `MVar` but within the `STM` monad through the following functions:

### `atomically :: STM a -> IO a`

Perform a series of STM actions atomically.

### `readTVar :: TVar a -> STM a`

Read the `TVar`'s value, e.g.:

### `writeTVar :: TVar a -> a -> STM ()`

Write a value to the given `TVar`.

This example is taken from the Haskell Wiki:



#### Remarks


Good resources for learning about concurrent and parallel programming in Haskell are:

<li>
[Parallel and Concurrent Programming in Haskell](http://chimera.labs.oreilly.com/books/1230000000929/index.html)
</li>
<li>
the [Haskell Wiki](https://wiki.haskell.org/Concurrency)
</li>

