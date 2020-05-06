---
metaTitle: "Haskell - Monad Transformers"
description: "A monadic counter"
---

# Monad Transformers



## A monadic counter


An example on how to compose the reader, writer, and state
monad using monad transformers. The source code can be found [in this repository](https://github.com/capitanbatata/sandbox/tree/master/monadic-counter)

We want to implement a counter, that increments its value by a given
constant.

We start by defining some types, and functions:

```hs
newtype Counter = MkCounter {cValue :: Int}
  deriving (Show)

-- | 'inc c n' increments the counter by 'n' units.
inc :: Counter -> Int -> Counter
inc (MkCounter c) n = MkCounter (c + n)

```

Assume we want to carry out the following computation using the counter:

- set the counter to 0
- set the increment constant to 3
- increment the counter 3 times
- set the increment constant to 5
- increment the counter 2 times

The [state monad](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-State-Lazy.html#t:StateT) provides abstractions for passing state around. We can make
use of the state monad, and define our increment function as a state
transformer.

```hs
-- | CounterS is a monad.
type CounterS = State Counter

-- | Increment the counter by 'n' units.
incS :: Int-> CounterS ()
incS n = modify (\c -> inc c n)

```

This already enables us to express a computation in a more clear and
succinct way:

```hs
-- | The computation we want to run, with the state monad.
mComputationS :: CounterS ()
mComputationS = do
  incS 3
  incS 3
  incS 3
  incS 5
  incS 5

```

But we still have to pass the increment constant at each invocation. We would
like to avoid this.

### Adding an environment

The [reader monad](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Reader.html#v:runReaderT) provides a convenient way to pass an environment around.
This monad is used in functional programming to perform what in the
OO world is known as **dependency injection**.

In its simplest version, the reader monad requires two types:

<li>
the type of the value being read (i.e. our environment, `r` below),
</li>
<li>
the value returned by the reader monad (`a` below).
Reader r a
</li>

However, we need to make use of the state monad as well. Thus, we need to use
the `ReaderT` transformer:

```hs
newtype ReaderT r m a :: * -> (* -> *) -> * -> *

```

Using `ReaderT`, we can define our counter with environment and state as
follows:

```hs
type CounterRS = ReaderT Int CounterS

```

We define an `incR` function that takes the increment constant from the
environment (using `ask`), and to define our increment function in terms of
our `CounterS` monad we make use of the `lift` function (which belongs to the
[monad transformer](https://hackage.haskell.org/package/transformers-0.1.3.0/docs/Control-Monad-Trans.html) class).

```hs
-- | Increment the counter by the amount of units specified by the environment.
incR :: CounterRS ()
incR = ask >>= lift . incS

```

Using the reader monad we can define our computation as follows:

```hs
-- | The computation we want to run, using reader and state monads.
mComputationRS :: CounterRS ()
mComputationRS = do
  local (const 3) $ do
    incR
    incR
    incR
    local (const 5) $ do
      incR
      incR

```

### The requirements changed: we need logging!

Now assume that we want to add logging to our computation, so that we can see
the evolution of our counter in time.

We also have a monad to perform this task, the [writer monad](https://hackage.haskell.org/package/mtl-2.2.1/docs/Control-Monad-Writer-Strict.html). As with the
reader monad, since we are composing them, we need to make use of the reader
monad transformer:

```hs
newtype WriterT w m a :: * -> (* -> *) -> * -> *

```

Here `w` represents the type of the output to accumulate (which has to be a
monoid, which allow us to accumulate this value), `m` is the inner monad, and
`a` the type of the computation.

We can then define our counter with logging, environment, and state as
follows:

```hs
type CounterWRS = WriterT [Int] CounterRS

```

And making use of `lift` we can define the version of the increment function
which logs the value of the counter after each increment:

```hs
incW :: CounterWRS ()
incW = lift incR >> get >>= tell . (:[]) . cValue

```

Now the computation that contains logging can be written as follows:

```hs
mComputationWRS :: CounterWRS ()
mComputationWRS = do
  local (const 3) $ do
    incW
    incW
    incW
    local (const 5) $ do
      incW
      incW

```

### Doing everything in one go

This example intended to show monad transformers at work. However, we can
achieve the same effect by composing all the aspects (environment, state, and
logging) in a single increment operation.

To do this we make use of type-constraints:

```hs
inc' :: (MonadReader Int m, MonadState Counter m, MonadWriter [Int] m) => m ()
inc' = ask >>= modify . (flip inc) >> get >>= tell . (:[]) . cValue

```

Here we arrive at a solution that will work for any monad that satisfies the
constraints above. The computation function is defined thus with type:

```hs
mComputation' :: (MonadReader Int m, MonadState Counter m, MonadWriter [Int] m) => m ()

```

since in its body we make use of inc'.

We could run this computation, in the `ghci` REPL for instance, as follows:

```hs
runState ( runReaderT ( runWriterT mComputation' ) 15 )  (MkCounter 0)

```

