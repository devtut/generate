---
metaTitle: "Haskell - Parallelism"
description: "The Eval Monad, rpar, rseq"
---

# Parallelism



## The Eval Monad


Parallelism in Haskell can be expressed using the `Eval` Monad from [`Control.Parallel.Strategies`](https://hackage.haskell.org/package/parallel-3.2.1.0/docs/Control-Parallel-Strategies.html), using the `rpar` and `rseq` functions (among others).

```hs
f1 :: [Int]
f1 = [1..100000000]

f2 :: [Int]
f2 = [1..200000000]

main = runEval $ do
  a <- rpar (f1) -- this'll take a while...
  b <- rpar (f2) -- this'll take a while and then some...
  return (a,b)

```

Running `main` above will execute and "return" immediately, while the two values, `a` and `b` are computed in the background through `rpar`.

Note: ensure you compile with `-threaded` for parallel execution to occur.



## rpar


`rpar :: Strategy a` executes the given strategy (recall: `type Strategy a = a -> Eval a`) in parallel:

```hs
import Control.Concurrent
import Control.DeepSeq
import Control.Parallel.Strategies
import Data.List.Ordered

main = loop
  where 
    loop = do
      putStrLn "Enter a number"
      n <- getLine

      let lim = read n :: Int
          hf  = quot lim 2
          result = runEval $ do
            -- we split the computation in half, so we can concurrently calculate primes
            as <- rpar (force (primesBtwn 2 hf))
            bs <- rpar (force (primesBtwn (hf + 1) lim))
            return (as ++ bs)

      forkIO $ putStrLn ("\nPrimes are: " ++ (show result) ++ " for " ++ n ++ "\n")
      loop

-- Compute primes between two integers
-- Deliberately inefficient for demonstration purposes
primesBtwn n m = eratos [n..m]
  where
    eratos []     = []
    eratos (p:xs) = p : eratos (xs `minus` [p, p+p..])

```

Running this will demonstrate the concurrent behaviour:

```hs
Enter a number
12
Enter a number

Primes are: [2,3,5,7,8,9,10,11,12] for 12

100
Enter a number

Primes are: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100] for 100

200000000
Enter a number
-- waiting for 200000000    
200
Enter a number

Primes are: [2,3,5,7,11,13,17,19,23,29,31,37,41,43,47,53,59,61,67,71,73,79,83,89,97,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200] for 200

-- still waiting for 200000000

```



## rseq


We can use `rseq :: Strategy a` to force an argument to Weak Head Normal Form:

```hs
f1 :: [Int]
f1 = [1..100000000]

f2 :: [Int]
f2 = [1..200000000]

main = runEval $ do
  a <- rpar (f1) -- this'll take a while...
  b <- rpar (f2) -- this'll take a while and then some...
  rseq a
  return (a,b)

```

This subtly changes the semantics of the `rpar` example; whereas the latter would return immediately whilst computing the values in the background, this example will wait until `a` can be evaluated to WHNF.



#### Parameters


|Type/Function|Detail
|---|---|---|---|---|---|---|---|---|---
|[`data Eval a`](https://hackage.haskell.org/package/parallel-3.2.1.0/docs/Control-Parallel-Strategies.html#t:Eval)|Eval is a Monad that makes it easier to define parallel strategies
|[`type Strategy a = a -> Eval a`](https://hackage.haskell.org/package/parallel-3.2.1.0/docs/Control-Parallel-Strategies.html#g:1)|a function that embodies a parallel evaluation strategy. The function traverses (parts of) its argument, evaluating subexpressions in parallel or in sequence
|`rpar :: Strategy a`|sparks its argument (for evaluation in parallel)
|`rseq :: Strategy a`|evaluates its argument to weak head normal form
|`force :: NFData a => a -> a`|evaluates the entire structure of its argument, reducing it to normal form, before returning the argument itself. It is provided by the Control.DeepSeq module



#### Remarks


[Simon Marlow's book](http://chimera.labs.oreilly.com/books/1230000000929), Concurrent and Parallel Programming in Haskell, is outstanding and covers a multitude of concepts. It is also very much accessible for even the newest Haskell programmer. It is highly recommended and available in PDF or online for free.

**Parallel vs Concurrent**

Simon Marlow [puts it best](http://chimera.labs.oreilly.com/books/1230000000929/ch01.html#sec_terminology):

> 
A parallel program is one that uses a multiplicity of computational hardware (e.g., several processor cores) to perform a computation more quickly. The aim is to arrive at the answer earlier, by delegating different parts of the computation to different processors that execute at the same time.


> 
By contrast, concurrency is a program-structuring technique in which there are multiple threads of control. Conceptually, the threads of control execute “at the same time”; that is, the user sees their effects interleaved. Whether they actually execute at the same time or not is an implementation detail; a concurrent program can execute on a single processor through interleaved execution or on multiple physical processors.


**Weak Head Normal Form**

It's important to be aware of how lazy-evaluation works. The first section of [this chapter](http://chimera.labs.oreilly.com/books/1230000000929/ch02.html) will give a strong introduction into WHNF and how this relates to parallel and concurrent programming.

