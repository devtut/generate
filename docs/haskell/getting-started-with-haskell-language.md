---
metaTitle: "Haskell - Getting started with Haskell Language"
description: "Hello, World!, Factorial, Getting started, Fibonacci, Using Lazy Evaluation, Primes, Declaring Values"
---

# Getting started with Haskell Language



## Hello, World!


A basic ["Hello, World!" program](https://en.wikipedia.org/wiki/%22Hello,_World!%22_program) in Haskell can be expressed concisely in just one or two lines:

```hs
main :: IO ()
main = putStrLn "Hello, World!"

```

The first line is an optional type annotation, indicating that `main` is a value of type `IO ()`, representing an I/O action which "computes" a value of type `()` (read "unit"; the empty tuple conveying no information) besides performing some side effects on the outside world (here, printing a string at the terminal). This type annotation is usually omitted for `main` because it is its **only** possible type.

Put this into a `helloworld.hs` file and compile it using a Haskell compiler, such as GHC:

```hs
ghc helloworld.hs

```

Executing the compiled file will result in the output `"Hello, World!"` being printed to the screen:

```hs
./helloworld
Hello, World!

```

Alternatively, `runhaskell` or `runghc` make it possible to run the program in interpreted mode without having to compile it:

```hs
runhaskell helloworld.hs

```

The interactive REPL can also be used instead of compiling. It comes shipped with most Haskell environments, such as `ghci` which comes with the GHC compiler:

```hs
ghci> putStrLn "Hello World!"
Hello, World!
ghci> 

```

Alternatively, load scripts into ghci from a file using `load` (or `:l`):

```hs
ghci> :load helloworld

```

`:reload` (or `:r`) reloads everything in ghci:

```hs
Prelude> :l helloworld.hs 
[1 of 1] Compiling Main             ( helloworld.hs, interpreted )

<some time later after some edits>

*Main> :r
Ok, modules loaded: Main.

```

### Explanation:

This first line is a type signature, declaring the type of `main`:

```hs
main :: IO ()

```

Values of type `IO ()` describe actions which can interact with the outside world.

Because Haskell has a fully-fledged [Hindley-Milner type system](https://en.wikipedia.org/wiki/Hindley-Milner_type_inference) which allows for automatic type inference, type signatures are technically optional: if you simply omit the `main :: IO ()`, the compiler will be able to infer the type on its own by analyzing the **definition** of `main`. However, it is very much considered bad style not to write type signatures for top-level definitions. The reasons include:

<li>
Type signatures in Haskell are a very helpful piece of documentation because the type system is so expressive that you often can see what sort of thing a function is good for simply by looking at its type. This “documentation” can be conveniently accessed with tools like GHCi. And unlike normal documentation, the compiler's type checker will make sure it actually matches the function definition!
</li>
<li>
Type signatures **keep bugs local**. If you make a mistake in a definition without providing its type signature, the compiler may not immediately report an error but instead simply infer a nonsensical type for it, with which it actually typechecks. You may then get a cryptic error message when **using** that value. With a signature, the compiler is very good at spotting bugs right where they happen.
</li>

This second line does the actual work:

```hs
main = putStrLn "Hello, World!"

```

If you come from an imperative language, it may be helpful to note that this definition can also be written as:

```hs
main = do {
   putStrLn "Hello, World!" ;
   return ()
   }

```

Or equivalently (Haskell has layout-based parsing; but **beware mixing tabs and spaces inconsistently** which will confuse this mechanism):

```hs
main = do
    putStrLn "Hello, World!"
    return ()

```

Each line in a `do` block represents some [monadic](https://stackoverflow.com/documentation/haskell/2968/monads#t=201702191704432959009) (here, I/O) **computation**, so that the whole `do` block represents the overall action comprised of these sub-steps  by combining them in a manner specific to the given monad (for I/O this means just executing them one after another).

The `do` syntax is itself a syntactic sugar for monads, like `IO` here, and `return` is a no-op action producing its argument without performing any side effects or additional computations which might be part of a particular monad definition.

The above is the same as defining `main = putStrLn "Hello, World!"`, because the value `putStrLn "Hello, World!"` already has the type `IO ()`. Viewed as a “statement”, `putStrLn "Hello, World!"` can be seen as a complete program, and you simply define `main` to refer to this program.

You can [look up the signature of `putStrLn` online](http://hayoo.fh-wedel.de/?query=putStrLn):

```hs
putStrLn :: String -> IO ()
-- thus,
putStrLn (v :: String) :: IO ()

```

`putStrLn` is a function that takes a string as its argument and outputs an I/O-action (i.e. a value representing a program that the runtime can execute). The runtime always executes the action named `main`, so we simply need to define it as equal to `putStrLn "Hello, World!"`.



## Factorial


The factorial function is a Haskell "Hello World!" (and for functional programming generally) in the sense that it succinctly demonstrates basic principles of the language.

### Variation 1

```hs
fac :: (Integral a) => a -> a
fac n = product [1..n]

```

[Live demo](http://coliru.stacked-crooked.com/a/7410a751e5007db6)

- `Integral` is the class of integral number types. Examples include `Int` and `Integer`.
- `(Integral a) =>` places a constraint on the type `a` to be in said class
- `fac :: a -> a` says that `fac` is a function that takes an `a` and returns an `a`
- `product` is a function that accumulates all numbers in a list by multiplying them together.
- `[1..n]` is special notation which desugars to `enumFromTo 1 n`, and is the range of numbers `1 ≤ x ≤ n`.

### Variation 2

```hs
fac :: (Integral a) => a -> a
fac 0 = 1
fac n = n * fac (n - 1)

```

[Live demo](http://coliru.stacked-crooked.com/a/1e80dec346f844f6)

This variation uses pattern matching to split the function definition into separate cases. The first definition is invoked if the argument is `0` (sometimes called the stop condition) and the second definition otherwise (the order of definitions is significant). It also exemplifies recursion as `fac` refers to itself.

It is worth noting that, due to rewrite rules, both versions of `fac` will compile to identical machine code when using GHC with optimizations activated. So, in terms of efficiency, the two would be equivalent.



## Getting started


### Online REPL

The easiest way to get started writing Haskell is probably by going to the [Haskell website](http://www.haskell.org) or [Try Haskell](http://tryhaskell.org/) and use the online REPL (read-eval-print-loop) on the home page. The online REPL supports most basic functionality and even some IO. There is also a basic tutorial available which can be started by typing the command `help`. An ideal tool to start learning the basics of Haskell and try out some stuff.

### **GHC(i)**

For programmers that are ready to engage a little bit more, there is **GHCi**, an interactive environment that comes with the [**Glorious/Glasgow Haskell Compiler**](http://www.haskell.org/ghc). The **GHC** can be installed separately, but that is only a compiler. In order to be able to install new libraries, tools like [**Cabal**](http://www.haskell.org/cabal/) and [**Stack**](http://haskellstack.org) must be installed as well. If you are running a Unix-like operating system, the easiest installation is to install [**Stack**](http://haskellstack.org)  using:

```hs
curl -sSL https://get.haskellstack.org/ | sh

```

This installs GHC isolated from the rest of your system, so it is easy to remove.  All commands must be preceded by `stack` though. Another simple approach is to install a [Haskell Platform](http://www.haskell.org/platform). The platform exists in two flavours:

1. The **minimal** distribution contains only **GHC** (to compile) and **Cabal/Stack** (to install and build packages)
1. The **full** distribution additionally contains tools for project development, profiling and coverage analysis. Also an additional set of widely-used packages is included.

These platforms can be installed by [downloading an installer](http://www.haskell.org/platform) and following the instructions or by using your distribution's package manager (note that this version is not guaranteed to be up-to-date):

<li>
Ubuntu, Debian, Mint:

```hs
sudo apt-get install haskell-platform

```


</li>
<li>
Fedora:

```hs
sudo dnf install haskell-platform

```


</li>
<li>
Redhat:

```hs
sudo yum install haskell-platform

```


</li>
<li>
Arch Linux:

```hs
sudo pacman -S ghc cabal-install haskell-haddock-api \
               haskell-haddock-library happy alex

```


</li>
<li>
Gentoo:

```hs
sudo layman -a haskell
sudo emerge haskell-platform

```


</li>
<li>
OSX with Homebrew:

```hs
brew cask install haskell-platform

```


</li>
<li>
OSX with MacPorts:

```hs
sudo port install haskell-platform

```


</li>

Once installed, it should be possible to start **GHCi** by invoking the `ghci` command anywhere in the terminal. If the installation went well, the console should look something like

```hs
me@notebook:~$ ghci
GHCi, version 6.12.1: http://www.haskell.org/ghc/  :? for help
Prelude> 

```

possibly with some more information on what libraries have been loaded before the `Prelude>`. Now, the console has become a Haskell REPL and you can execute Haskell code as with the online REPL. In order to quit this interactive environment, one can type `:q`or `:quit`. For more information on what commands are available in **GHCi**, type `:?` as indicated in the starting screen.

Because writing the same things again and again on a single line is not always that practically, it might be a good idea to write the Haskell code in files. These files normally have `.hs` for an extension and can be loaded into the REPL by using `:l` or `:load`.

As mentioned earlier, **GHCi** is a part of the **GHC**, which is actually a compiler. This compiler can be used to transform a `.hs` file with Haskell code into a running program. Because a `.hs` file can contain a lot of functions, a `main` function must be defined in the file. This will be the starting point for the program. The file `test.hs` can be compiled with the command

```hs
ghc test.hs

```

this will create object files and an executable if there were no errors and the `main` function was defined correctly.

### More advanced tools

<li>
It has already been mentioned earlier as package manager, but [**stack**](http://haskellstack.org) can be a useful tool for Haskell development in completely different ways. Once installed, it is capable of
<ul>
1. installing (multiple versions of) **GHC**
1. project creation and scaffolding
1. dependency management
1. building and testing projects
1. benchmarking
</ul>
</li>
<li>
IHaskell is a  [haskell kernel for IPython](https://github.com/gibiansky/IHaskell) and allows to combine (runnable) code with markdown and mathematical notation.
</li>



## Fibonacci, Using Lazy Evaluation


Lazy evaluation means Haskell will evaluate only list items whose values are needed.

The basic recursive definition is:

```hs
f (0)  <-  0
f (1)  <-  1
f (n)  <-  f (n-1) + f (n-2)

```

If evaluated directly, it will be **very** slow. But, imagine we have a list that records all the results,

```hs
fibs !! n  <-  f (n) 

```

Then

```

                 ┌──────┐   ┌──────┐   ┌──────┐
                  │ f(0) │   │ f(1) │   │ f(2) │
fibs  ->  0 : 1 : │  +   │ : │  +   │ : │  +   │ :  .....
                  │ f(1) │   │ f(2) │   │ f(3) │
                  └──────┘   └──────┘   └──────┘

                  ┌────────────────────────────────────────┐
                  │ f(0)   :   f(1)   :   f(2)   :  .....  │ 
                  └────────────────────────────────────────┘
      ->  0 : 1 :               +
                  ┌────────────────────────────────────────┐
                  │ f(1)   :   f(2)   :   f(3)   :  .....  │
                  └────────────────────────────────────────┘

```

This is coded as:

```hs
fibn n = fibs !! n
    where
    fibs = 0 : 1 : map f [2..]
    f n = fibs !! (n-1) + fibs !! (n-2)

```

Or even as

```hs
GHCi> let fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
GHCi> take 10 fibs
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

```

`zipWith` makes a list by applying a given binary function to corresponding elements of the two lists given to it, so `zipWith (+) [x1, x2, ...] [y1, y2, ...]` is equal to `[x1 + y1, x2 + y2, ...]`.

Another way of writing `fibs` is with the [`scanl` function](http://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html#v:scanl):

```hs
GHCi> let fibs = 0 : scanl (+) 1 fibs
GHCi> take 10 fibs
[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

```

`scanl` builds the list of partial results that `foldl` would produce, working from left to right along the input list. That is, `scanl f z0 [x1, x2, ...]` is equal to `[z0, z1, z2, ...] where z1 = f z0 x1; z2 = f z1 x2; ...`.

Thanks to lazy evaluation, both functions define infinite lists without computing them out entirely. That is, we can write a `fib` function, retrieving the nth element of the unbounded Fibonacci sequence:

```hs
GHCi> let fib n = fibs !! n  -- (!!) being the list subscript operator
-- or in point-free style:
GHCi> let fib = (fibs !!)
GHCi> fib 9
34

```



## Primes


A few **most salient** variants:

### **Below 100**

```hs
import Data.List ( (\\) )

ps100 = ((([2..100] \\ [4,6..100]) \\ [6,9..100]) \\ [10,15..100]) \\ [14,21..100]

   -- = (((2:[3,5..100]) \\ [9,15..100]) \\ [25,35..100]) \\ [49,63..100]

   -- = (2:[3,5..100]) \\ ([9,15..100] ++ [25,35..100] ++ [49,63..100])

```

### **Unlimited**

Sieve of Eratosthenes, using [data-ordlist package](https://hackage.haskell.org/package/data-ordlist-0.4.7.0/docs/Data-List-Ordered.html):

```hs
import qualified Data.List.Ordered

ps   = 2 : _Y ((3:) . minus [5,7..] . unionAll . map (\p -> [p*p, p*p+2*p..]))

_Y g = g (_Y g)   -- = g (g (_Y g)) = g (g (g (g (...)))) = g . g . g . g . ...

```

### **Traditional**

(a sub-optimal trial division sieve)

```hs
ps = sieve [2..]
     where
     sieve (x:xs) = [x] ++ sieve [y | y <- xs, rem y x > 0]

-- = map head ( iterate (\(x:xs) -> filter ((> 0).(`rem` x)) xs) [2..] )

```

### **Optimal trial division**

```hs
ps = 2 : [n | n <- [3..], all ((> 0).rem n) $ takeWhile ((<= n).(^2)) ps]

-- = 2 : [n | n <- [3..], foldr (\p r-> p*p > n || (rem n p > 0 && r)) True ps]

```

### **Transitional**

From trial division to sieve of Eratosthenes:

```hs
[n | n <- [2..], []==[i | i <- [2..n-1], j <- [0,i..n], j==n]]

```

### **The Shortest Code**

```hs
nubBy (((>1).).gcd) [2..]          -- i.e., nubBy (\a b -> gcd a b > 1) [2..]

```

`nubBy` is also [from `Data.List`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Data-List.html#v:nubBy), like `(\\)`.



## Declaring Values


We can declare a series of expressions in the REPL like this:

```hs
Prelude> let x = 5
Prelude> let y = 2 * 5 + x
Prelude> let result = y * 10
Prelude> x
5
Prelude> y
15
Prelude> result
150

```

To declare the same values in a file we write the
following:

```hs
-- demo.hs

module Demo where
-- We declare the name of our module so 
-- it can be imported by name in a project.

x = 5

y = 2 * 5 + x

result = y * 10

```

Module names are capitalized, unlike variable names.



#### Remarks


[<img src="https://i.stack.imgur.com/t4TYu.png" alt="Haskell logo" />](https://i.stack.imgur.com/t4TYu.png)

[**Haskell**](https://www.haskell.org/) is an advanced purely-functional programming language.

### Features:

- **Statically typed:** Every expression in Haskell has a type which is determined at compile time. Static type checking is the process of verifying the type safety of a program based on analysis of a program's text (source code). If a program passes a static type checker, then the program is guaranteed to satisfy some set of type safety properties for all possible inputs.
- **Purely functional**: Every function in Haskell is a function in the mathematical sense. There are no statements or instructions, only expressions which cannot mutate variables (local or global) nor access state like time or random numbers.
- **Concurrent:** Its flagship compiler, GHC, comes with a high-performance parallel garbage collector and light-weight concurrency library containing a number of useful concurrency primitives and abstractions.
- **Lazy evaluation:** Functions don't evaluate their arguments. Delays the evaluation of an expression until its value is needed.
- **General-purpose:** Haskell is built to be used in all contexts and environments.
- **Packages:** Open source contribution to Haskell is very active with a wide range of packages available on the public package servers.

The latest standard of Haskell is Haskell 2010. As of May 2016, a group is working on the next version, Haskell 2020.

The [official Haskell documentation](https://www.haskell.org/documentation) is also a comprehensive and useful resource. Great place to find books, courses, tutorials, manuals, guides, etc.

