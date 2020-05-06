---
metaTitle: "Haskell - IO"
description: "Getting the 'a' out of 'IO a', IO defines your program's `main` action, Checking for end-of-file conditions, Reading all contents of standard input into a string, Role and Purpose of IO, Reading a line from standard input, Reading words from an entire file, Writing to stdout, Parsing and constructing an object from standard input, Reading from file handles, Reading from `stdin`"
---

# IO




## Getting the 'a' "out of" 'IO a'


A common question is "I have a value of `IO a`, but I want to do something to that `a` value: how do I get access to it?" How can one operate on data that comes from the outside world (for example, incrementing a number typed by the user)?

The point is that if you use a pure function on data obtained impurely, then the result is still impure. It depends on what the user did! A value of type `IO a` stands for a "side-effecting computation resulting in a value of type `a`" which can **only** be run by (a) composing it into `main` and (b) compiling and executing your program. For that reason, there is no way within pure Haskell world to "get the `a` out".

Instead, we want to build a new computation, a new `IO` value, which makes use of the `a` value **at runtime**. This is another way of **composing** `IO` values and so again we can use `do`-notation:

```hs
-- assuming
myComputation :: IO Int

getMessage :: Int -> String
getMessage int = "My computation resulted in: " ++ show int
 
newComputation :: IO ()
newComputation = do
  int <- myComputation       -- we "bind" the result of myComputation to a name, 'int'
  putStrLn $ getMessage int   -- 'int' holds a value of type Int

```

Here we're using a pure function (`getMessage`) to turn an `Int` into a `String`, but we're using `do` notation to make it be applied to the result of an `IO` computation `myComputation` **when** (after) that computation runs. The result is a bigger `IO` computation, `newComputation`. This technique of using pure functions in an impure context is called **lifting**.



## IO defines your program's `main` action


To make a Haskell program executable you must provide a file with a `main` function of type `IO ()`

```hs
main :: IO ()
main = putStrLn "Hello world!"

```

When Haskell is compiled it examines the `IO` data here and turns it into a executable. When we run this program it will print `Hello world!`.

If you have values of type `IO a` other than `main` they won't do anything.

```hs
other :: IO ()
other = putStrLn "I won't get printed"

main :: IO ()
main = putStrLn "Hello world!"

```

Compiling this program and running it will have the same effect as the last example. The code in `other` is ignored.

In order to make the code in `other` have runtime effects you have to **compose** it into `main`. No `IO` values not eventually composed into `main` will have any runtime effect. To compose two `IO` values sequentially you can use `do`-notation:

```hs
other :: IO ()
other = putStrLn "I will get printed... but only at the point where I'm composed into main"

main :: IO ()
main = do 
  putStrLn "Hello world!"
  other

```

When you compile and run **this** program it outputs

```hs
Hello world!
I will get printed... but only at the point where I'm composed into main

```

Note that the order of operations is described by how `other` was composed into `main` and not the definition order.



## Checking for end-of-file conditions


A bit counter-intuitive to the way most other languages' standard I/O libraries do it, Haskell's `isEOF` does not require you to perform a read operation before checking for an EOF condition; the runtime will do it for you.

```hs
import System.IO( isEOF )


eofTest :: Int -> IO ()
eofTest line = do
    end <- isEOF
    if end then
        putStrLn $ "End-of-file reached at line " ++ show line ++ "."
    else do
        getLine
        eofTest $ line + 1


main :: IO ()
main =
    eofTest 1

```

Input:

```hs
Line #1.
Line #2.
Line #3.

```

Output:

```hs
End-of-file reached at line 4.

```



## Reading all contents of standard input into a string


```hs
main = do
    input <- getContents
    putStr input

```

Input:

```hs
This is an example sentence.
And this one is, too!

```

Output:

```hs
This is an example sentence.
And this one is, too!

```

Note: This program will actually print parts of the output before all of the input has been fully read in. This means that, if, for example, you use `getContents` over a 50MiB file, Haskell's lazy evaluation and garbage collector will ensure that only the parts of the file that are currently needed (read: indispensable for further execution) will be loaded into memory. Thus, the 50MiB file won't be loaded into memory at once.



## Role and Purpose of IO


Haskell is a pure language, meaning that expressions cannot have side effects. A side effect is anything that the expression or function does other than produce a value, for example, modify a global counter or print to standard output.

In Haskell, side-effectful computations (specifically, those which can have an effect on the real world) are modelled using `IO`. Strictly speaking, `IO` is a type constructor, taking a type and producing a type. For example, `IO Int` is the type of an I/O computation producing an `Int` value. The `IO` type is **abstract**, and the interface provided for `IO` ensures that certain illegal values (that is, functions with non-sensical types) cannot exist, by ensuring that all built-in functions which perform IO have a return type enclosed in `IO`.

When a Haskell program is run, the computation represented by the Haskell value named `main`, whose type can be `IO x` for any type `x`, is executed.

### Manipulating IO values

There are many functions in the standard library providing typical `IO` actions that a general purpose programming language should perform, such as reading and writing to file handles. General `IO` actions are created and combined primarily with two functions:

```

(>>=) :: IO a -> (a -> IO b) -> IO b

```

This function (typically called **bind**) takes an `IO` action and a function which returns an `IO` action, and produces the `IO` action which is the result of applying the function to the value produced by the first `IO` action.

```

return :: a -> IO a

```

This function takes any value (i.e., a pure value) and returns the IO computation which does no IO and produces the given value. In other words, it is a no-op I/O action.

There are additional general functions which are often used, but all can be written in terms of the two above. For example, `(>>) :: IO a -> IO b -> IO b` is similar to `(>>=)` but the result of the first action is ignored.

A simple program greeting the user using these functions:

```

main :: IO ()
 main =
   putStrLn "What is your name?" >>
   getLine >>= \name ->
   putStrLn ("Hello " ++ name ++ "!")

```

This program also uses `putStrLn :: String -> IO ()` and `getLine :: IO String`.

Note: the types of certain functions above are actually more general than those types given (namely `>>=`, `>>` and `return`).

### IO semantics

The `IO` type in Haskell has very similar semantics to that of imperative programming languages. For example, when one writes `s1 ; s2` in an imperative language to indicate executing statement `s1`, then statement `s2`, one can write `s1 >> s2` to model the same thing in Haskell.

However, the semantics of `IO` diverge slightly of what would be expected coming from an imperative background. The `return` function **does not** interrupt control flow - it has no effect on the program if another `IO` action is run in sequence. For example, `return () >> putStrLn "boom"` correctly prints "boom" to standard output.

The formal semantics of `IO` can given in terms of simple equalities involving the functions in the previous section:

```

return x >>= f ≡ f x, ∀ f x
 y >>= return ≡ return y, ∀ y
 (m >>= f) >>= g ≡ m >>= (\x -> (f x >>= g)), ∀ m f g

```

These laws are typically referred to as left identity, right identity, and composition, respectively. They can be stated more naturally in terms of the function

```

(>=>) :: (a -> IO b) -> (b -> IO c) -> a -> IO c
 (f >=> g) x = (f x) >>= g

```

as follows:

```

return >=> f ≡ f, ∀ f
 f >=> return ≡ f, ∀ f
 (f >=> g) >=> h ≡ f >=> (g >=> h), ∀ f g h

```

### Lazy IO

Functions performing I/O computations are typically strict, meaning that all preceding actions in a sequence of actions must be completed before the next action is begun. Typically this is useful and expected behaviour - `putStrLn "X" >> putStrLn "Y"` should print "XY". However, certain library functions perform I/O lazily, meaning that the I/O actions required to produce the value are only performed when the value is actually consumed. Examples of such functions are `getContents` and `readFile`. Lazy I/O can drastically reduce the performance of a Haskell program, so when using library functions, care should be taken to note which functions are lazy.

### IO and `do` notation

Haskell provides a simpler method of combining different IO values into larger IO values. This special syntax is known as `do` notation* and is simply syntactic sugar for usages of the `>>=`, `>>` and `return` functions.

The program in the previous section can be written in two different ways using `do` notation, the first being layout-sensitive and the second being  layout insensitive:

```

main = do
   putStrLn "What is your name?"
   name <- getLine
   putStrLn ("Hello " ++ name ++ "!")


 main = do {
   putStrLn "What is your name?" ;
   name <- getLine ;
   putStrLn ("Hello " ++ name ++ "!")
   }

```

All three programs are exactly equivalent.

*Note that `do` notation is also applicable to a broader class of type constructors called **monads**.



## Reading a line from standard input


```hs
main = do
    line <- getLine
    putStrLn line

```

Input:

```hs
This is an example.

```

Output:

```hs
This is an example.

```



## Reading words from an entire file


In Haskell, it often makes sense **not to bother with file handles** at all, but simply read or write an entire file straight from disk to memory<sup>†</sup>, and do all the partitioning/processing of the text with the pure string data structure. This avoids mixing IO and program logic, which can greatly help avoiding bugs.

```hs
-- | The interesting part of the program, which actually processes data
--   but doesn't do any IO!
reverseWords :: String -> [String]
reverseWords = reverse . words

-- | A simple wrapper that only fetches the data from disk, lets
--   'reverseWords' do its job, and puts the result to stdout.
main :: IO ()
main = do
   content <- readFile "loremipsum.txt"
   mapM_ putStrLn $ reverseWords content

```

If `loremipsum.txt` contains

```hs
Lorem ipsum dolor sit amet,
consectetur adipiscing elit

```

then the program will output

```hs
elit
adipiscing
consectetur
amet,
sit
dolor
ipsum
Lorem

```

Here, [`mapM_`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Monad.html#v:mapM_) went through the list of all words in the file, and printed each of them to a separate line with [`putStrLn`](http://hackage.haskell.org/package/base-4.9.0.0/docs/Prelude.html#v:putStrLn).

<sup>†</sup><sub>If you think this is wasteful on memory, you have a point. Actually, Haskell's laziness can often avoid that the entire file needs to reside in memory simultaneously... but beware, this kind of lazy IO causes its own set of problems. For performance-critical applications, it often makes sense to enforce the entire file to be read at once, strictly; you can do this with [the `Data.Text` version of `readFile`.](http://hackage.haskell.org/package/text-1.2.2.1/docs/Data-Text-IO.html#v:readFile)</sub>



## Writing to stdout


Per the [Haskell 2010 Language Specification](https://www.haskell.org/onlinereport/haskell2010/haskellch7.html#x14-1430007.1) the following are standard IO functions available in Prelude, so no imports are required to use them.

### `putChar :: Char -> IO ()` - writes a `char` to `stdout`

```hs
Prelude> putChar 'a'
aPrelude>  -- Note, no new line

```

### `putStr :: String -> IO ()` - writes a `String` to `stdout`

```hs
Prelude> putStr "This is a string!"
This is a string!Prelude>  -- Note, no new line

```

### `putStrLn :: String -> IO ()` - writes a `String` to `stdout` and adds a new line

```hs
Prelude> putStrLn "Hi there, this is another String!"
Hi there, this is another String!

```

### `print :: Show a => a -> IO ()` - writes `a` an instance of `Show` to `stdout`

```hs
Prelude> print "hi"
"hi"
Prelude> print 1
1
Prelude> print 'a'
'a'
Prelude> print (Just 'a')  -- Maybe is an instance of the `Show` type class
Just 'a'
Prelude> print Nothing
Nothing

```

Recall that you can instantiate `Show` for your own types using `deriving`:

```hs
-- In ex.hs
data Person = Person { name :: String } deriving Show
main = print (Person "Alex")  -- Person is an instance of `Show`, thanks to `deriving`

```

Loading & running in GHCi:

```hs
Prelude> :load ex.hs
[1 of 1] Compiling ex             ( ex.hs, interpreted )
Ok, modules loaded: ex.
*Main> main  -- from ex.hs
Person {name = "Alex"}
*Main>

```



## Parsing and constructing an object from standard input


```hs
readFloat :: IO Float
readFloat =
    fmap read getLine


main :: IO ()
main = do
    putStr "Type the first number: "
    first <- readFloat

    putStr "Type the second number: "
    second <- readFloat

    putStrLn $ show first ++ " + " ++ show second ++ " = " ++ show ( first + second )

```

Input:

```hs
Type the first number: 9.5
Type the second number: -2.02

```

Output:

```hs
9.5 + -2.02 = 7.48

```



## Reading from file handles


Like in several other parts of the I/O library, functions that implicitly use a standard stream have a counterpart in `System.IO` that performs the same job,  but with an extra parameter at the left, of type `Handle`, that represents the stream being handled. For instance, `getLine :: IO String` has a counterpart `hGetLine :: Handle -> IO String`.

```hs
import System.IO( Handle, FilePath, IOMode( ReadMode ), 
                  openFile, hGetLine, hPutStr, hClose, hIsEOF, stderr )

import Control.Monad( when )


dumpFile :: Handle -> FilePath -> Integer -> IO ()

dumpFile handle filename lineNumber = do      -- show file contents line by line
    end <- hIsEOF handle
    when ( not end ) $ do
        line <- hGetLine handle
        putStrLn $ filename ++ ":" ++ show lineNumber ++ ": " ++ line
        dumpFile handle filename $ lineNumber + 1


main :: IO ()

main = do
    hPutStr stderr "Type a filename: "
    filename <- getLine
    handle <- openFile filename ReadMode
    dumpFile handle filename 1
    hClose handle

```

Contents of file `example.txt`:

```hs
This is an example.
Hello, world!
This is another example.

```

Input:

```hs
Type a filename: example.txt

```

Output:

```hs
example.txt:1: This is an example.
example.txt:2: Hello, world!
example.txt:3: This is another example

```



## Reading from `stdin`


As-per the [Haskell 2010 Language Specification](https://www.haskell.org/onlinereport/haskell2010/haskellch7.html#x14-1430007.1), the following are standard IO functions available in Prelude, so no imports are required to use them.

### `getChar :: IO Char` - read a `Char` from `stdin`

```hs
-- MyChar.hs
main = do
  myChar <- getChar
  print myChar

-- In your shell

runhaskell MyChar.hs
a -- you enter a and press enter
'a'  -- the program prints 'a'

```

### `getLine :: IO String` - read a `String` from `stdin`, sans new line character

```hs
Prelude> getLine
Hello there!  -- user enters some text and presses enter
"Hello there!"

```

### `read :: Read a => String -> a` - convert a String to a value

```hs
Prelude> read "1" :: Int
1
Prelude> read "1" :: Float
1.0
Prelude> read "True" :: Bool
True

```

Other, less common functions are:

- `getContents :: IO String` - returns all user input as a single string, which is read lazily as it is needed
- `interact :: (String -> String) -> IO ()` - takes a function of type String->String as its argument. The entire input from the standard input device is passed to this function as its argument

