---
metaTitle: "Haskell - Using GHCi"
description: "Breakpoints with GHCi, Starting GHCi, Changing the GHCi default prompt, The GHCi configuration file, Loading a file, Quitting GHCi, Reloading a already loaded file, Multi-line statements"
---

# Using GHCi



## Breakpoints with GHCi


GHCi supports imperative-style breakpoints out of the box with interpreted code (code that's been `:loaded`).

With the following program:

```hs
-- mySum.hs
doSum n = do
  putStrLn ("Counting to " ++ (show n))
  let v = sum [1..n]
  putStrLn ("sum to " ++ (show n) ++ " = " ++ (show v))

```

loaded into GHCi:

```hs
Prelude> :load mySum.hs 
[1 of 1] Compiling Main             ( mySum.hs, interpreted )
Ok, modules loaded: Main.
*Main> 

```

We can now set breakpoints using line numbers:

```hs
*Main> :break 2
Breakpoint 0 activated at mySum.hs:2:3-39

```

and GHCi will stop at the relevant line when we run the function:

```hs
*Main> doSum 12
Stopped at mySum.hs:2:3-39
_result :: IO () = _
n :: Integer = 12
[mySum.hs:2:3-39] *Main> 

```

It might be confusing where we are in the program, so we can use `:list` to clarify:

```hs
[mySum.hs:2:3-39] *Main> :list
1  doSum n = do
2    putStrLn ("Counting to " ++ (show n))   -- GHCi will emphasise this line, as that's where we've stopped
3    let v = sum [1..n]

```

We can print variables, and continue execution too:

```hs
[mySum.hs:2:3-39] *Main> n
12
:continue 
Counting to 12
sum to 12 = 78
*Main> 

```



## Starting GHCi


Type `ghci` at a shell prompt to start GHCI.

```hs
$ ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Prelude> 

```



## Changing the GHCi default prompt


By default, GHCI's prompt shows all the modules you have loaded into your interactive session. If you have many modules loaded this can get long:

```hs
Prelude Data.List Control.Monad> -- etc

```

The `:set prompt` command changes the prompt for this interactive session.

```hs
Prelude Data.List Control.Monad> :set prompt "foo> "
foo> 

```

To change the prompt permanently, add `:set prompt "foo> "` to [the GHCi config file](http://stackoverflow.com/documentation/haskell/3407/using-ghci/11725/the-ghci-configuration-file#t=201607271346556420958).



## The GHCi configuration file


GHCi uses a configuration file in `~/.ghci`. A configuration file consists of a sequence of commands which GHCi will execute on startup.

```hs
$ echo ":set prompt \"foo> \"" > ~/.ghci
$ ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
Loaded GHCi configuration from ~/.ghci
foo> 

```



## Loading a file


The `:l` or `:load` command type-checks and loads a file.

```hs
$ echo "f = putStrLn \"example\"" > example.hs
$ ghci
GHCi, version 8.0.1: http://www.haskell.org/ghc/  :? for help
ghci> :l example.hs
[1 of 1] Compiling Main               ( example.hs, interpreted )
Ok, modules loaded: Main.
ghci> f
example

```



## Quitting GHCi


You can quit GHCi simply with `:q` or `:quit`

```hs
ghci> :q
Leaving GHCi.

ghci> :quit
Leaving GHCi.

```

Alternatively, the shortcut <kbd>CTRL</kbd>+<kbd>D</kbd> (<kbd>Cmd</kbd>+<kbd>D</kbd> for OSX) has the same effect as `:q`.



## Reloading a already loaded file


If you have loaded a file into GHCi (e.g. using `:l filename.hs`) and you have changed the file in an editor outside of GHCi you must reload the file with `:r` or `:reload` in order to make use of the changes, hence you don't need to type again the filename.

```hs
ghci> :r
OK, modules loaded: Main.

ghci> :reload
OK, modules loaded: Main.

```



## Multi-line statements


The `:{` instruction begins **multi-line mode** and `:}` ends it. In multi-line mode GHCi will interpret newlines as semicolons, not as the end of an instruction.

```hs
ghci> :{
ghci| myFoldr f z [] = z
ghci| myFoldr f z (y:ys) = f y (myFoldr f z ys)
ghci| :}
ghci> :t myFoldr
myFoldr :: (a -> b -> b) -> b -> [a] -> b

```



#### Remarks


GHCI is the interactive REPL that comes bundled with GHC.

