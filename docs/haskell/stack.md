---
metaTitle: "Haskell - Stack"
description: "Profiling with Stack, Structure, Build and Run a Stack Project, Viewing dependencies, Installing Stack, Creating a simple project, Stack install, Stackage Packages and changing the LTS (resolver) version"
---

# Stack



## Profiling with Stack


Configure profiling for a project via `stack`. First build the project with the `--profile` flag:

```hs
stack build --profile

```

GHC flags are not required in the cabal file for this to work (like `-prof`). `stack` will automatically turn on profiling for both the library and executables in the project. The next time an executable runs in the project, the usual `+RTS` flags can be used:

```hs
stack exec -- my-bin +RTS -p

```



## Structure


### File structure

A simple project has the following files included in it:

```hs
➜  helloworld ls 
LICENSE          Setup.hs         helloworld.cabal src              stack.yaml

```

In the folder `src` there is a file named `Main.hs`. This is the "starting point" of the `helloworld` project. By default `Main.hs` contains a simple "Hello, World!" program.

**Main.hs**

```hs
module Main where

main :: IO ()
main = do
  putStrLn "hello world"

```

### Running the program

Make sure you are in the directory `helloworld` and run:

```hs
stack build # Compile the program
stack exec helloworld # Run the program
# prints "hello world"

```



## Build and Run a Stack Project


In this example our project name is "helloworld" which was created with `stack new helloworld simple`

First we have to build the project with `stack build` and then we can run it with

```hs
stack exec helloworld-exe

```



## Viewing dependencies


To find out what packages your project directly depends on, you can simply use this command:

```hs
stack list-dependencies

```

This way you can find out what version of your dependencies where actually pulled down by stack.

Haskell projects frequently find themselves pulling in a lot of libraries indirectly, and sometimes these external dependencies cause problems that you need to track down. If you find yourself with a rogue external dependency that you'd like to identify, you can grep through the entire dependency graph and identify which of your dependencies is ultimately pulling in the undesired package:

```hs
stack dot --external | grep template-haskell

```

`stack dot` prints out a dependency graph in text form that can be searched. It can also be viewed:

```hs
stack dot --external | dot -Tpng -o my-project.png

```

You can also set the depth of the dependency graph if you want:

```hs
stack dot --external --depth 3 | dot -Tpng -o my-project.png

```



## Installing Stack


**Mac OSX**

Using [Homebrew](http://brew.sh/):

```hs
brew install haskell-stack

```



## Creating a simple project


To create a project called **helloworld** run:

```hs
stack new helloworld simple

```

This will create a directory called `helloworld` with the files necessary for a Stack project.



## Stack install


By running the command

```hs
stack install 

```

Stack will copy a executable file to the folder

```hs
/Users/<yourusername>/.local/bin/

```



## Stackage Packages and changing the LTS (resolver) version


[Stackage](https://www.stackage.org/) is a repository for Haskell packages. We can add these packages to a stack project.

### Adding lens to a project.

In a stack project, there is a file called `stack.yaml`. In `stack.yaml` there is a segment that looks like:

```hs
resolver: lts-6.8

```

Stackage keeps a list of packages for every revision of `lts`. In our case we want the list of packages for `lts-6.8` To find these packages visit:

```hs
https://www.stackage.org/lts-6.8 # if a different version is used, change 6.8 to the correct resolver number.

```

Looking through the packages, there is a [Lens-4.13](https://www.stackage.org/lts-6.8/package/lens-4.13).

We can now add the language package by modifying the section of `helloworld.cabal`:

```

 build-depends: base >= 4.7 && < 5

```

to:

```

 build-depends: base >= 4.7 && 5,
                 lens == 4.13

```

Obviously, if we want to change a newer LTS (after it's released), we just change the resolver number, eg.:

```hs
resolver: lts-6.9

```

With the next `stack build` Stack will use the LTS 6.9 version and hence download some new dependencies.

