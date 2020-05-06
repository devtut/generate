---
metaTitle: "Haskell - Cabal"
description: "Working with sandboxes, Install packages"
---

# Cabal



## Working with sandboxes


A Haskell project can either use the system wide packages or use a sandbox. A sandbox is an isolated package database and can prevent dependency conflicts, e. g. if multiple Haskell projects use different versions of a package.

To initialize a sandbox for a Haskell package go to its directory and run:

```hs
cabal sandbox init

```

Now packages can be installed by simply running `cabal install`.

Listing packages in a sandbox:

```hs
cabal sandbox hc-pkg list

```

Deleting a sandbox:

```hs
cabal sandbox delete

```

Add local dependency:

```hs
cabal sandbox add-source /path/to/dependency

```



## Install packages


To install a new package, e.g. aeson:

```hs
cabal install aeson

```



#### Syntax


- cabal <command> where <command> is one of:
<li>**[global]**
<ul>
<li>update
<ul>
- Updates list of known packages

- Install packages

- Help about commands

- Display detailed information about a particular package

- List packages matching a search string

- Downloads packages for later installation

- Display and update the user's global cabal configuration

<li>get
<ul>
- Download/Extract a package's source code (repository)

- Create a new .cabal package file (interactively)

- Prepare to build the package

- Compile all/specific components

- Clean up after a build

- Builds and runs an executable

- Open an interpreter session for the given component

- Run all/specific tests in the test suite

- Run all/specific benchmarks

- Check the package for common mistakes

- Generate a source distribution file (.tar.gz)

- Uploads source packages or documentation to Hackage

- Upload build reports to a remote server

- Freeze dependencies

- Generate dependency bounds

- Generate Haddock HTML documentation

- Generate HsColour colourised code, in HTML format

- Copy the files into the install locations

- Register this package with the compiler

<li>sandbox
<ul>
<li>Create/modify/delete a sandbox
<ul>
- cabal sandbox init          [FLAGS]
- cabal sandbox delete        [FLAGS]
- cabal sandbox add-source    [FLAGS] PATHS
- cabal sandbox delete-source [FLAGS] PATHS
- cabal sandbox list-sources  [FLAGS]
- cabal sandbox hc-pkg        [FLAGS] [--] COMMAND [--] [ARGS]

- Give a command access to the sandbox package repository

- Open interpreter with access to sandbox packages

