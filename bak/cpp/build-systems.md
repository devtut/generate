---
metaTitle: "C++ | Build Systems"
description: "Generating Build Environment with CMake, Compiling with GNU make, Building with SCons, Autotools (GNU), Ninja, NMAKE (Microsoft Program Maintenance Utility)"
---

# Build Systems




## Generating Build Environment with CMake


[CMake](https://cmake.org/) generates build environments for nearly any compiler or IDE from a single project definition. The following examples will demonstrate how to add a CMake file to the [cross-platform "Hello World" C++ code](http://stackoverflow.com/documentation/c%2B%2B/206/introduction-to-c/774/hello-world).

CMake files are always named "CMakeLists.txt" and should already exist in every project's root directory (and possibly in sub-directories too.) A basic CMakeLists.txt file looks like:

```cpp
cmake_minimum_required(VERSION 2.4)

project(HelloWorld)

add_executable(HelloWorld main.cpp)

```

See it [live on Coliru](http://coliru.stacked-crooked.com/a/616a0939bdd91b8b).

This file tells CMake the project name, what file version to expect, and instructions to generate an executable called "HelloWorld" that requires `main.cpp`.

Generate a build environment for your installed compiler/IDE from the command line:

```cpp
> cmake .

```

Build the application with:

```cpp
> cmake --build .

```

This generates the default build environment for the system, depending on the OS and installed tools. Keep source code clean from any build artifacts with use of "out-of-source" builds:

```cpp
> mkdir build
> cd build
> cmake ..
> cmake --build .

```

CMake can also abstract the platform shell's basic commands from the previous example:

```cpp
> cmake -E make_directory build
> cmake -E chdir build cmake .. 
> cmake --build build 

```

CMake includes [generators](https://cmake.org/cmake/help/v3.0/manual/cmake-generators.7.html) for a number of common build tools and IDEs. To generate makefiles for [Visual Studio's `nmake`](https://msdn.microsoft.com/en-us/library/dd9y37ha.aspx):

```cpp
> cmake -G "NMake Makefiles" ..
> nmake

```



## Compiling with GNU make


### Introduction

The GNU Make (styled `make`) is a program dedicated to the automation of executing shell commands. GNU Make is one specific program that falls under the Make family. Make remains popular among Unix-like and POSIX-like operating systems, including those derived from the Linux kernel, Mac OS X, and BSD.

GNU Make is especially notable for being attached to the GNU Project, which is attached to the popular GNU/Linux operating system. GNU Make also has compatible versions running on various flavors of Windows and Mac OS X. It is also a very stable version with historical significance that remains popular. It is for these reasons that GNU Make is often taught alongside C and C++.

### Basic rules

To compile with make, create a Makefile in your project directory. Your Makefile could be as simple as:

**Makefile**

```cpp
# Set some variables to use in our command
# First, we set the compiler to be g++
CXX=g++

# Then, we say that we want to compile with g++'s recommended warnings and some extra ones.
CXXFLAGS=-Wall -Wextra -pedantic

# This will be the output file
EXE=app

SRCS=main.cpp

# When you call `make` at the command line, this "target" is called.
# The $(EXE) at the right says that the `all` target depends on the `$(EXE)` target.
# $(EXE) expands to be the content of the EXE variable
# Note: Because this is the first target, it becomes the default target if `make` is called without target
all: $(EXE)

# This is equivalent to saying
# app: $(SRCS)
# $(SRCS) can be separated, which means that this target would depend on each file.
# Note that this target has a "method body": the part indented by a tab (not four spaces).
# When we build this target, make will execute the command, which is:
# g++ -Wall -Wextra -pedantic -o app main.cpp
# I.E. Compile main.cpp with warnings, and output to the file ./app
$(EXE): $(SRCS)
    @$(CXX) $(CXXFLAGS) -o $@ $(SRCS)

# This target should reverse the `all` target. If you call
# make with an argument, like `make clean`, the corresponding target
# gets called.
clean:
    @rm -f $(EXE)

```

> 
**NOTE: Make absolutely sure that the indentations are with a tab, not with four spaces. Otherwise, you'll get an error of** `Makefile:10: *** missing separator. Stop.`


To run this from the command-line, do the following:

```cpp
$ cd ~/Path/to/project
$ make
$ ls
app  main.cpp  Makefile

$ ./app
Hello World!

$ make clean
$ ls
main.cpp  Makefile

```

### Incremental builds

When you start having more files, make becomes more useful. What if you edited **a.cpp** but not **b.cpp**? Recompiling **b.cpp** would take more time.

With the following directory structure:

```cpp
.
+-- src
|   +-- a.cpp
|   +-- a.hpp
|   +-- b.cpp
|   +-- b.hpp
+-- Makefile

```

This would be a good Makefile:

**Makefile**

```cpp
CXX=g++
CXXFLAGS=-Wall -Wextra -pedantic
EXE=app

SRCS_GLOB=src/*.cpp
SRCS=$(wildcard $(SRCS_GLOB))
OBJS=$(SRCS:.cpp=.o)

all: $(EXE)

$(EXE): $(OBJS)
    @$(CXX) -o $@ $(OBJS)

depend: .depend

.depend: $(SRCS)
    @-rm -f ./.depend
    @$(CXX) $(CXXFLAGS) -MM $^>>./.depend

clean:
    -rm -f $(EXE)
    -rm $(OBJS)
    -rm *~
    -rm .depend

include .depend

```

Again watch the tabs. This new Makefile ensures that you only recompile changed files, minimizing compile time.

### Documentation

For more on make, see [the official documentation by the Free Software Foundation](https://www.gnu.org/software/make/manual/), [the stackoverflow documentation](https://stackoverflow.com/documentation/makefile) and [dmckee's elaborate answer on stackoverflow](http://stackoverflow.com/a/2481326/1896169).



## Building with SCons


You can build the [cross-platform "Hello World" C++ code](http://stackoverflow.com/documentation/c%2B%2B/206/introduction-to-c/774/hello-world), using [Scons](http://scons.org/) - A [Python](https://www.python.org/)-language software construction tool.

First, create a file called `SConstruct` (note that SCons will look for a file with this exact name by default). For now, the file should be in a directory right along your `hello.cpp`. Write in the new file the line

```

Program('hello.cpp')

```

Now, from the terminal, run `scons`. You should see something like

```cpp
$ scons
scons: Reading SConscript files ...
scons: done reading SConscript files.
scons: Building targets ...
g++ -o hello.o -c hello.cpp
g++ -o hello hello.o
scons: done building targets.

```

(although the details will vary depending on your operating system and installed compiler).

The `Environment` and `Glob` classes will help you further configure what to build. E.g., the `SConstruct` file

```cpp
env=Environment(CPPPATH='/usr/include/boost/',
    CPPDEFINES=[],
    LIBS=[],
    SCONS_CXX_STANDARD="c++11"
    )

env.Program('hello', Glob('src/*.cpp'))     

```

builds the executable `hello`, using all `cpp` files in `src`. Its `CPPPATH` is `/usr/include/boost` and it specifies the C++11 standard.



## Autotools (GNU)


### Introduction

The Autotools are a group of programs that create a GNU Build System for a given software package. It is a suite of tools that work together to produce various build resources, such as a Makefile (to be used with GNU Make). Thus, Autotools can be considered a de facto build system generator.

Some notable Autotools programs include:

- Autoconf
- Automake (not to be confused with `make`)

In general, Autotools is meant to generate the Unix-compatible script and Makefile to allow the following command to build (as well as install) most packages (in the simple case):

```cpp
./configure && make && make install

```

As such, Autotools also has a relationship with certain package managers, especially those that are attached to operating systems that conform to the POSIX Standard(s).



## Ninja


### Introduction

The Ninja build system is described by its project website as ["a small build system with a focus on speed."](https://ninja-build.org/) Ninja is designed to have its files generated by build system file generators, and takes a low-level approach to build systems, in contrast to higher-level build system managers like CMake or Meson.

Ninja is primarily written in C++ and Python, and was created as an alternative to the SCons build system for the Chromium project.



## NMAKE (Microsoft Program Maintenance Utility)


### Introduction

NMAKE is a command-line utility developed by Microsoft to be used primarily in conjunction with Microsoft Visual Studio and/or the Visual C++ command line tools.

NMAKE is build system that falls under the Make family of build systems, but has certain distinct features that diverge from Unix-like Make programs, such as supporting Windows-specific file path syntax (which itself differs from Unix-style file paths).



#### Remarks


Currently, there exists no universal or dominant build system for C++ that is both popular and cross-platform. However, there do exist several major build systems that are attached to major platforms/projects, the most notable being GNU Make with the GNU/Linux operating system and NMAKE with the Visual C++/Visual Studio project system.

Additionally, some Integrated Development Environments (IDEs) also include specialized build systems to be used specifically with the native IDE. Certain build system generators can generate these native IDE build system/project formats, such as CMake for Eclipse and Microsoft Visual Studio 2012.

