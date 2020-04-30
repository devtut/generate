---
metaTitle: "Getting started with C Language"
description: "Hello World, Original Hello, World! in K&R C"
---

# Getting started with C Language



## Hello World


To create a simple C program which prints **"Hello, World"** on the screen, use a [text editor](https://en.wikipedia.org/wiki/Text_editor) to create a new file (e.g. `hello.c` — the file extension must be `.c`)
containing the following source code:

### hello.c

```c
#include <stdio.h>

int main(void)
{
    puts("Hello, World");
    return 0;
}

```

[Live demo on Coliru](http://coliru.stacked-crooked.com/a/263e35298419ef1d)

### Let's look at this simple program line by line

```c
#include <stdio.h>

```

This line tells the compiler to include the contents of the standard library header file `stdio.h` in the program. Headers are usually files containing function declarations, macros and data types, and you must include the header file before you use them. This line includes `stdio.h` so it can call the function `puts()`.

[See more about headers.](https://stackoverflow.com/documentation/c/6257/create-and-include-header-files)

```c
int main(void)

```

This line starts the definition of a function. It states the name of the function (`main`), the type and number of arguments it expects (`void`, meaning none), and the type of value that this function returns (`int`).  Program execution starts in the `main()` function.

```c
{
    …
}

```

The curly braces are used in pairs to indicate where a block of code begins and ends. They can be used in a lot of ways, but in this case they indicate where the function begins and ends.

```

   puts("Hello, World");

```

This line calls the `puts()` function to output text to standard output (the screen, by default), followed by a newline. The string to be output is included within the parentheses.

`"Hello, World"` is the string that will be written to the screen. In C, every string literal value must be inside the double quotes `"…"`.

[See more about strings.](https://stackoverflow.com/documentation/c/1990/strings)

In C programs, every statement needs to be terminated by a semi-colon (i.e. `;`).

```

   return 0;

```

When we defined `main()`, we declared it as a function returning an `int`, meaning it needs to return an integer. In this example, we are returning the integer value 0, which is used to indicate that the program exited successfully.
After the `return 0;` statement, the execution process will terminate.

### Editing the program

Simple text editors include [`vim`](http://www.vim.org/) or [`gedit`](https://wiki.gnome.org/Apps/Gedit) on Linux, or [`Notepad`](https://en.wikipedia.org/wiki/Microsoft_Notepad) on Windows.
Cross-platform editors also include [`Visual Studio Code`](https://code.visualstudio.com) or [`Sublime Text`](https://www.sublimetext.com/).

The editor must create plain text files, not RTF or other any other format.

### Compiling and running the program

To run the program, this source file (`hello.c`) first needs to be compiled into an executable file (e.g. `hello` on Unix/Linux system or `hello.exe` on Windows). This is done using a compiler for the C language.

[See more about compiling](https://stackoverflow.com/documentation/c/1337/compilation)

### Compile using GCC

[GCC](https://gcc.gnu.org/) (GNU Compiler Collection) is a widely used C compiler. To use it, open a terminal, use the command line to navigate to the source file's location and then run:

```c
gcc hello.c -o hello

```

If no errors are found in the the source code (`hello.c`), the compiler will create a **binary file**, the name of which is given by the argument to the `-o` command line option (`hello`). This is the final executable file.

We can also use the warning options `-Wall -Wextra -Werror`, that help to identify problems that can cause the program to fail or produce unexpected results. They are not necessary for this simple program but this is way of adding them:

```c
gcc -Wall -Wextra -Werror -o hello hello.c

```

### Using the clang compiler

To compile the program using [`clang`](https://llvm.org/) you can use:

```c
clang -Wall -Wextra -Werror -o hello hello.c

```

By design, the `clang` command line options are similar to those of GCC.

### Using the Microsoft C compiler from the command line

If using the Microsoft `cl.exe` compiler on a Windows system which supports [Visual Studio](https://www.visualstudio.com/downloads/) and if all environment variables are set, this C example may be compiled using the following command which will produce an executable `hello.exe` within the directory the command is executed in (There are warning options such as `/W3` for `cl`, roughly analogous to `-Wall` etc for GCC or clang).

```c
cl hello.c

```

### Executing the program

Once compiled, the binary file may then be executed by typing `./hello` in the terminal. Upon execution, the compiled program will print `Hello, World`, followed by a newline, to the command prompt.



## Original "Hello, World!" in K&R C


The following is the original "Hello, World!" program from the book [The C Programming Language](https://en.wikipedia.org/wiki/The_C_Programming_Language) by Brian Kernighan and Dennis Ritchie (Ritchie was the original developer of the C programming language at Bell Labs), referred to as "K&R":

```c
#include <stdio.h>

main()
{
    printf("hello, world\n");
}

```

Notice that the C programming language was not standardized at the time of writing the first edition of this book (1978), and that this program will probably not compile on most modern compilers unless they are instructed to accept C90 code.

This very first example in the K&R book is now considered poor quality, in part because it lacks an explicit return type for `main()` and in part because it lacks a `return` statement. The 2nd edition of the book was written for the old C89 standard. In C89, the type of `main` would default to `int`, but the K&R example does not return a defined value to the environment. In C99 and later standards, the return type is required, but it is safe to leave out the `return` statement of `main` (and only `main`), because of a special case introduced with C99 5.1.2.2.3 — it is equivalent to returning 0, which indicates success.

The recommended and most portable form of `main` for hosted systems is `int main (void)` when the program does not use any command line arguments, or `int main(int argc, char **argv)` when the program does use the command line arguments.

C90 §5.1.2.2.3 **Program termination**

> 
A return from the initial call to the `main` function is equivalent to calling the `exit` function with the value returned by the `main` function as its argument. If the `main` function executes a return that specifies no value, the termination status returned to the host environment is undefined.


C90 §6.6.6.4 **The `return` statement**

> 
<p>If a `return` statement without an expression is executed, and the value of the function call
is used by the caller, the behavior is undefined. Reaching the `}` that terminates a function is
equivalent to executing a `return` statement without an expression.</p>


C99 §5.1.2.2.3 **Program termination**

> 
<p>If the return type of the `main` function is a type compatible with `int`, a return from the
initial call to the `main` function is equivalent to calling the `exit` function with the value
returned by the `main` function as its argument; reaching the `}` that terminates the
`main` function returns a value of 0. If the return type is not compatible with `int`, the
termination status returned to the host environment is unspecified.</p>




#### Remarks


C is a general-purpose, imperative computer programming language, supporting structured programming, lexical variable scope and recursion, while a static type system prevents many unintended operations. By design, C provides constructs that map efficiently to typical machine instructions, and therefore it has found lasting use in applications that had formerly been coded in assembly language, including operating systems, as well as various application software for computers ranging from supercomputers to embedded systems.

Despite its low-level capabilities, the language was designed to encourage cross-platform programming. A standards-compliant and portably written C program can be compiled for a very wide variety of computer platforms and operating systems with few changes to its source code. The language has become available on a very wide range of platforms, from embedded microcontrollers to supercomputers.

C was originally developed by Dennis Ritchie between 1969 and 1973 at Bell Labs and used to re-implement the [Unix](https://en.wikipedia.org/wiki/Unix) operating systems. It has since become one of the most widely used programming languages of all time, with C compilers from various vendors available for the majority of existing computer architectures and operating systems.

### Common Compilers

The process to compile a C program differs between compilers and operating systems. Most operating systems ship without a compiler, so you will have to install one. Some common compilers choices are:

- [GCC, the GNU Compiler Collection](https://gcc.gnu.org/)
- [clang: a C language family front-end for LLVM](http://clang.llvm.org/)
- [MSVC, Microsoft Visual C/C++ build tools](http://landinghub.visualstudio.com/visual-cpp-build-tools)

The following documents should give you a good overview on how to get started using a few of the most common compilers:

- [Getting started with Microsoft Visual C](https://msdn.microsoft.com/en-us/library/bb384838.aspx)
- [Getting started with GCC](https://www3.ntu.edu.sg/home/ehchua/programming/cpp/gcc_make.html)

### Compiler C version Support

Note that compilers have varying levels of support for standard C with many still not completely supporting C99. For example, as of the 2015 release, MSVC supports much of C99 yet still has some important exceptions for support of the language itself (e.g the preprocessing seems non-conformant) and for the C library (e.g. `<tgmath.h>`), nor do they necessarily document their "implementation dependent choices". [Wikipedia has a table](https://en.wikipedia.org/wiki/C99#Implementations) showing support offered by some popular compilers.

Some compilers (notably GCC) have offered, or continue to offer, **compiler extensions** that implement additional features that the compiler producers deem necessary, helpful or believe may become part of a future C version, but that are not currently part of any C standard. As these extensions are compiler specific they can be considered to not be cross-compatible and compiler developers may remove or alter them in later compiler versions. The use of such extensions can generally be controlled by compiler flags.

Additionally, many developers have compilers that support only specific versions of C imposed by the environment or platform they are targeting.

If selecting a compiler, it is recommended to choose a compiler that has the best support for the latest version of C allowed for the target environment.

### Code style (off-topic here):

Because white space is insignificant in C (that is, it does not affect the operation of the code), programmers often use white space to make the code easier to read and comprehend, this is called the **code style**. It is a set of rules and guidelines used when writing the source code. It covers concerns such as how lines should be indented, whether spaces or tabs should be used, how braces should be placed, how spaces should be used around operators and brackets, how variables should be named and so forth.

Code style is not covered by the standard and is primarily opinion based (different people find different styles easier to read), as such, it is generally considered off-topic on SO. The overriding advice on style in one's own code is that consistency is paramount - pick, or make, a style and stick to it. Suffice it to explain that there are various named styles in common usage that are often chosen by programmers rather than creating their own style.

Some common indent styles are: K & R style, Allman style, GNU style and so on. Some of these styles have different variants. Allman, for example, is used as either regular Allman or the popular variant, Allman-8. Information on some of the popular styles may be found on [Wikipedia](https://en.wikipedia.org/wiki/Indent_style). Such style names are taken from the standards the authors or organizations often publish for use by the many people contributing to their code, so that everyone can easily read the code when they know the style, such as the [GNU formatting guide](https://www.gnu.org/prep/standards/html_node/Writing-C.html) that makes up part of the [GNU coding standards](https://www.gnu.org/prep/standards/html_node/index.html) document.

Some common naming conventions are: UpperCamelCase, lowerCamelCase, lower_case_with_underscore, ALL_CAPS, etc. These styles are combined in various ways for use with different objects and types (e.g., macros often use ALL_CAPS style)

K & R style is generally recommended for use within SO documentation, whereas the more esoteric styles, such as Pico, are discouraged.

### Libraries and APIs not covered by the C Standard (and therefore being off-topic here):

- [POSIX](http://stackoverflow.com/documentation/posix/topics) API (covering for example [PThreads](http://stackoverflow.com/documentation/posix/4508/threads#t=201607282046185855289), [Sockets](http://stackoverflow.com/documentation/posix/4706/sockets#t=201608081932486939439), [Signals](http://stackoverflow.com/documentation/posix/4532/signals#t=201607291207204211441))

