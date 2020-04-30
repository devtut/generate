---
metaTitle: "C++ Debugging and Debug-prevention Tools & Techniques"
description: "Segfault analysis with GDB, Static analysis, Clean code, My C++ program ends with segfault - valgrind, Safe-stack (Stack corruptions)"
---

# C++ Debugging and Debug-prevention Tools & Techniques


A lot of time from C++ developers is spent debugging. This topic is meant to assist with this task and give inspiration for techniques. Don't expect an extensive list of issues and solutions fixed by the tools or a manual on the mentioned tools.



## Segfault analysis with GDB


Lets use the same code as above for this example.

```cpp
#include <iostream>

void fail() {
    int *p1;
    int *p2(NULL);
    int *p3 = p1;
    if (p3) {
        std::cout << *p2 << std::endl;
    } 
}

int main() { 
    fail();
}

```

First lets compile it

```cpp
g++ -g -o main main.cpp

```

Lets run it with gdb

```cpp
gdb ./main

```

Now we will be in gdb shell. Type run.

```cpp
(gdb) run
The program being debugged has been started already.
Start it from the beginning? (y or n) y
Starting program: /home/opencog/code-snippets/stackoverflow/a.out 

Program received signal SIGSEGV, Segmentation fault.
0x0000000000400850 in fail () at debugging_with_gdb.cc:11
11            std::cout << *p2 << std::endl;

```

We see the segmentation fault is happening at line 11. So the only variable being used at this line is pointer p2. Lets examine its content typing print.

```cpp
(gdb) print p2
$1 = (int *) 0x0 

```

Now we see that p2 was initialized to 0x0 which stands for NULL. At this line, we know that we are trying to dereference a NULL pointer. So we go and fix it.



## Static analysis


Static analysis is the technique in which on checks the code for patterns linked to known bugs. Using this technique is less time consuming than a code review, though, its checks are only limited to those programmed in the tool.

Checks can include the incorrect semi-colon behind the if-statement (`if (var);`) till advanced graph algorithms which determine if a variable is not initialized.

### Compiler warnings

Enabling static analysis is easy, the most simplistic version is already build-in in your compiler:

- [`clang++ -Wall -Weverything -Werror ...`](https://clang.llvm.org/docs/DiagnosticsReference.html)
- [`g++ -Wall -Weverything -Werror ...`](https://gcc.gnu.org/onlinedocs/gcc/Warning-Options.html)
- [`cl.exe /W4 /WX ...`](https://docs.microsoft.com/en-us/cpp/build/reference/compiler-option-warning-level)

If you enable these options, you will notice that each compiler will find bugs the others don't and that you will get errors on techniques which might be valid or valid in a specific context. `while (staticAtomicBool);` might be acceptable even if `while (localBool);` ain't.

So unlike code review, you are fighting a tool which understands your code, tells you a lot of useful bugs and sometimes disagrees with you. In this last case, you might have to suppress the warning locally.

As the options above enable all warnings, they might enable warnings you don't want. (Why should your code be C++98 compatible?) If so, you can simply disable that specific warning:

- `clang++ -Wall -Weverything -Werror -Wno-errortoaccept ...`
- `g++ -Wall -Weverything -Werror -Wno-errortoaccept ...`
- `cl.exe /W4 /WX /wd<no of warning>...`

Where compiler warnings assist you during development, they slow down compilation quite a bit. That is why you might not always want to enable them by default. Either you run them by default or you enable some continuous integration with the more expensive checks (or all of them).

### External tools

If you decide to have some continuous integration, the use of other tools ain't such a stretch. A tool like [clang-tidy](http://clang.llvm.org/extra/clang-tidy/) has an [list of checks](http://clang.llvm.org/extra/clang-tidy/checks/list.html) which covers a wide range of issues, some examples:

<li>Actual bugs
<ul>
- Prevention of slicing
- Asserts with side effects

- Misleading indentation
- Check identifier naming

- Use make_unique()
- Use nullptr

- Find unneeded copies
- Find inefficient algorithm calls

The list might not be that large, as Clang already has a lot of compiler warnings, however it will bring you one step closer to a high quality code base.

### Other tools

Other tools with similar purpose exist, like:

- [the visual studio static analyzer](https://blogs.msdn.microsoft.com/hkamel/2013/10/24/visual-studio-2013-static-code-analysis-in-depth-what-when-and-how/) as external tool
- [clazy](https://blogs.kde.org/2015/11/15/new-cqt-code-checks-clazy-static-analyzer), a Clang compiler plugin for checking Qt code

### Conclusion

A lot static analysis tools exist for C++, both build-in in the compiler as external tools. Trying them out doesn't take that much time for easy setups and they will find bugs you might miss in code review.



## Clean code


Debugging starts with understanding the code you are trying to debug.

Bad code:

```cpp
int main() {
    int value;
    std::vector<int> vectorToSort;
    vectorToSort.push_back(42); vectorToSort.push_back(13);
    for (int i = 52; i; i = i - 1)
    {
    vectorToSort.push_back(i *2);
    }
    /// Optimized for sorting small vectors
    if (vectorToSort.size() == 1);
    else
        {
        if (vectorToSort.size() <= 2)
            std::sort(vectorToSort.begin(), std::end(vectorToSort));
        }
    for (value : vectorToSort) std::cout << value << ' ';
return 0; }

```

Better code:

```cpp
std::vector<int> createSemiRandomData() {
    std::vector<int> data;
    data.push_back(42);
    data.push_back(13);
    for (int i = 52; i; --i)
        vectorToSort.push_back(i *2);
    return data;
}

/// Optimized for sorting small vectors
void sortVector(std::vector &v) {
    if (vectorToSort.size() == 1)
        return;
    if (vectorToSort.size() > 2)
        return;

    std::sort(vectorToSort.begin(), vectorToSort.end());
}

void printVector(const std::vector<int> &v) {
    for (auto i : v)
        std::cout << i << ' ';
}

int main() {
    auto vectorToSort = createSemiRandomData();
    sortVector(std::ref(vectorToSort));
    printVector(vectorToSort);

    return 0;
 }

```

Regardless of the coding styles you prefer and use, having a consistent coding (and formatting) style will help you understanding the code.

Looking at the code above, one can identify a couple of improvements to improve readability and debuggability:

### The use of separate functions for separate actions

The use of separate functions allow you to skip over some functions in the debugger if you ain't interested in the details. In this specific case, you might not be interested in the creation or printing of the data and only want to step into the sorting.

Another advantage is that you need to read less code (and memorize it) while stepping through the code. You now only need to read 3 lines of code in `main()` in order to understand it, instead of the whole function.

The third advantage is that you simply have less code to look at, which helps a trained eye in spotting this bug within seconds.

### Using consistent formatting/constructions

The use of consistent formatting and constructions will remove clutter from the code making it easier to focus on the code instead of text. A lot of discussions have been fed on the 'right' formatting style. Regardless of that style, having a single consistent style in the code will improve familiarity and make it easier to focus on the code.

As formatting code is time consuming task, it is recommended to use a dedicated tool for this. Most IDEs have at least some kind of support for this and can do formatting more consistent than humans.

You might note that the style is not limited to spaces and newlines as we no longer mix the free-style and the member functions to get begin/end of the container. (`v.begin()` vs `std::end(v)`).

### Point attention to the important parts of your code.

Regardless of the style you determine to choose, the above code contains a couple of markers which might give you a hint on what might be important:

- A comment stating `optimized`, this indicates some fancy techniques
- Some early returns in `sortVector()` indicate that we are doing something special
- The `std::ref()` indicates that something is going on with the `sortVector()`

### Conclusion

Having clean code will help you understanding the code and will reduce the time you need to debug it. In the second example, a code reviewer might even spot the bug at first glance, while the bug might be hidden in the details in the first one. (PS: The bug is in the compare with `2`.)



## My C++ program ends with segfault - valgrind


Let's have a basic failing program:

```cpp
#include <iostream>

void fail() {
    int *p1;
    int *p2(NULL);
    int *p3 = p1;
    if (p3) {
        std::cout << *p3 << std::endl;
    } 
}

int main() { 
    fail();
}

```

Build it (add -g to include debug info):

```cpp
g++ -g -o main main.cpp

```

Run:

```cpp
$ ./main
Segmentation fault (core dumped)
$

```

Let's debug it with valgrind:

```cpp
$ valgrind ./main
==8515== Memcheck, a memory error detector
==8515== Copyright (C) 2002-2015, and GNU GPL'd, by Julian Seward et al.
==8515== Using Valgrind-3.11.0 and LibVEX; rerun with -h for copyright info
==8515== Command: ./main
==8515==
==8515== Conditional jump or move depends on uninitialised value(s)
==8515==    at 0x400813: fail() (main.cpp:7)
==8515==    by 0x40083F: main (main.cpp:13)
==8515==
==8515== Invalid read of size 4
==8515==    at 0x400819: fail() (main.cpp:8)
==8515==    by 0x40083F: main (main.cpp:13)
==8515==  Address 0x0 is not stack'd, malloc'd or (recently) free'd
==8515==
==8515==
==8515== Process terminating with default action of signal 11 (SIGSEGV): dumping core
==8515==  Access not within mapped region at address 0x0
==8515==    at 0x400819: fail() (main.cpp:8)
==8515==    by 0x40083F: main (main.cpp:13)
==8515==  If you believe this happened as a result of a stack
==8515==  overflow in your program's main thread (unlikely but
==8515==  possible), you can try to increase the size of the
==8515==  main thread stack using the --main-stacksize= flag.
==8515==  The main thread stack size used in this run was 8388608.
==8515==
==8515== HEAP SUMMARY:
==8515==     in use at exit: 72,704 bytes in 1 blocks
==8515==   total heap usage: 1 allocs, 0 frees, 72,704 bytes allocated
==8515==
==8515== LEAK SUMMARY:
==8515==    definitely lost: 0 bytes in 0 blocks
==8515==    indirectly lost: 0 bytes in 0 blocks
==8515==      possibly lost: 0 bytes in 0 blocks
==8515==    still reachable: 72,704 bytes in 1 blocks
==8515==         suppressed: 0 bytes in 0 blocks
==8515== Rerun with --leak-check=full to see details of leaked memory
==8515==
==8515== For counts of detected and suppressed errors, rerun with: -v
==8515== Use --track-origins=yes to see where uninitialised values come from
==8515== ERROR SUMMARY: 2 errors from 2 contexts (suppressed: 0 from 0)
$

```

First we focus on this block:

```cpp
==8515== Invalid read of size 4
==8515==    at 0x400819: fail() (main.cpp:8)
==8515==    by 0x40083F: main (main.cpp:13)
==8515==  Address 0x0 is not stack'd, malloc'd or (recently) free'd

```

The first line tells us that segfault is caused by reading 4 bytes. The second and the third lines are call stack. It means that the invalid read is performed at the `fail()` function, line 8 of main.cpp, which is called by main, line 13 of main.cpp.

Looking at line 8 of main.cpp we see

```cpp
std::cout << *p3 << std::endl;

```

But we check the pointer first, so what's wrong? Lets check the other block:

```cpp
==8515== Conditional jump or move depends on uninitialised value(s)
==8515==    at 0x400813: fail() (main.cpp:7)
==8515==    by 0x40083F: main (main.cpp:13)

```

It tells us that there is an unitialized variable at line 7 and we read it:

```cpp
if (p3) {

```

Which points us to the line where we check p3 instead of p2. But how is it possible that p3 is uninitialized? We initialize it by:

```cpp
int *p3 = p1;

```

Valgrind advices us to rerun with `--track-origins=yes`, let's do it:

```cpp
valgrind --track-origins=yes ./main

```

The argument for valgrind is just after valgrind. If we put it after our program, it would be passed to our program.

The output is almost the same, there is only one difference:

```cpp
==8517== Conditional jump or move depends on uninitialised value(s)
==8517==    at 0x400813: fail() (main.cpp:7)
==8517==    by 0x40083F: main (main.cpp:13)
==8517==  Uninitialised value was created by a stack allocation
==8517==    at 0x4007F6: fail() (main.cpp:3)

```

Which tells us that the uninitialized value we used at line 7 was created at line 3:

```cpp
int *p1;

```

which guides us to our uninitialized pointer.



## Safe-stack (Stack corruptions)


Stack corruptions are annoying bugs to look at. As the stack is corrupted, the debugger often can't give you a good stack trace of where you are and how you got there.

This is where safe-stack comes into play. Instead of using a single stack for your threads, it will use two: A safe stack and a dangerous stack. The safe stack works exactly like it did before, except that some parts are moved to the dangerous stack.

### Which parts of the stack get moved?

Every part which has the potential of corrupting the stack will get moved out of the safe stack. As soon as a variable on the stack gets passed by reference or one takes the address of this variable, the compiler will decide to allocate this on the second stack instead of the safe one.

As a result, any operation you do with those pointers, any modification you make on the memory (based on those pointers/references) can only effect the memory in the second stack. As one never gets a pointer which is close to the safe stack, the stack cannot corrupt the stack and the debugger can still read all functions on the stack to give a nice trace.

### What is it actually used for?

The safe stack was not invented to give you better debugging experience, however, it is a nice side effect for nasty bugs. It's original purpose is as part of the [Code-Pointer Integrity (CPI) Project](http://dslab.epfl.ch/proj/cpi/), in which they try to prevent overriding the return addresses to prevent code injection. In other words, they try to prevent executing a hackers code.

For this reason, the feature has been activated on chromium and has been [reported](http://linuxplumbersconf.org/2015/ocw//system/presentations/3261/original/Beyond%20Traditional%20Compilation.pdf) to have a <1% CPU overhead.

### How to enable it?

Right now, the option is only available in the [clang compiler](https://clang.llvm.org/docs/SafeStack.html), where one can pass `-fsanitize=safe-stack` to the compiler. A [proposal](https://gcc.gnu.org/ml/gcc/2016-04/msg00083.html) was made to implement the same feature in GCC.

### Conclusion

Stack corruptions can become easier to debug when safe stack is enabled. Due to a low performance overhead, you can even activated by default in your build configuration.



#### Remarks


This topic ain't complete yet, examples on following techniques/tools would be useful:

- Mention more static analysis tools
- Binary instrumentation tools (like UBSan, TSan, MSan, ESan ...)
- Hardening (CFI ...)
- Fuzzing

