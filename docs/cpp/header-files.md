---
metaTitle: "Header Files"
description: "Basic Example, Templates in Header Files"
---

# Header Files




## Basic Example


The following example will contain a block of code that is meant to be split into several source files, as denoted by `// filename` comments.

### Source Files

```cpp
// my_function.h

/* Note how this header contains only a declaration of a function.
 * Header functions usually do not define implementations for declarations
 * unless code must be further processed at compile time, as in templates.
 */

/* Also, usually header files include preprocessor guards so that every header
 * is never included twice.
 *
 * The guard is implemented by checking if a header-file unique preprocessor
 * token is defined, and only including the header if it hasn't been included
 * once before.
 */
#ifndef MY_FUNCTION_H
#define MY_FUNCTION_H

// global_value and my_function() will be
// recognized as the same constructs if this header is included by different files.
const int global_value = 42;
int my_function();

#endif // MY_FUNCTION_H

```

```cpp
// my_function.cpp

/* Note how the corresponding source file for the header includes the interface  
 * defined in the header so that the compiler is aware of what the source file is 
 * implementing.
 *
 * In this case, the source file requires knowledge of the global constant
 * global_value only defined in my_function.h. Without inclusion of the header
 * file, this source file would not compile.
 */
#include "my_function.h" // or #include "my_function.hpp"
int my_function() {
  return global_value; // return 42;
}

```

Header files are then included by other source files that want to use the functionality defined by the header interface, but don't require knowledge of its implementation (thus, reducing code coupling). The following program makes use of the header `my_function.h` as defined above:

```cpp
// main.cpp

#include <iostream>       // A C++ Standard Library header.
#include "my_function.h"  // A personal header

int main(int argc, char** argv) {
  std::cout << my_function() << std::endl;
  return 0;
}

```

### The Compilation Process

Since header files are often part of a compilation process workflow, a typical compilation process making use of the header/source file convention will usually do the following.

Assuming that the header file and source code file is already in the same directory, a programmer would execute the following commands:

```cpp
g++ -c my_function.cpp       # Compiles the source file my_function.cpp
                             # --> object file my_function.o

g++ main.cpp my_function.o   # Links the object file containing the 
                             # implementation of int my_function()
                             # to the compiled, object version of main.cpp
                             # and then produces the final executable a.out

```

Alternatively, if one wishes to compile `main.cpp` to an object file first, and
then link only object files together as the final step:

```cpp
g++ -c my_function.cpp
g++ -c main.cpp

g++ main.o my_function.o

```



## Templates in Header Files


Templates require compile-time generation of code: a templated function, for example, will be effectively turned into multiple distinct functions once a templated function is parameterized by use in source code.

This means that template function, member function, and class definitions cannot be delegated to a separate source code file, as any code that will use any templated construct requires knowledge of its definition to generally generate any derivative code.

Thus, templated code, if put in headers, must also contain its definition. An example of this is below:

```cpp
// templated_function.h

template <typename T>
T* null_T_pointer() {
  T* type_point = NULL; // or, alternatively, nullptr instead of NULL
                        // for C++11 or later
  return type_point;
} 

```



#### Remarks


In C++, as in C, the C++ compiler and compilation process makes use of the C preprocessor. As specified by the GNU C Preprocessor manual, a header file is defined as the following:

> 
<p>A header file is a file containing C declarations and macro
definitions (see Macros) to be shared between several source files.
You request the use of a header file in your program by including it,
with the C preprocessing directive ‘#include’.</p>
Header files serve two purposes.
<ul>
<li>System header files declare the interfaces to parts of the operating system. You include them in your program to supply the
definitions and declarations you need to invoke system calls and
libraries.</li>
<li>Your own header files contain declarations for interfaces between the source files of your program. Each time you have a group of
related declarations and macro definitions all or most of which are
needed in several different source files, it is a good idea to create
a header file for them.</li>
</ul>


However, to the C preprocessor itself, a header file is no different than a source file.

The header/source file organization scheme is simply a strongly-held and standard convention set by various software projects in order to provide separation between interface and implementation.

Although it is not formally enforced by the C++ Standard itself, following the header/source file convention is highly recommended, and, in practice, is already almost ubiquitous.

Note that header files may be replaced as a project file structure convention by the upcoming feature of modules, which is still to be considered for inclusion in a future C++ Standard as of the time of writing (e.g. C++20).

