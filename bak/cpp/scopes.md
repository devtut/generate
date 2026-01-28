---
metaTitle: "C++ | Scopes"
description: "Global variables, Simple block scope"
---

# Scopes



## Global variables


To declare a single instance of a variable which is accessible in different source files, it is possible to make it in the global scope with keyword `extern`. This keyword says the compiler that somewhere in the code there is a definition for this variable, so it can be used everywhere and all write/read will be done in one place of memory.

```cpp
// File my_globals.h:

#ifndef __MY_GLOBALS_H__
#define __MY_GLOBALS_H__

extern int circle_radius; // Promise to the compiler that circle_radius 
                          // will be defined somewhere

#endif

```

 

```cpp
// File foo1.cpp:

#include "my_globals.h"

int circle_radius = 123; // Defining the extern variable

```

 

```cpp
// File main.cpp:

#include "my_globals.h"
#include <iostream>

int main()
{
    std::cout << "The radius is: " << circle_radius << "\n";'
    return 0;
}

```

Output:

```cpp
The radius is: 123

```



## Simple block scope


The scope of a variable in a block `{ ... }`, begins after declaration and ends at the end of the block. If there is nested block, the inner block can hide the scope of a variable which is declared in the outer block.

```cpp
{
    int x = 100;
    //   ^
    //   Scope of `x` begins here
    //
}   // <- Scope of `x` ends here

```

If a nested block starts within an outer block, a new declared variable with the same name which is before in the outer class, hides the first one.

```cpp
{
    int x = 100;

    {
        int x = 200;

        std::cout << x;  // <- Output is 200
    }

    std::cout << x;  // <- Output is 100
}

```

