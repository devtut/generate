---
metaTitle: "Common compile/linker errors (GCC)"
description: "undefined reference to `***', error: '***' was not declared in this scope, fatal error: ***: No such file or directory"
---

# Common compile/linker errors (GCC)



## undefined reference to `***'


This linker error happens, if the linker can't find a used symbol.
Most of the time, this happens if a used library is not linked against.

**qmake:**

```cpp
LIBS += nameOfLib

```

**cmake:**

```cpp
TARGET_LINK_LIBRARIES(target nameOfLib)

```

**g++ call:**

```cpp
g++ -o main main.cpp -Llibrary/dir -lnameOfLib

```

One might also forget to compile and link all used `.cpp` files (functionsModule.cpp defines the needed function):

```cpp
g++  -o binName main.o functionsModule.o

```



## error: '***' was not declared in this scope


This error happens if a unknown object is used.

### Variables

Not compiling:

```cpp
#include <iostream>

int main(int argc, char *argv[])
{
    {
        int i = 2;
    }

    std::cout << i << std::endl; // i is not in the scope of the main function

    return 0;
}

```

Fix:

```cpp
#include <iostream>

int main(int argc, char *argv[])
{
    {
        int i = 2;
        std::cout << i << std::endl;
    }

    return 0;
}

```

### Functions

> 
<p>Most of the time this error occurs if the needed header is not
included (e.g. using `std::cout` without `#include <iostream>`)</p>


Not compiling:

```cpp
#include <iostream>

int main(int argc, char *argv[])
{
    doCompile();

    return 0;
}

void doCompile()
{
    std::cout << "No!" << std::endl;
}

```

Fix:

```cpp
#include <iostream>

void doCompile(); // forward declare the function

int main(int argc, char *argv[])
{
    doCompile();

    return 0;
}

void doCompile()
{
    std::cout << "No!" << std::endl;
}

```

Or:

```cpp
#include <iostream>

void doCompile() // define the function before using it
{
    std::cout << "No!" << std::endl;
}

int main(int argc, char *argv[])
{
    doCompile();

    return 0;
}

```

**Note:** The compiler interprets the code from top to bottom (simplification). Everything must be at least [declared (or defined)](http://www.cprogramming.com/declare_vs_define.html) before usage.



## fatal error: ***: No such file or directory


The compiler can't find a file (a source file uses `#include "someFile.hpp"`).

**qmake:**

```cpp
INCLUDEPATH += dir/Of/File

```

**cmake:**

```cpp
include_directories(dir/Of/File)

```

**g++ call:**

```cpp
g++ -o main main.cpp -Idir/Of/File

```

