---
metaTitle: "C++ | One Definition Rule (ODR)"
description: "Multiply defined function, Inline functions, ODR violation via overload resolution"
---

# One Definition Rule (ODR)



## Multiply defined function


The most important consequence of the One Definition Rule is that non-inline functions with external linkage should only be defined once in a program, although they can be declared multiple times. Therefore, such functions should not be defined in headers, since a header can be included multiple times from different translation units.

`foo.h`:

```cpp
#ifndef FOO_H
#define FOO_H
#include <iostream>
void foo() { std::cout << "foo"; }
void bar();
#endif

```

`foo.cpp`:

```cpp
#include "foo.h"
void bar() { std:: cout << "bar"; }

```

`main.cpp`:

```cpp
#include "foo.h"
int main() {
    foo();
    bar();
}

```

In this program, the function `foo` is defined in the header `foo.h`, which is included twice: once from `foo.cpp` and once from `main.cpp`. Each translation unit therefore contains its own definition of `foo`. Note that the include guards in `foo.h` do not prevent this from happening, since `foo.cpp` and `main.cpp` both **separately** include `foo.h`. The most likely result of trying to build this program is a link-time error identifying `foo` as having been multiply defined.

To avoid such errors, one should **declare** functions in headers and **define** them in the corresponding `.cpp` files, with some exceptions (see other examples).



## Inline functions


A function declared `inline` may be defined in multiple translation units, provided that all definitions are identical. It also must be defined in every translation unit in which it is used. Therefore, inline functions **should** be defined in headers and there is no need to mention them in the implementation file.

The program will behave as though there is a single definition of the function.

`foo.h`:

```cpp
#ifndef FOO_H
#define FOO_H
#include <iostream>
inline void foo() { std::cout << "foo"; }
void bar();
#endif

```

`foo.cpp`:

```cpp
#include "foo.h"
void bar() {
    // more complicated definition
}

```

`main.cpp`:

```cpp
#include "foo.h"
int main() {
    foo();
    bar();
}

```

In this example, the simpler function `foo` is defined inline in the header file while the more complicated function `bar` is not inline and is defined in the implementation file. Both the `foo.cpp` and `main.cpp` translation units contain definitions of `foo`, but this program is well-formed since `foo` is inline.

A function defined within a class definition (which may be a member function or a friend function) is **implicitly** inline. Therefore, if a class is defined in a header, member functions of the class may be defined within the class definition, even though the definitions may be included in multiple translation units:

```cpp
// in foo.h
class Foo {
    void bar() { std::cout << "bar"; }
    void baz();
};

// in foo.cpp
void Foo::baz() {
   // definition
}

```

The function `Foo::baz` is defined out-of-line, so it is **not** an inline function, and must not be defined in the header.



## ODR violation via overload resolution


Even with identical tokens for inline functions, ODR can be violated if lookup of names doesn't refer to the same entity. let's consider `func` in following:

<li>
header.h

```cpp
void overloaded(int);
inline void func() { overloaded('*'); }

```


</li>
<li>
foo.cpp

```cpp
#include "header.h"

void foo()
{
    func(); // `overloaded` refers to `void overloaded(int)`
}

```


</li>
<li>
bar.cpp

```cpp
void overloaded(char); // can come from other include
#include "header.h"

void bar()
{
    func(); // `overloaded` refers to `void overloaded(char)`
}

```


</li>

We have an ODR violation as `overloaded` refers to different entities depending of the translation unit.

