---
metaTitle: "Unspecified behavior"
description: "Value of an out-of-range enum, Evaluation order of function arguments, Result of some reinterpret_cast conversions, Space occupied by a reference, Moved-from state of most standard library classes, Order of initialization of globals across TU, Static cast from bogus void* value, Result of some pointer comparisons"
---

# Unspecified behavior



## Value of an out-of-range enum


If a scoped enum is converted to an integral type that is too small to hold its value, the resulting value is unspecified. Example:

```cpp
enum class E {
    X = 1,
    Y = 1000,
};
// assume 1000 does not fit into a char
char c1 = static_cast<char>(E::X); // c1 is 1
char c2 = static_cast<char>(E::Y); // c2 has an unspecified value

```

Also, if an integer is converted to an enum and the integer's value is outside the range of the enum's values, the resulting value is unspecified. Example:

```cpp
enum Color {
    RED = 1,
    GREEN = 2,
    BLUE = 3,
};
Color c = static_cast<Color>(4);

```

However, in the next example, the behavior is **not** unspecified, since the source value is within the **range** of the enum, although it is unequal to all enumerators:

```cpp
enum Scale {
    ONE = 1,
    TWO = 2,
    FOUR = 4,
};
Scale s = static_cast<Scale>(3);

```

Here `s` will have the value 3, and be unequal to `ONE`, `TWO`, and `FOUR`.



## Evaluation order of function arguments


If a function has multiple arguments, it is unspecified what order they are evaluated in. The following code could print `x = 1, y = 2` or `x = 2, y = 1` but it is unspecified which.

```cpp
int f(int x, int y) {
    printf("x = %d, y = %d\n", x, y);
}
int get_val() {
    static int x = 0;
    return ++x;
}
int main() {
    f(get_val(), get_val());
}

```

In C++17, the order of evaluation of function arguments remains unspecified.

However, each function argument is completely evaluated, and the calling object is guaranteed evaluated before any function arguments are.

```cpp
struct from_int {
  from_int(int x) { std::cout << "from_int (" << x << ")\n"; }
};
int make_int(int x){ std::cout << "make_int (" << x << ")\n"; return x; }


void foo(from_int a, from_int b) {
}
void bar(from_int a, from_int b) {
}

auto which_func(bool b){
  std::cout << b?"foo":"bar" << "\n";
  return b?foo:bar;
}

int main(int argc, char const*const* argv) {
  which_func( true )( make_int(1), make_int(2) );
}

```

this must print:

```cpp
bar
make_int(1)
from_int(1)
make_int(2)
from_int(2)

```

or

```cpp
bar
make_int(2)
from_int(2)
make_int(1)
from_int(1)

```

it may **not** print `bar` after any of the `make` or `from`'s, and it may not print:

```cpp
bar
make_int(2)
make_int(1)
from_int(2)
from_int(1)

```

or similar.  Prior to C++17 printing `bar` after `make_int`s was legal, as was doing both `make_int`s prior to doing any `from_int`s.



## Result of some reinterpret_cast conversions


The result of a `reinterpret_cast` from one function pointer type to another, or one function reference type to another, is unspecified. Example:

```cpp
int f();
auto fp = reinterpret_cast<int(*)(int)>(&f); // fp has unspecified value

```

The result of a `reinterpret_cast` from one object pointer type to another, or one object reference type to another, is unspecified. Example:

```cpp
int x = 42;
char* p = reinterpret_cast<char*>(&x); // p has unspecified value

```

However, with most compilers, this was equivalent to `static_cast<char*>(static_cast<void*>(&x))` so the resulting pointer `p` pointed to the first byte of `x`. This was made the standard behavior in C++11. See [type punning conversion](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/12169/type-punning-conversion) for more details.



## Space occupied by a reference


A reference is not an object, and unlike an object, it is not guaranteed to occupy some contiguous bytes of memory. The standard leaves it unspecified whether a reference requires any storage at all. A number of features of the language conspire to make it impossible to portably examine any storage the reference might occupy:

- If `sizeof` is applied to a reference, it returns the size of the referenced type, thereby giving no information about whether the reference occupies any storage.
- Arrays of references are illegal, so it is not possible to examine the addresses of two consecutive elements of a hypothetical reference of arrays in order to determine the size of a reference.
- If the address of a reference is taken, the result is the address of the referent, so we cannot get a pointer to the reference itself.
- If a class has a reference member, attempting to extract the address of that member using `offsetof` yields undefined behavior since such a class is not a standard-layout class.
- If a class has a reference member, the class is no longer standard layout, so attempts to access any data used to store the reference results in undefined or unspecified behavior.

In practice, in some cases a reference variable may be implemented similarly to a pointer variable and hence occupy the same amount of storage as a pointer, while in other cases a reference may occupy no space at all since it can be optimized out. For example, in:

```cpp
void f() {
    int x;
    int& r = x;
    // do something with r
}

```

the compiler is free to simply treat `r` as an alias for `x` and replace all occurrences of `r` in the rest of the function `f` with `x`, and not allocate any storage to hold `r`.



## Moved-from state of most standard library classes


All standard library containers are left in a **valid but unspecified** state after being moved from. For example, in the following code, `v2` will contain `{1, 2, 3, 4}` after the move, but `v1` is not guaranteed to be empty.

```cpp
int main() {
    std::vector<int> v1{1, 2, 3, 4};
    std::vector<int> v2 = std::move(v1);
}

```

Some classes do have a precisely defined moved-from state. The most important case is that of `std::unique_ptr<T>`, which is guaranteed to be null after being moved from.



## Order of initialization of globals across TU


Whereas inside a Translation Unit, order of initialization of global variables is specified, order of initialization across Translation Units is unspecified.

So program with following files

<li>
foo.cpp

```cpp
#include <iostream>

int dummyFoo = ((std::cout << "foo"), 0);

```


</li>
<li>
bar.cpp

```cpp
#include <iostream>

int dummyBar = ((std::cout << "bar"), 0);

```


</li>
<li>
main.cpp

```cpp
int main() {}

```


</li>

might produce as output:

```cpp
foobar

```

or

```cpp
barfoo

```

That may lead to **Static Initialization Order Fiasco**.



## Static cast from bogus void* value


If a `void*` value is converted to a pointer to object type, `T*`, but is not properly aligned for `T`, the resulting pointer value is unspecified. Example:

```cpp
// Suppose that alignof(int) is 4
int x = 42;
void* p1 = &x;
// Do some pointer arithmetic...
void* p2 = static_cast<char*>(p1) + 2;
int* p3 = static_cast<int*>(p2);

```

The value of `p3` is unspecified because `p2` cannot point to an object of type `int`; its value is not a properly aligned address.



## Result of some pointer comparisons


If two pointers are compared using `<`, `>`, `<=`, or `>=`, the result is unspecified in the following cases:

<li>
The pointers point into different arrays. (A non-array object is considered an array of size 1.)

```cpp
int x;
int y;
const bool b1 = &x < &y;            // unspecified
int a[10];
const bool b2 = &a[0] < &a[1];      // true
const bool b3 = &a[0] < &x;         // unspecified
const bool b4 = (a + 9) < (a + 10); // true
                                    // note: a+10 points past the end of the array

```


</li>
<li>
The pointers point into the same object, but to members with different access control.

```cpp
class A {
  public:
    int x;
    int y;
    bool f1() { return &x < &y; } // true; x comes before y
    bool f2() { return &x < &z; } // unspecified
  private:
    int z;
};

```


</li>



#### Remarks


If the behavior of a construct is unspecified, then the standard places some constraints on the behavior, but leaves some freedom to the implementation, which is **not** required to document what happens in a given situation. It contrasts with [implementation-defined behavior](http://stackoverflow.com/documentation/c%2b%2b/1363/implementation-defined-behavior), in which the implementation **is** required to document what happens, and undefined behavior, in which anything can happen.

