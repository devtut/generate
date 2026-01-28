---
metaTitle: "C++ | Optimization"
description: "Inline Expansion/Inlining, Empty base optimization"
---

# Optimization


When compiling, the compiler will often modify the program to increase performance. This is permitted by the [as-if rule](http://en.cppreference.com/w/cpp/language/as_if), which allows any and all transformations that do not change observable behavior.



## Inline Expansion/Inlining


Inline expansion (also known as inlining) is compiler optimisation that replaces a call to a function with the body of that function. This saves the function call overhead, but at the cost of space, since the function may be duplicated several times.

```cpp
// source:

int process(int value)
{
    return 2 * value;
}

int foo(int a)
{
    return process(a);
}

// program, after inlining:

int foo(int a)
{
    return 2 * a; // the body of process() is copied into foo()
}

```

Inlining is most commonly done for small functions, where the function call overhead is significant compared to the size of the function body.



## Empty base optimization


The size of any object or member subobject is required to be at least 1 even if the type is an empty `class` type (that is, a `class` or `struct` that has no non-static data members), in order to be able to guarantee that the addresses of distinct objects of the same type are always distinct.

However, base `class` subobjects are not so constrained, and can be completely optimized out from the object layout:

```cpp
#include <cassert>

struct Base {}; // empty class

struct Derived1 : Base {
    int i;
};

int main() {
    // the size of any object of empty class type is at least 1
    assert(sizeof(Base) == 1);

    // empty base optimization applies
    assert(sizeof(Derived1) == sizeof(int));
}

```

Empty base optimization is commonly used by allocator-aware standard library classes (`std::vector`, `std::function`, `std::shared_ptr`, etc) to avoid occupying any additional storage for its allocator member if the allocator is stateless. This is achieved by storing one of the required data members (e.g., `begin`, `end`, or `capacity` pointer for the `vector`).

Reference: [cppreference](http://en.cppreference.com/w/cpp/language/ebo)

