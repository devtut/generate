---
metaTitle: "C++ | Alignment"
description: "Controlling alignment, Querying the alignment of a type"
---

# Alignment


All types in C++ have an alignment. This is a restriction on the memory address that objects of that type can be created within. A memory address is valid for an object's creation if dividing that address by the object's alignment is a whole number.

Type alignments are always a power of two (including 1).



## Controlling alignment


The `alignas` [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords) can be used to force a variable, class data member, declaration or definition of a class, or declaration or definition of an enum, to have a particular alignment, if supported. It comes in two forms:

- `alignas(x)`, where `x` is a constant expression, gives the entity the alignment `x`, if supported.
- `alignas(T)`, where `T` is a type, gives the entity an alignment equal to the alignment requirement of `T`, that is, `alignof(T)`, if supported.

If multiple `alignas` specifiers are applied to the same entity, the strictest one applies.

In this example, the buffer `buf` is guaranteed to be appropriately aligned to hold an `int` object, even though its element type is `unsigned char`, which may have a weaker alignment requirement.

```cpp
alignas(int) unsigned char buf[sizeof(int)];
new (buf) int(42);

```

`alignas` cannot be used to give a type a smaller alignment than the type would have without this declaration:

```cpp
alignas(1) int i; //Il-formed, unless `int` on this platform is aligned to 1 byte.
alignas(char) int j; //Il-formed, unless `int` has the same or smaller alignment than `char`.

```

`alignas`, when given an integer constant expression, must be given a valid alignment. Valid alignments are always powers of two, and must be greater than zero. Compilers are required to support all valid alignments up to the alignment of the type `std::max_align_t`. They **may** support larger alignments than this, but support for allocating memory for such objects is limited. The upper limit on alignments is implementation dependent.

C++17 features direct support in `operator new` for allocating memory for over-aligned types.



## Querying the alignment of a type


The alignment requirement of a type can be queried using the `alignof` [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords) as a unary operator. The result is a constant expression of type `std::size_t`, **i.e.,** it can be evaluated at compile time.

```cpp
#include <iostream>
int main() {
    std::cout << "The alignment requirement of int is: " << alignof(int) << '\n';
}

```

Possible output

> 
The alignment requirement of int is: 4


If applied to an array, it yields the alignment requirement of the element type. If applied to a reference type, it yields the alignment requirement of the referenced type. (References themselves have no alignment, since they are not objects.)



#### Remarks


The standard guarantees the following:

- The alignment requirement of a type is a divisor of its size. For example, a class with size 16 bytes could have an alignment of 1, 2, 4, 8, or 16, but not 32. (If a class's members only total 14 bytes in size, but the class needs to have an alignment requirement of 8, the compiler will insert 2 padding bytes to make the class's size equal to 16.)

- The signed and unsigned versions of an integer type have the same alignment requirement.
- A pointer to `void` has the same alignment requirement as a pointer to `char`.
- The cv-qualified and cv-unqualified versions of a type have the same alignment requirement.

Note that while alignment exists in C++03, it was not until C++11 that it became possible to query alignment (using `alignof`) and control alignment (using `alignas`).

