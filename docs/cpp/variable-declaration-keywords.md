---
metaTitle: "Variable Declaration Keywords"
description: "decltype, const, signed, unsigned, volatile"
---

# Variable Declaration Keywords




## decltype


Yields the type of its operand, which is not evaluated.

<li>
If the operand `e` is a name without any additional parentheses, then `decltype(e)` is the **declared type** of `e`.

```cpp
int x = 42;
std::vector<decltype(x)> v(100, x); // v is a vector<int>

```


</li>
<li>
If the operand `e` is a class member access without any additional parentheses, then `decltype(e)` is the **declared type** of the member accessed.

```cpp
struct S {
    int x = 42;
};
const S s;
decltype(s.x) y; // y has type int, even though s.x is const

```


</li>
<li>
In all other cases, `decltype(e)` yields both the type and the [value category](http://stackoverflow.com/documentation/c%2b%2b/763/value-categories) of the expression `e`, as follows:
<ul>
- If `e` is an lvalue of type `T`, then `decltype(e)` is `T&`.
- If `e` is an xvalue of type `T`, then `decltype(e)` is `T&&`.
- If `e` is a prvalue of type `T`, then `decltype(e)` is `T`.

This includes the case with extraneous parentheses.

```cpp
int f() { return 42; }
int& g() { static int x = 42; return x; }
int x = 42;
decltype(f()) a = f(); // a has type int
decltype(g()) b = g(); // b has type int&
decltype((x)) c = x;   // c has type int&, since x is an lvalue

```

The special form `decltype(auto)` deduces the type of a variable from its initializer or the return type of a function from the `return` statements in its definition, using the type deduction rules of `decltype` rather than those of `auto`.

```cpp
const int x = 123;
auto y = x;           // y has type int
decltype(auto) z = x; // z has type const int, the declared type of x

```



## const


A type specifier; when applied to a type, produces the const-qualified version of the type. See [const keyword](http://stackoverflow.com/documentation/c%2b%2b/2386/const-keyword) for details on the meaning of `const`.

```cpp
const int x = 123;
x = 456;    // error
int& r = x; // error

struct S {
    void f();
    void g() const;
};
const S s;
s.f(); // error
s.g(); // OK

```



## signed


A keyword that is part of certain integer type names.

- When used alone, `int` is implied, so that `signed`, `signed int`, and `int` are the same type.
- When combined with `char`, yields the type `signed char`, which is a different type from `char`, even if `char` is also signed. `signed char` has a range that includes at least -127 to +127, inclusive.
- When combined with `short`, `long`, or `long long`, it is redundant, since those types are already signed.
- `signed` cannot be combined with `bool`, `wchar_t`, `char16_t`, or `char32_t`.

Example:

```cpp
signed char celsius_temperature;
std::cin >> celsius_temperature;
if (celsius_temperature < -35) {
    std::cout << "cold day, eh?\n";
}

```



## unsigned


A type specifier that requests the unsigned version of an integer type.

- When used alone, `int` is implied, so `unsigned` is the same type as `unsigned int`.
- The type `unsigned char` is different from the type `char`, even if `char` is unsigned. It can hold integers up to at least 255.
- `unsigned` can also be combined with `short`, `long`, or `long long`. It cannot be combined with `bool`, `wchar_t`, `char16_t`, or `char32_t`.

Example:

```cpp
char invert_case_table[256] = { ..., 'a', 'b', 'c', ..., 'A', 'B', 'C', ... };
char invert_case(char c) {
    unsigned char index = c;
    return invert_case_table[index];
    // note: returning invert_case_table[c] directly does the
    // wrong thing on implementations where char is a signed type
}

```



## volatile


A type qualifier; when applied to a type, produces the volatile-qualified version of the type. Volatile qualification plays the same role as `const` qualification in the type system, but `volatile` does not prevent objects from being modified; instead, it forces the compiler to treat all accesses to such objects as side effects.

In the example below, if `memory_mapped_port` were not volatile, the compiler could optimize the function so that it performs only the final write, which would be incorrect if `sizeof(int)` is greater than 1. The `volatile` qualification forces it to treat all `sizeof(int)` writes as different side effects and hence perform all of them (in order).

```cpp
extern volatile char memory_mapped_port;
void write_to_device(int x) {
    const char* p = reinterpret_cast<const char*>(&x);
    for (int i = 0; i < sizeof(int); i++) {
        memory_mapped_port = p[i];
    }
}

```

