---
metaTitle: "C++ | Literals"
description: "true, false, nullptr, this, Integer literal"
---

# Literals


Traditionally, a literal is an expression denoting a constant whose type and value are evident from its spelling. For example, `42` is a literal, while `x` is not since one must see its declaration to know its type and read previous lines of code to know its value.

However, C++11 also added [user-defined literals](http://stackoverflow.com/documentation/c%2b%2b/2745/user-defined-literals), which are not literals in the traditional sense but can be used as a shorthand for function calls.



## true


A [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords) denoting one of the two possible values of type `bool`.

```cpp
bool ok = true;
if (!f()) {
    ok = false;
    goto end;
}

```



## false


A [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords) denoting one of the two possible values of type `bool`.

```cpp
bool ok = true;
if (!f()) {
    ok = false;
    goto end;
}

```



## nullptr


A [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords) denoting a null pointer constant. It can be converted to any pointer or pointer-to-member type, yielding a null pointer of the resulting type.

```cpp
Widget* p = new Widget();
delete p;
p = nullptr; // set the pointer to null after deletion

```

Note that `nullptr` is not itself a pointer. The type of `nullptr` is a fundamental type known as `std::nullptr_t`.

```cpp
void f(int* p);

template <class T>
void g(T* p);

void h(std::nullptr_t p);

int main() {
    f(nullptr); // ok
    g(nullptr); // error
    h(nullptr); // ok
}

```



## this


Within a member function of a class, the [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords) `this` is a pointer to the instance of the class on which the function was called. `this` cannot be used in a static member function.

```cpp
struct S {
    int x;
    S& operator=(const S& other) {
        x = other.x;
        // return a reference to the object being assigned to
        return *this;
    }
};

```

The type of `this` depends on the cv-qualification of the member function: if `X::f` is `const`, then the type of `this` within `f` is `const X*`, so `this` cannot be used to modify non-static data members from within a `const` member function. Likewise, `this` inherits `volatile` qualification from the function it appears in.

`this` can also be used in a **brace-or-equal-initializer** for a non-static data member.

```cpp
struct S;
struct T {
    T(const S* s);
    // ...
};
struct S {
    // ...
    T t{this};
};

```

`this` is an rvalue, so it cannot be assigned to.



## Integer literal


An integer literal is a primary expression of the form

- decimal-literal

It is a non-zero decimal digit (1, 2, 3, 4, 5, 6, 7, 8, 9), followed by zero or more decimal digits (0, 1, 2, 3, 4, 5, 6, 7, 8, 9)

`int d = 42;`

- octal-literal

It is the digit zero (0) followed by zero or more octal digits (0, 1, 2, 3, 4, 5, 6, 7)

`int o = 052`

- hex-literal

It is the character sequence 0x or the character sequence 0X followed by one or more hexadecimal digits (0, 1, 2, 3, 4, 5, 6, 7, 8, 9, a, A, b, B, c, C, d, D, e, E, f, F)

`int x = 0x2a;`
`int X = 0X2A;`

- binary-literal     (since C++14)

It is the character sequence 0b or the character sequence 0B followed by one or more binary digits (0, 1)

`int b = 0b101010; // C++14`

Integer-suffix, if provided, may contain one or both of the following (if both are provided, they may appear in any order:

- unsigned-suffix (the character u or the character U)

`unsigned int u_1 = 42u;`

- long-suffix (the character l or the character L) or the long-long-suffix (the character sequence ll or the character sequence LL) (since C++11)

The following variables are also initialized to the same value:

```cpp
unsigned long long l1 = 18446744073709550592ull; // C++11
unsigned long long l2 = 18'446'744'073'709'550'592llu; // C++14
unsigned long long l3 = 1844'6744'0737'0955'0592uLL; // C++14
unsigned long long l4 = 184467'440737'0'95505'92LLU; // C++14

```

**Notes**

Letters in the integer literals are case-insensitive: 0xDeAdBaBeU and 0XdeadBABEu represent the same number (one exception is the long-long-suffix, which is either ll or LL, never lL or Ll)

There are no negative integer literals. Expressions such as -1 apply the unary minus operator to the value represented by the literal, which may involve implicit type conversions.

In C prior to C99 (but not in C++), unsuffixed decimal values that do not fit in long int are allowed to have the type unsigned long int.

When used in a controlling expression of #if or #elif, all signed integer constants act as if they have type std::intmax_t and all unsigned integer constants act as if they have type std::uintmax_t.

