---
metaTitle: "Using declaration"
description: "Importing names individually from a namespace, Redeclaring members from a base class to avoid name hiding, Inheriting constructors"
---

# Using declaration


A `using` declaration introduces a single name into the current scope that was previously declared elsewhere.



## Importing names individually from a namespace


Once `using` is used to introduce the name `cout` from the namespace `std` into the scope of the `main` function, the `std::cout` object can be referred to as `cout` alone.

```cpp
#include <iostream>
int main() {
    using std::cout;
    cout << "Hello, world!\n";
}

```



## Redeclaring members from a base class to avoid name hiding


If a **using-declaration** occurs at class scope, it is only allowed to redeclare a member of a base class. For example, `using std::cout` is not allowed at class scope.

Often, the name redeclared is one that would otherwise be hidden. For example, in the below code, `d1.foo` only refers to `Derived1::foo(const char*)` and a compilation error will occur. The function `Base::foo(int)` is hidden not considered at all. However, `d2.foo(42)` is fine because the **using-declaration** brings `Base::foo(int)` into the set of entities named `foo` in `Derived2`. Name lookup then finds both `foo`s and overload resolution selects `Base::foo`.

```cpp
struct Base {
    void foo(int);
};
struct Derived1 : Base {
    void foo(const char*);
};
struct Derived2 : Base {
    using Base::foo;
    void foo(const char*);
};
int main() {
    Derived1 d1;
    d1.foo(42);  // error
    Derived2 d2;
    d2.foo(42);  // OK
}

```



## Inheriting constructors


As a special case, a **using-declaration** at class scope can refer to the constructors of a direct base class. Those constructors are then **inherited** by the derived class and can be used to initialize the derived class.

```cpp
struct Base {
    Base(int x, const char* s);
};
struct Derived1 : Base {
    Derived1(int x, const char* s) : Base(x, s) {}
};
struct Derived2 : Base {
    using Base::Base;
};
int main() {
    Derived1 d1(42, "Hello, world");
    Derived2 d2(42, "Hello, world");
}

```

In the above code, both `Derived1` and `Derived2` have constructors that forward the arguments directly to the corresponding constructor of `Base`. `Derived1` performs the forwarding explicitly, while `Derived2`, using the C++11 feature of inheriting constructors, does so implicitly.



#### Syntax


- using typename(**opt**) **nested-name-specifier** **unqualified-id**;
- using :: **unqualified-id**;



#### Remarks


A **using-declaration** is distinct from a [using directive](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces/1624/using-directive), which tells the compiler to look in a particular namespace when looking up **any** name. A **using-directive** begins with `using namespace`.

A **using-declaration** is also distinct from an alias declaration, which gives a new name to an existing type in the same manner as [`typedef`](http://stackoverflow.com/documentation/c%2b%2b/7840/variable-declaration-keywords/18785/typedef). An alias declaration contains an equals sign.

