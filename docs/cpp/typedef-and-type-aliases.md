---
metaTitle: "Typedef and type aliases"
description: "Basic typedef syntax, More complex uses of typedef, Declaring multiple types with typedef, Alias declaration with using"
---

# Typedef and type aliases


The `typedef` and (since C++11) `using` [keywords](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords) can be used to give a new name to an existing type.



## Basic typedef syntax


A `typedef` declaration has the same syntax as a variable or function declaration, but it contains the word `typedef`. The presence of `typedef` causes the declaration to declare a type instead of a variable or function.

```cpp
int T;         // T has type int
typedef int T; // T is an alias for int

int A[100];         // A has type "array of 100 ints"
typedef int A[100]; // A is an alias for the type "array of 100 ints"

```

Once a type alias has been defined, it can be used interchangeably with the original name of the type.

```cpp
typedef int A[100];
// S is a struct containing an array of 100 ints
struct S {
    A data;
};

```

`typedef` never creates a distinct type. It only gives another way of referring to an existing type.

```cpp
struct S {
    int f(int);
};
typedef int I;
// ok: defines int S::f(int)
I S::f(I x) { return x; }

```



## More complex uses of typedef


The rule that `typedef` declarations have the same syntax as ordinary variable and function declarations can be used to read and write more complex declarations.

```cpp
void (*f)(int);         // f has type "pointer to function of int returning void"
typedef void (*f)(int); // f is an alias for "pointer to function of int returning void"

```

This is especially useful for constructs with confusing syntax, such as pointers to non-static members.

```cpp
void (Foo::*pmf)(int);         // pmf has type "pointer to member function of Foo taking int
                               // and returning void"
typedef void (Foo::*pmf)(int); // pmf is an alias for "pointer to member function of Foo
                               // taking int and returning void"

```

It is hard to remember the syntax of the following function declarations, even for experienced programmers:

```cpp
void (Foo::*Foo::f(const char*))(int);
int (&g())[100];

```

`typedef` can be used to make them easier to read and write:

```cpp
typedef void (Foo::pmf)(int);  // pmf is a pointer to member function type
pmf Foo::f(const char*);       // f is a member function of Foo

typedef int (&ra)[100];        // ra means "reference to array of 100 ints"
ra g();                        // g returns reference to array of 100 ints

```



## Declaring multiple types with typedef


The `typedef` keyword is a specifier, so it applies separately to each declarator. Therefore, each name declared refers to the type that that name would have in the absence of `typedef`.

```cpp
int *x, (*p)();         // x has type int*, and p has type int(*)()
typedef int *x, (*p)(); // x is an alias for int*, while p is an alias for int(*)()

```



## Alias declaration with "using"


The syntax of `using` is very simple: the name to be defined goes on the left hand side, and the definition goes on the right hand side. No need to scan to see where the name is.

```cpp
using I = int;
using A = int[100];             // array of 100 ints
using FP = void(*)(int);        // pointer to function of int returning void
using MP = void (Foo::*)(int);  // pointer to member function of Foo of int returning void

```

Creating a type alias with `using` has exactly the same effect as creating a type alias with `typedef`. It is simply an alternative syntax for accomplishing the same thing.

Unlike `typedef`, `using` can be templated. A "template typedef" created with `using` is called an [alias template](http://stackoverflow.com/documentation/c%2b%2b/460/templates/10144/alias-template).



#### Syntax


- typedef **type-specifier-seq** **init-declarator-list**;
- **attribute-specifier-seq** typedef **decl-specifier-seq** **init-declarator-list**; // since C++11
- using **identifier** **attribute-specifier-seq**(**opt**) = **type-id**; // since C++11

