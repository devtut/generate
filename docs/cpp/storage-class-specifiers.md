---
metaTitle: "Storage class specifiers"
description: "register, extern, mutable, static, auto"
---

# Storage class specifiers


Storage class specifiers are [keywords](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords) that can be used in declarations. They do not affect the type of the declaration, but typically modify the way in which the entity is stored.



## register


A storage class specifier that hints to the compiler that a variable will be heavily used. The word "register" is related to the fact that a compiler might choose to store such a variable in a CPU register so that it can be accessed in fewer clock cycles. It was deprecated starting in C++11.

```cpp
register int i = 0;
while (i < 100) {
    f(i);
    int g = i*i;
    i += h(i, g);
}

```

Both local variables and function parameters may be declared `register`. Unlike C, C++ does not place any restrictions on what can be done with a `register` variable. For example, it is valid to take the address of a `register` variable, but this may prevent the compiler from actually storing such a variable in a register.

The keyword `register` is unused and reserved. A program that uses the keyword `register` is ill-formed.



## extern


The `extern` storage class specifier can modify a declaration in one of the three following ways, depending on context:

<li>
It can be used to declare a variable without defining it. Typically, this is used in a header file for a variable that will be defined in a separate implementation file.

```cpp
// global scope
int x;             // definition; x will be default-initialized
extern int y;      // declaration; y is defined elsewhere, most likely another TU
extern int z = 42; // definition; "extern" has no effect here (compiler may warn)

```


</li>
<li>
It gives external linkage to a variable at namespace scope even if `const` or `constexpr` would have otherwise caused it to have internal linkage.

```cpp
// global scope
const int w = 42;            // internal linkage in C++; external linkage in C
static const int x = 42;     // internal linkage in both C++ and C
extern const int y = 42;     // external linkage in both C++ and C
namespace {
    extern const int z = 42; // however, this has internal linkage since
                             // it's in an unnamed namespace
}

```


</li>
<li>
It redeclares a variable at block scope if it was previously declared with linkage. Otherwise, it declares a new variable with linkage, which is a member of the nearest enclosing namespace.

```cpp
// global scope
namespace {
    int x = 1;
    struct C {
        int x = 2;
        void f() {
            extern int x;           // redeclares namespace-scope x
            std::cout << x << '\n'; // therefore, this prints 1, not 2
        }
    };
}
void g() {
    extern int y; // y has external linkage; refers to global y defined elsewhere
}

```


</li>

A function can also be declared `extern`, but this has no effect. It is usually used as a hint to the reader that a function declared here is defined in another translation unit. For example:

```

void f();        // typically a forward declaration; f defined later in this TU
 extern void g(); // typically not a forward declaration; g defined in another TU

```

In the above code, if `f` were changed to `extern` and `g` to non-`extern`, it would not affect the correctness or semantics of the program at all, but would likely confuse the reader of the code.



## mutable


A specifier that can be applied to the declaration of a non-static, non-reference data member of a class. A mutable member of a class is not `const` even when the object is `const`.

```cpp
class C {
    int x;
    mutable int times_accessed;
  public:
    C(): x(0), times_accessed(0) {
    }
    int get_x() const {
        ++times_accessed; // ok: const member function can modify mutable data member
        return x;
    }
    void set_x(int x) {
        ++times_accessed;
        this->x = x;
    }
};

```

A second meaning for `mutable` was added in C++11. When it follows the parameter list of a lambda, it suppresses the implicit `const` on the lambda's function call operator. Therefore, a mutable lambda can modify the values of entities captured by copy. See [mutable lambdas](http://stackoverflow.com/documentation/c%2b%2b/2705/mutable-keyword/9059/mutable-lambdas) for more details.

```cpp
std::vector<int> my_iota(int start, int count) {
    std::vector<int> result(count);
    std::generate(result.begin(), result.end(),
                  [start]() mutable { return start++; });
    return result;
}

```

Note that `mutable` is **not** a storage class specifier when used this way to form a mutable lambda.



## static


The `static` storage class specifier has three different meanings.

<li>
Gives internal linkage to a variable or function declared at namespace scope.

```cpp
// internal function; can't be linked to
static double semiperimeter(double a, double b, double c) {
    return (a + b + c)/2.0;
}
// exported to client
double area(double a, double b, double c) {
    const double s = semiperimeter(a, b, c);
    return sqrt(s*(s-a)*(s-b)*(s-c));
}

```


</li>
<li>
Declares a variable to have static storage duration (unless it is `thread_local`). Namespace-scope variables are implicitly static. A static local variable is initialized only once, the first time control passes through its definition, and is not destroyed every time its scope is exited.

```cpp
void f() {
    static int count = 0;
    std::cout << "f has been called " << ++count << " times so far\n";
}

```


</li>
<li>
When applied to the declaration of a class member, declares that member to be a [static member](http://stackoverflow.com/documentation/c%2b%2b/508/classes-structures/15150/static-class-members).

```cpp
struct S {
    static S* create() {
        return new S;
    }
};
int main() {
    S* s = S::create();
}

```


</li>

Note that in the case of a static data member of a class, both 2 and 3 apply simultaneously: the `static` keyword both makes the member into a static data member and makes it into a variable with static storage duration.



## auto


Declares a variable to have automatic storage duration. It is redundant, since automatic storage duration is already the default at block scope, and the auto specifier is not allowed at namespace scope.

```cpp
void f() {
    auto int x; // equivalent to: int x;
    auto y;     // illegal in C++; legal in C89
}
auto int z;     // illegal: namespace-scope variable cannot be automatic

```

In C++11, `auto` changed meaning completely, and is no longer a storage class specifier, but is instead used for [type deduction](http://stackoverflow.com/documentation/c%2b%2b/7863/type-deduction/25567/auto-type-deduction).



#### Remarks


There are six storage class specifiers, although not all in the same version of the language: `auto` (until C++11), `register` (until C++17), `static`, [`thread_local`](http://stackoverflow.com/documentation/c%2b%2b) (since C++11), `extern`, and `mutable`.

According to the standard,

> 
At most one **storage-class-specifier** shall appear in a given **decl-specifier-seq,** except that `thread_local` may appear with `static` or `extern`.


A declaration may contain no storage class specifier. In that case, the language specifies a default behaviour. For example, by default, a variable declared at block scope implicitly has automatic storage duration.

