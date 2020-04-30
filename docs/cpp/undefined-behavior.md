---
metaTitle: "Undefined Behavior"
description: "Reading or writing through a null pointer, Using an uninitialized local variable, Accessing an out-of-bounds index, Deleting a derived object via a pointer to a base class that doesn't have a virtual destructor., Extending the `std` or `posix` Namespace, No return statement for a function with a non-void return type, Integer division by zero, Accessing a dangling reference, Invalid pointer arithmetic, Signed Integer Overflow, Incorrect pairing of memory allocation and deallocation, Shifting by an invalid number of positions, Multiple non-identical definitions (the One Definition Rule), Modifying a const object, Modifying a string literal, Accessing an object as the wrong type, Overflow during conversion to or from floating point type, Returning from a [[noreturn]] function, Infinite template recursion, Floating point overflow, Calling (Pure) Virtual Members From Constructor Or Destructor, Invalid base-to-derived static cast, Function call through mismatched function pointer type, Access to nonexistent member through pointer to member, Invalid derived-to-base conversion for pointers to members, Destroying an object that has already been destroyed"
---

# Undefined Behavior


What is undefined behavior (UB)? According to the ISO C++ Standard (§1.3.24, N4296), it is "behavior for which this International Standard imposes no requirements."

This means that when a program encounters UB, it is allowed to do whatever it wants. This often means a crash, but it may simply do nothing, [make demons fly out of your nose](http://catb.org/jargon/html/N/nasal-demons.html), or even **appear** to work properly!

Needless to say, you should avoid writing code that invokes UB.



## Reading or writing through a null pointer


```cpp
int *ptr = nullptr;
*ptr = 1; // Undefined behavior

```

This is **undefined behavior**, because a null pointer does not point to any valid object, so there is no object at `*ptr` to write to.

Although this most often causes a segmentation fault, it is undefined and anything can happen.



## Using an uninitialized local variable


```cpp
int a;
std::cout << a; // Undefined behavior!

```

This results in **undefined behavior**, because `a` is uninitialised.

It is often, incorrectly, claimed that this is because the value is "indeterminate", or "whatever value was in that memory location before".  However, it is the act of accessing the value of `a` in the above example that gives undefined behaviour.  In practice, printing a "garbage value" is a common symptom in this case, but that is only one possible form of undefined behaviour.

Although highly unlikely in practice (since it is reliant on specific hardware support) the compiler could equally well electrocute the programmer when compiling the code sample above.   With such a compiler and hardware support, such a response to undefined behaviour would markedly increase average (living) programmer understanding of the true meaning of undefined behaviour - which is that the standard places no constraint on the resultant behaviour.

Using an indeterminate value of `unsigned char` type does not produce undefined behavior if the value is used as:

- the second or third operand of the ternary conditional operator;
- the right operand of the built-in comma operator;
- the operand of a conversion to `unsigned char`;
- the right operand of the assignment operator, if the left operand is also of type `unsigned char`;
- the initializer for an `unsigned char` object;

or if the value is discarded. In such cases, the indeterminate value simply propagates to the result of the expression, if applicable.

Note that a `static` variable is **always** zero-initialized (if possible):

```cpp
static int a;
std::cout << a; // Defined behavior, 'a' is 0

```



## Accessing an out-of-bounds index


It is **undefined behavior** to access an index that is out of bounds for an array (or standard library container for that matter, as they are all implemented using a **raw** array):

```

int array[] = {1, 2, 3, 4, 5};
 array[5] = 0;  // Undefined behavior

```

It is **allowed** to have a pointer pointing to the end of the array (in this case `array + 5`), you just can't dereference it, as it is not a valid element.

```

const int *end = array + 5;  // Pointer to one past the last index
 for (int *p = array; p != end; ++p)
   // Do something with `p`

```

In general, you're not allowed to create an out-of-bounds pointer. A pointer must point to an element within the array, or one past the end.



## Deleting a derived object via a pointer to a base class that doesn't have a virtual destructor.


```cpp
class base { };
class derived: public base { }; 

int main() {
    base* p = new derived();
    delete p; // The is undefined behavior!
}

```

In section [expr.delete] §5.3.5/3 the standard says that if `delete` is called on an object whose static type does not have a `virtual` destructor:

> 
<p>if the static type of the object to be deleted is different from its
dynamic type, the static type shall be a base class of the dynamic type of the object to be deleted and the
static type shall have a virtual destructor or the behavior is undefined.</p>


This is the case regardless of the question whether the derived class added any data members to the base class.



## Extending the `std` or `posix` Namespace


[The standard (17.6.4.2.1/1)](https://isocpp.org/files/papers/N3690.pdf) generally forbids extending the `std` namespace:

> 
The behavior of a C++ program is undefined if it adds declarations or definitions to namespace std or to a namespace within namespace std unless otherwise specified.


The same goes for `posix` (17.6.4.2.2/1):

> 
The behavior of a C++ program is undefined if it adds declarations or definitions to namespace posix or to a namespace within namespace posix unless otherwise specified.


Consider the following:

```cpp
#include <algorithm>

namespace std
{
    int foo(){}
}

```

Nothing in the standard forbids `algorithm` (or one of the headers it includes) defining the same definition, and so this code would violate the [One Definition Rule](https://en.wikipedia.org/wiki/One_Definition_Rule).

So, in general, this is forbidden. There are [specific exceptions allowed](http://en.cppreference.com/w/cpp/language/extending_std), though. Perhaps most usefully, it is allowed to add specializations for user defined types. So, for example, suppose your code has

```cpp
class foo
{
    // Stuff
};

```

Then the following is fine

```cpp
namespace std
{
    template<>
    struct hash<foo>
    {
    public:
        size_t operator()(const foo &f) const;
    };
}

```



## No return statement for a function with a non-void return type


Omitting the `return` statement in a function which is has a return type that is not `void` is **undefined behavior**.

```cpp
int function() {  
    // Missing return statement
} 

int main() {
    function(); //Undefined Behavior
}

```

Most modern day compilers emit a warning at compile time for this kind of undefined behavior.

**Note:** `main` is the only exception to the rule. If `main` doesn't have a `return` statement, the compiler automatically inserts `return 0;` for you, so it can be safely left out.



## Integer division by zero


```cpp
int x = 5 / 0;    // Undefined behavior

```

Division by `0` is mathematically undefined, and as such it makes sense that this is undefined behavior.

However:

```cpp
float x = 5.0f / 0.0f;   // x is +infinity

```

Most implementation implement IEEE-754, which defines floating point division by zero to return `NaN` (if numerator is `0.0f`), `infinity` (if numerator is positive) or `-infinity` (if numerator is negative).



## Accessing a dangling reference


It is illegal to access a reference to an object that has gone out of scope or been otherwise destroyed. Such a reference is said to be **dangling** since it no longer refers to a valid object.

```cpp
#include <iostream>
int& getX() {
    int x = 42;
    return x;
}
int main() {
    int& r = getX();
    std::cout << r << "\n";
}

```

In this example, the local variable `x` goes out of scope when `getX` returns. (Note that **lifetime extension** cannot extend the lifetime of a local variable past the scope of the block in which it is defined.) Therefore `r` is a dangling reference. This program has undefined behavior, although it may appear to work and print `42` in some cases.



## Invalid pointer arithmetic


The following uses of pointer arithmetic cause undefined behavior:

<li>
Addition or subtraction of an integer, if the result does not belong to the same array object as the pointer operand. (Here, the element one past the end is considered to still belong to the array.)

```cpp
int a[10];
int* p1 = &a[5];
int* p2 = p1 + 4; // ok; p2 points to a[9]
int* p3 = p1 + 5; // ok; p2 points to one past the end of a
int* p4 = p1 + 6; // UB
int* p5 = p1 - 5; // ok; p2 points to a[0]
int* p6 = p1 - 6; // UB
int* p7 = p3 - 5; // ok; p7 points to a[5]

```


</li>
<li>
Subtraction of two pointers if they do not both belong to the same array object. (Again, the element one past the end is considered to belong to the array.) The exception is that two null pointers may be subtracted, yielding 0.

```cpp
int a[10];
int b[10];
int *p1 = &a[8], *p2 = &a[3];
int d1 = p1 - p2; // yields 5
int *p3 = p1 + 2; // ok; p3 points to one past the end of a
int d2 = p3 - p2; // yields 7
int *p4 = &b[0];
int d3 = p4 - p1; // UB

```


</li>
<li>
Subtraction of two pointers if the result overflows `std::ptrdiff_t`.
</li>
<li>
<p>Any pointer arithmetic where either operand's pointee type does not match the dynamic type of the object pointed to (ignoring cv-qualification). According to the standard, "[in] particular, a pointer to a base
class cannot be used for pointer arithmetic when the array contains objects of a derived class type."</p>

```cpp
struct Base { int x; };
struct Derived : Base { int y; };
Derived a[10];
Base* p1 = &a[1];           // ok
Base* p2 = p1 + 1;          // UB; p1 points to Derived
Base* p3 = p1 - 1;          // likewise
Base* p4 = &a[2];           // ok
auto p5 = p4 - p1;          // UB; p4 and p1 point to Derived
const Derived* p6 = &a[1];
const Derived* p7 = p6 + 1; // ok; cv-qualifiers don't matter

```


</li>



## Signed Integer Overflow


```cpp
int x = INT_MAX + 1;

// x can be anything -> Undefined behavior

```

> 
If during the evaluation of an expression, the result is not mathematically defined or not in the range of representable values for its type, the behavior is undefined.


<sup>(C++11 Standard paragraph 5/4)</sup>

This is one of the more nasty ones, as it usually yields reproducible, non-crashing behavior so developers may be tempted to rely heavily on the observed behavior.

On the other hand:

```cpp
unsigned int x = UINT_MAX + 1;

// x is 0

```

is well defined since:

> 
Unsigned integers, declared unsigned, shall obey the laws of arithmetic modulo `2^n` where `n` is the number of bits in the value representation of that particular size of integer.


<sup>(C++11 Standard paragraph 3.9.1/4)</sup>

Sometimes compilers may exploit an undefined behavior and optimize

```cpp
signed int x ;
if(x > x + 1)
{
    //do something
}

```

Here since a signed integer overflow is not defined, compiler is free to assume
that it may never happen and hence it can optimize away the "if" block



## Incorrect pairing of memory allocation and deallocation


An object can only be deallocated by `delete` if it was allocated by `new` and is not an array. If the argument to `delete` was not returned by `new` or is an array, the behavior is undefined.

An object can only be deallocated by `delete[]` if it was allocated by `new` and is an array. If the argument to `delete[]` was not returned by `new` or is not an array, the behavior is undefined.

If the argument to `free` was not returned by `malloc`, the behavior is undefined.

```cpp
int* p1 = new int;
delete p1;      // correct
// delete[] p1; // undefined
// free(p1);    // undefined

int* p2 = new int[10];
delete[] p2;    // correct
// delete p2;   // undefined
// free(p2);    // undefined

int* p3 = static_cast<int*>(malloc(sizeof(int)));
free(p3);       // correct
// delete p3;   // undefined
// delete[] p3; // undefined

```

Such issues can be avoided by completely avoiding `malloc` and `free` in C++ programs, preferring the standard library smart pointers over raw `new` and `delete`, and preferring `std::vector` and `std::string` over raw `new` and `delete[]`.



## Shifting by an invalid number of positions


For the built-in shift operator, the right operand must be nonnegative and strictly less than the bit width of the promoted left operand. Otherwise, the behavior is undefined.

```cpp
const int a = 42;
const int b = a << -1; // UB
const int c = a << 0;  // ok
const int d = a << 32; // UB if int is 32 bits or less
const int e = a >> 32; // also UB if int is 32 bits or less
const signed char f = 'x';
const int g = f << 10; // ok even if signed char is 10 bits or less;
                       // int must be at least 16 bits

```



## Multiple non-identical definitions (the One Definition Rule)


If a class, enum, inline function, template, or member of a template has external linkage and is defined in multiple translation units, all definitions must be identical or the behavior is undefined according to the [One Definition Rule (ODR)](https://en.wikipedia.org/wiki/One_Definition_Rule).

`foo.h`:

```cpp
class Foo {
  public:
    double x;
  private:
    int y;
};

Foo get_foo();

```

`foo.cpp`:

```cpp
#include "foo.h"
Foo get_foo() { /* implementation */ }

```

`main.cpp`:

```cpp
// I want access to the private member, so I am going to replace Foo with my own type
class Foo {
  public:
    double x;
    int y;
};
Foo get_foo(); // declare this function ourselves since we aren't including foo.h
int main() {
    Foo foo = get_foo();
    // do something with foo.y
}

```

The above program exhibits undefined behavior because it contains two definitions of the class `::Foo`, which has external linkage, in different translation units, but the two definitions are not identical. Unlike redefinition of a class within the **same** translation unit, this problem is not required to be diagnosed by the compiler.



## Modifying a const object


Any attempt to modify a `const` object results in undefined behavior. This applies to `const` variables, members of `const` objects, and class members declared `const`. (However, a `mutable` member of a `const` object is not `const`.)

Such an attempt can be made through `const_cast`:

```cpp
const int x = 123;
const_cast<int&>(x) = 456;
std::cout << x << '\n';

```

A compiler will usually inline the value of a `const int` object, so it's possible that this code compiles and prints `123`. Compilers can also place `const` objects' values in read-only memory, so a segmentation fault may occur. In any case, the behavior is undefined and the program might do anything.

The following program conceals a far more subtle error:

```cpp
#include <iostream>

class Foo* instance;

class Foo {
  public:
    int get_x() const { return m_x; }
    void set_x(int x) { m_x = x; }
  private:
    Foo(int x, Foo*& this_ref): m_x(x) {
        this_ref = this;
    }
    int m_x;
    friend const Foo& getFoo();
};

const Foo& getFoo() {
    static const Foo foo(123, instance);
    return foo;
}

void do_evil(int x) {
    instance->set_x(x);
}

int main() {
    const Foo& foo = getFoo();
    do_evil(456);
    std::cout << foo.get_x() << '\n';
}

```

In this code, `getFoo` creates a singleton of type `const Foo` and its member `m_x` is initialized to `123`. Then `do_evil` is called and the value of `foo.m_x` is apparently changed to 456. What went wrong?

Despite its name, `do_evil` does nothing particularly evil; all it does is call a setter through a `Foo*`. But that pointer points to a `const Foo` object even though `const_cast` was not used. This pointer was obtained through `Foo`'s constructor. A `const` object does not become `const` until its initialization is complete, so `this` has type `Foo*`, not `const Foo*`, within the constructor.

Therefore, undefined behavior occurs even though there are no obviously dangerous constructs in this program.



## Modifying a string literal


```cpp
char *str = "hello world";
str[0] = 'H';

```

`"hello world"` is a string literal, so modifying it gives undefined behaviour.

The initialisation of `str` in the above example was formally deprecated (scheduled for removal from a future version of the standard) in C++03.  A number of compilers before 2003 might issue a warning about this (e.g. a suspicious conversion).  After 2003, compilers typically warn about a deprecated conversion.

The above example is illegal, and results in a compiler diagnostic, in C++11 and later.   A similar example may be constructed to exhibit undefined behaviour by explicitly permitting the type conversion, such as:

```cpp
char *str = const_cast<char *>("hello world");
str[0] = 'H'; 

```



## Accessing an object as the wrong type


In most cases, it is illegal to access an object of one type as though it were a different type (disregarding cv-qualifiers). Example:

```cpp
float x = 42;
int y = reinterpret_cast<int&>(x);

```

The result is undefined behavior.

There are some exceptions to  this **strict aliasing** rule:

- An object of class type can be accessed as though it were of a type that is a base class of the actual class type.
- Any type can be accessed as a `char` or `unsigned char`, but the reverse is not true: a char array cannot be accessed as though it were an arbitrary type.
- A signed integer type can be accessed as the corresponding unsigned type and **vice versa**.

A related rule is that if a non-static member function is called on an object that does not actually have the same type as the defining class of the function, or a derived class, then undefined behavior occurs. This is true even if the function does not access the object.

```cpp
struct Base {
};
struct Derived : Base {
    void f() {}
};
struct Unrelated {};
Unrelated u;
Derived& r1 = reinterpret_cast<Derived&>(u); // ok
r1.f();                                      // UB
Base b;
Derived& r2 = reinterpret_cast<Derived&>(b); // ok
r2.f();                                      // UB

```



## Overflow during conversion to or from floating point type


If, during the conversion of:

- an integer type to a floating point type,
- a floating point type to an integer type, or
- a floating point type to a shorter floating point type,

the source value is outside the range of values that can be represented in the destination type, the result is undefined behavior. Example:

```cpp
double x = 1e100;
int y = x; // int probably cannot hold numbers that large, so this is UB

```



## Returning from a [[noreturn]] function


Example from the Standard, [dcl.attr.noreturn]:

```cpp
[[ noreturn ]] void f() {
  throw "error"; // OK
}
[[ noreturn ]] void q(int i) { // behavior is undefined if called with an argument <= 0
  if (i > 0)
    throw "positive";
}

```



## Infinite template recursion


Example from the Standard, [temp.inst]/17:

```cpp
template<class T> class X {
    X<T>* p; // OK
    X<T*> a; // implicit generation of X<T> requires
             // the implicit instantiation of X<T*> which requires
             // the implicit instantiation of X<T**> which ...
};

```



## Floating point overflow


If an arithmetic operation that yields a floating point type produces a value that is not in the range of representable values of the result type, the behavior is undefined according to the C++ standard, but may be defined by other standards the machine might conform to, such as IEEE 754.

```cpp
float x = 1.0;
for (int i = 0; i < 10000; i++) {
    x *= 10.0; // will probably overflow eventually; undefined behavior
}

```



## Calling (Pure) Virtual Members From Constructor Or Destructor


[The Standard (10.4)](http://www.open-std.org/jtc1/sc22/wg21/docs/papers/2015/n4527.pdf) states:

> 
Member functions can be called from a constructor (or destructor) of an abstract class; the effect of making a virtual call (10.3) to a pure virtual function directly or indirectly for the object being created (or destroyed) from such a constructor (or destructor) is undefined.


More generally, some C++ authorities, e.g. Scott Meyers, [suggest](http://www.artima.com/cppsource/nevercall.html) never calling virtual functions (even non-pure ones) from constructors and dstructors.

Consider the following example, modified from the above link:

```cpp
class transaction
{
public:
    transaction(){ log_it(); }
    virtual void log_it() const = 0;
};

class sell_transaction : public transaction
{
public:
    virtual void log_it() const { /* Do something */ }
};

```

Suppose we create a `sell_transaction` object:

```cpp
sell_transaction s;

```

This implicitly calls the constructor of `sell_transaction`, which first calls the constructor of `transaction`. When the constructor of `transaction` is called though, the object is not yet of the type `sell_transaction`, but rather only of the type `transaction`.

Consequently, the call in `transaction::transaction()` to `log_it`, won't do what might seem to be the intuitive thing - namely call `sell_transaction::log_it`.

<li>
If `log_it` is pure virtual, as in this example, the behaviour is undefined.
</li>
<li>
If `log_it` is non-pure virtual, `transaction::log_it` will be called.
</li>



## Invalid base-to-derived static cast


If `static_cast` is used to convert a pointer (resp. reference) to base class to a pointer (resp. reference) to derived class, but the operand does not point (resp. refer) to an object of the derived class type, the behavior is undefined. See [Base to derived conversion](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/10518/base-to-derived-conversion#t=201608021020103102096).



## Function call through mismatched function pointer type


In order to call a function through a function pointer, the function pointer's type must exactly match the function's type. Otherwise, the behaviour is undefined. Example:

```cpp
int f();
void (*p)() = reinterpret_cast<void(*)()>(f);
p(); // undefined

```



## Access to nonexistent member through pointer to member


When accessing a non-static member of an object through a pointer to member, if the object does not actually contain the member denoted by the pointer, the behavior is undefined. (Such a pointer to member can be obtained through `static_cast`.)

```cpp
struct Base { int x; };
struct Derived : Base { int y; };
int Derived::*pdy = &Derived::y;
int Base::*pby = static_cast<int Base::*>(pdy);

Base* b1 = new Derived;
b1->*pby = 42; // ok; sets y in Derived object to 42
Base* b2 = new Base;
b2->*pby = 42; // undefined; there is no y member in Base

```



## Invalid derived-to-base conversion for pointers to members


When `static_cast` is used to convert `T D::*` to `T B::*`, the member pointed to must belong to a class that is a base class or derived class of `B`. Otherwise the behavior is undefined.     See [Derived to base conversion for pointers to members](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/18752/derived-to-base-conversion-for-pointers-to-members)



## Destroying an object that has already been destroyed


In this example, a destructor is explicitly invoked for an object that will later be automatically destroyed.

```cpp
struct S {
    ~S() { std::cout << "destroying S\n"; }
};
int main() {
    S s;
    s.~S();
} // UB: s destroyed a second time here

```

A similar issue occurs when a `std::unique_ptr<T>` is made to point at a `T` with automatic or static storage duration.

```cpp
void f(std::unique_ptr<S> p);
int main() {
    S s;
    std::unique_ptr<S> p(&s);
    f(std::move(p)); // s destroyed upon return from f
}                    // UB: s destroyed

```

Another way to destroy an object twice is by having two `shared_ptr`s both manage the object without sharing ownership with each other.

```cpp
void f(std::shared_ptr<S> p1, std::shared_ptr<S> p2);
int main() {
    S* p = new S;
    // I want to pass the same object twice...
    std::shared_ptr<S> sp1(p);
    std::shared_ptr<S> sp2(p);
    f(sp1, sp2);
} // UB: both sp1 and sp2 will destroy s separately
// NB: this is correct:
// std::shared_ptr<S> sp(p);
// f(sp, sp);

```



#### Remarks


If a program contains undefined behavior, the C++ standard places no constraints on its behavior.

- It may appear to work as the developer intended, but it may also crash or produce strange results.
- The behavior may vary between runs of the same program.
- Any part of the program may malfunction, including lines that come before the line that contains undefined behavior.
- The implementation is not required to document the result of undefined behavior.

An implementation **may** document the result of an operation that produces undefined behavior according to the standard, but a program that depends on such documented behavior is not portable.

**Why undefined behavior exists**

Intuitively, undefined behavior is considered a bad thing as such errors can't be handled graciously through, say, exception handlers.

But leaving some behavior undefined is actually an integral part of C++'s promise "you don't pay for what you don't use".
Undefined behavior allows a compiler to assume the developer knows what he's doing and not introduce code to check for the mistakes highlighted in the above examples.

**Finding and avoiding undefined behavior**

Some tools can be used to discover undefined behavior during development:

- Most compilers have warning flags to warn about some cases of undefined behavior at compile time.
- Newer versions of gcc and clang include a so-called "Undefined Behavior Sanitizer" flag (`-fsanitize=undefined`) that will check for undefined behavior at runtime, at a performance cost.
- `lint`-like tools may perform more thorough undefined behavior analysis.

**Undefined, unspecified and [implementation-defined](http://stackoverflow.com/documentation/c%2B%2B/1363/implementation-defined-behavior) behavior**

From C++14 standard (ISO/IEC 14882:2014) section 1.9 (Program Execution):

> 
<ol>
<li>
The semantic descriptions in this International Standard define a parameterized nondeterministic abstract machine. [CUT]
</li>
<li>
Certain aspects and operations of the abstract machine are described in this International Standard as **implementation-defined** (for example, `sizeof(int)`). These constitute **the parameters of the abstract machine**. Each implementation shall include documentation describing its characteristics and behavior in these respects. [CUT]
</li>
<li>
Certain other aspects and operations of the abstract machine are described in this International Standard as **unspecified** (for example, evaluation of expressions in a **new-initializer** if the allocation function fails to allocate memory). Where possible, this International Standard defines a set of allowable behaviors. These define the nondeterministic aspects of the abstract machine. An instance of the abstract machine can thus have more than one possible execution for a given program and a given input.
</li>
<li>
Certain other operations are described in this International Standard as **undefined** (or example, the effect of attempting to modify a `const` object). [ **Note**: this International Standard imposes no requirements on the behavior of programs that contain undefined behavior. —**end note** ]
</li>
</ol>


