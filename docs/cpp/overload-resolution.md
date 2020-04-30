---
metaTitle: "Overload resolution"
description: "Categorization of argument to parameter cost, Exact match, Name lookup and access checking, Overloading on Forwarding Reference, Arithmetic promotions and conversions, Overloading on constness and volatility, Steps of Overload Resolution, Overloading within a class hierarchy"
---

# Overload resolution



## Categorization of argument to parameter cost


Overload resolution partitions the cost of passing an argument to a parameter into one of four different categorizes, called "sequences". Each sequence may include zero, one or several conversions

<li>
Standard conversion sequence

```cpp
void f(int a); f(42);

```


</li>
<li>
User defined conversion sequence

```cpp
void f(std::string s); f("hello");

```


</li>
<li>
Ellipsis conversion sequence

```cpp
void f(...); f(42);

```


</li>
<li>
List initialization sequence

```cpp
void f(std::vector<int> v); f({1, 2, 3});

```


</li>

The general principle is that Standard conversion sequences are the cheapest, followed by user defined conversion sequences, followed by ellipsis conversion sequences.

A special case is the list initialization sequence, which does not constitute a conversion (an initializer list is not an expression with a type). Its cost is determined by defining it to be equivalent to one of the other three conversion sequences, depending on the parameter type and form of initializer list.



## Exact match


An overload without conversions needed for parameter types or only conversions needed between types that are still considered exact matches is preferred over an overload that requires other conversions in order to call.

```cpp
void f(int x);
void f(double x);
f(42); // calls f(int)

```

When an argument binds to a reference to the same type, the match is considered to not require a conversion even if the reference is more cv-qualified.

```cpp
void f(int& x);
void f(double x);
int x = 42;
f(x); // argument type is int; exact match with int&

void g(const int& x);
void g(int x);
g(x); // ambiguous; both overloads give exact match

```

For the purposes of overload resolution, the type "array of `T`" is considered to match exactly with the type "pointer to `T`", and the function type `T` is considered to match exactly with the function pointer type `T*`, even though both require conversions.

```cpp
void f(int* p);
void f(void* p);

void g(int* p);
void g(int (&p)[100]);

int a[100];
f(a); // calls f(int*); exact match with array-to-pointer conversion
g(a); // ambiguous; both overloads give exact match

```



## Name lookup and access checking


Overload resolution occurs **after** name lookup. This means that a better-matching function will not be selected by overload resolution if it loses name lookup:

```cpp
void f(int x);
struct S {
    void f(double x);
    void g() { f(42); } // calls S::f because global f is not visible here,
                        // even though it would be a better match
};

```

Overload resolution occurs **before** access checking. An inaccessible function might be selected by overload resolution if it is a better match than an accessible function.

```cpp
class C {
  public:
    static void f(double x);
  private:
    static void f(int x);
};
C::f(42); // Error! Calls private C::f(int) even though public C::f(double) is viable.

```

Similarly, overload resolution happens without checking whether the resulting call is well-formed with regards to `explicit`:

```cpp
struct X {
    explicit X(int );
    X(char );
};

void foo(X );
foo({4}); // X(int) is better much, but expression is 
          // ill-formed because selected constructor is explicit

```



## Overloading on Forwarding Reference


You must be very careful when providing a forwarding reference overload as it may match too well:

```cpp
struct A {
    A() = default;           // #1
    A(A const& ) = default;  // #2

    template <class T>
    A(T&& );                 // #3
};

```

The intent here was that `A` is copyable, and that we have this other constructor that might initialize some other member. However:

```cpp
A a;     // calls #1
A b(a);  // calls #3!

```

There are two viable matches for the construction call:

```cpp
A(A const& ); // #2
A(A& );       // #3, with T = A&

```

Both are Exact Matches, but `#3` takes a reference to a less **cv**-qualified object than `#2` does, so it has the better standard conversion sequence and is the best viable function.

The solution here is to always constrain these constructors (e.g. using SFINAE):

```cpp
template <class T,
    class = std::enable_if_t<!std::is_convertible<std::decay_t<T>*, A*>::value>
    >
A(T&& );

```

The type trait here is to exclude any `A` or class publicly and unambiguously derived from `A` from consideration, which would make this constructor ill-formed in the example described earlier (and hence removed from the overload set). As a result, the copy constructor is invoked - which is what we wanted.



## Arithmetic promotions and conversions


Converting an integer type to the corresponding promoted type is better than converting it to some other integer type.

```cpp
void f(int x);
void f(short x);
signed char c = 42;
f(c); // calls f(int); promotion to int is better than conversion to short
short s = 42;
f(s); // calls f(short); exact match is better than promotion to int

```

Promoting a `float` to `double` is better than converting it to some other floating point type.

```cpp
void f(double x);
void f(long double x);
f(3.14f); // calls f(double); promotion to double is better than conversion to long double

```

Arithmetic conversions other than promotions are neither better nor worse than each other.

```cpp
void f(float x);
void f(long double x);
f(3.14); // ambiguous

void g(long x);
void g(long double x);
g(42); // ambiguous
g(3.14); // ambiguous

```

Therefore, in order to ensure that there will be no ambiguity when calling a function `f` with either integral or floating-point arguments of any standard type, a total of eight overloads are needed, so that for each possible argument type, either an overload matches exactly or the unique overload with the promoted argument type will be selected.

```cpp
void f(int x);
void f(unsigned int x);
void f(long x);
void f(unsigned long x);
void f(long long x);
void f(unsigned long long x);
void f(double x);
void f(long double x);

```



## Overloading on constness and volatility


Passing a pointer argument to a `T*` parameter, if possible, is better than passing it to a `const T*` parameter.

```cpp
struct Base {};
struct Derived : Base {};
void f(Base* pb);
void f(const Base* pb);
void f(const Derived* pd);
void f(bool b);

Base b;
f(&b); // f(Base*) is better than f(const Base*)
Derived d;
f(&d); // f(const Derived*) is better than f(Base*) though;
       // constness is only a "tie-breaker" rule

```

Likewise, passing an argument to a `T&` parameter, if possible, is better than passing it to a `const T&` parameter, even if both have exact match rank.

```cpp
void f(int& r);
void f(const int& r);
int x;
f(x); // both overloads match exactly, but f(int&) is still better
const int y = 42;
f(y); // only f(const int&) is viable

```

This rule applies to const-qualified member functions as well, where it is important for allowing mutable access to non-const objects and immutable access to const objects.

```cpp
class IntVector {
  public:
    // ...
    int* data() { return m_data; }
    const int* data() const { return m_data; }
  private:
    // ...
    int* m_data;
};
IntVector v1;
int* data1 = v1.data();       // Vector::data() is better than Vector::data() const;
                              // data1 can be used to modify the vector's data
const IntVector v2;
const int* data2 = v2.data(); // only Vector::data() const is viable;
                              // data2 can't be used to modify the vector's data

```

In the same way, a volatile overload will be less preferred than a non-volatile overload.

```cpp
class AtomicInt {
  public:
    // ...
    int load();
    int load() volatile;
  private:
    // ...
};
AtomicInt a1;
a1.load(); // non-volatile overload preferred; no side effect
volatile AtomicInt a2;
a2.load(); // only volatile overload is viable; side effect
static_cast<volatile AtomicInt&>(a1).load(); // force volatile semantics for a1

```



## Steps of Overload Resolution


The steps of overload resolution are:

<li>
Find candidate functions via name lookup. Unqualified calls will perform both regular unqualified lookup as well as argument-dependent lookup (if applicable).
</li>
<li>
Filter the set of candidate functions to a set of **viable** functions. A viable function for which there exists an implicit conversion sequence between the arguments the function is called with and the parameters the function takes.

```cpp
void f(char);          // (1)
void f(int ) = delete; // (2)
void f();              // (3)
void f(int& );         // (4)

f(4); // 1,2 are viable (even though 2 is deleted!) 
      // 3 is not viable because the argument lists don't match
      // 4 is not viable because we cannot bind a temporary to 
      //     a non-const lvalue reference

```


</li>
<li>
Pick the best viable candidate. A viable function `F1` is a better function than another viable function `F2` if the implicit conversion sequence for each argument in `F1` is not worse than the corresponding implicit conversion sequence in `F2`, and...:
3.1. For some argument, the implicit conversion sequence for that argument in `F1` is a better conversion sequence than for that argument in `F2`, or

```cpp
void f(int );  // (1)
void f(char ); // (2)

f(4);  // call (1), better conversion sequence

```


3.2. In a user-defined conversion, the standard conversion sequence from the return of `F1` to the destination type is a better conversion sequence than that of the return type of `F2`, or

```cpp
struct A 
{
    operator int();
    operator double();
} a;

int i = a; // a.operator int() is better than a.operator double() and a conversion
float f = a; // ambiguous

```



3.3. In a direct reference binding, `F1` has the same kind of reference by `F2` is not, or

```cpp
struct A 
{
    operator X&();  // #1
    operator X&&(); // #2
};
A a;
X& lx = a;  // calls #1
X&& rx = a; // calls #2

```


3.4. `F1` is not a function template specialization, but `F2` is, or

```cpp
template <class T> void f(T ); // #1
void f(int );                  // #2

f(42); // calls #2, the non-template

```


3.5. `F1` and `F2` are both function template specializations, but `F1` is more specialized than `F2`.

```cpp
template <class T> void f(T );  // #1
template <class T> void f(T* ); // #2

int* p;
f(p); // calls #2, more specialized

```


</li>

The ordering here is significant. The better conversion sequence check happens before the template vs non-template check. This leads to a common error with overloading on forwarding reference:

```cpp
struct A {
    A(A const& ); // #1
    
    template <class T>
    A(T&& );      // #2, not constrained
};

A a;
A b(a); // calls #2!
        // #1 is not a template but #2 resolves to
        // A(A& ), which is a less cv-qualified reference than #1
        // which makes it a better implicit conversion sequence

```

If there's no single best viable candidate at the end, the call is ambiguous:

```cpp
void f(double ) { }
void f(float ) { }

f(42); // error: ambiguous

```



## Overloading within a class hierarchy


The following examples will use this class hierarchy:

```cpp
struct A { int m; };
struct B : A {};
struct C : B {};

```

The conversion from derived class type to base class type is preferred to user-defined conversions. This applies when passing by value or by reference, as well as when converting pointer-to-derived to pointer-to-base.

```cpp
struct Unrelated {
    Unrelated(B b);
};
void f(A a);
void f(Unrelated u);
B b;
f(b); // calls f(A)

```

A pointer conversion from derived class to base class is also better than conversion to `void*`.

```cpp
void f(A* p);
void f(void* p);
B b;
f(&b); // calls f(A*)

```

If there are multiple overloads within the same chain of inheritance, the most derived base class overload is preferred. This is based on a similar principle as virtual dispatch: the "most specialized" implementation is chosen. However, overload resolution always occurs at compile time and will never implicitly down-cast.

```cpp
void f(const A& a);
void f(const B& b);
C c;
f(c); // calls f(const B&)
B b;
A& r = b;
f(r); // calls f(const A&); the f(const B&) overload is not viable

```

For pointers to members, which are contravariant with respect to the class, a similar rule applies in the opposite direction: the least derived derived class is preferred.

```cpp
void f(int B::*p);
void f(int C::*p);
int A::*p = &A::m;
f(p); // calls f(int B::*)

```



#### Remarks


Overload resolution happens in several different situations

- Calls to named overloaded functions. The candidates are all the functions found by name lookup.
- Calls to class object. The candidates are usually all the overloaded function call operators of the class.
- Use of an operator. The candidates are the overloaded operator functions at namespace scope, the overloaded operator functions in the left class object (if any) and the built-in operators.
<li>Overload resolution to find the correct conversion operator function or constructor to invoke for an initialization
<ul>
- For non-list direct initialization (`Class c(value)`), the candidates are constructors of `Class`.
- For non-list copy initialization (`Class c = value`) and for finding the user defined conversion function to invoke in a user defined conversion sequence. The candidates are the constructors of `Class` and if the source is a class object, its conversion operator functions.
- For initialization of a non-class from a class object (`Nonclass c = classObject`). The candidates are the conversion operator functions of the initializer object.
- For initializing a reference with a class object (`R &r = classObject`), when the class has conversion operator functions that yield values that can be bound directly to `r`. The candidates are such conversion operator functions.
- For list-initialization of a non-aggregate class object (`Class c{1, 2, 3}`), the candidates are the initializer list constructors for a first pass through overload resolution. If this doesn't find a viable candidate, a second pass through overload resolution is done, with the constructors of `Class` as candidates.

