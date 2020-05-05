---
metaTitle: "C++ | Explicit type conversions"
description: "Casting away constness, C-style casting, Base to derived conversion, Conversion between pointer and integer, Type punning conversion, Conversion by explicit constructor or explicit conversion function, Implicit conversion, Enum conversions, Derived to base conversion for pointers to members, void* to T*"
---

# Explicit type conversions


An expression can be **explicitly converted** or **cast** to type `T` using `dynamic_cast<T>`, `static_cast<T>`, `reinterpret_cast<T>`, or `const_cast<T>`, depending on what type of cast is intended.

C++ also supports function-style cast notation, `T(expr)`, and C-style cast notation, `(T)expr`.



## Casting away constness


A pointer to a const object can be converted to a pointer to non-const object using the `const_cast` [keyword](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords). Here we use `const_cast` to call a function that is not const-correct. It only accepts a non-const `char*` argument even though it never writes through the pointer:

```cpp
void bad_strlen(char*);
const char* s = "hello, world!";
bad_strlen(s);                    // compile error
bad_strlen(const_cast<char*>(s)); // OK, but it's better to make bad_strlen accept const char*

```

`const_cast` to reference type can be used to convert a const-qualified lvalue into a non-const-qualified value.

`const_cast` is dangerous because it makes it impossible for the C++ type system to prevent you from trying to modify a const object. Doing so results in undefined behavior.

```cpp
const int x = 123;
int& mutable_x = const_cast<int&>(x);
mutable_x = 456; // may compile, but produces *undefined behavior*

```



## C-style casting


C-Style casting can be considered 'Best effort' casting and is named so as it is the only cast which could be used in C. The syntax for this cast is `(NewType)variable`.

Whenever this cast is used, it uses one of the following c++ casts (in order):

- `const_cast<NewType>(variable)`
- `static_cast<NewType>(variable)`
- `const_cast<NewType>(static_cast<const NewType>(variable))`
- `reinterpret_cast<const NewType>(variable)`
- `const_cast<NewType>(reinterpret_cast<const NewType>(variable))`

Functional casting is very similar, though as a few restrictions as the result of its syntax: `NewType(expression)`. As a result, only types without spaces can be cast to.

It's better to use new c++ cast, because s more readable and can be spotted easily anywhere inside a C++ source code and errors will be detected in compile-time, instead in run-time.

As this cast can result in unintended `reinterpret_cast`, it is often considered dangerous.



## Base to derived conversion


A pointer to base class can be converted to a pointer to derived class using `static_cast`. `static_cast` does not do any run-time checking and can lead to undefined behaviour when the pointer does not actually point to the desired type.

```cpp
struct Base {};
struct Derived : Base {};
Derived d;
Base* p1 = &d;
Derived* p2 = p1;                        // error; cast required
Derived* p3 = static_cast<Derived*>(p1); // OK; p2 now points to Derived object
Base b;
Base* p4 = &b;
Derived* p5 = static_cast<Derived*>(p4); // undefined behaviour since p4 does not
                                         // point to a Derived object

```

Likewise, a reference to base class can be converted to a reference to derived class using `static_cast`.

```cpp
struct Base {};
struct Derived : Base {};
Derived d;
Base& r1 = d;
Derived& r2 = r1;                        // error; cast required
Derived& r3 = static_cast<Derived&>(r1); // OK; r3 now refers to Derived object

```

If the source type is polymorphic, `dynamic_cast` can be used to perform a base to derived conversion. It performs a run-time check and failure is recoverable instead of producing undefined behaviour. In the pointer case, a null pointer is returned upon failure. In the reference case, an exception is thrown upon failure of type `std::bad_cast` (or a class derived from `std::bad_cast`).

```cpp
struct Base { virtual ~Base(); }; // Base is polymorphic
struct Derived : Base {};
Base* b1 = new Derived;
Derived* d1 = dynamic_cast<Derived*>(b1); // OK; d1 points to Derived object
Base* b2 = new Base;
Derived* d2 = dynamic_cast<Derived*>(b2); // d2 is a null pointer

```



## Conversion between pointer and integer


An object pointer (including `void*`) or function pointer can be converted to an integer type using `reinterpret_cast`. This will only compile if the destination type is long enough. The result is implementation-defined and typically yields the numeric address of the byte in memory that the pointer pointers to.

Typically, `long` or `unsigned long` is long enough to hold any pointer value, but this is not guaranteed by the standard.

If the types `std::intptr_t` and `std::uintptr_t` exist, they are guaranteed to be long enough to hold a `void*` (and hence any pointer to object type). However, they are not guaranteed to be long enough to hold a function pointer.

Similarly, `reinterpret_cast` can be used to convert an integer type into a pointer type. Again the result is implementation-defined, but a pointer value is guaranteed to be unchanged by a round trip through an integer type. The standard does not guarantee that the value zero is converted to a null pointer.

```cpp
void register_callback(void (*fp)(void*), void* arg); // probably a C API
void my_callback(void* x) {
    std::cout << "the value is: " << reinterpret_cast<long>(x); // will probably compile
}
long x;
std::cin >> x;
register_callback(my_callback,
                  reinterpret_cast<void*>(x)); // hopefully this doesn't lose information...

```



## Type punning conversion


A pointer (resp. reference) to an object type can be converted to a pointer (resp. reference) to any other object type using `reinterpret_cast`. This does not call any constructors or conversion functions.

```cpp
int x = 42;
char* p = static_cast<char*>(&x);      // error: static_cast cannot perform this conversion
char* p = reinterpret_cast<char*>(&x); // OK
*p = 'z';                              // maybe this modifies x (see below)

```

The result of `reinterpret_cast` represents the same address as the operand, provided that the address is appropriately aligned for the destination type. Otherwise, the result is unspecified.

```cpp
int x = 42;
char& r = reinterpret_cast<char&>(x);
const void* px = &x;
const void* pr = &r;
assert(px == pr); // should never fire

```

The result of `reinterpret_cast` is unspecified, except that a pointer (resp. reference) will survive a round trip from the source type to the destination type and back, as long as the destination type's alignment requirement is not stricter than that of the source type.

```cpp
int x = 123;
unsigned int& r1 = reinterpret_cast<unsigned int&>(x);
int& r2 = reinterpret_cast<int&>(r1);
r2 = 456; // sets x to 456

```

On most implementations, `reinterpret_cast` does not change the address, but this requirement was not standardized until C++11.

`reinterpret_cast` can also be used to convert from one pointer-to-data-member type to another, or one pointer-to-member-function type to another.

Use of `reinterpret_cast` is considered dangerous because reading or writing through a pointer or reference obtained using `reinterpret_cast` may trigger undefined behaviour when the source and destination types are unrelated.



## Conversion by explicit constructor or explicit conversion function


A conversion that involves calling an [explicit](http://stackoverflow.com/documentation/c%2b%2b/4891/keywords/18568/explicit) constructor or conversion function can't be done implicitly. We can request that the conversion be done explicitly using `static_cast`. The meaning is the same as that of a direct initialization, except that the result is a temporary.

```cpp
class C {
    std::unique_ptr<int> p;
  public:
    explicit C(int* p) : p(p) {}
};
void f(C c);
void g(int* p) {
    f(p);                 // error: C::C(int*) is explicit
    f(static_cast<C>(p)); // ok
    f(C(p));              // equivalent to previous line
    C c(p); f(c);         // error: C is not copyable
}

```



## Implicit conversion


`static_cast` can perform any implicit conversion. This use of `static_cast` can occasionally be useful, such as in the following examples:

<li>
When passing arguments to an ellipsis, the "expected" argument type is not statically known, so no implicit conversion will occur.

```cpp
const double x = 3.14;
printf("%d\n", static_cast<int>(x)); // prints 3
// printf("%d\n", x); // undefined behaviour; printf is expecting an int here
// alternative:
// const int y = x; printf("%d\n", y);

```


Without the explicit type conversion, a `double` object would be passed to the ellipsis, and undefined behaviour would occur.
</li>
<li>
A derived class assignment operator can call a base class assignment operator like so:

```cpp
struct Base { /* ... */ };
struct Derived : Base {
    Derived& operator=(const Derived& other) {
        static_cast<Base&>(*this) = other;
        // alternative:
        // Base& this_base_ref = *this; this_base_ref = other;
    }
};

```


</li>



## Enum conversions


`static_cast` can convert from an integer or floating point type to an enumeration type (whether scoped or unscoped), and **vice versa.** It can also convert between enumeration types.

- The conversion from an unscoped enumeration type to an arithmetic type is an implicit conversion; it is possible, but not necessary, to use `static_cast`.

<li>
When a scoped enumeration type is converted to an arithmetic type:
<ul>
- If the enum's value can be represented exactly in the destination type, the result is that value.
- Otherwise, if the destination type is an integer type, the result is unspecified.
- Otherwise, if the destination type is a floating point type, the result is the same as that of converting to the underlying type and then to the floating point type.

Example:

```cpp
enum class Format {
    TEXT = 0,
    PDF = 1000,
    OTHER = 2000,
};
Format f = Format::PDF;
int a = f;                         // error
int b = static_cast<int>(f);       // ok; b is 1000
char c = static_cast<char>(f);     // unspecified, if 1000 doesn't fit into char
double d = static_cast<double>(f); // d is 1000.0... probably

```


<li>
When an integer or enumeration type is converted to an enumeration type:
<ul>
- If the original value is within the destination enum's range, the result is that value. Note that this value might be unequal to all enumerators.
- Otherwise, the result is unspecified (<= C++14) or undefined (>= C++17).

Example:

```cpp
enum Scale {
    SINGLE = 1,
    DOUBLE = 2,
    QUAD = 4
};
Scale s1 = 1;                     // error
Scale s2 = static_cast<Scale>(2); // s2 is DOUBLE
Scale s3 = static_cast<Scale>(3); // s3 has value 3, and is not equal to any enumerator
Scale s9 = static_cast<Scale>(9); // unspecified value in C++14; UB in C++17

```


<li>
When a floating point type is converted to an enumeration type, the result is the same as converting to the enum's underlying type and then to the enum type.

```cpp
enum Direction {
    UP = 0,
    LEFT = 1,
    DOWN = 2,
    RIGHT = 3,
};
Direction d = static_cast<Direction>(3.14); // d is RIGHT

```


</li>



## Derived to base conversion for pointers to members


A pointer to member of derived class can be converted to a pointer to member of base class using `static_cast`. The types pointed to must match.

If the operand is a null pointer to member value, the result is also a null pointer to member value.

Otherwise, the conversion is only valid if the member pointed to by the operand actually exists in the destination class, or if the destination class is a base or derived class of the class containing the member pointed to by the operand. `static_cast` does not check for validity. If the conversion is not valid, the behaviour is undefined.

```cpp
struct A {};
struct B { int x; };
struct C : A, B { int y; double z; };
int B::*p1 = &B::x;
int C::*p2 = p1;                              // ok; implicit conversion
int B::*p3 = p2;                              // error
int B::*p4 = static_cast<int B::*>(p2);       // ok; p4 is equal to p1
int A::*p5 = static_cast<int A::*>(p2);       // undefined; p2 points to x, which is a member
                                              // of the unrelated class B
double C::*p6 = &C::z;
double A::*p7 = static_cast<double A::*>(p6); // ok, even though A doesn't contain z
int A::*p8 = static_cast<int A::*>(p6);       // error: types don't match

```



## void* to T*


In C++, `void*` cannot be implicitly converted to `T*` where `T` is an object type. Instead, `static_cast` should be used to perform the conversion explicitly. If the operand actually points to a `T` object, the result points to that object. Otherwise, the result is unspecified.

Even if the operand does not point to a `T` object, as long as the operand points to a byte whose address is properly aligned for the type `T`, the result of the conversion points to the same byte.

```cpp
// allocating an array of 100 ints, the hard way
int* a = malloc(100*sizeof(*a));                    // error; malloc returns void*
int* a = static_cast<int*>(malloc(100*sizeof(*a))); // ok
// int* a = new int[100];                           // no cast needed
// std::vector<int> a(100);                         // better

const char c = '!';
const void* p1 = &c;
const char* p2 = p1;                           // error
const char* p3 = static_cast<const char*>(p1); // ok; p3 points to c
const int* p4 = static_cast<const int*>(p1);   // unspecified in C++03;
                                               // possibly unspecified in C++11 if
                                               // alignof(int) > alignof(char)
char* p5 = static_cast<char*>(p1);             // error: casting away constness

```



#### Syntax


- **simple-type-specifier** `(` `)`
- **simple-type-specifier** `(` **expression-list**  `)`
- **simple-type-specifier** **braced-init-list**
- **typename-specifier** `(` `)`
- **typename-specifier** `(` **expression-list** `)`
- **typename-specifier** **braced-init-list**
- `dynamic_cast` `<` **type-id** `>` `(` **expression** `)`
- `static_cast` `<` **type-id** `>` `(` **expression** `)`
- `reinterpret_cast` `<` **type-id** `>` `(` **expression** `)`
- `const_cast` `<` **type-id** `>` `(` **expression** `)`
- `(` **type-id** `)` **cast-expression**



#### Remarks


All six cast notations have one thing in common:

- Casting to an lvalue reference type, as in `dynamic_cast<Derived&>(base)`, yields an lvalue. Therefore, when you want to do something with the same object but treat it as a different type, you would cast to an lvalue reference type.
- Casting to an rvalue reference type, as in `static_cast<string&&>(s)`, yields an rvalue.
- Casting to a non-reference type, as in `(int)x`, yields a prvalue, which may be thought of as a **copy** of the value being cast, but with a different type from the original.

The `reinterpret_cast` keyword is responsible for performing two different kinds of "unsafe" conversions:

- The ["type punning"](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/12169/type-punning-conversion) conversions, which can be used to access memory of one type as though it is of a different type.
- Conversions [between integer types and pointer types](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/13555/conversion-between-pointer-and-integer), in either direction.

The `static_cast` keyword can perform a variety of different conversions:

<li>
[Base to derived](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/10518/base-to-derived-conversion) conversions
</li>
<li>
Any conversion that can be done by a direct initialization, including both implicit conversions and conversions that call an explicit constructor or conversion function. See [here](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/18732/implicit-conversion) and [here](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/18731/conversion-by-explicit-constructor-or-explicit-conversion-function) for more details.
</li>
<li>
To `void`, which discards the value of the expression.

```cpp
// on some compilers, suppresses warning about x being unused
static_cast<void>(x);

```


</li>
<li>
Between arithmetic and enumeration types, and between different enumeration types. See [enum conversions](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/18751/enum-conversions)
</li>
<li>
From pointer to member of derived class, to pointer to member of base class. The types pointed to must match. See [derived to base conversion for pointers to members](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/18752/derived-to-base-conversion-for-pointers-to-members)
</li>
<li>
[`void*` to `T*`](http://stackoverflow.com/documentation/c%2b%2b/3090/explicit-type-conversions/18753/void-to-t).
</li>

- From an lvalue to an xvalue, as in `std::move`. See [move semantics](http://stackoverflow.com/documentation/c%2b%2b/2129/move-semantics).

