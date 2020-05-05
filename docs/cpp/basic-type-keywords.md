---
metaTitle: "C++ | Basic Type Keywords"
description: "int, bool, char, char16_t, char32_t, float, double, long, short, void, wchar_t"
---

# Basic Type Keywords



## int


Denotes a signed integer type with "the natural size suggested by the
architecture of the execution environment", whose range includes at least -32767 to +32767, inclusive.

```cpp
int x = 2;
int y = 3;
int z = x + y;

```

Can be combined with `unsigned`, `short`, `long`, and `long long` (q.v.) in order to yield other integer types.



## bool


An integer type whose value can be either `true` or `false`.

```cpp
bool is_even(int x) {
    return x%2 == 0;
}
const bool b = is_even(47); // false

```



## char


An integer type which is "large enough to store any member of the implementation’s basic
character set". It is implementation-defined whether `char` is signed (and has a range of at least -127 to +127, inclusive) or unsigned (and has a range of at least 0 to 255, inclusive).

```cpp
const char zero = '0';
const char one = zero + 1;
const char newline = '\n';
std::cout << one << newline; // prints 1 followed by a newline

```



## char16_t


An unsigned integer type with the same size and alignment as `uint_least16_t`, which is therefore large enough to hold a UTF-16 code unit.

```cpp
const char16_t message[] = u"你好，世界\n";            // Chinese for "hello, world\n"
std::cout << sizeof(message)/sizeof(char16_t) << "\n"; // prints 7

```



## char32_t


An unsigned integer type with the same size and alignment as `uint_least32_t`, which is therefore large enough to hold a UTF-32 code unit.

```cpp
const char32_t full_house[] = U"🂣🂳🂨🂸🃈";               // non-BMP characters
std::cout << sizeof(full_house)/sizeof(char32_t) << "\n"; // prints 6

```



## float


A floating point type. Has the narrowest range out of the three floating point types in C++.

```cpp
float area(float radius) {
    const float pi = 3.14159f;
    return pi*radius*radius;
}

```



## double


A floating point type. Its range includes that of `float`. When combined with `long`, denotes the `long double` floating point type, whose range includes that of `double`.

```cpp
double area(double radius) {
    const double pi = 3.141592653589793;
    return pi*radius*radius;
}

```



## long


Denotes a signed integer type that is at least as long as `int`, and whose range includes at least -2147483647 to +2147483647, inclusive (that is, -(2^31 - 1) to +(2^31 - 1)). This type can also be written as `long int`.

```cpp
const long approx_seconds_per_year = 60L*60L*24L*365L;

```

The combination `long double` denotes a floating point type, which has the widest range out of the three floating point types.

```cpp
long double area(long double radius) {
    const long double pi = 3.1415926535897932385L;
    return pi*radius*radius;
}

```

When the `long` specifier occurs twice, as in `long long`, it denotes a signed integer type that is at least as long as `long`, and whose range includes at least -9223372036854775807 to +9223372036854775807, inclusive (that is, -(2^63 - 1) to +(2^63 - 1)).

```cpp
// support files up to 2 TiB
const long long max_file_size = 2LL << 40;

```



## short


Denotes a signed integer type that is at least as long as `char`, and whose range includes at least -32767 to +32767, inclusive. This type can also be written as `short int`.

```cpp
// (during the last year)
short hours_worked(short days_worked) {
    return 8*days_worked;
}

```



## void


An incomplete type; it is not possible for an object to have type `void`, nor are there arrays of `void` or references to `void`. It is used as the return type of functions that do not return anything.

Moreover, a function may redundantly be declared with a single parameter of type `void`; this is equivalent to declaring a function with no parameters (e.g. `int main()` and `int main(void)` declare the same function). This syntax is allowed for compatibility with C (where function declarations have a different meaning than in C++).

The type `void*` ("pointer to `void`") has the property that any object pointer can be converted to it and back and result in the same pointer. This feature makes the type `void*` suitable for certain kinds of (type-unsafe) type-erasing interfaces, for example for generic contexts in C-style APIs (e.g. `qsort`, `pthread_create`).

Any expression may be converted to an expression of type `void`; this is called a **discarded-value expression**:

```cpp
static_cast<void>(std::printf("Hello, %s!\n", name));  // discard return value

```

This may be useful to signal explicitly that the value of an expression is not of interest and that the expression is to be evaluated for its side effects only.



## wchar_t


An integer type large enough to represent all characters of the largest supported extended character set, also known as the wide-character set. (It is not portable to make the assumption that `wchar_t` uses any particular encoding, such as UTF-16.)

It is normally used when you need to store characters over ASCII 255 , as it has a greater size than the character type `char`.

```cpp
const wchar_t message_ahmaric[] = L"ሰላም ልዑል\n"; //Ahmaric for "hello, world\n"
const wchar_t message_chinese[] = L"你好，世界\n";// Chinese for "hello, world\n"
const wchar_t message_hebrew[]  = L"שלום עולם\n"; //Hebrew for "hello, world\n"
const wchar_t message_russian[] = L"Привет мир\n";  //Russian for "hello, world\n"
const wchar_t message_tamil[]   = L"ஹலோ உலகம்\n"; //Tamil for "hello, world\n"

```

