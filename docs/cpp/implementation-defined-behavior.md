---
metaTitle: "C++ | Implementation-defined behavior"
description: "Size of integral types, Char might be unsigned or signed, Number of bits in a byte, Numeric value of a pointer, Ranges of numeric types, Value representation of floating point types, Overflow when converting from integer to signed integer, Underlying type (and hence size) of an enum"
---

# Implementation-defined behavior



## Size of integral types


The following types are defined as **integral types**:

- `char`
- Signed integer types
- Unsigned integer types
- `char16_t` and `char32_t`
- `bool`
- `wchar_t`

With the exception of `sizeof(char)` / `sizeof(signed char)` / `sizeof(unsigned char)`, which is split between § 3.9.1.1 [basic.fundamental/1] and § 5.3.3.1 [expr.sizeof], and `sizeof(bool)`, which is entirely implementation-defined and has no minimum size, the minimum size requirements of these types are given in section § 3.9.1 [basic.fundamental] of the standard, and shall be detailed below.

### Size of `char`

All versions of the C++ standard specify, in § 5.3.3.1, that `sizeof` yields `1` for `unsigned char`, `signed char`, and `char` (it is implementation defined whether the `char` type is `signed` or `unsigned`).

`char` is large enough to represent 256 different values, to be suitable for storing UTF-8 code units.

### Size of signed and unsigned integer types

The standard specifies, in § 3.9.1.2, that in the list of **standard signed integer types**, consisting of `signed char`, `short int`, `int`, `long int`, and `long long int`, each type will provide at least as much storage as those preceding it in the list.  Furthermore, as specified in § 3.9.1.3, each of these types has a corresponding **standard unsigned integer type**, `unsigned char`, `unsigned short int`, `unsigned int`, `unsigned long int`, and `unsigned long long int`, which has the same size and alignment as its corresponding signed type.  Additionally, as specified in § 3.9.1.1, `char` has the same size and alignment requirements as both `signed char` and `unsigned char`.

Prior to C++11, `long long` and `unsigned long long` were not officially part of the C++ standard.  However, after their introduction to C, in C99, many compilers supported `long long` as an **extended signed integer type**, and `unsigned long long` as an **extended unsigned integer type**, with the same rules as the C types.

The standard thus guarantees that:

```cpp
1 == sizeof(char)  == sizeof(signed char) == sizeof(unsigned char)
  <= sizeof(short) == sizeof(unsigned short)
  <= sizeof(int)   == sizeof(unsigned int)
  <= sizeof(long)  == sizeof(unsigned long)

```

```cpp
 <= sizeof(long long) == sizeof(unsigned long long)

```

Specific minimum sizes for each type are not given by the standard.  Instead, each type has a minimum range of values it can support, which is, as specified in § 3.9.1.3, inherited from the C standard, in §5.2.4.2.1.  The minimum size of each type can be roughly inferred from this range, by determining the minimum number of bits required; note that for any given platform, any type's actual supported range may be larger than the minimum.  Note that for signed types, ranges correspond to one's complement, not the more commonly used two's complement; this is to allow a wider range of platforms to comply with the standard.

<th align="left">Type|Minimum range</th><th align="right">Minimum bits required</th>
|---|---|---|---|---|---|---|---|---|---
<td align="left">`signed char`|-127 to 127 (-(2<sup>7</sup> - 1) to (2<sup>7</sup> - 1))</td><td align="right">8</td>
<td align="left">`unsigned char`|0 to 255 (0 to 2<sup>8</sup> - 1)</td><td align="right">8</td>
<td align="left">`signed short`|-32,767 to 32,767 (-(2<sup>15</sup> - 1) to (2<sup>15</sup> - 1))</td><td align="right">16</td>
<td align="left">`unsigned short`|0 to 65,535 (0 to 2<sup>16</sup> - 1)</td><td align="right">16</td>
<td align="left">`signed int`|-32,767 to 32,767 (-(2<sup>15</sup> - 1) to (2<sup>15</sup> - 1))</td><td align="right">16</td>
<td align="left">`unsigned int`|0 to 65,535 (0 to 2<sup>16</sup> - 1)</td><td align="right">16</td>
<td align="left">`signed long`|-2,147,483,647 to 2,147,483,647 (-(2<sup>31</sup> - 1) to (2<sup>31</sup> - 1))</td><td align="right">32</td>
<td align="left">`unsigned long`|0 to 4,294,967,295 (0 to 2<sup>32</sup> - 1)</td><td align="right">32</td>

<th align="left">Type|Minimum range</th><th align="right">Minimum bits required</th>
|---|---|---|---|---|---|---|---|---|---
<td align="left">`signed long long`|-9,223,372,036,854,775,807 to 9,223,372,036,854,775,807 (-(2<sup>63</sup> - 1) to (2<sup>63</sup> - 1))</td><td align="right">64</td>
<td align="left">`unsigned long long`|0 to 18,446,744,073,709,551,615 (0 to 2<sup>64</sup> - 1)</td><td align="right">64</td>

As each type is allowed to be greater than its minimum size requirement, types may differ in size between implementations.  The most notable example of this is with the 64-bit data models LP64 and LLP64, where LLP64 systems (such as 64-bit Windows) have 32-bit `ints` and `long`s, and LP64 systems (such as 64-bit Linux) have 32-bit `int`s and 64-bit `long`s.  Due to this, integer types cannot be assumed to have a fixed width across all platforms.

If integer types with fixed width are required, use types from the [`<cstdint>`](http://en.cppreference.com/w/cpp/header/cstdint) header, but note that the standard makes it optional for implementations to support the exact-width types `int8_t`, `int16_t`, `int32_t`, `int64_t`, `intptr_t`, `uint8_t`, `uint16_t`, `uint32_t`, `uint64_t` and `uintptr_t`.

### Size of `char16_t` and `char32_t`

The sizes of `char16_t` and `char32_t` are implementation-defined, as specified in § 5.3.3.1, with the stipulations given in § 3.9.1.5:

<li>
`char16_t` is large enough to represent any UTF-16 code unit, and has the same size, signedness, and alignment as [`uint_least16_t`](http://en.cppreference.com/w/cpp/header/cstdint); it is thus required to be at least 16 bits in size.
</li>
<li>
`char32_t` is large enough to represent any UTF-32 code unit, and has the same size, signedness, and alignment as [`uint_least32_t`](http://en.cppreference.com/w/cpp/header/cstdint); it is thus required to be at least 32 bits in size.
</li>

### Size of `bool`

The size of `bool` is implementation defined, and may or may not be `1`.

### Size of wchar_t

`wchar_t`, as specified in § 3.9.1.5, is a distinct type, whose range of values can represent every distinct code unit of the largest extended character set among the supported locales.  It has the same size, signedness, and alignment as one of the other integral types, which is known as its **underlying type**.  This type's size is implementation-defined, as specified in § 5.3.3.1, and may be, for example, at least 8, 16, or 32 bits; if a system supports Unicode, for example, `wchar_t` is required to be at least 32 bits (an exception to this rule is Windows, where `wchar_t` is 16 bits for compatibility purposes).  It is inherited from the C90 standard, ISO 9899:1990 § 4.1.5, with only minor rewording.

Depending on the implementation, the size of `wchar_t` is often, but not always, 8, 16, or 32 bits.  The most common examples of these are:

- In Unix and Unix-like systems, `wchar_t` is 32-bit, and is usually used for UTF-32.
- In Windows, `wchar_t` is 16-bit, and is used for UTF-16.
- On a system which only has 8-bit support, `wchar_t` is 8 bit.

If Unicode support is desired, it is recommended to use `char` for UTF-8, `char16_t` for UTF-16, or `char32_t` for UTF-32, instead of using `wchar_t`.

### Data Models

As mentioned above, the widths of integer types can differ between platforms.  The most common models are as follows, with sizes specified in bits:

|Model|`int`|`long`|pointer
|---|---|---|---|---|---|---|---|---|---
|LP32 (2/4/4)|16|32|32
|ILP32 (4/4/4)|32|32|32
|LLP64 (4/4/8)|32|32|64
|LP64 (4/8/8)|32|64|64

Out of these models:

- 16-bit Windows used LP32.
- 32-bit *nix systems (Unix, Linux, Mac OSX, and other Unix-like OSes) and Windows use ILP32.
- 64-bit Windows uses LLP64.
- 64-bit *nix systems use LP64.

Note, however, that these models aren't specifically mentioned in the standard itself.



## Char might be unsigned or signed


The standard doesn't specify if `char` should be signed or unsigned. Different compilers implement it differently, or might allow to change it using a command line switch.



## Number of bits in a byte


In C++, a **byte** is the space occupied by a `char` object. The number of bits in a byte is given by `CHAR_BIT`, which is defined in `climits` and required to be at least 8. While most modern systems have 8-bit bytes, and POSIX requires `CHAR_BIT` to be exactly 8, there are some systems where `CHAR_BIT` is greater than 8 i.e a single byte may be comprised of 8, 16, 32 or 64 bits.



## Numeric value of a pointer


The result of casting a pointer to an integer using `reinterpret_cast` is implementation-defined, but "... is intended to be unsurprising to those who know the addressing structure of the underlying machine."

```cpp
int x = 42;
int* p = &x;
long addr = reinterpret_cast<long>(p);
std::cout << addr << "\n"; // prints some numeric address,
                           // probably in the architecture's native address format

```

Likewise, the pointer obtained by conversion from an integer is also implementation-defined.

The right way to store a pointer as an integer is using the `uintptr_t` or `intptr_t` types:

```cpp
// `uintptr_t` was not in C++03. It's in C99, in <stdint.h>, as an optional type
#include <stdint.h>

uintptr_t uip;

```

```cpp
// There is an optional `std::uintptr_t` in C++11
#include <cstdint>

std::uintptr_t uip;

```

C++11 refers to C99 for the definition `uintptr_t` (C99 standard, 6.3.2.3):

> 
an unsigned integer type with the property that any valid pointer to `void` can be converted to this type, then converted back to pointer to `void`, and the result will compare equal to the original pointer.


While, for the majority of modern platforms, you can assume a flat address space and that arithmetic on `uintptr_t` is equivalent to arithmetic on `char *`, it's entirely possible for an implementation to perform any transformation when casting `void *` to `uintptr_t` as long the transformation can be reversed when casting back from `uintptr_t` to `void *`.

**Technicalities**

<li>
On XSI-conformant (X/Open System Interfaces) systems, `intptr_t` and `uintptr_t` types are required, otherwise they are **optional**.
</li>
<li>
Within the meaning of the C standard, functions aren't objects; it isn't guaranteed by the C standard that `uintptr_t` can hold a function pointer. Anyway POSIX (2.12.3) conformance requires that:
<blockquote>
All function pointer types shall have the same representation as the type pointer to void. Conversion of a function pointer to void * shall not alter the representation. A void * value resulting from such a conversion can be converted back to the original function pointer type, using an explicit cast, without loss of information.
</blockquote>
</li>
<li>
C99 §7.18.1:
<blockquote>
When typedef names differing only in the absence or presence of the initial u are defined, they shall denote corresponding signed and unsigned types as described in 6.2.5; an implementation providing one of these corresponding types shall also provide the other.
</blockquote>
`uintptr_t` might make sense if you want to do things to the bits of the pointer that you can't do as sensibly with a signed integer.
</li>

> 
When typedef names differing only in the absence or presence of the initial u are defined, they shall denote corresponding signed and unsigned types as described in 6.2.5; an implementation providing one of these corresponding types shall also provide the other.




## Ranges of numeric types


The ranges of the integer types are implementation-defined. The header `<limits>` provides the `std::numeric_limits<T>` template which provides the minimum and maximum values of all fundamental types. The values satisfy guarantees provided by the C standard through the `<climits>` and (>= C++11) `<cinttypes>` headers.

- `std::numeric_limits<signed char>::min()` equals `SCHAR_MIN`, which is less than or equal to -127.
- `std::numeric_limits<signed char>::max()` equals `SCHAR_MAX`, which is greater than or equal to 127.
- `std::numeric_limits<unsigned char>::max()` equals `UCHAR_MAX`, which is greater than or equal to 255.
- `std::numeric_limits<short>::min()` equals `SHRT_MIN`, which is less than or equal to -32767.
- `std::numeric_limits<short>::max()` equals `SHRT_MAX`, which is greater than or equal to 32767.
- `std::numeric_limits<unsigned short>::max()` equals `USHRT_MAX`, which is greater than or equal to 65535.
- `std::numeric_limits<int>::min()` equals `INT_MIN`, which is less than or equal to -32767.
- `std::numeric_limits<int>::max()` equals `INT_MAX`, which is greater than or equal to 32767.
- `std::numeric_limits<unsigned int>::max()` equals `UINT_MAX`, which is greater than or equal to 65535.
- `std::numeric_limits<long>::min()` equals `LONG_MIN`, which is less than or equal to -2147483647.
- `std::numeric_limits<long>::max()` equals `LONG_MAX`, which is greater than or equal to 2147483647.
- `std::numeric_limits<unsigned long>::max()` equals `ULONG_MAX`, which is greater than or equal to 4294967295.

- `std::numeric_limits<long long>::min()` equals `LLONG_MIN`, which is less than or equal to -9223372036854775807.
- `std::numeric_limits<long long>::max()` equals `LLONG_MAX`, which is greater than or equal to 9223372036854775807.
- `std::numeric_limits<unsigned long long>::max()` equals `ULLONG_MAX`, which is greater than or equal to 18446744073709551615.

For floating-point types `T`, `max()` is the maximum finite value while `min()` is the minimum positive normalized value. Additional members are provided for floating-point types, which are also implementation-defined but satisfy certain guarantees provided by the C standard through the `<cfloat>` header.

<li>The member `digits10` gives the number of decimal digits of precision.
<ul>
- `std::numeric_limits<float>::digits10` equals `FLT_DIG`, which is at least 6.
- `std::numeric_limits<double>::digits10` equals `DBL_DIG`, which is at least 10.
- `std::numeric_limits<long double>::digits10` equals `LDBL_DIG`, which is at least 10.

- `std::numeric_limits<float>::min_exponent10` equals `FLT_MIN_10_EXP`, which is at most -37.
<li>`std::numeric_limits<double>::min_exponent10` equals `DBL_MIN_10_EXP`, which is at most -37.
`std::numeric_limits<long double>::min_exponent10` equals `LDBL_MIN_10_EXP`, which is at most -37.</li>

- `std::numeric_limits<float>::max_exponent10` equals `FLT_MIN_10_EXP`, which is at least 37.
- `std::numeric_limits<double>::max_exponent10` equals `DBL_MIN_10_EXP`, which is at least 37.
- `std::numeric_limits<long double>::max_exponent10` equals `LDBL_MIN_10_EXP`, which is at least 37.



## Value representation of floating point types


The standard requires that `long double` provides at least as much precision as `double`, which provides at least as much precision as `float`; and that a `long double` can represent any value that a `double` can represent, while a `double` can represent any value that a `float` can represent. The details of the representation are, however, implementation-defined.

For a floating point type `T`, `std::numeric_limits<T>::radix` specifies the radix used by the representation of `T`.

If `std::numeric_limits<T>::is_iec559` is true, then the representation of `T` matches one of the formats defined by IEC 559 / IEEE 754.



## Overflow when converting from integer to signed integer


When either a signed or unsigned integer is converted to a signed integer type, and its value is not representable in the destination type, the value produced is implementation-defined. Example:

```cpp
// Suppose that on this implementation, the range of signed char is -128 to +127 and
// the range of unsigned char is 0 to 255
int x = 12345;
signed char sc = x;   // sc has an implementation-defined value
unsigned char uc = x; // uc is initialized to 57 (i.e., 12345 modulo 256)

```



## Underlying type (and hence size) of an enum


If the underlying type is not explicitly specified for an unscoped enumeration type, it is determined in an implementation-defined manner.

```cpp
enum E {
    RED,
    GREEN,
    BLUE,
};
using T = std::underlying_type<E>::type; // implementation-defined

```

However, the standard does require the underlying type of an enumeration to be no larger than `int` unless both `int` and `unsigned int` are unable to represent all the values of the enumeration. Therefore, in the above code, `T` could be `int`, `unsigned int`, or `short`, but not `long long`, to give a few examples.

Note that an enum has the same size (as returned by `sizeof`) as its underlying type.

