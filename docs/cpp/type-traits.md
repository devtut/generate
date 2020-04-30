---
metaTitle: "Type Traits"
description: "Standard type traits, Type Properties, Type relations with std::is_same<T, T> , Fundamental type traits"
---

# Type Traits



## Standard type traits


The `type_traits` header contains a set of template classes and helpers to transform and check properties of types at compile-time.

These traits are typically used in templates to check for user errors, support generic programming, and allow for optimizations.

Most type traits are used to check if a type fulfils some criteria. These have the following form:

```cpp
template <class T> struct is_foo;

```

If the template class is instantiated with a type which fulfils some criteria `foo`, then  `is_foo<T>` inherits from `std::integral_constant<bool,true>` (a.k.a. `std::true_type`), otherwise it inherits from `std::integral_constant<bool,false>` (a.k.a. `std::false_type`). This gives the trait the following members:

### Constants

`static constexpr bool value`

`true` if `T` fulfils the criteria `foo`, `false` otherwise

### Functions

`operator bool`

Returns `value`

`bool operator()`

Returns `value`

### Types

|Name|Definition
|------
|`value_type`|`bool`
|`type`|`std::integral_constant<bool,value>`

The trait can then be used in constructs such as [`static_assert`](http://stackoverflow.com/documentation/c%2b%2b/3822/static-assert#t=201607281007230765701) or [`std::enable_if`](http://stackoverflow.com/documentation/c%2b%2b/1169/sfinae-substitution-failure-is-not-an-error/3777/enable-if#t=201607281006475141537). An example with `std::is_pointer`:

```cpp
template <typename T>
void i_require_a_pointer (T t) {
    static_assert(std::is_pointer<T>::value, "T must be a pointer type");
}

//Overload for when T is not a pointer type
template <typename T>
typename std::enable_if<!std::is_pointer<T>::value>::type
does_something_special_with_pointer (T t) {
    //Do something boring
}

//Overload for when T is a pointer type
template <typename T>
typename std::enable_if<std::is_pointer<T>::value>::type 
does_something_special_with_pointer (T t) {
    //Do something special
}

```

There are also various traits which transform types, such as `std::add_pointer` and `std::underlying_type`. These traits generally expose a single `type` member type which contains the transformed type. For example, `std::add_pointer<int>::type` is `int*`.



## Type Properties


Type properties compare the modifiers that can be placed upon different variables. The usefulness of these type traits is not always obvious.

**Note:** The example below would only offer an improvement on a non-optimizing compiler. It is a simple a proof of concept, rather than complex example.

e.g. Fast divide by four.

```cpp
template<typename T>
inline T FastDivideByFour(cont T &var) {
  // Will give an error if the inputted type is not an unsigned integral type.    
  static_assert(std::is_unsigned<T>::value && std::is_integral<T>::value,
    "This function is only designed for unsigned integral types.");
  return (var >> 2);
}

```

**Is Constant:**

This will evaluate as true when type is constant.

```cpp
std::cout << std::is_const<const int>::value << "\n"; // Prints true.
std::cout << std::is_const<int>::value << "\n"; // Prints false.

```

**Is Volatile:**

This will evaluate as true when the type is volatile.

```cpp
std::cout << std::is_volatile<static volatile int>::value << "\n"; // Prints true.
std::cout << std::is_const<const int>::value << "\n"; // Prints false.

```

**Is signed:**

This will evaluate as true for all signed types.

```cpp
std::cout << std::is_signed<int>::value << "\n"; // Prints true.
std::cout << std::is_signed<float>::value << "\n"; // Prints true.
std::cout << std::is_signed<unsigned int>::value << "\n"; // Prints false.
std::cout << std::is_signed<uint8_t>::value << "\n"; // Prints false.

```

**Is Unsigned:**

Will evaluate as true for all unsigned types.

```cpp
std::cout << std::is_unsigned<unsigned int>::value << "\n"; // Prints true.
std::cout << std::is_signed<uint8_t>::value << "\n"; // Prints true.
std::cout << std::is_unsigned<int>::value << "\n"; // Prints false.
std::cout << std::is_signed<float>::value << "\n"; // Prints false.

```



## Type relations with std::is_same<T, T> 


The `std::is_same<T, T>` type relation is used to compare two types. It will evaluate as boolean, true if the types are the same and false if otherwise.

e.g.

```cpp
// Prints true on most x86 and x86_64 compilers.
std::cout << std::is_same<int, int32_t>::value << "\n";
// Prints false on all compilers.
std::cout << std::is_same<float, int>::value << "\n";
// Prints false on all compilers.
std::cout  << std::is_same<unsigned int, int>::value << "\n";

```

The `std::is_same` type relation will also work regardless of typedefs. This is actually demonstrated in the first example when comparing `int == int32_t` however this is not entirely clear.

e.g.

```cpp
// Prints true on all compilers.
typedef int MyType
std::cout << std::is_same<int, MyType>::value <<  "\n";

```

**Using `std::is_same`  to warn when improperly using a templated class or function.**

When combined with a static assert the `std::is_same` template can be valuable tool in enforcing proper usage of templated classes and functions.

e.g. A function that only allows input from an `int` and a choice of two structs.

```cpp
#include <type_traits>
struct foo {
  int member;
  // Other variables
};

struct bar {
  char member;
};

template<typename T>
int AddStructMember(T var1, int var2) {
  // If type T != foo || T != bar then show error message.
  static_assert(std::is_same<T, foo>::value || 
    std::is_same<T, bar>::value,
    "This function does not support the specified type.");
  return var1.member + var2;
}

```



## Fundamental type traits


There are a number of different type traits that compare more general types.

**Is Integral:**

Evaluates as true for all integer types `int`, `char`, `long`, `unsigned int` etc.

```cpp
std::cout << std::is_integral<int>::value << "\n"; // Prints true.
std::cout << std::is_integral<char>::value << "\n"; // Prints true.
std::cout << std::is_integral<float>::value << "\n"; // Prints false.

```

**Is Floating Point:**

Evaluates as true for all floating point types. `float`,`double`, `long double` etc.

```cpp
std::cout << std::is_floating_point<float>::value << "\n"; // Prints true.
std::cout << std::is_floating_point<double>::value << "\n"; // Prints true.
std::cout << std::is_floating_point<char>::value << "\n"; // Prints false.

```

**Is Enum:**

Evaluates as true for all enumerated types, including `enum class`.

```cpp
enum fruit {apple, pair, banana};
enum class vegetable {carrot, spinach, leek};
std::cout << std::is_enum<fruit>::value << "\n"; // Prints true.
std::cout << std::is_enum<vegetable>::value << "\n"; // Prints true.
std::cout << std::is_enum<int>::value << "\n"; // Prints false.

```

**Is Pointer:**

Evaluates as true for all pointers.

```cpp
std::cout << std::is_pointer<int *>::value << "\n"; // Prints true.
typedef int* MyPTR;
std::cout << std::is_pointer<MyPTR>::value << "\n"; // Prints true.
std::cout << std::is_pointer<int>::value << "\n"; // Prints false.

```

**Is Class:**

Evaluates as true for all classes and struct, with the exception of `enum class`.

```cpp
struct FOO {int x, y;};
class BAR {
 public:
  int x, y;
};
enum class fruit {apple, pair, banana};
std::cout << std::is_class<FOO>::value << "\n"; // Prints true.
std::cout << std::is_class<BAR>::value << "\n"; // Prints true.
std::cout << std::is_class<fruit>::value << "\n"; // Prints false.
std::cout << std::is_class<int>::value << "\n"; // Prints false.

```



#### Remarks


Type traits are templated constructs used to compare and test the properties of different types at compile time. They can be used to provide conditional logic at compile time that can limit or extend the functionality of your code in a specific manner. The type traits library was brought in with the `c++11` standard which provides a number different functionalities. It is also possible to create your own type trait comparison templates.

