---
metaTitle: "C++ | static_assert"
description: "static_assert"
---

# static_assert



## static_assert


Assertations mean that a condition should be checked and if it's false, it's an error. For `static_assert()`, this is done compile-time.

```cpp
template<typename T>
T mul10(const T t)
{
    static_assert( std::is_integral<T>::value, "mul10() only works for integral types" );
    return (t << 3) + (t << 1);
}

```

A `static_assert()` has a mandatory first parameter, the condition, that is a bool constexpr. It **might** have a second parameter, the message, that is a string literal. From C++17, the second parameter is optional; before that, it's mandatory.

```cpp
template<typename T>
T mul10(const T t)
{
    static_assert(std::is_integral<T>::value);
    return (t << 3) + (t << 1);
}

```

It is used when:

- In general, a verification at compile-time is required on some type on constexpr value
- A template function needs to verify certain properties of a type passed to it
<li>One wants to write test cases for:
<ul>
- template metafunctions
- constexpr functions
- macro metaprogramming

Note that `static_assert()` does not participate in [SFINAE](https://stackoverflow.com/documentation/c%2b%2b/1169/sfinae-substitution-failure-is-not-an-error): thus, when additional overloads / specializations are possible, one should not use it instead of template metaprogramming techniques (like `std::enable_if<>`). It might be used in template code when the expected overload / specialization is already found, but further verifications are required. In such cases, it might provide more concrete error message(s) than relying on SFINAE for this.



#### Syntax


- static_assert( **bool_constexpr**, **message** )
- static_assert( **bool_constexpr** ) /* Since C++17 */



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|**bool_constexpr**|Expression to check
|**message**|Message to print when **bool_constexpr** is **false**



#### Remarks


Unlike [runtime assertions](http://en.cppreference.com/w/cpp/error/assert), static assertions are checked at compile-time and are also enforced when compiling optimized builds.

