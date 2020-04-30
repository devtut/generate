---
metaTitle: "User-Defined Literals"
description: "Self-made user-defined literal for binary, User-defined literals with long double values, Standard user-defined literals for duration, Standard user-defined literals for strings, Standard user-defined literals for complex"
---

# User-Defined Literals



## Self-made user-defined literal for binary


Despite you can write a binary number in C++14 like:

`int number =0b0001'0101; // ==21`

here comes a famous example with a self-made implementation for binary numbers:

Note: The whole template expanding program is running at compile time.

```cpp
template< char FIRST, char... REST > struct binary
{
    static_assert( FIRST == '0' || FIRST == '1', "invalid binary digit" ) ;
    enum { value = ( ( FIRST - '0' ) << sizeof...(REST) ) + binary<REST...>::value  } ;
};

template<> struct binary<'0'> { enum { value = 0 } ; };
template<> struct binary<'1'> { enum { value = 1 } ; };


// raw literal operator
template<  char... LITERAL > inline
constexpr unsigned int operator "" _b() { return binary<LITERAL...>::value ; }

// raw literal operator
template<  char... LITERAL > inline
constexpr unsigned int operator "" _B() { return binary<LITERAL...>::value ; }

#include <iostream>

int main()
{
    std::cout  << 10101_B << ", " << 011011000111_b  << '\n' ; // prints 21, 1735
}

```



## User-defined literals with long double values


```cpp
#include <iostream>

long double operator"" _km(long double val)
{
    return val * 1000.0;
}

long double operator"" _mi(long double val)
{
    return val * 1609.344;
}

int main()
{
    std::cout << "3 km = " << 3.0_km << " m\n";
    std::cout << "3 mi = " << 3.0_mi << " m\n";
    return 0;
}

```

The output of this program is the following:

```cpp
3 km = 3000 m
3 mi = 4828.03 m

```



## Standard user-defined literals for duration


Those following duration user literals are declared in the `namespace` `std::literals::chrono_literals`, where both `literals` and `chrono_literals` are [inline namespaces](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces/4556/inline-namespace#t=20160725201733952379). Access to these operators can be gained with `using namespace std::literals`, `using namespace std::chrono_literals`, and `using namespace std::literals::chrono_literals`.

```cpp
#include <chrono>
#include <iostream>

int main()
{
    using namespace std::literals::chrono_literals;

    std::chrono::nanoseconds t1 = 600ns;
    std::chrono::microseconds t2 = 42us;
    std::chrono::milliseconds t3 = 51ms;
    std::chrono::seconds t4 = 61s;
    std::chrono::minutes t5 = 88min;
    auto t6 = 2 * 0.5h;

    auto total = t1 + t2 + t3 + t4 + t5 + t6;

    std::cout.precision(13);
    std::cout << total.count() << " nanoseconds" << std::endl; // 8941051042600 nanoseconds
    std::cout << std::chrono::duration_cast<std::chrono::hours>(total).count()
              << " hours" << std::endl; // 2 hours
}

```



## Standard user-defined literals for strings


Those following string user literals are declared in the `namespace` `std::literals::string_literals`, where both `literals` and `string_literals` are [inline namespaces](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces/4556/inline-namespace#t=20160725201733952379). Access to these operators can be gained with `using namespace std::literals`, `using namespace std::string_literals`, and `using namespace std::literals::string_literals`.

```cpp
#include <codecvt>
#include <iostream>
#include <locale>
#include <string>

int main()
{
    using namespace std::literals::string_literals;

    std::string s = "hello world"s;
    std::u16string s16 = u"hello world"s;
    std::u32string s32 = U"hello world"s;
    std::wstring ws = L"hello world"s;
    
    std::cout << s << std::endl;

    std::wstring_convert<std::codecvt_utf8_utf16<char16_t>, char16_t> utf16conv;
    std::cout << utf16conv.to_bytes(s16) << std::endl;

    std::wstring_convert<std::codecvt_utf8_utf16<char32_t>, char32_t> utf32conv;
    std::cout << utf32conv.to_bytes(s32) << std::endl;

    std::wcout << ws << std::endl;
}

```

Note:

Literal string may containing `\0`

```cpp
std::string s1 = "foo\0\0bar";  // constructor from C-string: results in "foo"s
std::string s2 = "foo\0\0bar"s; // That string contains 2 '\0' in its middle

```



## Standard user-defined literals for complex


Those following complex user literals are declared in the `namespace` `std::literals::complex_literals`, where both `literals` and `complex_literals` are [inline namespaces](http://stackoverflow.com/documentation/c%2b%2b/495/namespaces/4556/inline-namespace#t=20160725201733952379). Access to these operators can be gained with `using namespace std::literals`, `using namespace std::complex_literals`, and `using namespace std::literals::complex_literals`.

```cpp
#include <complex>
#include <iostream>

int main()
{
    using namespace std::literals::complex_literals;
    
    std::complex<double> c = 2.0 + 1i;        // {2.0, 1.}
    std::complex<float> cf = 2.0f + 1if;      // {2.0f, 1.f}
    std::complex<long double> cl = 2.0L + 1il; // {2.0L, 1.L}
    
    std::cout << "abs" << c << " = " << abs(c) << std::endl;   // abs(2,1) = 2.23607
    std::cout << "abs" << cf << " = " << abs(cf) << std::endl; // abs(2,1) = 2.23607
    std::cout << "abs" << cl << " = " << abs(cl) << std::endl; // abs(2,1) = 2.23607
}

```

