---
metaTitle: "C++ | Arithmitic Metaprogramming"
description: "Calculating power in O(log n)"
---

# Arithmitic Metaprogramming


These are example of using C++ template metaprogramming in processing arithmitic operations in compile time.



## Calculating power in O(log n)


This example shows an efficient way of calculating power using template metaprogramming.

```cpp
template <int base, unsigned int exponent>
struct power
{
    static const int halfvalue = power<base, exponent / 2>::value;
    static const int value = halfvalue * halfvalue * power<base, exponent % 2>::value;
};

template <int base>
struct power<base, 0>
{
   static const int value = 1;
   static_assert(base != 0, "power<0, 0> is not allowed");
};


template <int base>
struct power<base, 1>
{
    static const int value = base;
};

```

Example Usage:

```cpp
std::cout << power<2, 9>::value;

```

This one also handles negative exponents:

```cpp
template <int base, int exponent>
struct powerDouble
{
    static const int exponentAbs = exponent < 0 ? (-exponent) : exponent;
    static const int halfvalue = powerDouble<base, exponentAbs / 2>::intermediateValue;
    static const int intermediateValue = halfvalue * halfvalue * powerDouble<base, exponentAbs % 2>::intermediateValue;

    constexpr static double value = exponent < 0 ? (1.0 / intermediateValue) : intermediateValue;

};

template <int base>
struct powerDouble<base, 0>
{    
    static const int intermediateValue = 1;
    constexpr static double value = 1;
    static_assert(base != 0, "powerDouble<0, 0> is not allowed");
};


template <int base>
struct powerDouble<base, 1>
{
    static const int intermediateValue = base;
    constexpr static double value = base;
};


int main()
{
    std::cout << powerDouble<2,-3>::value;
}

```

