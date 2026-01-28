---
metaTitle: "C++ | Floating Point Arithmetic"
description: "Floating Point Numbers are Weird"
---

# Floating Point Arithmetic



## Floating Point Numbers are Weird


The first mistake that nearly every single programmer makes is presuming that this code will work as intended:

```cpp
float total = 0;
for(float a = 0; a != 2; a += 0.01f) {
    total += a;
}

```

The novice programmer assumes that this will sum up every single number in the range `0, 0.01, 0.02, 0.03, ..., 1.97, 1.98, 1.99`, to yield the result `199`—the mathematically correct answer.

Two things happen that make this untrue:

1. The program as written never concludes. `a` never becomes equal to `2`, and the loop never terminates.
1. If we rewrite the loop logic to check `a < 2` instead, the loop terminates, but the total ends up being something different from `199`. On IEEE754-compliant machines, it will often sum up to about `201` instead.

The reason that this happens is that **Floating Point Numbers represent Approximations of their assigned values**.

The classical example is the following computation:

```cpp
double a = 0.1;
double b = 0.2;
double c = 0.3;
if(a + b == c)
    //This never prints on IEEE754-compliant machines
    std::cout << "This Computer is Magic!" << std::endl; 
else
    std::cout << "This Computer is pretty normal, all things considered." << std::endl;

```

Though what we the programmer see is three numbers written in base10, what the compiler (and the underlying hardware) see are binary numbers. Because `0.1`, `0.2`, and `0.3` require perfect division by `10`—which is quite easy in a base-10 system, but impossible in a base-2 system—these numbers have to be stored in imprecise formats, similar to how the number `1/3` has to be stored in the imprecise form `0.333333333333333...` in base-10.

```cpp
//64-bit floats have 53 digits of precision, including the whole-number-part.
double a =     0011111110111001100110011001100110011001100110011001100110011010; //imperfect representation of 0.1
double b =     0011111111001001100110011001100110011001100110011001100110011010; //imperfect representation of 0.2
double c =     0011111111010011001100110011001100110011001100110011001100110011; //imperfect representation of 0.3
double a + b = 0011111111010011001100110011001100110011001100110011001100110100; //Note that this is not quite equal to the "canonical" 0.3!

```

