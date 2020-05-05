---
metaTitle: "C++ | Bit Operators"
description: "| - bitwise OR, ^ - bitwise XOR (exclusive OR), & - bitwise AND, << - left shift, >> - right shift, ~ - bitwise NOT (unary complement)"
---

# Bit Operators



## | - bitwise OR


```cpp
int a = 5;     // 0101b  (0x05)
int b = 12;    // 1100b  (0x0C)
int c = a | b; // 1101b  (0x0D)

std::cout << "a = " << a << ", b = " << b << ", c = " << c << std::endl;

```

**Output**

`a = 5, b = 12, c = 13`

**Why**

A bit wise `OR` operates on the bit level and uses the following Boolean truth table:

```cpp
true OR true = true
true OR false = true
false OR false = false

```

When the binary value for `a` (`0101`) and the binary value for `b` (`1100`) are `OR`'ed together we get the binary value of `1101`:

```cpp
int a = 0 1 0 1
int b = 1 1 0 0 |
        ---------
int c = 1 1 0 1

```

The bit wise OR does not change the value of the original values unless specifically assigned to using the bit wise assignment compound operator `|=`:

```cpp
int a = 5;  // 0101b  (0x05)
a |= 12;    // a = 0101b | 1101b

```



## ^ - bitwise XOR (exclusive OR)


```cpp
int a = 5;     // 0101b  (0x05)
int b = 9;     // 1001b  (0x09)
int c = a ^ b; // 1100b  (0x0C)

std::cout << "a = " << a << ", b = " << b << ", c = " << c << std::endl;

```

**Output**

`a = 5, b = 9, c = 12`

**Why**

A bit wise `XOR` (exclusive or) operates on the bit level and uses the following Boolean truth table:

```cpp
true OR true = false
true OR false = true
false OR false = false

```

Notice that with an XOR operation `true OR true = false` where as with operations `true AND/OR true = true`, hence the exclusive nature of the XOR operation.

Using this, when the binary value for `a` (`0101`) and the binary value for `b` (`1001`) are `XOR`'ed together we get the binary value of `1100`:

```cpp
int a = 0 1 0 1
int b = 1 0 0 1 ^
        ---------
int c = 1 1 0 0

```

The bit wise XOR does not change the value of the original values unless specifically assigned to using the bit wise assignment compound operator `^=`:

```cpp
int a = 5;  // 0101b  (0x05)
a ^= 9;    // a = 0101b ^ 1001b

```

The bit wise XOR can be utilized in many ways and is often utilized in bit mask operations for encryption and compression.

**Note:** The following example is often shown as an example of a nice trick. But should not be used in production code (there are better ways `std::swap()` to achieve the same result).

You can also utilize an XOR operation to swap two variables without a temporary:

```cpp
int a = 42;
int b = 64;

// XOR swap
a ^= b;
b ^= a;
a ^= b;

std::cout << "a = " << a << ", b = " << b << "\n";

```

To productionalize this you need to add a check to make sure it can be used.

```cpp
void doXORSwap(int& a, int& b)
{
    // Need to add a check to make sure you are not swapping the same
    // variable with itself. Otherwise it will zero the value.
    if (&a != &b)
    {
        // XOR swap
        a ^= b;
        b ^= a;
        a ^= b;
    }
}

```

So though it looks like a nice trick in isolation it is not useful in real code.
xor is not a base logical operation,but a combination of others:
a^c=~(a&c)&(a|c)

also in 2015+ compilers variables may be assigned as binary:

```cpp
int cn=0b0111;

```



## & - bitwise AND


```cpp
int a = 6;     // 0110b  (0x06)
int b = 10;    // 1010b  (0x0A)
int c = a & b; // 0010b  (0x02)

std::cout << "a = " << a << ", b = " << b << ", c = " << c << std::endl;

```

**Output**

`a = 6, b = 10, c = 2`

**Why**

A bit wise `AND` operates on the bit level and uses the following Boolean truth table:

```cpp
TRUE  AND TRUE  = TRUE
TRUE  AND FALSE = FALSE
FALSE AND FALSE = FALSE

```

When the binary value for `a` (`0110`) and the binary value for `b` (`1010`) are `AND`'ed together we get the binary value of `0010`:

```cpp
int a = 0 1 1 0
int b = 1 0 1 0 &
        ---------
int c = 0 0 1 0

```

The bit wise AND does not change the value of the original values unless specifically assigned to using the bit wise assignment compound operator `&=`:

```cpp
int a = 5;  // 0101b  (0x05)
a &= 10;    // a = 0101b & 1010b

```



## << - left shift


```cpp
int a = 1;      // 0001b
int b = a << 1; // 0010b

std::cout << "a = " << a << ", b = " << b << std::endl;

```

**Output**

`a = 1, b = 2`

**Why**

The left bit wise shift will shift the bits of the left hand value (`a`) the number specified on the right (`1`), essentially padding the least significant bits with 0's, so shifting the value of `5` (binary `0000 0101`) to the left 4 times (e.g. `5 << 4`) will yield the value of `80` (binary `0101 0000`).  You might note that shifting a value to the left 1 time is also the same as multiplying the value by 2, example:

```cpp
int a = 7;
while (a < 200) {
    std::cout << "a = " << a << std::endl;
    a <<= 1;
}

a = 7;
while (a < 200) {
    std::cout << "a = " << a << std::endl;
    a *= 2;
}

```

But it should be noted that the left shift operation will shift *all* bits to the left, including the sign bit, example:

```cpp
int a = 2147483647; // 0111 1111 1111 1111 1111 1111 1111 1111
int b = a << 1;     // 1111 1111 1111 1111 1111 1111 1111 1110

std::cout << "a = " << a << ", b = " << b << std::endl;

```

Possible output: `a = 2147483647, b = -2`

While some compilers will yield results that seem expected, it should be noted that if you left shift a signed number so that the sign bit is affected, the result is **undefined**. It is also **undefined** if the number of bits you wish to shift by is a negative number or is larger than the number of bits the type on the left can hold, example:

```cpp
int a = 1;
int b = a << -1;  // undefined behavior
char c = a << 20; // undefined behavior

```

The bit wise left shift does not change the value of the original values unless specifically assigned to using the bit wise assignment compound operator `<<=`:

```cpp
int a = 5;  // 0101b
a <<= 1;    // a = a << 1;

```



## >> - right shift


```cpp
int a = 2;      // 0010b
int b = a >> 1; // 0001b

std::cout << "a = " << a << ", b = " << b << std::endl;

```

**Output**

`a = 2, b = 1`

**Why**

The right bit wise shift will shift the bits of the left hand value (`a`) the number specified on the right (`1`); it should be noted that while the operation of a right shift is standard, what happens to the bits of a right shift on a *signed negative* number is *implementation defined* and thus cannot be guaranteed to be portable, example:

```cpp
int a = -2;    
int b = a >> 1; // the value of b will be depend on the compiler

```

It is also undefined if the number of bits you wish to shift by is a negative number, example:

```cpp
int a = 1;
int b = a >> -1;  // undefined behavior

```

The bit wise right shift does not change the value of the original values unless specifically assigned to using the bit wise assignment compound operator `>>=`:

```cpp
int a = 2;  // 0010b
a >>= 1;    // a = a >> 1;

```



## ~ - bitwise NOT (unary complement)


```cpp
unsigned char a = 234;  // 1110 1010b  (0xEA)
unsigned char b = ~a;   // 0001 0101b  (0x15)

std::cout << "a = " << static_cast<int>(a) <<
             ", b = " << static_cast<int>(b) << std::endl;

```

**Output**

`a = 234, b = 21`

**Why**

A bit wise `NOT` (unary complement) operates on the bit level and simply flips each bit. If it's a `1`, it's changed to a `0`, if it's a `0`, it's changed to a `1`. The bit wise NOT has the same effect as XOR'ing a value against the max value for a specific type:

```cpp
unsigned char a = 234;  // 1110 1010b  (0xEA)
unsigned char b = ~a;   // 0001 0101b  (0x15)
unsigned char c = a ^ ~0;

```

The bit wise NOT can also be a convenient way to check the maximum value for a specific integral type:

```cpp
unsigned int i = ~0;
unsigned char c = ~0;

std::cout << "max uint = " << i << std::endl <<
             "max uchar = " << static_cast<short>(c) << std::endl;

```

The bit wise NOT does not change the value of the original value and does not have a compound assignment operator, so you can not do `a ~= 10` for example.

The *bit wise* NOT (`~`) should not be confused with the *logical* NOT (`!`); where a bit wise NOT will flip each bit, a logical NOT will use the whole value to do its operation on, in other words `(!1) != (~1)`



#### Remarks


Bit shift operations are not portable across all processor architectures, different processors can have different bit-widths. In other words, if you wrote

```cpp
int a = ~0;
int b = a << 1;

```

This value would be different on a 64 bit machine vs. on a 32 bit machine, or from an x86 based processor to a PIC based processor.

Endian-ness does not need to be taken into account for the bit wise operations themselves, that is, the right shift (`>>`) will shift the bits towards the least significant bit and an XOR will perform an exclusive or on the bits. Endian-ness only needs to be taken into account with the data itself, that is, if endian-ness is a concern for your application, it's a concern regardless of bit wise operations.

