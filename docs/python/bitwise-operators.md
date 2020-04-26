# Bitwise Operators


Bitwise operations alter binary strings at the bit level. These operations are incredibly basic and are directly supported by the processor. These few operations are necessary in working with device drivers, low-level graphics, cryptography, and network communications. This section provides useful knowledge and examples of Python's bitwise operators.



## Bitwise NOT


The `~` operator will flip all of the bits in the number. Since computers use [signed number representations](https://en.wikipedia.org/wiki/Signed_number_representations) — most notably, the [two's complement notation](https://en.wikipedia.org/wiki/Two%27s_complement) to encode negative binary numbers where negative numbers are written with a leading one (1) instead of a leading zero (0).

This means that if you were using 8 bits to represent your two's-complement numbers, you would treat patterns from `0000 0000` to `0111 1111` to represent numbers from 0 to 127 and reserve `1xxx xxxx` to represent negative numbers.

Eight-bit two's-complement numbers

|Bits|Unsigned Value|Two's-complement Value
|------
|0000 0000|0|0
|0000 0001|1|1
|0000 0010|2|2
|0111 1110|126|126
|0111 1111|127|127
|1000 0000|128|-128
|1000 0001|129|-127
|1000 0010|130|-126
|1111 1110|254|-2
|1111 1111|255|-1

In essence, this means that whereas `1010 0110` has an unsigned value of 166 (arrived at by adding `(128 * 1) + (64 * 0) + (32 * 1) + (16 * 0) + (8 * 0) + (4 * 1) + (2 * 1) + (1 * 0)`), it has a two's-complement value of -90 (arrived at by adding `(128 * 1) - (64 * 0) - (32 * 1) - (16 * 0) - (8 * 0) - (4 * 1) - (2 * 1) - (1 * 0)`, and complementing the value).

In this way, negative numbers range down to -128 (`1000 0000`). Zero (0) is represented as `0000 0000`, and  minus one (-1) as `1111 1111`.

In general, though, this means `~n = -n - 1`.

```
# 0 = 0b0000 0000
~0
# Out: -1
# -1 = 0b1111 1111
    
# 1 = 0b0000 0001
~1
# Out: -2
# -2 = 1111 1110

# 2 = 0b0000 0010
~2
# Out: -3
# -3 = 0b1111 1101

# 123 = 0b0111 1011
~123
# Out: -124
# -124 = 0b1000 0100

```

**Note**, the overall effect of this operation when applied to positive numbers can be summarized:

> 
`~n -> -|n+1|`


And then, when applied to negative numbers, the corresponding effect is:

> 
`~-n -> |n-1|`


The following examples illustrate this last rule...

```
# -0 = 0b0000 0000
~-0
# Out: -1 
# -1 = 0b1111 1111
# 0 is the obvious exception to this rule, as -0 == 0 always
    
# -1 = 0b1000 0001
~-1
# Out: 0
# 0 = 0b0000 0000

# -2 = 0b1111 1110
~-2
# Out: 1
# 1 = 0b0000 0001

# -123 = 0b1111 1011
~-123
# Out: 122
# 122 = 0b0111 1010

```



## Bitwise XOR (Exclusive OR)


The `^` operator will perform a binary **XOR** in which a binary `1` is copied if and only if it is the value of exactly **one** operand. Another way of stating this is that the result is `1` only if the operands are different. Examples include:

```
# 0 ^ 0 = 0
# 0 ^ 1 = 1
# 1 ^ 0 = 1
# 1 ^ 1 = 0

# 60 = 0b111100
# 30 = 0b011110
60 ^ 30
# Out: 34
# 34 = 0b100010

bin(60 ^ 30)
# Out: 0b100010

```



## Bitwise AND


The `&amp;` operator will perform a binary **AND**, where a bit is copied if it exists in **both** operands. That means:

```
# 0 &amp; 0 = 0
# 0 &amp; 1 = 0
# 1 &amp; 0 = 0
# 1 &amp; 1 = 1

# 60 = 0b111100
# 30 = 0b011110
60 &amp; 30
# Out: 28
# 28 = 0b11100

bin(60 &amp; 30)
# Out: 0b11100

```



## Bitwise OR


The `|` operator will perform a binary &quot;or,&quot; where a bit is copied if it exists in either operand. That means:

```
# 0 | 0 = 0
# 0 | 1 = 1 
# 1 | 0 = 1
# 1 | 1 = 1

# 60 = 0b111100 
# 30 = 0b011110
60 | 30
# Out: 62
# 62 = 0b111110

bin(60 | 30)
# Out: 0b111110

```



## Bitwise Left Shift


The `<<` operator will perform a bitwise &quot;left shift,&quot; where the left operand's value is moved left by the number of bits given by the right operand.

```
# 2 = 0b10
2 << 2
# Out: 8
# 8 = 0b1000

bin(2 << 2)
# Out: 0b1000

```

Performing a left bit shift of `1` is equivalent to multiplication by `2`:

```
7 << 1
# Out: 14

```

Performing a left bit shift of `n` is equivalent to multiplication by `2**n`:

```
3 << 4
# Out: 48

```



## Bitwise Right Shift


The `>>` operator will perform a bitwise &quot;right shift,&quot; where the left operand's value is moved right by the number of bits given by the right operand.

```
# 8 = 0b1000
8 >> 2
# Out: 2
# 2 = 0b10

bin(8 >> 2)
# Out: 0b10

```

Performing a right bit shift of `1` is equivalent to integer division by `2`:

```
36 >> 1
# Out: 18

15 >> 1
# Out: 7

```

Performing a right bit shift of `n` is equivalent to integer division by `2**n`:

```
48 >> 4
# Out: 3

59 >> 3
# Out: 7

```



## Inplace Operations


All of the Bitwise operators (except `~`) have their own in place versions

```
a = 0b001
a &amp;= 0b010 
# a = 0b000

a = 0b001
a |= 0b010 
# a = 0b011

a = 0b001
a <<= 2 
# a = 0b100

a = 0b100
a >>= 2 
# a = 0b001

a = 0b101
a ^= 0b011 
# a = 0b110

```



#### Syntax


<li>
x << y  # Bitwise Left Shift
</li>
<li>
x >> y # Bitwise Right Shift
</li>
<li>
x &amp; y  # Bitwise AND
</li>
<li>
x | y  # Bitwise OR
</li>
<li>
~ x    # Bitwise NOT
</li>
<li>
x ^ y  # Bitwise XOR
</li>

