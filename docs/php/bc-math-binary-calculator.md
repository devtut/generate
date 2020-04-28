---
metaTitle: "BC Math (Binary Calculator)"
description: "Comparison between BCMath and float arithmetic operations, Using bcmath to read/write a binary long on 32-bit system"
---

# BC Math (Binary Calculator)


The Binary Calculator can be used to calculate with numbers of any size and precision up to 2147483647-1 decimals, in string format. The Binary Calculator is more precise than the float calculation of PHP.



## Comparison between BCMath and float arithmetic operations


### bcadd vs float+float

```
var_dump('10' + '-9.99');           // float(0.0099999999999998)
var_dump(10 + -9.99);               // float(0.0099999999999998)
var_dump(10.00 + -9.99);            // float(0.0099999999999998)
var_dump(bcadd('10', '-9.99', 20)); // string(22) "0.01000000000000000000"

```

### bcsub vs float-float

```
var_dump('10' - '9.99');           // float(0.0099999999999998)
var_dump(10 - 9.99);               // float(0.0099999999999998)
var_dump(10.00 - 9.99);            // float(0.0099999999999998)
var_dump(bcsub('10', '9.99', 20)); // string(22) "0.01000000000000000000"

```

### bcmul vs int*int

```
var_dump('5.00' * '2.00');            // float(10)
var_dump(5.00 * 2.00);                // float(10)
var_dump(bcmul('5.0', '2', 20));      // string(4) "10.0"
var_dump(bcmul('5.000', '2.00', 20)); // string(8) "10.00000"
var_dump(bcmul('5', '2', 20));        // string(2) "10"

```

### bcmul vs float*float

```
var_dump('1.6767676767' * '1.6767676767');           // float(2.8115498416259)
var_dump(1.6767676767 * 1.6767676767);               // float(2.8115498416259)
var_dump(bcmul('1.6767676767', '1.6767676767', 20)); // string(22) "2.81154984162591572289"

```

### bcdiv vs float/float

```
var_dump('10' / '3.01');           // float(3.3222591362126)
var_dump(10 / 3.01);               // float(3.3222591362126)
var_dump(10.00 / 3.01);            // float(3.3222591362126)
var_dump(bcdiv('10', '3.01', 20)); // string(22) "3.32225913621262458471"

```



## Using bcmath to read/write a binary long on 32-bit system


On 32-bit systems, integers greater than `0x7FFFFFFF` cannot be stored primitively, while integers between `0x0000000080000000` and `0x7FFFFFFFFFFFFFFF` can be stored primitively on 64-bit systems but not 32-bit systems (`signed long long`). However, since 64-bit systems and many other languages support storing `signed long long` integers, it is sometimes necessary to store this range of integers in exact value. There are several ways to do so, such as creating an array with two numbers, or converting the integer into its decimal human-readable form. This has several advantages, such as the convenience in presenting to the user, and the ability to manipulate it with bcmath directly.

The [`pack`](https://php.net/pack)/[`unpack`](https://php.net/unpack) methods can be used to convert between binary bytes and decimal form of the numbers (both of type `string`, but one is binary and one is ASCII), but they will always try to cast the ASCII string into a 32-bit int on 32-bit systems. The following snippet provides an alternative:

```
/** Use pack("J") or pack("p") for 64-bit systems */
function writeLong(string $ascii) : string {
    if(bccomp($ascii, "0") === -1) { // if $ascii < 0
        // 18446744073709551616 is equal to (1 << 64)
        // remember to add the quotes, or the number will be parsed as a float literal
        $ascii = bcadd($ascii, "18446744073709551616");
    }

    // "n" is big-endian 16-bit unsigned short. Use "v" for small-endian.
    return pack("n", bcmod(bcdiv($ascii, "281474976710656"), "65536")) .
        pack("n", bcmod(bcdiv($ascii, "4294967296"), "65536")) .
        pack("n", bcdiv($ascii, "65536"), "65536")) .
        pack("n", bcmod($ascii, "65536"));
}

function readLong(string $binary) : string {
    $result = "0";
    $result = bcadd($result, unpack("n", substr($binary, 0, 2)));
    $result = bcmul($result, "65536");
    $result = bcadd($result, unpack("n", substr($binary, 2, 2)));
    $result = bcmul($result, "65536");
    $result = bcadd($result, unpack("n", substr($binary, 4, 2)));
    $result = bcmul($result, "65536");
    $result = bcadd($result, unpack("n", substr($binary, 6, 2)));

    // if $binary is a signed long long
    // 9223372036854775808 is equal to (1 << 63) (note that this expression actually does not work even on 64-bit systems)
    if(bccomp($result, "9223372036854775808") !== -1) { // if $result >= 9223372036854775807
        $result = bcsub($result, "18446744073709551616"); // $result -= (1 << 64)
    }
    return $result;
}

```



#### Syntax


- string bcadd ( string $left_operand , string $right_operand [, int $scale = 0 ] )
- int bccomp ( string $left_operand , string $right_operand [, int $scale = 0 ] )
- string bcdiv ( string $left_operand , string $right_operand [, int $scale = 0 ] )
- string bcmod ( string $left_operand , string $modulus )
- string bcmul ( string $left_operand , string $right_operand [, int $scale = 0 ] )
- string bcpowmod ( string $left_operand , string $right_operand , string $modulus [, int $scale = 0 ] )
- bool bcscale ( int $scale )
- string bcsqrt ( string $operand [, int $scale = 0 ] )
- string bcsub ( string $left_operand , string $right_operand [, int $scale = 0 ] )



#### Parameters


|bcadd|**Add two arbitrary precision numbers.**
|------
|`left_operand`|The left operand, as a string.
|`right_operand`|The right operand, as a string.
|`scale`|A optional parameter to set the number of digits after the decimal place in the result.
|**bccomp**|****Compare two arbitrary precision numbers.****
|`left_operand`|The left operand, as a string.
|`right_operand`|The right operand, as a string.
|`scale`|A optional parameter to set the number of digits after the decimal place which will be used in the comparison.
|**bcdiv**|****Divide two arbitrary precision numbers.****
|`left_operand`|The left operand, as a string.
|`right_operand`|The right operand, as a string.
|`scale`|A optional parameter to set the number of digits after the decimal place in the result.
|**bcmod**|****Get modulus of an arbitrary precision number.****
|`left_operand`|The left operand, as a string.
|`modulus`|The modulus, as a string.
|**bcmul**|****Multiply two arbitrary precision numbers.****
|`left_operand`|The left operand, as a string.
|`right_operand`|The right operand, as a string.
|`scale`|A optional parameter to set the number of digits after the decimal place in the result.
|**bcpow**|****Raise an arbitrary precision number to another.****
|`left_operand`|The left operand, as a string.
|`right_operand`|The right operand, as a string.
|`scale`|A optional parameter to set the number of digits after the decimal place in the result.
|**bcpowmod**|****Raise an arbitrary precision number to another, reduced by a specified modulus.****
|`left_operand`|The left operand, as a string.
|`right_operand`|The right operand, as a string.
|`modulus`|The modulus, as a string.
|`scale`|A optional parameter to set the number of digits after the decimal place in the result.
|**bcscale**|****Set default scale parameter for all bc math functions.****
|`scale`|The scale factor.
|**bcsqrt**|****Get the square root of an arbitrary precision number.****
|`operand`|The operand, as a string.
|`scale`|A optional parameter to set the number of digits after the decimal place in the result.
|**bcsub**|****Subtract one arbitrary precision number from another.****
|`left_operand`|The left operand, as a string.
|`right_operand`|The right operand, as a string.
|`scale`|A optional parameter to set the number of digits after the decimal place in the result.



#### Remarks


For all BC functions, if the `scale` parameter is not set, it defaults to 0, which will make all operations integer operations.

