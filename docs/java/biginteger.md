---
metaTitle: "BigInteger"
description: "Initialization, BigInteger Mathematical Operations Examples, Comparing BigIntegers, Binary Logic Operations on BigInteger, Generating random BigIntegers"
---

# BigInteger


The [`BigInteger`](https://docs.oracle.com/javase/8/docs/api/java/math/BigInteger.html) class is used for mathematical operations involving large integers with magnitudes too large for primitive data types. For example 100-factorial is 158 digits - much larger than a `long` can represent. `BigInteger` provides analogues to all of Java's primitive integer operators, and all relevant methods from `java.lang.Math` as well as few other operations.



## Initialization


The `java.math.BigInteger` class provides operations analogues to all of Java's primitive integer operators and for all relevant methods from `java.lang.Math`. As the `java.math` package is not automatically made available you may have to import `java.math.BigInteger` before you can use the simple class name.

To convert `long` or `int` values to `BigInteger` use:

```java
long longValue = Long.MAX_VALUE;
BigInteger valueFromLong = BigInteger.valueOf(longValue); 

```

or, for integers:

```java
int intValue = Integer.MIN_VALUE; // negative
BigInteger valueFromInt = BigInteger.valueOf(intValue);

```

which will **widen** the `intValue` integer to long, using sign bit extension for negative values, so that negative values will stay negative.

To convert a numeric `String` to `BigInteger` use:

```java
String decimalString = "-1";
BigInteger valueFromDecimalString = new BigInteger(decimalString);

```

Following constructor is used to translate the String representation of a `BigInteger` in the specified radix into a `BigInteger`.

```java
String binaryString = "10";
int binaryRadix = 2;
BigInteger valueFromBinaryString = new BigInteger(binaryString , binaryRadix);

```

Java also supports direct conversion of bytes to an instance of `BigInteger`. Currently only signed and unsigned big endian encoding may be used:

```java
byte[] bytes = new byte[] { (byte) 0x80 }; 
BigInteger valueFromBytes = new BigInteger(bytes);

```

This will generate a `BigInteger` instance with value -128 as the first bit is interpreted as the sign bit.

```java
byte[] unsignedBytes = new byte[] { (byte) 0x80 };
int sign = 1; // positive
BigInteger valueFromUnsignedBytes = new BigInteger(sign, unsignedBytes);

```

This will generate a `BigInteger` instance with value 128 as the bytes are interpreted as unsigned number, and the sign is explicitly set to 1, a positive number.

There are predefined constants for common values:

- `BigInteger.ZERO` — value of "0".
- `BigInteger.ONE` — value of "1".
- `BigInteger.TEN` — value of "10".

There's also `BigInteger.TWO` (value of "2"), but you can't use it in your code because it's `private`.



## BigInteger Mathematical Operations Examples


BigInteger is in an immutable object, so you need to assign the results of any mathematical operation, to a new BigInteger instance.

**Addition:**
**10 + 10 = 20**

```java
BigInteger value1 = new BigInteger("10");
BigInteger value2 = new BigInteger("10");

BigInteger sum = value1.add(value2);
System.out.println(sum);

```

> 
output: 20


**Substraction:**
**10 - 9 = 1**

```java
BigInteger value1 = new BigInteger("10");
BigInteger value2 = new BigInteger("9");

BigInteger sub = value1.subtract(value2);
System.out.println(sub);

```

> 
output: 1


**Division:**
**10 / 5 = 2**

```java
BigInteger value1 = new BigInteger("10");
BigInteger value2 = new BigInteger("5");

BigInteger div = value1.divide(value2);
System.out.println(div);

```

> 
output: 2


**Division:**
**17/4 = 4**

```java
BigInteger value1 = new BigInteger("17");
BigInteger value2 = new BigInteger("4");

BigInteger div = value1.divide(value2);
System.out.println(div);

```

> 
output: 4


**Multiplication:**
**10 * 5 = 50**

```java
BigInteger value1 = new BigInteger("10");
BigInteger value2 = new BigInteger("5");

BigInteger mul = value1.multiply(value2);
System.out.println(mul);

```

> 
output: 50


**Power:**
**10 ^ 3 = 1000**

```java
BigInteger value1 = new BigInteger("10");
BigInteger power = value1.pow(3);
System.out.println(power);

```

> 
output: 1000


**Remainder:**
**10 % 6 = 4**

```java
BigInteger value1 = new BigInteger("10");
BigInteger value2 = new BigInteger("6");

BigInteger power = value1.remainder(value2);
System.out.println(power);

```

> 
output: 4


**GCD:** Greatest Common Divisor (GCD) for `12`and `18` is `6`.

```java
BigInteger value1 = new BigInteger("12");
BigInteger value2 = new BigInteger("18");

System.out.println(value1.gcd(value2));

```

> 
Output: 6


**Maximum** of two BigIntegers:

```java
BigInteger value1 = new BigInteger("10");
BigInteger value2 = new BigInteger("11");

System.out.println(value1.max(value2));

```

> 
Output: 11


**Minimum** of two BigIntegers:

```java
BigInteger value1 = new BigInteger("10");
BigInteger value2 = new BigInteger("11");

System.out.println(value1.min(value2));

```

> 
Output: 10




## Comparing BigIntegers


You can compare `BigIntegers` same as you compare `String` or other objects in Java.

For example:

```java
BigInteger one = BigInteger.valueOf(1);
BigInteger two = BigInteger.valueOf(2);

if(one.equals(two)){
    System.out.println("Equal");
}
else{
    System.out.println("Not Equal");
}

```

**Output:**

```java
Not Equal

```

**Note:**

In general, do **not** use use the `==` operator to compare BigIntegers

- `==` operator: compares references; i.e. whether two values refer to the same object
- `equals()` method: compares the content of two BigIntegers.

For example, BigIntegers should **not** be compared in the following way:

```java
if (firstBigInteger == secondBigInteger) {
  // Only checks for reference equality, not content equality!
}

```

Doing so may lead to unexpected behavior, as the `==` operator only checks for reference equality. If both BigIntegers contain the same content, but do not refer to the same object, **this will fail.** Instead, compare BigIntegers using  the `equals` methods, as explained above.

You can also compare your `BigInteger` to constant values like 0,1,10.

for example:

```java
BigInteger reallyBig = BigInteger.valueOf(1);
if(BigInteger.ONE.equals(reallyBig)){
    //code when they are equal.
}    

```

You can also compare two BigIntegers by using `compareTo()` method, as following:
`compareTo()` returns 3 values.

- **0:** When both are **equal**.
- **1:** When first is **greater than** second (the one in brackets).
- **-1:** When first is **less than** second.

```java
BigInteger reallyBig = BigInteger.valueOf(10);
BigInteger reallyBig1 = BigInteger.valueOf(100);

if(reallyBig.compareTo(reallyBig1) == 0){
    //code when both are equal.
}
else if(reallyBig.compareTo(reallyBig1) == 1){
    //code when reallyBig is greater than reallyBig1.
}
else if(reallyBig.compareTo(reallyBig1) == -1){
    //code when reallyBig is less than reallyBig1.
}

```



## Binary Logic Operations on BigInteger


BigInteger supports the binary logic operations that are available to `Number` types as well. As with all operations they are implemented by calling a method.

**Binary Or:**

```java
BigInteger val1 = new BigInteger("10");
BigInteger val2 = new BigInteger("9");

val1.or(val2);

```

> 
Output: 11 (which is equivalent to `10 | 9`)


**Binary And:**

```java
BigInteger val1 = new BigInteger("10");
BigInteger val2 = new BigInteger("9");

val1.and(val2);

```

> 
Output: 8 (which is equivalent to `10 & 9`)


**Binary Xor:**

```java
BigInteger val1 = new BigInteger("10");
BigInteger val2 = new BigInteger("9");

val1.xor(val2);

```

> 
Output: 3 (which is equivalent to `10 ^ 9`)


**RightShift:**

```java
BigInteger val1 = new BigInteger("10");

val1.shiftRight(1);   // the argument be an Integer    

```

> 
Output: 5 (equivalent to `10 >> 1`)


**LeftShift:**

```java
BigInteger val1 = new BigInteger("10");

val1.shiftLeft(1);   // here parameter should be Integer    

```

> 
Output: 20 (equivalent to `10 << 1`)


**Binary Inversion (Not) :**

```java
BigInteger val1 = new BigInteger("10");

val1.not();

```

> 
Output: 5


**NAND (And-Not):***

```java
BigInteger val1 = new BigInteger("10");
BigInteger val2 = new BigInteger("9");

val1.andNot(val2);

```

> 
Output: 7




## Generating random BigIntegers


The `BigInteger` class has a constructor dedicated to generate random `BigIntegers`, given an instance of `java.util.Random` and an `int` that specifies how many bits will the `BigInteger` have. Its usage is quite simple - when you call the constructor [`BigInteger(int, Random)`](https://docs.oracle.com/javase/8/docs/api/java/math/BigInteger.html#BigInteger-int-java.util.Random-) like this:

```java
BigInteger randomBigInt = new BigInteger(bitCount, sourceOfRandomness);

```

then you'll end up with a `BigInteger` whose value is between 0 (inclusive) and 2<sup>`bitCount`</sup> (exclusive).

This also means that `new BigInteger(2147483647, sourceOfRandomness)` may return all positive `BigInteger`s given enough time.

What will the `sourceOfRandomness` be is up to you. For example, a `new Random()` is good enough in most cases:

```java
new BigInteger(32, new Random());

```

If you're willing to give up speed for higher-quality random numbers, you can use a `new [SecureRandom](https://docs.oracle.com/javase/8/docs/api/java/security/SecureRandom.html)()` instead:

```java
import java.security.SecureRandom;

// somewhere in the code...
new BigInteger(32, new SecureRandom());

```

You can even implement an algorithm on-the-fly with an anonymous class! Note that **rolling out your own RNG algorithm **will** end you up with low quality randomness**, so always be sure to use an algorithm that is proven to be decent unless you want the resulting `BigInteger`(s) to be predictable.

```java
new BigInteger(32, new Random() {
    int seed = 0;

    @Override
    protected int next(int bits) {
        seed = ((22695477 * seed) + 1) & 2147483647; // Values shamelessly stolen from [Wikipedia](https://en.wikipedia.org/wiki/Linear_congruential_generator#Parameters_in_common_use)
        return seed;
    }
});

```



#### Syntax


- BigInteger variable_name = new BigInteger("12345678901234567890"); // a decimal integer as a string
- BigInteger variable_name = new BigInteger("1010101101010100101010011000110011101011000111110000101011010010", 2) // a binary integer as a string
- BigInteger variable_name = new BigInteger("ab54a98ceb1f0800", 16) // a hexadecimal integer as a string
- BigInteger variable_name = new BigInteger(64, new Random()); // a pseudorandom number generator supplying 64 bits to construct an integer
- BigInteger variable_name = new BigInteger(new byte[]{0, -85, 84, -87, -116, -21, 31, 10, -46}); // signed two's complement representation of an integer (big endian)
- BigInteger variable_name = new BigInteger(1, new byte[]{-85, 84, -87, -116, -21, 31, 10, -46}); // unsigned two's complement representation of a positive integer (big endian)



#### Remarks


`BigInteger` is immutable. Therefore you can't change its state. For example, the following won't work as `sum` won't be updated due to immutability.

```java
BigInteger sum = BigInteger.ZERO;
for(int i = 1; i < 5000; i++) {
   sum.add(BigInteger.valueOf(i));  
}

```

Assign the result to the `sum` variable to make it work.

```java
sum = sum.add(BigInteger.valueOf(i));

```

[The official documentation of `BigInteger`](https://docs.oracle.com/javase/8/docs/api/java/math/BigInteger.html) states that `BigInteger` implementations should support all integers between -2<sup>2147483647</sup> and 2<sup>2147483647</sup> (exclusive). This means `BigInteger`s can have more than 2 **billion** bits!

