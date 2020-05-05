---
metaTitle: "Java - Primitive Data Types"
description: "The char primitive, Primitive Types Cheatsheet, The float primitive, The int primitive, Memory consumption of primitives vs. boxed primitives, Converting Primitives, The double primitive, The long primitive, The boolean primitive, The byte primitive, Negative value representation, The short primitive"
---

# Primitive Data Types




## The char primitive


A `char` can store a single 16-bit Unicode character. A character literal is enclosed in single quotes

```java
char myChar = 'u';
char myChar2 = '5';
char myChar3 = 65; // myChar3 == 'A'

```

It has a minimum value of `\u0000` (0 in the decimal representation, also called the **null character**) and a maximum value of `\uffff` (65,535).

The default value of a `char` is `\u0000`.

```java
char defaultChar;    // defaultChar == \u0000

```

In order to define a char of `'` value an escape sequence (character preceded by a backslash) has to be used:

```java
char singleQuote = '\'';

```

There are also other escape sequences:

```java
char tab = '\t';
char backspace = '\b';
char newline = '\n';
char carriageReturn = '\r';
char formfeed = '\f';
char singleQuote = '\'';
char doubleQuote = '\"'; // escaping redundant here; '"' would be the same; however still allowed
char backslash = '\\';
char unicodeChar = '\uXXXX' // XXXX represents the Unicode-value of the character you want to display

```

You can declare a `char` of any Unicode character.

```java
char heart = '\u2764';
System.out.println(Character.toString(heart)); // Prints a line containing "❤".

```

It is also possible to add to a `char`. e.g. to iterate through every lower-case letter, you could do to the following:

```java
for (int i = 0; i <= 26; i++) {
    char letter = (char) ('a' + i);
    System.out.println(letter);
}

```



## Primitive Types Cheatsheet


Table showing size and values range of all primitive types:

|data type|numeric representation|range of values|default value
|---|---|---|---|---|---|---|---|---|---
|boolean|n/a|false and true|false
|byte|8-bit signed|-2<sup>7</sup> to 2<sup>7</sup> - 1|0
|||-128 to +127|
|short|16-bit signed|-2<sup>15</sup> to 2<sup>15</sup> - 1|0
|||-32,768 to +32,767|
|int|32-bit signed|-2<sup>31</sup> to 2<sup>31</sup> - 1|0
|||-2,147,483,648 to +2,147,483,647|
|long|64-bit signed|-2<sup>63</sup> to 2<sup>63</sup> - 1|0L
|||-9,223,372,036,854,775,808 to 9,223,372,036,854,775,807|
|float|32-bit floating point|1.401298464e-45 to 3.402823466e+38 (positive or negative)|0.0F
|double|64-bit floating point|4.94065645841246544e-324d to 1.79769313486231570e+308d (positive or negative)|0.0D
|char|16-bit unsigned|0 to 2<sup>16</sup> - 1|0
|||0 to 65,535|

Notes:

1. The Java Language Specification mandates that signed integral types (`byte` through `long`) use binary twos-complement representation, and the floating point types use standard IEE 754 binary floating point representations.
1. Java 8 and later provide methods to perform unsigned arithmetic operations on `int` and `long`.  While these methods allow a program to **treat** values of the respective types as unsigned, the types remain signed types.
1. The smallest floating point shown above are **subnormal**; i.e. they have less precision than a **normal** value.  The smallest normal numbers are 1.175494351e−38 and 2.2250738585072014e−308
1. A `char` conventionally represents a Unicode / UTF-16 **code unit**.
1. Although a `boolean` contains just one bit of information, its size in memory varies depending on the Java Virtual Machine implementation (see [boolean type](http://docs.oracle.com/javase/specs/jvms/se8/html/jvms-2.html#jvms-2.3.4)).



## The float primitive


A `float` is a single-precision 32-bit IEEE 754 floating point number. By default, decimals are interpreted as doubles. To create a `float`, simply append an `f` to the decimal literal.

```java
double doubleExample = 0.5;      // without 'f' after digits = double
float floatExample = 0.5f;       // with 'f' after digits    = float

float myFloat = 92.7f;           // this is a float...
float positiveFloat = 89.3f;     // it can be positive,
float negativeFloat = -89.3f;    // or negative
float integerFloat = 43.0f;      // it can be a whole number (not an int)
float underZeroFloat = 0.0549f;  // it can be a fractional value less than 0

```

Floats handle the five common arithmetical operations: addition, subtraction, multiplication, division, and modulus.

**Note: The following may vary slightly as a result of floating point errors. Some         results have been rounded for clarity and readability purposes (i.e. the printed result of the addition example was actually 34.600002).**

```java
// addition
float result = 37.2f + -2.6f;  // result: 34.6

// subtraction
float result = 45.1f - 10.3f;    // result: 34.8

// multiplication
float result = 26.3f * 1.7f;   // result: 44.71

// division
float result = 37.1f / 4.8f;   // result: 7.729166

// modulus
float result = 37.1f % 4.8f;   // result: 3.4999971

```

Because of the way floating point numbers are stored (i.e. in binary form), many numbers don't have an exact representation.

```java
float notExact = 3.1415926f;
System.out.println(notExact); // 3.1415925

```

While using `float` is fine for most applications, neither `float` nor `double` should be used to store exact representations of decimal numbers (like monetary amounts), or numbers where higher precision is required. Instead, the `BigDecimal` class should be used.

The default value of a `float` is **0.0f**.

```java
float defaultFloat;    // defaultFloat == 0.0f

```

A `float` is precise to roughly an error of 1 in 10 million.

**Note:** `Float.POSITIVE_INFINITY`, `Float.NEGATIVE_INFINITY`, `Float.NaN` are `float` values. `NaN` stands for results of operations that cannot be determined, such as dividing 2 infinite values. Furthermore `0f` and `-0f` are different, but `==` yields true:

```java
float f1 = 0f;
float f2 = -0f;
System.out.println(f1 == f2); // true
System.out.println(1f / f1); // Infinity
System.out.println(1f / f2); // -Infinity
System.out.println(Float.POSITIVE_INFINITY / Float.POSITIVE_INFINITY); // NaN

```



## The int primitive


A primitive data type such as `int` holds values directly into the variable that is using it, meanwhile a variable that was declared using `Integer` holds a reference to the value.

According to [java API](https://docs.oracle.com/javase/7/docs/api/): "The Integer class wraps a value of the primitive type int in an object. An object of type Integer contains a single field whose type is int."

By default, `int` is a 32-bit signed integer. It can store a minimum value of -2<sup>31</sup>, and a maximum value of 2<sup>31</sup> - 1.

```java
int example = -42;
int myInt = 284;
int anotherInt = 73;

int addedInts = myInt + anotherInt; // 284 + 73 = 357
int subtractedInts = myInt - anotherInt; // 284 - 73 = 211

```

If you need to store a number outside of this range, `long` should be used instead. Exceeding the value range of `int` leads to an integer overflow, causing the value exceeding the range to be added to the opposite site of the range (positive becomes negative and vise versa). The value is `((value - MIN_VALUE) % RANGE) + MIN_VALUE`, or `((value + 2147483648) % 4294967296) - 2147483648`

```java
int demo = 2147483647; //maximum positive integer
System.out.println(demo); //prints 2147483647
demo = demo + 1; //leads to an integer overflow
System.out.println(demo); // prints -2147483648

```

The maximum and minimum values of `int` can be found at:

```java
int high = Integer.MAX_VALUE;    // high == 2147483647
int low = Integer.MIN_VALUE;     // low == -2147483648

```

The default value of an `int` is **0**

```java
int defaultInt;    // defaultInt == 0

```



## Memory consumption of primitives vs. boxed primitives


|Primitive|Boxed Type|Memory Size of primitive / boxed
|---|---|---|---|---|---|---|---|---|---
|boolean|Boolean|1 byte / 16 bytes
|byte|Byte|1 byte / 16 bytes
|short|Short|2 bytes / 16 bytes
|char|Char|2 bytes / 16 bytes
|int|Integer|4 bytes / 16 bytes
|long|Long|8 bytes / 16 bytes
|float|Float|4 bytes / 16 bytes
|double|Double|8 bytes / 16 bytes

Boxed objects always require 8 bytes for type and memory management, and because the size of objects is always a multiple of 8, boxed types **all require 16 bytes total**.  In **addition**, each usage of a boxed object entails storing a reference which accounts for another 4 or 8 bytes, depending on the JVM and JVM options.

In data-intensive operations, memory consumption can have a major impact on performance. Memory consumption grows even more when using arrays: a `float[5]` array will require only 32 bytes; whereas a `Float[5]` storing 5 distinct non-null values will require 112 bytes total (on 64 bit without compressed pointers, this increases to 152 bytes).

### Boxed value caches

The space overheads of the boxed types can be mitigated to a degree by the boxed value caches.  Some of the boxed types implement a cache of instances.  For example, by default, the `Integer` class will cache instances to represent numbers in the range `-128` to `+127`. This does not, however, reduce the additional cost arising from the additional memory indirection.

If you create an instance of a boxed type either by autoboxing or by calling the static `valueOf(primitive)` method, the runtime system will attempt to use a cached value.  If your application uses a lot of values in the range that is cached, then this can substantially reduce the memory penalty of using boxed types.  Certainly, if you are creating boxed value instances "by hand", it is better to use `valueOf` rather than `new`.  (The `new` operation always creates a new instance.) If, however, the majority of your values are **not** in the cached range, it can be faster to call `new` and save the cache lookup.



## Converting Primitives


In Java, we can convert between integer values and floating-point values. Also, since every character corresponds to a number in the Unicode encoding, `char` types can be converted to and from the integer and floating-point types. `boolean` is the only primitive datatype that cannot be converted to or from any other primitive datatype.

There are two types of conversions: **widening conversion** and **narrowing conversion**.

A **widening conversion** is when a value of one datatype is converted to a value of another datatype that occupies more bits than the former. There is no issue of data loss in this case.

Correspondingly, A **narrowing conversion** is when a value of one datatype is converted to a value of another datatype that occupies fewer bits than the former. Data loss can occur in this case.

Java performs **widening conversions** automatically. But if you want to perform a **narrowing conversion** (if you are sure that no data loss will occur), then you can force Java to perform the conversion using a language construct known as a `cast`.

**Widening Conversion:**

```java
int a = 1;    
double d = a;    // valid conversion to double, no cast needed (widening)

```

**Narrowing Conversion:**

```java
double d = 18.96
int b = d;       // invalid conversion to int, will throw a compile-time error
int b = (int) d; // valid conversion to int, but result is truncated (gets rounded down)
                 // This is type-casting
                 // Now, b = 18

```



## The double primitive


A `double` is a double-precision 64-bit IEEE 754 floating point number.

```java
double example = -7162.37;
double myDouble = 974.21;
double anotherDouble = 658.7;

double addedDoubles = myDouble + anotherDouble; // 315.51
double subtractedDoubles = myDouble - anotherDouble; // 1632.91

double scientificNotationDouble = 1.2e-3;    // 0.0012

```

Because of the way floating point numbers are stored, many numbers don't have an exact representation.

```java
double notExact = 1.32 - 0.42; // result should be 0.9
System.out.println(notExact); // 0.9000000000000001

```

While using `double` is fine for most applications, neither `float` nor `double` should be used to store precise numbers such as currency. Instead, the `BigDecimal` class should be used

The default value of a `double` is **0.0d**

```java
public double defaultDouble;    // defaultDouble == 0.0

```

**Note:** `Double.POSITIVE_INFINITY`, `Double.NEGATIVE_INFINITY`, `Double.NaN` are `double` values. `NaN` stands for results of operations that cannot be determined, such as dividing 2 infinite values. Furthermore `0d` and `-0d` are different, but `==` yields true:

```java
double d1 = 0d;
double d2 = -0d;
System.out.println(d1 == d2); // true
System.out.println(1d / d1); // Infinity
System.out.println(1d / d2); // -Infinity
System.out.println(Double.POSITIVE_INFINITY / Double.POSITIVE_INFINITY); // NaN

```



## The long primitive


By default, `long` is a 64-bit signed integer (in Java 8, it can be either signed or unsigned). Signed, it can store a minimum value of -2<sup>63</sup>, and a maximum value of 2<sup>63</sup> - 1, and unsigned it can store a minimum value of 0 and a maximum value of 2<sup>64</sup> - 1

```java
long example = -42;
long myLong = 284;
long anotherLong = 73;

//an "L" must be appended to the end of the number, because by default,
//numbers are assumed to be the int type. Appending an "L" makes it a long
//as 549755813888 (2 ^ 39) is larger than the maximum value of an int (2^31 - 1),
//"L" must be appended 
long bigNumber = 549755813888L;

long addedLongs = myLong + anotherLong; // 284 + 73 = 357
long subtractedLongs = myLong - anotherLong; // 284 - 73 = 211

```

The maximum and minimum values of `long` can be found at:

```java
long high = Long.MAX_VALUE;    // high == 9223372036854775807L
long low = Long.MIN_VALUE;     // low == -9223372036854775808L

```

The default value of a `long` is **0L**

```java
long defaultLong;    // defaultLong == 0L

```

Note: letter "L" appended at the end of `long` literal is case insensitive, however it is good practice to use capital as it is easier to distinct from digit one:

```java
2L == 2l;            // true

```

Warning: Java caches Integer objects instances from the range -128 to 127. The reasoning is explained here: [https://blogs.oracle.com/darcy/entry/boxing_and_caches_integer_valueof](https://blogs.oracle.com/darcy/entry/boxing_and_caches_integer_valueof)

The following results can be found:

```java
Long val1 = 127L;
Long val2 = 127L;

System.out.println(val1 == val2); // true

Long val3 = 128L;
Long val4 = 128L;

System.out.println(val3 == val4); // false

```

To properly compare 2 Object Long values, use the following code(From Java 1.7 onward):

```java
Long val3 = 128L;
Long val4 = 128L;

System.out.println(Objects.equal(val3, val4)); // true

```

Comparing a primitive long to an Object long will not result in a false negative like comparing 2 objects with == does.



## The boolean primitive


A `boolean` can store one of two values, either `true` or `false`

```java
boolean foo = true;
System.out.println("foo = " + foo);                // foo = true

boolean bar = false;
System.out.println("bar = " + bar);                // bar = false

boolean notFoo = !foo;
System.out.println("notFoo = " + notFoo);          // notFoo = false

boolean fooAndBar = foo && bar;
System.out.println("fooAndBar = " + fooAndBar);    // fooAndBar = false

boolean fooOrBar = foo || bar;
System.out.println("fooOrBar = " + fooOrBar);      // fooOrBar = true

boolean fooXorBar = foo ^ bar;
System.out.println("fooXorBar = " + fooXorBar);    // fooXorBar = true

```

The default value of a `boolean` is **false**

```java
boolean defaultBoolean;    // defaultBoolean == false

```



## The byte primitive


A `byte` is a 8-bit signed integer. It can store a minimum value of -2<sup>7</sup> (-128), and a maximum value of 2<sup>7</sup> - 1 (127)

```java
byte example = -36;
byte myByte = 96;
byte anotherByte = 7;

byte addedBytes = (byte) (myByte + anotherByte); // 103
byte subtractedBytes = (byte) (myBytes - anotherByte); // 89

```

The maximum and minimum values of `byte` can be found at:

```java
byte high = Byte.MAX_VALUE;        // high == 127
byte low = Byte.MIN_VALUE;         // low == -128

```

The default value of a `byte` is **0**

```java
byte defaultByte;    // defaultByte == 0

```



## Negative value representation


Java and most other languages store negative integral numbers in a representation called **2's complement** notation.

For a unique binary representation of a data type using `n` bits, values are encoded like this:

The least significant `n-1` bits store a positive integral number `x` in integral representation. Most significant value stores a bit vith value `s`. The value repesented by those bits is

x - s * 2<sup>n-1</sup>

i.e. if the most significant bit is 1, then a value that is just by 1 larger than the number you could represent with the other bits (`2<sup>n-2</sup> + 2<sup>n-3</sup> + ... + 2<sup>1</sup> + 2<sup>0</sup> = 2<sup>n-1</sup> - 1`) is subtracted allowing a unique binary representation for each value from - 2<sup>n-1</sup> (s = 1; x = 0) to 2<sup>n-1</sup> - 1 (s = 0; x = 2<sup>n-1</sup> - 1).

This also has the nice side effect, that you can add the binary representations as if they were positive binary numbers:

```java
(~i) + 1

```

**Example:** taking the negative value of 0 (`byte`):

The result of negating `0`, is `11111111`. Adding 1 gives a value of `100000000` (9 bits). Because a `byte` can only store 8 bits, the leftmost value is truncated, and  the result is `00000000`

|Original|Process|Result
|---|---|---|---|---|---|---|---|---|---
|0 (00000000)|Negate|-0 (11111111)
|11111111|Add 1 to binary|100000000
|100000000|Truncate to 8 bits|00000000 (-0 equals 0)



## The short primitive


A `short` is a 16-bit signed integer. It has a minimum value of -2<sup>15</sup> (-32,768), and a maximum value of 2<sup>15</sup> ‑1 (32,767)

```java
short example = -48;
short myShort = 987;
short anotherShort = 17;

short addedShorts = (short) (myShort + anotherShort); // 1,004
short subtractedShorts = (short) (myShort - anotherShort); // 970

```

The maximum and minimum values of `short` can be found at:

```java
short high = Short.MAX_VALUE;        // high == 32767
short low = Short.MIN_VALUE;         // low == -32768

```

The default value of a `short` is **0**

```java
short defaultShort;    // defaultShort == 0

```



#### Syntax


<li>
int aInt = 8; // The defining (number) part of this int declaration is called a literal.
</li>
<li>
int hexInt = 0x1a; // = 26; You can define literals with hex values prefixed with **0x**.
</li>
<li>
int binInt = 0b11010; // = 26; You can also define binary literals; prefixed with **0b**.
</li>
<li>
long goodLong = 10000000000L; // By default, integer literals are of type int. By adding the L at the end of the literal you are telling the compiler that the literal is a long. Without this the compiler would throw an "Integer number too large" error.
</li>
<li>
double aDouble = 3.14; // Floating-Point Literals are of type double by default.
</li>
<li>
float aFloat = 3.14F; // By default this literal would have been a double (and caused an "Incompatible Types" error), but by adding an F we tell the compiler it is a float.
</li>



#### Remarks


Java has 8 **primitive data types**, namely `boolean`, `byte`, `short`, `char`, `int`, `long`, `float` and `double`.  (All other types are **reference** types.  This includes all array types, and built-in object types / classes that have special significance in the Java language; e.g. `String`, `Class` and `Throwable` and its subclasses.)

The result of all operations (addition, subtraction, multiplication, etc) on a primitive type is at least an `int`, thus adding a `short` to a `short` produces an `int`, as does adding a `byte` to a `byte`, or a `char` to a `char`. If you want to assign the result of that back to a value of the same type, you must cast it. e.g.

```java
byte a = 1;
byte b = 2;
byte c = (byte) (a + b);

```

Not casting the operation will result in a compile error.

This is due to the following part of the [Java Language Spec, §2.11.1](http://docs.oracle.com/javase/specs/jvms/se7/html/jvms-2.html#jvms-2.11.1):

> 
A compiler encodes loads of literal values of types `byte` and `short` using Java Virtual Machine instructions that sign-extend those values to values of type `int` at compile-time or run-time. Loads of literal values of types `boolean` and `char` are encoded using instructions that zero-extend the literal to a value of type `int` at compile-time or run-time. [..]. Thus, most operations on values of actual types `boolean`, `byte`, `char`, and `short` are correctly performed by instructions operating on values of computational type `int`.


The reason behind this is also specified in that section:

> 
Given the Java Virtual Machine's **one-byte opcode size**, encoding types into opcodes places pressure on the design of its instruction set. If each typed instruction supported all of the Java Virtual Machine's run-time data types, there would be more instructions than could be represented in a `byte`. [...] Separate instructions can be used to convert between unsupported and supported data types as necessary.


