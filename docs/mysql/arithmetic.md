---
metaTitle: "Arithmetic"
description: "Arithmetic Operators, Mathematical Constants, Trigonometry (SIN, COS), Rounding (ROUND, FLOOR, CEIL), Raise a number to a power (POW), Square Root (SQRT), Random Numbers (RAND), Absolute Value and Sign (ABS, SIGN)"
---

# Arithmetic



## Arithmetic Operators


MySQL provides the following arithmetic operators

|Operator|Name|Example
|---|---|---|---
|`+`|Addition|`SELECT 3+5;` -> 8<br/> `SELECT 3.5+2.5;` -> 6.0<br />  `SELECT 3.5+2;` -> 5.5
|`-`|Subtraction|`SELECT 3-5;` -> -2
|`*`|Multiplication|`SELECT 3 * 5;` -> 15
|`/`|Division|`SELECT 20 / 4;` -> 5 <br /> `SELECT 355 / 113;` -> 3.1416<br /> `SELECT 10.0 / 0;` -> NULL
|`DIV`|Integer Division|`SELECT 5 DIV 2;` -> 2
|`%` or `MOD`|Modulo|`SELECT 7 % 3;` -> 1 <br/> `SELECT 15 MOD 4` -> 3 <br/> `SELECT 15 MOD -4` -> 3<br/> `SELECT -15 MOD 4` -> -3<br/> `SELECT -15 MOD -4` -> -3 <br/> `SELECT 3 MOD 2.5` -> 0.5

### BIGINT

If the numbers in your arithmetic are all integers, MySQL uses the `BIGINT` (signed 64-bit) integer data type to do its work. For example:

`select (1024 * 1024 * 1024 * 1024 *1024 * 1024) + 1`  -> 1,152,921,504,606,846,977

and

`select (1024 * 1024 * 1024 * 1024 *1024 * 1024 * 1024`  -> `BIGINT` out of range error

### DOUBLE

If any numbers in your arithmetic are fractional, MySQL uses [64-bit IEEE 754 floating point arithmetic](https://en.wikipedia.org/wiki/IEEE_floating_point).  You must be careful when using floating point arithmetic, because many [floating point numbers are, inherently, approximations rather than exact values](http://dev.mysql.com/doc/refman/5.7/en/problems-with-float.html).



## Mathematical Constants


### Pi

The following returns the value of `PI` formatted to 6 decimal places.  The actual value is good to `DOUBLE`;

```sql
SELECT PI();    -> 3.141593

```



## Trigonometry (SIN, COS)


Angles are in Radians, not Degrees. All computations are done in [IEEE 754 64-bit floating point](https://en.wikipedia.org/wiki/Double-precision_floating-point_format). All floating point computations are subject to small errors, known as [machine ε (epsilon) errors](https://en.wikipedia.org/wiki/Machine_epsilon), so avoid trying to compare them for equality. There is no way to avoid these errors when using floating point; they are built in to the technology.

If you use `DECIMAL` values in trigonometric computations, they are implicitly converted to floating point, and then back to decimal.

### Sine

Returns the sine of a number X expressed in radians

```sql
SELECT SIN(PI()); -> 1.2246063538224e-16

```

### Cosine

Returns the cosine of X when X is given in radians

```sql
SELECT COS(PI()); -> -1

```

### Tangent

Returns the tangent of a number X expressed in radians. Notice the result is very close to zero, but not exactly zero. This is an example of machine ε.

```sql
SELECT TAN(PI());   -> -1.2246063538224e-16

```

### Arc Cosine (inverse cosine)

Returns the arc cosine of X if X is in the range `-1 to 1`

```sql
SELECT ACOS(1);    -> 0
SELECT ACOS(1.01); -> NULL

```

### Arc Sine (inverse sine)

Returns the arc sine of X if X is in the range `-1 to 1`

```sql
SELECT ASIN(0.2); -> 0.20135792079033

```

### Arc Tangent (inverse tangent)

`ATAN(x)` returns the arc tangent of a single number.

```sql
SELECT ATAN(2); -> 1.1071487177941

```

`ATAN2(X, Y)` returns the arc tangent of the two variables X and Y. It is similar to calculating the arc tangent of Y / X. But it is numerically more robust: t functions correctly when X is near zero, and the signs of both arguments are used to determine the quadrant of the result.

Best practice suggests writing formulas to use `ATAN2()` rather than `ATAN()` wherever possible.

```

ATAN2(1,1);    -> 0.7853981633974483 (45 degrees)
 ATAN2(1,-1);   -> 2.356194490192345  (135 degrees)
 ATAN2(0, -1);  -> PI  (180 degrees)  don't try ATAN(-1 / 0)... it won't work

```

### Cotangent

Returns the cotangent of X

```sql
SELECT COT(12); -> -1.5726734063977

```

### Conversion

```sql
SELECT RADIANS(90) -> 1.5707963267948966
SELECT SIN(RADIANS(90)) -> 1
SELECT DEGREES(1), DEGREES(PI()) -> 57.29577951308232, 180

```



## Rounding (ROUND, FLOOR, CEIL)


### Round a decimal number to an integer value

For exact numeric values (e.g. `DECIMAL`): If the first decimal place of a number is 5 or higher, this function will round a number to the next integer **away from zero**. If that decimal place is 4 or lower, this function will round to the next integer value **closest to zero**.

```sql
SELECT ROUND(4.51) -> 5
SELECT ROUND(4.49) -> 4
SELECT ROUND(-4.51) -> -5

```

For approximate numeric values (e.g. `DOUBLE`): The result of the `ROUND()` function depends on the C library; on many systems, this means that `ROUND()` uses the **round to the nearest even** rule:

```sql
SELECT ROUND(45e-1) -> 4  -- The nearest even value is 4
SELECT ROUND(55e-1) -> 6  -- The nearest even value is 6

```

### Round up a number

To round up a number use either the `CEIL()` or `CEILING()` function

```sql
SELECT CEIL(1.23)    -> 2
SELECT CEILING(4.83) -> 5

```

### Round down a number

To round down a number, use the `FLOOR()` function

```sql
SELECT FLOOR(1.99) -> 1

```

FLOOR and CEIL go toward / away from -infinity:

```sql
SELECT FLOOR(-1.01), CEIL(-1.01) -> -2 and -1
SELECT FLOOR(-1.99), CEIL(-1.99) -> -2 and -1

```

### Round a decimal number to a specified number of decimal places.

```sql
SELECT ROUND(1234.987, 2) -> 1234.99
SELECT ROUND(1234.987, -2) -> 1200

```

The discussion of up versus down and "5" applies, too.



## Raise a number to a power (POW)


To raise a number `x` to a power `y`, use either the `POW()` or `POWER()` functions

```sql
SELECT POW(2,2); => 4
SELECT POW(4,2); => 16

```



## Square Root (SQRT)


Use the `SQRT()` function. If the number is negative, `NULL` will be returned

```sql
SELECT SQRT(16); -> 4
SELECT SQRT(-3); -> NULL

```



## Random Numbers (RAND)


### Generate a random number

To generate a pseudorandom floating point number between `0` and `1`, use the `RAND()` function

Suppose you have the following query

```sql
SELECT i, RAND() FROM t;

```

This will return something like this

|i|RAND()
|---|---|---|---
|1|0.6191438870682
|2|0.93845168309142
|3|0.83482678498591

### Random Number in a range

To generate a random number in the range a <= n <= b, you can use the following formula

```sql
FLOOR(a + RAND() * (b - a + 1))

```

For example, this will generate a random number between 7 and 12

```sql
SELECT FLOOR(7 + (RAND() * 6));

```

A simple way to randomly return the rows in a table:

```sql
SELECT * FROM tbl ORDER BY RAND();

```

These are **pseudorandom** numbers.

The pseudorandom number generator in MySQL is not cryptographically secure. That is, if you use MySQL to generate random numbers to be used as secrets, a determined adversary who knows you used MySQL will be able to guess your secrets more easily than you might believe.



## Absolute Value and Sign (ABS, SIGN)


Return the absolute value of a number

```sql
SELECT ABS(2);   -> 2
SELECT ABS(-46); -> 46

```

The `sign` of a number compares it to 0.

|Sign|Result|Example
|---|---|---|---
|-1|n < 0|`SELECT SIGN(42);` -> 1
|0|n = 0|`SELECT SIGN(0);` -> 0
|1|n > 0|`SELECT SIGN(-3);` -> -1

```sql
SELECT SIGN(-423421); -> -1

```



#### Remarks


MySQL, on most machines, uses [64-bit IEEE 754 floating point arithmetic](https://en.wikipedia.org/wiki/IEEE_floating_point) for its calculations.

In integer contexts it uses integer arithmetic.

- `RAND()` is not a perfect random number generator. It is mainly used to quickly generate pseudorandom numbers

