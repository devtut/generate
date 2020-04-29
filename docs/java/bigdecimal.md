---
metaTitle: "BigDecimal"
description: "Comparing BigDecimals, Using BigDecimal instead of float, Mathematical operations with BigDecimal, BigDecimal.valueOf(), Initialization of BigDecimals with value zero, one or ten, BigDecimal objects are immutable"
---

# BigDecimal


The [BigDecimal](http://docs.oracle.com/javase/8/docs/api/java/math/BigDecimal.html) class provides operations for arithmetic (add, subtract, multiply, divide), scale manipulation, rounding, comparison, hashing, and format conversion. The BigDecimal represents immutable, arbitrary-precision signed decimal numbers. This class shall be used in a necessity of high-precision calculation.



## Comparing BigDecimals


The method [`compareTo`](https://docs.oracle.com/javase/8/docs/api/java/math/BigDecimal.html#compareTo-java.math.BigDecimal-) should be used to compare `BigDecimals`:

```java
BigDecimal a = new BigDecimal(5);
a.compareTo(new BigDecimal(0));    // a is greater, returns 1
a.compareTo(new BigDecimal(5));    // a is equal, returns 0
a.compareTo(new BigDecimal(10));   // a is less, returns -1

```

Commonly you should **not** use the [`equals`](https://docs.oracle.com/javase/8/docs/api/java/math/BigDecimal.html#equals-java.lang.Object-) method since it considers two `BigDecimals` equal only if they are equal in value and also **scale**:

```java
BigDecimal a = new BigDecimal(5);
a.equals(new BigDecimal(5));       // value and scale are equal, returns true
a.equals(new BigDecimal(5.00));    // value is equal but scale is not, returns false

```



## Using BigDecimal instead of float


Due to way that the float type is represented in computer memory, results of operations using this type can be inaccurate - some values are stored as approximations. Good examples of this are monetary calculations.
If high precision is necessary, other types should be used. e.g. Java 7 provides BigDecimal.

```java
import java.math.BigDecimal;

public class FloatTest {

public static void main(String[] args) {
    float accountBalance = 10000.00f;
    System.out.println("Operations using float:");
    System.out.println("1000 operations for 1.99");
    for(int i = 0; i<1000; i++){
        accountBalance -= 1.99f;
    }
    System.out.println(String.format("Account balance after float operations: %f", accountBalance));
    
    BigDecimal accountBalanceTwo = new BigDecimal("10000.00");
    System.out.println("Operations using BigDecimal:");
    System.out.println("1000 operations for 1.99");
    BigDecimal operation = new BigDecimal("1.99");
    for(int i = 0; i<1000; i++){
        accountBalanceTwo = accountBalanceTwo.subtract(operation);
    }
    System.out.println(String.format("Account balance after BigDecimal operations: %f", accountBalanceTwo));
}

```

Output of this program is:

```java
Operations using float:
1000 operations for 1.99
Account balance after float operations: 8009,765625
Operations using BigDecimal:
1000 operations for 1.99
Account balance after BigDecimal operations: 8010,000000

```

For a starting balance of 10000.00, after 1000 operations for 1.99, we expect the balance to be 8010.00. Using the float type gives us an answer around 8009.77, which is unacceptably imprecise in the case of monetary calculations. Using BigDecimal gives us the proper result.



## Mathematical operations with BigDecimal


This example shows how to perform basic mathematical operations using BigDecimals.

### 1.Addition

```java
BigDecimal a = new BigDecimal("5");
BigDecimal b = new BigDecimal("7");

//Equivalent to result = a + b
BigDecimal result = a.add(b);
System.out.println(result);

```

> 
Result : 12


### 2.Subtraction

```java
BigDecimal a = new BigDecimal("5");
BigDecimal b = new BigDecimal("7");

//Equivalent to result = a - b
BigDecimal result = a.subtract(b);
System.out.println(result);

```

> 
Result : -2


### 3.Multiplication

When multiplying two `BigDecimal`s the result is going to have scale equal to the sum of the scales of operands.

```java
BigDecimal a = new BigDecimal("5.11");
BigDecimal b = new BigDecimal("7.221");

//Equivalent to result = a * b
BigDecimal result = a.multiply(b);
System.out.println(result);

```

> 
Result : 36.89931


To change the scale of the result use the overloaded multiply method which allows passing `MathContext` - an object describing the rules for operators, in particular the precision and rounding mode of the result. For more information about available rounding modes please refer to the Oracle Documentation.

```java
BigDecimal a = new BigDecimal("5.11");
BigDecimal b = new BigDecimal("7.221");

MathContext returnRules = new MathContext(4, RoundingMode.HALF_DOWN);

//Equivalent to result = a * b
BigDecimal result = a.multiply(b, returnRules);
System.out.println(result);

```

> 
Result : 36.90


### 4.Division

Division is a bit more complicated than the other arithmetic operations, for instance consider the below example:

```java
BigDecimal a = new BigDecimal("5");
BigDecimal b = new BigDecimal("7");

BigDecimal result = a.divide(b);
System.out.println(result);

```

We would expect this to give something similar to : 0.7142857142857143, but we would get:

> 
Result: **java.lang.ArithmeticException: Non-terminating decimal expansion; no exact representable decimal result.**


This would work perfectly well when the result would be a terminating decimal say if I wanted to divide 5 by 2, but for those numbers which upon dividing would give a non terminating result we would get an `ArithmeticException`. In the real world scenario, one cannot predict the values that would be encountered during the division, so we need to specify the **Scale** and the **Rounding Mode** for BigDecimal division. For more information on the Scale and Rounding Mode, refer the [Oracle Documentation](https://docs.oracle.com/javase/7/docs/api/java/math/BigDecimal.html).

For example, I could do:

```java
BigDecimal a = new BigDecimal("5");
BigDecimal b = new BigDecimal("7");

//Equivalent to result = a / b (Upto 10 Decimal places and Round HALF_UP)
BigDecimal result = a.divide(b,10,RoundingMode.HALF_UP);
System.out.println(result);

```

> 
Result : 0.7142857143


### 5.Remainder or Modulus

```java
BigDecimal a = new BigDecimal("5");
BigDecimal b = new BigDecimal("7");

//Equivalent to result = a % b
BigDecimal result = a.remainder(b);
System.out.println(result);

```

> 
Result : 5


### 6.Power

```java
BigDecimal a = new BigDecimal("5");

//Equivalent to result = a^10    
BigDecimal result = a.pow(10);
System.out.println(result);

```

> 
Result : 9765625


### 7.Max

```java
BigDecimal a = new BigDecimal("5");
BigDecimal b = new BigDecimal("7");

//Equivalent to result = MAX(a,b) 
BigDecimal result = a.max(b);
System.out.println(result);

```

> 
Result : 7


### 8.Min

```java
BigDecimal a = new BigDecimal("5");
BigDecimal b = new BigDecimal("7");

//Equivalent to result = MIN(a,b) 
BigDecimal result = a.min(b);
System.out.println(result);

```

> 
Result : 5


### 9.Move Point To Left

```java
BigDecimal a = new BigDecimal("5234.49843776");

//Moves the decimal point to 2 places left of current position
BigDecimal result = a.movePointLeft(2);
System.out.println(result);

```

> 
Result : 52.3449843776


### 10.Move Point To Right

```java
BigDecimal a = new BigDecimal("5234.49843776");

//Moves the decimal point to 3 places right of current position
BigDecimal result = a.movePointRight(3);
System.out.println(result);

```

> 
Result : 5234498.43776


There are many more options and combination of parameters for the above mentioned examples (For instance, there are 6 variations of the divide method), this set is a non-exhaustive list and covers a few basic examples.



## BigDecimal.valueOf()


The BigDecimal class contains an internal cache of frequently used numbers e.g. 0 to 10. The BigDecimal.valueOf() methods are provided in preference to constructors with similar type parameters i.e. in the below example a is preferred to b.

```java
BigDecimal a = BigDecimal.valueOf(10L); //Returns cached Object reference
BigDecimal b = new BigDecimal(10L); //Does not return cached Object reference

BigDecimal a = BigDecimal.valueOf(20L); //Does not return cached Object reference
BigDecimal b = new BigDecimal(20L); //Does not return cached Object reference


BigDecimal a = BigDecimal.valueOf(15.15); //Preferred way to convert a double (or float) into a BigDecimal, as the value returned is equal to that resulting from constructing a BigDecimal from the result of using Double.toString(double)
BigDecimal b = new BigDecimal(15.15); //Return unpredictable result

```



## Initialization of BigDecimals with value zero, one or ten


BigDecimal provides static properties for the numbers zero, one and ten. It's good practise to use these instead of using the actual numbers:

- [`BigDecimal.ZERO`](http://docs.oracle.com/javase/7/docs/api/java/math/BigDecimal.html#ZERO)
- [`BigDecimal.ONE`](http://docs.oracle.com/javase/7/docs/api/java/math/BigDecimal.html#ONE)
- [`BigDecimal.TEN`](http://docs.oracle.com/javase/7/docs/api/java/math/BigDecimal.html#TEN)

By using the static properties, you avoid an unnecessary instantiation, also you've got a literal in your code instead of a 'magic number'.

```java
//Bad example:
BigDecimal bad0 = new BigDecimal(0);
BigDecimal bad1 = new BigDecimal(1);
BigDecimal bad10 = new BigDecimal(10);

//Good Example:
BigDecimal good0 = BigDecimal.ZERO;
BigDecimal good1 = BigDecimal.ONE;
BigDecimal good10 = BigDecimal.TEN;

```



## BigDecimal objects are immutable


If you want to calculate with BigDecimal you have to use the returned value because BigDecimal objects are immutable:

```java
BigDecimal a = new BigDecimal("42.23");
BigDecimal b = new BigDecimal("10.001");

a.add(b); // a will still be 42.23

BigDecimal c = a.add(b); // c will be 52.231 

```

