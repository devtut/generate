---
metaTitle: "Java - Java Floating Point Operations"
description: "Comparing floating point values, OverFlow and UnderFlow, Formatting the floating point values, Strict Adherence to the IEEE Specification"
---

# Java Floating Point Operations


Floating-point numbers are numbers that have fractional parts (usually expressed with a decimal point). In Java, there is two primitive types for floating-point numbers which are `float` (uses 4 bytes), and `double` (uses 8 bytes). This documentation page is for detailing with examples operations that can be done on floating points in Java.



## Comparing floating point values


You should be careful when comparing floating-point values (`float` or `double`) using relational operators:  `==`, `!=`, `<` and so on.  These operators give results according to the binary representations of the floating point values.  For example:

```java
public class CompareTest {
    public static void main(String[] args) {
        double oneThird = 1.0 / 3.0;
        double one = oneThird * 3;
        System.out.println(one == 1.0);      // prints "false"
    }
}

```

The calculation `oneThird` has introduced a tiny rounding error, and when we multiply `oneThird` by `3` we get a result that is slightly different to `1.0`.

This problem of inexact representations is more stark when we attempt to mix `double` and `float` in calculations.  For example:

```java
public class CompareTest2 {
    public static void main(String[] args) {
        float floatVal = 0.1f;
        double doubleVal = 0.1;
        double doubleValCopy = floatVal;

        System.out.println(floatVal);      // 0.1
        System.out.println(doubleVal);     // 0.1
        System.out.println(doubleValCopy); // 0.10000000149011612
        
        System.out.println(floatVal == doubleVal); // false
        System.out.println(doubleVal == doubleValCopy); // false
    }
}

```

The floating point representations used in Java for the `float` and `double` types have limited number of digits of precision.  For the `float` type, the precision is 23 binary digits or about 8 decimal digits.  For the `double` type, it is 52 bits or about 15 decimal digits.  On top of that, some arithmetical operations will introduce rounding errors.  Therefore, when a program compares floating point values, it standard practice to define an **acceptable delta** for the comparison.  If the difference between the two numbers is less than the delta, they are deemed to be equal.  For example

```java
if (Math.abs(v1 - v2) < delta)

```

**Delta compare example:**

```java
public class DeltaCompareExample {

    private static boolean deltaCompare(double v1, double v2, double delta) {
        // return true iff the difference between v1 and v2 is less than delta
        return Math.abs(v1 - v2) < delta;
    }
    
    public static void main(String[] args) {
        double[] doubles = {1.0, 1.0001, 1.0000001, 1.000000001, 1.0000000000001};
        double[] deltas = {0.01, 0.00001, 0.0000001, 0.0000000001, 0};

        // loop through all of deltas initialized above
        for (int j = 0; j < deltas.length; j++) {
            double delta = deltas[j];
            System.out.println("delta: " + delta);

            // loop through all of the doubles initialized above
            for (int i = 0; i < doubles.length - 1; i++) {
                double d1 = doubles[i];
                double d2 = doubles[i + 1];
                boolean result = deltaCompare(d1, d2, delta);

                System.out.println("" + d1 + " == " + d2 + " ? " + result);
                
            }

            System.out.println();
        }
    }
}

```

Result:

```java
delta: 0.01
1.0 == 1.0001 ? true
1.0001 == 1.0000001 ? true
1.0000001 == 1.000000001 ? true
1.000000001 == 1.0000000000001 ? true

delta: 1.0E-5
1.0 == 1.0001 ? false
1.0001 == 1.0000001 ? false
1.0000001 == 1.000000001 ? true
1.000000001 == 1.0000000000001 ? true

delta: 1.0E-7
1.0 == 1.0001 ? false
1.0001 == 1.0000001 ? false
1.0000001 == 1.000000001 ? true
1.000000001 == 1.0000000000001 ? true

delta: 1.0E-10
1.0 == 1.0001 ? false
1.0001 == 1.0000001 ? false
1.0000001 == 1.000000001 ? false
1.000000001 == 1.0000000000001 ? false

delta: 0.0
1.0 == 1.0001 ? false
1.0001 == 1.0000001 ? false
1.0000001 == 1.000000001 ? false
1.000000001 == 1.0000000000001 ? false

```

Also for comparison of `double` and `float` primitive types static `compare` method of corresponding boxing type can be used. For example:

```java
double a = 1.0;
double b = 1.0001;

System.out.println(Double.compare(a, b));//-1
System.out.println(Double.compare(b, a));//1

```

Finally, determining what deltas are most appropriate for a comparison can be tricky.  A commonly used approach is to pick delta values that are our intuition says are about right.  However, if you know scale and (true) accuracy of the input values, and the calculations performed, it may be possible to come up with mathematically sound bounds on the accuracy of the results, and hence for the deltas.  (There is a formal branch of Mathematics known as Numerical Analysis that used to be taught to computational scientists that covered this kind of analysis.)



## OverFlow and UnderFlow


**Float** data type

The float data type is a single-precision 32-bit IEEE 754 floating point.

`Float` overflow

Maximum possible value is `3.4028235e+38` , When it exceeds this value it  produces `Infinity`

```java
float f = 3.4e38f;
float result = f*2;        
System.out.println(result); //Infinity

```

`Float` **UnderFlow**

Minimum value is 1.4e-45f, when is goes below this value it produces `0.0`

```

   float f = 1e-45f;
    float result = f/1000;
    System.out.println(result);

```

**double** data type

The double data type is a double-precision `64-bit` IEEE 754 floating point.

`Double` **OverFlow**

Maximum possible value is `1.7976931348623157e+308` , When it exceeds this value it  produces `Infinity`

```java
double d = 1e308;
double result=d*2;      
System.out.println(result); //Infinity

```

`Double` **UnderFlow**

Minimum value is  4.9e-324, when is goes below this value it produces `0.0`

```

   double d = 4.8e-323;
    double result = d/1000;
    System.out.println(result); //0.0

```



## Formatting the floating point values


Floating point Numbers can be formatted as a decimal number using `String.format` with `'f'` flag

```

   //Two digits in fracttional part are rounded
    String format1 = String.format("%.2f", 1.2399);
    System.out.println(format1); // "1.24"

    // three digits in fractional part are rounded 
    String format2 = String.format("%.3f", 1.2399);
    System.out.println(format2); // "1.240"
    
    //rounded to two digits, filled with zero 
    String format3 = String.format("%.2f", 1.2);
    System.out.println(format3); // returns "1.20"
    
    //rounder to two digits
    String format4 = String.format("%.2f", 3.19999);
    System.out.println(format4); // "3.20"

```

Floating point Numbers can be formatted as a decimal number using `DecimalFormat`

```

  // rounded with one digit fractional part 
    String format = new DecimalFormat("0.#").format(4.3200);
    System.out.println(format); // 4.3
    
   // rounded with two digit fractional part 
    String format = new DecimalFormat("0.##").format(1.2323000);
    System.out.println(format); //1.23

    // formatting floating numbers to decimal number
    double dv = 123456789;
    System.out.println(dv); // 1.23456789E8
    String format =  new DecimalFormat("0").format(dv);
    System.out.println(format); //123456789

```



## Strict Adherence to the IEEE Specification


By default, floating point operations on `float` and `double` **do not** strictly adhere to the rules of the IEEE 754 specification. An expression is allowed to use implementation-specific extensions to the range of these values; essentially allowing them to be **more** accurate than required.

`strictfp` disables this behavior. It is applied to a class, interface, or method, and applies to everything contained in it, such as classes, interfaces, methods, constructors, variable initializers, etc. With `strictfp`, the intermediate values of a floating-point expression **must** be within the float value set or the double value set. This causes the results of such expressions to be exactly those that the IEEE 754 specification predicts.

All constant expressions are implicitly strict, even if they aren't inside a `strictfp` scope.

Therefore, `strictfp` has the net effect of sometimes making certain corner case computations **less** accurate, and can also make floating point operations **slower** (as the CPU is now doing more work to ensure any native extra precision does not affect the result). However, it also causes the results to be exactly the same on all platforms. It is therefore useful in things like scientific programs, where reproducibility is more important than speed.

```java
public class StrictFP { // No strictfp -> default lenient
    public strictfp float strict(float input) {
        return input * input / 3.4f; // Strictly adheres to the spec.
                                     // May be less accurate and may be slower.
    }

    public float lenient(float input) {
        return input * input / 3.4f; // Can sometimes be more accurate and faster,
                                     // but results may not be reproducable.
    }

    public static final strictfp class Ops { // strictfp affects all enclosed entities
        private StrictOps() {}

        public static div(double dividend, double divisor) { // implicitly strictfp
            return dividend / divisor;
        }
    }
}

```

