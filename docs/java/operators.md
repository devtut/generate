---
metaTitle: "Operators"
description: "The Increment/Decrement Operators (++/--), The Conditional Operator (? :), The Bitwise and Logical Operators (~, &, |, ^), The String Concatenation Operator (+), The Arithmetic Operators (+, -, *, /, %), The Instanceof Operator, The Assignment Operators (=, +=, -=, *=, /=, %=, <<=, >>= , >>>=, &=, |= and ^=), The conditional-and and conditional-or Operators ( && and || ), The Shift Operators (<<, >> and >>>), The Equality Operators (==, !=), The Lambda operator ( -> ), The Relational Operators (<, <=, >, >=)"
---

# Operators


[Operators](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/operators.html) in Java programming language are special symbols that perform specific operations on one, two, or three operands, and then return a result.



## The Increment/Decrement Operators (++/--)


Variables can be incremented or decremented by 1 using the `++` and `--` operators, respectively.

When the `++` and `--` operators follow variables, they are called **post-increment** and **post-decrement** respectively.

```java
int a = 10;
a++; // a now equals 11
a--; // a now equals 10 again

```

When the `++` and `--` operators precede the variables the operations are called **pre-increment** and **pre-decrement** respectively.

```java
int x = 10;
--x; // x now equals 9
++x; // x now equals 10

```

If the operator precedes the variable, the value of the expression is the value of the variable after being incremented or decremented. If the operator follows the variable, the value of the expression is the value of the variable prior to being incremented or decremented.

```java
int x=10;

System.out.println("x=" + x + " x=" + x++ + " x=" + x); // outputs x=10 x=10 x=11
System.out.println("x=" + x + " x=" + ++x + " x=" + x); // outputs x=11 x=12 x=12
System.out.println("x=" + x + " x=" + x-- + " x=" + x); // outputs x=12 x=12 x=11
System.out.println("x=" + x + " x=" + --x + " x=" + x); // outputs x=11 x=10 x=10

```

Be careful not to overwrite post-increments or decrements. This happens if you use a post-in/decrement operator at the end of an expression which is reassigned to the in/decremented variable itself. The in/decrement will not have an effect. Even though the variable on the left hand side is incremented correctly, its value will be immediately overwritten with the previously evaluated result from the right hand side of the expression:

```java
int x = 0; 
x = x++ + 1 + x++;      // x = 0 + 1 + 1 
                        // do not do this - the last increment has no effect (bug!) 
System.out.println(x);  // prints 2 (not 3!) 

```

Correct:

```java
int x = 0;
x = x++ + 1 + x;        // evaluates to x = 0 + 1 + 1
x++;                    // adds 1
System.out.println(x);  // prints 3 

```



## The Conditional Operator (? :)


### Syntax

**{condition-to-evaluate}** **?** **{statement-executed-on-true}** **:** **{statement-executed-on-false}**

As shown in the syntax, the Conditional Operator (also known as the Ternary Operator<sup>1</sup>) uses the `?` (question mark) and `:` (colon) characters to enable a conditional expression of two possible outcomes. It can be used to replace longer `if-else` blocks to return one of two values based on condition.

```java
result = testCondition ? value1 : value2

```

Is equivalent to

```java
if (testCondition) { 
    result = value1; 
} else { 
    result = value2; 
}

```

It can be read as ****“If testCondition is true, set result to value1; otherwise, set result to value2”.****

For example:

```java
// get absolute value using conditional operator 
a = -10;
int absValue = a < 0 ? -a : a;
System.out.println("abs = " + absValue); // prints "abs = 10"

```

Is equivalent to

```java
// get absolute value using if/else loop
a = -10;
int absValue;
if (a < 0) {
    absValue = -a;
} else {
    absValue = a;
}
System.out.println("abs = " + absValue); // prints "abs = 10"

```

### Common Usage

You can use the conditional operator for conditional assignments (like null checking).

```java
String x = y != null ? y.toString() : ""; //where y is an object

```

This example is equivalent to:

```java
String x = "";

if (y != null) {
    x = y.toString();
}

```

Since the Conditional Operator has the second-lowest precedence, above the [Assignment Operators](http://stackoverflow.com/documentation/java/176/operators/12239/the-assignment-operators-and), there is rarely a need for use parenthesis around the **condition**, but parenthesis is required around the entire Conditional Operator construct when combined with other operators:

```java
// no parenthesis needed for expressions in the 3 parts
10 <= a && a < 19 ? b * 5 : b * 7

// parenthesis required
7 * (a > 0 ? 2 : 5)

```

Conditional operators nesting can also be done in the third part, where it works more like chaining or like a switch statement.

```java
a ? "a is true" :
b ? "a is false, b is true" :
c ? "a and b are false, c is true" :
    "a, b, and c are false"

//Operator precedence can be illustrated with parenthesis:

a ? x : (b ? y : (c ? z : w))

```

Footnote:

<sup>1 - Both the [Java Language Specification](https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.25) and the [Java Tutorial](https://docs.oracle.com/javase/tutorial/java/nutsandbolts/op2.html) call the (`? :`) operator the **Conditional Operator**.  The Tutorial says that it is "also known as the Ternary Operator" as it is (currently) the only ternary operator defined by Java.  The "Conditional Operator" terminology is consistent with C and C++ and other languages with an equivalent operator.</sup>



## The Bitwise and Logical Operators (~, &, |, ^)


The Java language provides 4 operators that perform bitwise or logical operations on integer or boolean operands.

- The complement (`~`) operator is a unary operator that performs a bitwise or logical inversion of the bits of one operand; see [JLS 15.15.5.](https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.15.5).
- The AND (`&`) operator is a binary operator that performs a bitwise or logical "and" of two operands; see [JLS 15.22.2.](https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.22.2).
- The OR (`|`) operator is a binary operator that performs a bitwise or logical "inclusive or" of two operands; see [JLS 15.22.2.](https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.22.2).
- The XOR (`^`) operator is a binary operator that performs a bitwise or logical "exclusive or" of two operands; see [JLS 15.22.2.](https://docs.oracle.com/javase/specs/jls/se7/html/jls-15.html#jls-15.22.2).

The logical operations performed by these operators when the operands are booleans can be summarized as follows:

|A|B|~A|A & B|A | B|A ^ B
|------
|0|0|1|0|0|0
|0|1|1|0|1|1
|1|0|0|0|1|1
|1|1|0|1|1|0

Note that for integer operands, the above table describes what happens for individual bits.  The operators actually operate on all 32 or 64 bits of the operand or operands in parallel.

### Operand types and result types.

The usual arithmetic conversions apply when the operands are integers.
Common use-cases for the bitwise operators

The `~` operator is used to reverse a boolean value, or change all the bits in an integer operand.

The `&` operator is used for "masking out" some of the bits in an integer operand.  For example:

```java
int word = 0b00101010;
int mask = 0b00000011;   // Mask for masking out all but the bottom 
                         // two bits of a word
int lowBits = word & mask;            // -> 0b00000010
int highBits = word & ~mask;          // -> 0b00101000

```

The `|` operator is used to combine the truth values of two operands.  For example:

```java
int word2 = 0b01011111; 
// Combine the bottom 2 bits of word1 with the top 30 bits of word2
int combined = (word & mask) | (word2 & ~mask);   // -> 0b01011110

```

The `^` operator is used for toggling or "flipping" bits:

```java
int word3 = 0b00101010;
int word4 = word3 ^ mask;             // -> 0b00101001

```

For more examples of the use of the bitwise operators, see [Bit Manipulation](http://stackoverflow.com/documentation/java/1177/bit-manipulation#t=201610101439344327372)



## The String Concatenation Operator (+)


The `+` symbol can mean three distinct operators in Java:

- If there is no operand before the `+`, then it is the unary Plus operator.
- If there are two operands, and they are both numeric. then it is the binary Addition operator.
- If there are two operands, and at least one of them is a `String`, then it it the binary Concatenation operator.

In the simple case, the Concatenation operator joins two strings to give a third string.  For example:

```java
String s1 = "a String";
String s2 = "This is " + s1;    // s2 contains "This is a String"

```

When one of the two operands is not a string, it is converted to a `String` as follows:

<li>
An operand whose type is a primitive type is converted **as if** by calling `toString()` on the boxed value.
</li>
<li>
An operand whose type is a reference type is converted by calling the operand's `toString()` method.  If the operand is `null`, or if the `toString()` method returns `null`, then the string literal `"null"` is used instead.
</li>

For example:

```java
int one = 1;
String s3 = "One is "  + one;         // s3 contains "One is 1"
String s4 = null + " is null";        // s4 contains "null is null"
String s5 = "{1} is " + new int[]{1}; // s5 contains something like
                                      // "{} is [I@xxxxxxxx"

```

The explanation for the `s5` example is that the `toString()` method on array types is inherited from `java.lang.Object`, and the behavior is to produce a string that consists of the type name, and the object's identity hashcode.

The Concatenation operator is specified to create a new `String` object, except in the case where the expression is a Constant Expression.  In the latter case, the expression is evaluated at compile type, and its runtime value is equivalent to a string literal.  This means that there is no runtime overhead in splitting a long string literal like this:

```java
String typing = "The quick brown fox " +
                "jumped over the " +
                "lazy dog";           // constant expression

```

### Optimization and efficiency

As noted above, with the exception of constant expressions, each string concatenation expression creates a new `String` object.  Consider this code:

```java
public String stars(int count) {
    String res = "";
    for (int i = 0; i < count; i++) {
        res = res + "*";
    }
    return res;
}

```

In the method above, each iteration of the loop will create a new `String` that is one character longer than the previous iteration.  Each concatenation copies all of the characters in the operand strings to form the new `String`.  Thus, `stars(N)` will:

- create `N` new `String` objects, and throw away all but the last one,
- copy `N * (N + 1) / 2` characters, and
- generate `O(N^2)` bytes of garbage.

This is very expensive for large `N`.  Indeed, any code that concatenates strings in a loop is liable to have this problem.  A better way to write this would be as follows:

```java
public String stars(int count) {
    // Create a string builder with capacity 'count' 
    StringBuilder sb = new StringBuilder(count);
    for (int i = 0; i < count; i++) {
        sb.append("*");
    }
    return sb.toString();
}

```

Ideally, you should set the capacity of the `StringBuilder`, but if this is not practical, the class will automatically **grow** the backing array that the builder uses to hold characters.  (Note: the implementation expands the backing array exponentially.  This strategy keeps that amount of character copying to a `O(N)` rather than `O(N^2)`.)

Some people apply this pattern to all string concatenations.  However, this is unnecessary because the JLS **allows** a Java compiler to optimize string concatenations within a single expression.  For example:

```java
String s1 = ...;
String s2 = ...;    
String test = "Hello " + s1 + ". Welcome to " + s2 + "\n";

```

will **typically** be optimized by the bytecode compiler to something like this;

```java
StringBuilder tmp = new StringBuilder();
tmp.append("Hello ")
tmp.append(s1 == null ? "null" + s1);
tmp.append("Welcome to ");
tmp.append(s2 == null ? "null" + s2);
tmp.append("\n");
String test = tmp.toString();

```

(The JIT compiler may optimize that further if it can deduce that `s1` or `s2` cannot be `null`.)  But note that this optimization is only permitted within a single expression.

In short, if you are concerned about the efficiency of string concatenations:

- Do hand-optimize if you are doing repeated concatenation in a loop (or similar).
- Don't hand-optimize a single concatenation expression.



## The Arithmetic Operators (+, -, *, /, %)


The Java language provides 7 operators that perform arithmetic on integer and floating point values.

<li>There are two `+` operators:
<ul>
- The binary addition operator adds one number to another one.  (There is also a binary `+` operator that performs string concatenation. That is described in a separate example.)
- The unary plus operator does nothing apart from triggering numeric promotion (see below)

- The binary subtraction operator subtracts one number from another one.
- The unary minus operator is equivalent to subtracting its operand from zero.

<sup>1. This is often incorrectly referred to as the "modulus" operator. "Remainder" is the term that is used by the JLS. "Modulus" and "remainder" are not the same thing.</sup>

### Operand and result types, and numeric promotion

The operators require numeric operands and produce numeric results.  The operand types can be any primitive numeric type (i.e. `byte`, `short`, `char`, `int`, `long`, `float` or `double`) or any numeric wrapper type define in `java.lang`; i.e. (`Byte`, `Character`, `Short`, `Integer`, `Long`, `Float` or `Double`.

The result type is determined base on the types of the operand or operands, as follows:

- If either of the operands is a `double` or `Double`, then the result type is `double`.
- Otherwise, if either of the operands is a `float` or `Float`, then the result type is `float`.
- Otherwise, if either of the operands is a `long` or `Long`, then the result type is `long`.
- Otherwise, the result type is `int`.  This covers `byte`, `short` and `char` operands as well as `int.

The result type of the operation determines how the arithmetic operation is performed, and how the operands are handled

- If the result type is `double`, the operands are promoted to `double`, and the operation is performed using 64-bit (double precision binary) IEE 754 floating point arithmetic.
- If the result type is `float`, the operands are promoted to `float`, and the operation is performed using 32-bit (single precision binary) IEE 754 floating point arithmetic.
- If the result type is `long`, the operands are promoted to `long`, and the operation is performed using 64-bit signed twos-complement binary integer arithmetic.
- If the result type is `int`, the operands are promoted to `int`, and the operation is performed using 32-bit signed twos-complement binary integer arithmetic.

Promotion is performed in two stages:

- If the operand type is a wrapper type, the operand value is **unboxed** to a value of the corresponding primitive type.
<li>If necessary, the primitive type is promoted to the required type:
<ul>
- Promotion of integers to `int` or `long` is loss-less.
- Promotion of `float` to `double` is loss-less.
- Promotion of an integer to a floating point value can lead to loss of precision.  The conversion is performed using IEE 768 "round-to-nearest" semantics.

### The meaning of division

The / operator divides the left-hand operand `n` (the **dividend**) and the right-hand operand `d` (the **divisor**) and produces the result `q` (the **quotient**).

Java integer division rounds towards zero.  The [JLS Section 15.17.2](https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.17.2) specifies the behavior of Java integer division as follows:

> 
The quotient produced for operands `n` and `d`  is an integer value `q` whose magnitude is as large as possible while satisfying `|d ⋅ q| ≤ |n|`. Moreover, `q` is positive when `|n| ≥ |d|` and `n` and `d` have the same sign, but `q` is negative when `|n| ≥ |d|` and `n` and `d` have opposite signs.


There are a couple of special cases:

- If the `n` is `MIN_VALUE`, and the divisor is -1, then integer overflow occurs and the result is `MIN_VALUE`. No exception is thrown in this case.
- If `d` is 0, then `ArithmeticException is thrown.

Java floating point division has more edge cases to consider.  However the basic idea is that the result `q` is the value that is closest to satisfying `d . q = n`.

Floating point division will never result in an exception.  Instead, operations that divide by zero result in an INF and NaN values; see below.

### The meaning of remainder

Unlike C and C++, the remainder operator in Java works with both integer and floating point operations.

For integer cases, the result of `a % b` is defined to be the number `r` such that `(a / b) * b + r` is equal to `a`, where `/`, `*` and `+` are the appropriate Java integer operators.  This applies in all cases except when `b` is zero.  That case, remainder results in an `ArithmeticException`.

It follows from the above definition that `a % b` can be negative only if `a` is negative, and it be positive only if `a` is positive. Moreover, the magnitude of `a % b` is always less than the magnitude of `b`.

Floating point remainder operation is a generalization of the integer case.  The result of `a % b` is the remainder `r` is defined by the mathematical relation `r = a - (b ⋅ q)` where:

- `q` is an integer,
- it is negative only if `a / b` is negative an positive only if `a / b` is positive, and
- its magnitude is as large as possible without exceeding the magnitude of the true mathematical quotient of `a` and `b`.

Floating point remainder can produce `INF` and `NaN` values in edge-cases such as when `b` is zero; see below.  It will not throw an exception.

Important note:

> 
The result of a floating-point remainder operation as computed by `%` **is not the same** as that produced by the remainder operation defined by IEEE 754.  The IEEE 754 remainder may be computed using the `Math.IEEEremainder` library method.


### Integer Overflow

Java 32 and 64 bit integer values are signed and use twos-complement binary representation.  For example, the range of numbers representable as (32 bit) `int`  -2<sup>31</sup> through +2<sup>31</sup> - 1.

When you add, subtract or multiple two N bit integers (N == 32 or 64), the result of the operation may be too large to represent as an N bit integer.  In this case, the operation leads to **integer overflow**, and the result can be computed as follows:

- The mathematical operation is performed to give a intermediate two's-complement representation of the entire number.  This representation will be larger than N bits.
- The bottom 32 or 64 bits of the intermediate representation are used as the result.

It should be noted that integer overflow does not result in exceptions under any circumstances.

### Floating point INF and NAN values

Java uses IEE 754 floating point representations for `float` and `double`.  These representations have some special values for representing values that fall outside of the domain of Real numbers:

- The "infinite" or INF values denote numbers that are too large.  The `+INF` value denote numbers that are too large and positive.  The `-INF` value denote numbers that are too large and negative.
- The "indefinite" / "not a number" or NaN denote values resulting from meaningless operations.

The INF values are produced by floating operations that cause overflow, or by division by zero.

The NaN values are produced by dividing zero by zero, or computing zero remainder zero.

Surprisingly, it is possible perform arithmetic using INF and NaN operands without triggering exceptions.  For example:

- Adding +INF and a finite value gives +INF.
- Adding +INF and +INF gives +INF.
- Adding +INF and -INF gives NaN.
- Dividing by INF gives either +0.0 or -0.0.
- All operations with one or more NaN operands give NaN.

For full details, please refer to the relevant subsections of [JLS 15](https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html).  Note that this is largely "academic".  For typical calculations, an `INF` or `NaN` means that something has gone wrong; e.g. you have incomplete or incorrect input data, or the calculation has been programmed incorrectly.



## The Instanceof Operator


This operator checks whether the object is of a particular class/interface type. **instanceof** operator is written as:

```java
( Object reference variable ) instanceof  (class/interface type)

```

Example:

```java
public class Test {

   public static void main(String args[]){
      String name = "Buyya";
      // following will return true since name is type of String
      boolean result = name instanceof String;  
      System.out.println( result );
   }
}

```

This would produce the following result:

```java
true

```

This operator will still return true if the object being compared is the assignment compatible with the type on the right.

Example:

```java
class Vehicle {}

public class Car extends Vehicle {
   public static void main(String args[]){
      Vehicle a = new Car();
      boolean result =  a instanceof Car;
      System.out.println( result );
   }
}

```

This would produce the following result:

```java
true

```



## The Assignment Operators (=, +=, -=, *=, /=, %=, <<=, >>= , >>>=, &=, |= and ^=)


The left hand operand for these operators must be a either a non-final variable or an element of an array.  The right hand operand must be **assignment compatible** with the left hand operand.  This means that either the types must be the same, or the right operand type must be convertible to the left operands type by a combination of boxing, unboxing or widening.  (For complete details refer to [JLS 5.2](http://docs.oracle.com/javase/specs/jls/se8/html/jls-5.html#jls-5.2).)

The precise meaning of the "operation and assign" operators is specified by [JLS 15.26.2](http://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.26.2) as:

> 
A compound assignment expression of the form `E1 op= E2` is equivalent to `E1 = (T) ((E1) op (E2))`, where `T` is the type of `E1`, except that `E1` is evaluated only once.


Note that there is an implicit type-cast before the final assignment.

**1.   `=`**

The simple assignment operator: assigns the value of the right hand operand to the left hand operand.

> 
Example: `c = a + b` will add the value of `a + b` to the value of `c` and assign it to `c`


**2.    `+=`**

The "add and assign" operator: adds the value of right hand operand to the value of the left hand operand and assigns the result to left hand operand. If the left hand operand has type `String`, then this a "concatenate and assign" operator.

> 
Example: `c += a` is roughly the same as `c = c + a`


**3.    `-=`**

The "subtract and assign" operator: subtracts the value of the right operand from the value of the left hand operand and assign the result to left hand operand.

> 
Example: `c -= a` is roughly the same as `c = c - a`


**4.    `*=`**

The "multiply and assign" operator: multiplies the value of the right hand operand by the value of the left hand operand and assign the result to left hand operand.  .

> 
Example: `c *= a` is roughly the same as `c = c * a`


**5.    `/=`**

The "divide and assign" operator: divides the value of the right hand operand by the value of the left hand operand and assign the result to left hand operand.

> 
Example: `c /*= a` is roughly the same as `c = c / a`


**6.    `%=`**

The "modulus and assign" operator: calculates the modulus of the value of the right hand operand by the value of the left hand operand and assign the result to left hand operand.

> 
Example: `c %*= a` is roughly the same as `c = c % a`


**7.    `<<=`**

The "left shift and assign" operator.

> 
Example: `c <<= 2` is roughly the same as `c = c << 2`


**8.    `>>=`**

The "arithmetic right shift and assign" operator.

> 
Example: `c >>= 2` is roughly the same as `c = c >> 2`


**9.    `>>>=`**

The "logical right shift and assign" operator.

> 
Example: `c >>>= 2` is roughly the same as `c = c >>> 2`


**10.    `&=`**

The "bitwise and and assign" operator.

> 
Example: `c &= 2` is roughly the same as `c = c & 2`


**11.    `|=`**

The "bitwise or and assign" operator.

> 
Example: `c |= 2` is roughly the same as `c = c | 2`


**12.    `^=`**

The "bitwise exclusive or and assign" operator.

> 
Example: `c ^= 2` is roughly the same as `c = c ^ 2`




## The conditional-and and conditional-or Operators ( && and || )


Java provides a conditional-and and a conditional-or operator, that both take one or two operands of type `boolean` and produce a `boolean` result.  These are:

<li>
`&&` - the conditional-AND operator,
</li>
<li>
<p>`||` - the conditional-OR operators.
The evaluation of `<left-expr> && <right-expr>` is equivalent to the following pseudo-code:</p>

```java
{
   boolean L = evaluate(<left-expr>);
   if (L) {
       return evaluate(<right-expr>);
   } else {
       // short-circuit the evaluation of the 2nd operand expression
       return false;
   }
}

```


</li>

The evaluation of `<left-expr> || <right-expr>` is equivalent to the following pseudo-code:

```

   {
       boolean L = evaluate(<left-expr>);
       if (!L) {
           return evaluate(<right-expr>);
       } else {
           // short-circuit the evaluation of the 2nd operand expression
           return true;
       }
    }

```

As the pseudo-code above illustrates, the behavior of the short-circuit operators are equivalent to using `if` / `else` statements.

### Example - using && as a guard in an expression

The following example shows the most common usage pattern for the `&&` operator.  Compare these two versions of a method to test if a supplied `Integer` is zero.

```java
public boolean isZero(Integer value) {
    return value == 0;
}

public boolean isZero(Integer value) {
    return value != null && value == 0;
}

```

The first version works in most cases, but if the `value` argument is `null`, then a `NullPointerException` will be thrown.

In the second version we have added a "guard" test.  The `value != null && value == 0` expression is evaluated by first performing the `value != null` test.  If the `null` test succeeds (i.e. it evaluates to `true`) then the `value == 0` expression is evaluated.  If the `null` test fails, then the evaluation of `value == 0` is skipped (short-circuited), and we **don't** get a `NullPointerException`.

### Example - using && to avoid a costly calculation

The following example shows how `&&` can be used to avoid a relatively costly calculation:

```java
public boolean verify(int value, boolean needPrime) {
    return !needPrime | isPrime(value);
}

public boolean verify(int value, boolean needPrime) {
    return !needPrime || isPrime(value);
}

```

In the first version, both operands of the `|` will always be evaluated, so the (expensive) `isPrime` method will be called unnecessarily.  The second version avoids the unnecessary call by using `||` instead of `|`.



## The Shift Operators (<<, >> and >>>)


The Java language provides three operator for performing bitwise shifting on 32 and 64 bit integer values.  These are all binary operators with the first operand being the value to be shifted, and the second operand saying how far to shift.

<li>
The `<<` or **left shift** operator shifts the value given by the first operand **leftwards** by the number of bit positions given by the second operand.  The empty positions at the right end are filled with zeros.
</li>
<li>
The '>>' or **arithmetic shift** operator shifts the value given by the first operand **rightwards** by the number of bit positions given by the second operand.  The empty positions at the left end are filled by copying the left-most bit.  This process is known as **sign extension**.
</li>
<li>
The '>>>' or **logical right shift** operator shifts the value given by the first operand **rightwards** by the number of bit positions given by the second operand.  The empty positions at the left end are filled with zeros.
</li>

Notes:

<li>
These operators require an `int` or `long` value as the first operand, and produce a value with the same type as the first operand.  (You will need to use an explicit type cast when assigning the result of a shift to a `byte`, `short` or `char` variable.)
</li>
<li>
If you use a shift operator with a first operand that is a `byte`, `char` or `short`, it is promoted to an `int` and the operation produces an `int`.)
</li>
<li>
The second operand is reduced **modulo the number of bits of the operation** to give the amount of the shift.  For more about the **mod mathematical concept**, see [Modulus examples](http://stackoverflow.com/documentation/java/176/operators/14283/modulus-examples#t=201607262031550001554).
</li>
<li>
The bits that are shifted off the left or right end by the operation are discarded.  (Java does not provide a primitive "rotate" operator.)
</li>
<li>
The arithmetic shift operator is equivalent dividing a (two's complement) number by a power of 2.
</li>
<li>
The left shift operator is equivalent multiplying a (two's complement) number by a power of 2.
</li>

The following table will help you see the effects of the three shift operators.  (The numbers have been expressed in binary notation to aid vizualization.)

|<sub>Operand1</sub>|<sub>Operand2</sub>|<sub>`<<`</sub>|<sub>`>>`</sub>|<sub>`>>>`</sub>
|------
|<sub>0b0000000000001011</sub>|<sub>0</sub>|<sub>0b0000000000001011</sub>|<sub>0b0000000000001011</sub>|<sub>0b0000000000001011</sub>
|<sub>0b0000000000001011</sub>|<sub>1</sub>|<sub>0b0000000000010110</sub>|<sub>0b0000000000000101</sub>|<sub>0b0000000000000101</sub>
|<sub>0b0000000000001011</sub>|<sub>2</sub>|<sub>0b0000000000101100</sub>|<sub>0b0000000000000010</sub>|<sub>0b0000000000000010</sub>
|<sub>0b0000000000001011</sub>|<sub>28</sub>|<sub>0b1011000000000000</sub>|<sub>0b0000000000000000</sub>|<sub>0b0000000000000000</sub>
|<sub>0b0000000000001011</sub>|<sub>31</sub>|<sub>0b1000000000000000</sub>|<sub>0b0000000000000000</sub>|<sub>0b0000000000000000</sub>
|<sub>0b0000000000001011</sub>|<sub>32</sub>|<sub>0b0000000000001011</sub>|<sub>0b0000000000001011</sub>|<sub>0b0000000000001011</sub>
|<sub>...</sub>|<sub>...</sub>|<sub>...</sub>|<sub>...</sub>|<sub>...</sub>
|<sub>0b1000000000001011</sub>|<sub>0</sub>|<sub>0b1000000000001011</sub>|<sub>0b1000000000001011</sub>|<sub>0b1000000000001011</sub>
|<sub>0b1000000000001011</sub>|<sub>1</sub>|<sub>0b0000000000010110</sub>|<sub>0b1100000000000101</sub>|<sub>0b0100000000000101</sub>
|<sub>0b1000000000001011</sub>|<sub>2</sub>|<sub>0b0000000000101100</sub>|<sub>0b1110000000000010</sub>|<sub>0b00100000000000100</sub>
|<sub>0b1000000000001011</sub>|<sub>31</sub>|<sub>0b1000000000000000</sub>|<sub>0b1111111111111111</sub>|<sub>0b0000000000000001</sub>

There examples of the user of shift operators in [Bit manipulation](http://stackoverflow.com/documentation/java/1177/bit-manipulation#t=201610101439344327372)



## The Equality Operators (==, !=)


The `==` and `!=` operators are binary operators that evaluate to `true` or `false` depending on whether the operands are equal.  The `==` operator gives `true` if the operands are equal and `false` otherwise.  The `!=` operator gives `false` if the operands are equal and `true` otherwise.

These operators can be used operands with primitive and reference types, but the behavior is significantly different.  According to the JLS, there are actually three distinct sets of these operators:

- The Boolean `==` and `!=` operators.
- The Numeric `==` and `!=` operators.
- The Reference `==` and `!=` operators.

However, in all cases, the result type of the `==` and `!=` operators is `boolean`.

### The Numeric `==` and `!=` operators

When one (or both) of the operands of an `==` or `!=` operator is a primitive numeric type (`byte`, `short`, `char`, `int,` `long`, `float` or `double`), the operator is a numeric comparison.  The second operand must be either a primitive numeric type, or a boxed numeric type.

The behavior other numeric operators is as follows:

1. If one of the operands is a boxed type, it is unboxed.
1. If either of the operands now a `byte`, `short` or `char`, it is promoted to an `int`.
1. If the types of the operands are not the same, then the operand with the "smaller" type is promoted to the "larger" type.
<li>The comparison is then carried out as follows:
<ul>
1. If the promoted operands are `int` or `long` then the values are tested to see if they are identical.
<li>If the promoted operands are `float` or `double` then:
<ul>
1. the two versions of zero (`+0.0` and `-0.0`) are treated as equal
1. a `NaN` value is treated as not equals to anything, and
1. other values are equal if their IEEE 754 representations are identical.
</ul>
</li>
</ul>
</li>

Note: you need to be careful when using `==` and `!=` to compare floating point values.

### The Boolean `==` and `!=` operators

If both operands are `boolean`, or one is `boolean` and the other is `Boolean`, these operators the Boolean `==` and `!=` operators.  The behavior is as follows:

1. If one of the operands is a `Boolean`, it is unboxed.
1. The unboxed operands are tested and the boolean result is calculated according to the following truth table

|A|B|A == B|A != B
|------
|false|false|true|false
|false|true|false|true
|true|false|false|true
|true|true|true|false

There are two "pitfalls" that make it advisable to use `==` and `!=` sparingly with truth values:

<li>
If you use `==` or `!=` to compare two `Boolean` objects, then the Reference operators are used.  This may give an unexpected result; see [Pitfall: using == to compare primitive wrappers objects such as Integer](http://stackoverflow.com/documentation/java/4388/java-pitfalls/8996/pitfall-using-to-compare-primitive-wrappers-objects-such-as-integer#t=201612061208590356301)
</li>
<li>
The `==` operator can easily be mistyped as `=`.  For most operand types, this mistake leads to a compilation error.  However, for `boolean` and `Boolean` operands the mistake leads to incorrect runtime behavior; see [Pitfall - Using &#39;==&#39; to test a boolean](http://stackoverflow.com/documentation/java/5382/java-pitfalls-language-syntax/20112/pitfall-using-to-test-a-boolean#t=201612061213124111596)
</li>

### The Reference `==` and `!=` operators

If both operands are object references, the `==` and `!=` operators test if the two operands **refer to the same object**.  This often not what you want.  To test if two objects are equal **by value**, the `.equals()` method should be used instead.

```java
String s1 = "We are equal";
String s2 = new String("We are equal");

s1.equals(s2); // true

// WARNING - don't use == or != with String values
s1 == s2;      // false

```

Warning: using `==` and `!=` to compare `String` values is **incorrect** in most cases; see [http://stackoverflow.com/documentation/java/4388/java-pitfalls/16290/using-to-compare-strings](http://stackoverflow.com/documentation/java/4388/java-pitfalls/16290/using-to-compare-strings) .  A similar problem applies to primitive wrapper types; see [http://stackoverflow.com/documentation/java/4388/java-pitfalls/8996/using-to-compare-primitive-wrappers-objects-such-as-integer](http://stackoverflow.com/documentation/java/4388/java-pitfalls/8996/using-to-compare-primitive-wrappers-objects-such-as-integer) .

### About the NaN edge-cases

[JLS 15.21.1](https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.21.1) states the following:

> 
If either operand is `NaN`, then the result of `==` is `false` but the result of `!=` is `true`.   Indeed, the test `x != x` is `true` if and only if the value of `x` is `NaN`.


This behavior is (to most programmers) unexpected.  If you test if a `NaN` value is equal to itself, the answer is "No it isn't!".  In other words, `==` is not **reflexive** for `NaN` values.

However, this is not a Java "oddity", this behavior is specified in the IEEE 754 floating-point standards, and you will find that it is implemented by most modern programming languages.  (For more information, see [http://stackoverflow.com/a/1573715/139985](http://stackoverflow.com/a/1573715/139985) ... noting that this is written by someone who was "in the room when the decisions were made"!)



## The Lambda operator ( -> )


From Java 8 onwards, the Lambda operator ( `->` ) is the operator used to introduce a Lambda Expression.  There are two common syntaxes, as illustrated by these examples:

```

 a -> a + 1              // a lambda that adds one to its argument
  a -> { return a + 1; }  // an equivalent lambda using a block.

```

A lambda expression defines an anonymous function, or more correctly an instance of an anonymous class that implements a **functional interface**.

(This example is included here for completeness.  Refer to the [Lambda Expressions](http://stackoverflow.com/documentation/java/91/lambda-expressions#t=201701012346164142062) topic for the full treatment.)



## The Relational Operators (<, <=, >, >=)


The operators `<`, `<=`, `>` and `>=` are binary operators for comparing numeric types.  The meaning of the operators is as you would expect.  For example, if `a` and `b` are declared as any of `byte`, `short`, `char`, `int`, `long`, `float`, `double` or the corresponding boxed types:

```java
- `a < b` tests if the value of `a` is less than the value of `b`. 
- `a <= b` tests if the value of `a` is less than or equal to the value of `b`. 
- `a > b` tests if the value of `a` is greater than the value of `b`. 
- `a >= b` tests if the value of `a` is greater than or equal to the value of `b`. 

```

The result type for these operators is `boolean` in all cases.

Relational operators can be used to compare numbers with different types.  For example:

```java
int i = 1;
long l = 2;
if (i < l) {
    System.out.println("i is smaller");
}

```

Relational operators can be used when either or both numbers are instances of boxed numeric types.  For example:

```java
Integer i = 1;   // 1 is autoboxed to an Integer
Integer j = 2;   // 2 is autoboxed to an Integer
if (i < j) {
    System.out.println("i is smaller");
}

```

The precise behavior is summarized as follows:

1. If one of the operands is a boxed type, it is unboxed.
1. If either of the operands now a `byte`, `short` or `char`, it is promoted to an `int`.
1. If the types of the operands are not the same, then the operand with the "smaller" type is promoted to the "larger" type.
1. The comparison is performed on the resulting `int`, `long`, `float` or `double` values.

You need to be careful with relational comparisons that involve floating point numbers:

- Expressions that compute floating point numbers often incur rounding errors due to the fact that the computer floating-point representations have limited precision.
- When comparing an integer type and a floating point type, the conversion of the integer to floating point can also lead to rounding errors.

Finally, Java does bit support the use of relational operators with any types other than the ones listed above.  For example, you **cannot** use these operators to compare strings, arrays of numbers, and so on.



#### Remarks


An **operator** is a symbol (or symbols) that tells a Java program to perform an **operation** on one, two or three **operands**.  An operator and its operands form an **expression** (see the Expressions topic).  The operands of an operator are themselves expressions.

This topic describes the 40 or so distinct operators defined by Java.  The separate Expressions topic explains:

- how operators, operands and other things are combined into expressions,
- how the expressions are evaluated, and
- how expression typing, conversions, and expression evaluation work.

