---
metaTitle: "Java - Expressions"
description: "Operator Precedence, Expression evaluation order, Expression Basics, Constant Expressions"
---

# Expressions




## Operator Precedence


When an expression contains multiple operators, it can potentially be read in different ways.  For example, the mathematical expression `1 + 2 x 3` could be read in two ways:

1. Add `1` and `2` and multiply the result by `3`.  This gives the answer `9`.  If we added parentheses, this would look like `( 1 + 2 ) x 3`.
1. Add `1` to the result of multiplying `2` and `3`.  This gives the answer `7`.  If we added parentheses, this would look like `1 + ( 2 x 3 )`.

In mathematics, the convention is to read the expression the second way.  The general rule is that multiplication and division are done before addition and subtraction.  When more advanced mathematical notation is used, either the meaning is either "self-evident" (to a trained mathematician!), or parentheses are added to disambiguate. In either case, the effectiveness of the notation to convey meaning depends on the intelligence and shared knowledge of the mathematicians.

Java has the same clear rules on how to read an expression, based on the **precedence** of the operators that are used.

In general, each operator is ascribed a **precedence** value; see the table below.

For example:

```

 1 + 2 * 3

```

The precedence of `+` is lower than the precedence of `*`, so the result of the expression is 7, not 9.

|Description|Operators / constructs (primary)|Precedence|Associativity
|---|---|---|---|---|---|---|---|---|---
|Qualifier<br>Parentheses<br>Instance creation<br>Field access<br>Array access<br>Method invocation<br>Method reference|name`.`name<br>`(`expr`)`<br>`new`<br>primary`.`name<br>primary`[`expr`]`<br>primary`(`expr, ...`)`<br>primary`::`name|15|Left to right
|Post increment|expr`++`, expr`--`|14|-
|Pre increment<br>Unary<br>Cast<sup>1</sup>|`++`expr, `--`expr,<br>`+`expr, `-`expr, `~`expr, `!`expr,<br>`(`type`)`expr|13|-<br>Right to left<br>Right to left
|Multiplicative|* / %|12|Left to right
|Additive|+ -|11|Left to right
|Shift|<< >> >>>|10|Left to right
|Relational|< > <= >= `instanceof`|9|Left to right
|Equality|== !=|8|Left to right
|Bitwise AND|&|7|Left to right
|Bitwise exclusive OR|^|6|Left to right
|Bitwise inclusive OR|||5|Left to right
|Logical AND|&&|4|Left to right
|Logical OR||||3|Left to right
|Conditional<sup>1</sup>|? :|2|Right to left
|Assignment<br>Lambda<sup>1</sup>|= *= /= %= += -= <<= >>= >>>= &= ^= |=<br>->|1|Right to left

<sup>1</sup> Lambda expression precedence is complex, as it can also occur after a cast, or as the third part of the conditional ternary operator.



## Expression evaluation order


Java expressions are evaluated following the following rules:

- Operands are evaluated from left to right.
- The operands of an operator are evaluated before the operator.
- Operators are evaluated according to operator precedence
- Argument lists are evaluated from left to right.

### Simple Example

In the following example:

```java
int i = method1() + method2();

```

the order of evaluation is:

1. The left operand of `=` operator is evaluated to the address of `i`.
1. The left operand of the `+` operator (`method1()`) is evaluated.
1. The right operand of the `+` operator (`method2()`) is evaluated.
1. The `+` operation is evaluated.
1. The `=` operation is evaluated, assigning the result of the addition to `i`.

Note that if the effects of the calls are observable, you will be able to observe that the call to `method1` occurs before the call to `method2`.

### Example with an operator which has a side-effect

In the following example:

```java
int i = 1;
intArray[i] = ++i + 1;

```

the order of evaluation is:

1. The left operand of `=` operator is evaluated.  This gives the address of `intArray[1]`.
1. The pre-increment is evaluated.  This adds `1` to `i`, and evaluates to `2`.
1. The right hand operand of the `+` is evaluated.
1. The `+` operation is evaluated to: `2 + 1` -> `3`.
1. The `=` operation is evaluated, assigning `3` to `intArray[1]`.

Note that since the left-hand operand of the `=` is evaluated first, it is not influenced by the side-effect of the `++i` subexpression.

Reference:

- [JLS 15.7 - Evaluation Order](https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.7)



## Expression Basics


Expressions in Java are the primary construct for doing calculations.  Here are some examples:

```java
1                 // A simple literal is an expression
1 + 2             // A simple expression that adds two numbers
(i + j) / k       // An expression with multiple operations
(flag) ? c : d    // An expression using the "conditional" operator
(String) s        // A type-cast is an expression
obj.test()        // A method call is an expression
new Object()      // Creation of an object is an expression
new int[]         // Creation of an object is an expression

```

In general, an expression consists of the following forms:

<li>Expression names which consist of:
<ul>
- Simple identifiers; e.g. `someIdentifier`
- Qualified identifiers; e.g. `MyClass.someField`

- Literals; e.g. `1`, `1.0`, `'X'`, `"hello"`, `false` and `null`
- Class literal expressions; e.g. `MyClass.class`
- `this` and `<TypeName> . this`
- Parenthesized expressions; e.g. `( a + b )`
- Class instance creation expressions; e.g. `new MyClass(1, 2, 3)`
- Array instance creation expressions; e.g. `new int[3]`
- Field access expressions; e.g. `obj.someField` or `this.someField`
- Array access expressions; e.g. `vector[21]`
- Method invocations; e.g. `obj.doIt(1, 2, 3)`
- Method references (Java 8 and later); e.g. `MyClass::doIt`

The details of the different forms of expressions may be found in other Topics.

- The [Operators](http://stackoverflow.com/documentation/java/176/operators) topic covers unary, binary and ternary operator expressions.
- The [Lambda expressions](http://stackoverflow.com/documentation/java/91/lambda-expressions#t=20161216024035664514) topic covers lambda expressions and method reference expressions.
- The [Classes and Objects](http://stackoverflow.com/documentation/java/114/classes-and-objects#t=201612160817408962117) topic covers class instance creation expressions.
- The [Arrays](http://stackoverflow.com/documentation/java/99/arrays#t=201612160241346240996) topic covers array access expressions and array instance creation expressions.
- The [Literals](http://stackoverflow.com/documentation/java/8250/literals#t=201612190020004937953) topic covers the different kinds of literals expressions.

### The Type of an Expression

In most cases, an expression has a static type that can be determined at compile time by examining and its subexpressions.  These are referred to as **stand-alone** expressions.

However, (in Java 8 and later) the following kinds of expressions may be **poly expressions**:

- Parenthesized expressions
- Class instance creation expressions
- Method invocation expressions
- Method reference expressions
- Conditional expressions
- Lambda expressions

When an expression is a poly expression, its type may be influenced by the expression's **target type**; i.e. what it is being used for.

### The value of an Expression

The value of an expression is assignment compatible with its type.  The exception to this is when **heap pollution** has occurred; e.g. because "unsafe conversion" warnings have been (inappropriately) suppressed or ignored.

### Expression Statements

Unlike many other languages, Java does not generally allow expressions to be used as statements.  For example:

```java
public void compute(int i, int j) {
    i + j;   // ERROR
}

```

Since the result of evaluating an expression like cannot be use, and since it cannot affect the execution of the program in any other way, the Java designers took the position that such usage is either a mistake, or misguided.

However, this does not apply to all expressions.  A subset of expressions are (in fact) legal as statements.  The set comprises:

- Assignment expression, including **operation-and-becomes** assignments.
- Pre and post increment and decrement expressions.
- Method calls (`void` or non-`void`).
- Class instance creation expressions.



## Constant Expressions


A constant expression is an expression that yields a primitive type or a String, and whose value can be evaluated at compile time to a literal.  The expression must evaluate without throwing an exception, and it must be composed of only the following:

<li>
Primitive and String literals.
</li>
<li>
Type casts to primitive types or `String`.
</li>
<li>
The following unary operators: `+`, `-`, `~` and `!`.
</li>
<li>
The following binary operators: `*`, `/`, `%`, `+`, `-`, `<<`, `>>`, `>>>`, `<`, `<=`, `>`, `>=`, `==`, `!=`, `&`, `^`, `|`, `&&` and `||`.
</li>
<li>
The ternary conditional operator `?` `:`.
</li>
<li>
Parenthesized constant expressions.
</li>
<li>
Simple names that refer to constant variables. (A constant variable is a variable declared as `final` where the initializer expression is itself a constant expression.)
</li>
<li>
Qualified names of the form `<TypeName> . <Identifier>` that refer to constant variables.
</li>

Note that the above list **excludes** `++` and `--`, the assignment operators, `class` and `instanceof`, method calls and references to general variables or fields.

Constant expressions of type `String` result in an "interned" `String`, and floating point operations in constant expressions are evaluated with FP-strict semantics.

### Uses for Constant Expressions

Constant expressions can be used (just about) anywhere that a normal expression can be used. However, they have a special significance in the following contexts.

Constant expressions are required for case labels in switch statements. For example:

```java
switch (someValue) {
case 1 + 1:            // OK
case Math.min(2, 3):   // Error - not a constant expression
    doSomething();
}

```

When the expression on the right hand side of an assignment is a constant expression, then the assignment can perform a primitive narrowing conversion. This is allowed provided that the value of the constant expression is within the range of the type on the left hand side. (See [JLS 5.1.3](https://docs.oracle.com/javase/specs/jls/se8/html/jls-5.html#jls-5.1.3) and [5.2](https://docs.oracle.com/javase/specs/jls/se8/html/jls-5.html#jls-5.2)) For example:

```java
byte b1 = 1 + 1;             // OK - primitive narrowing conversion.
byte b2 = 127 + 1;           // Error - out of range
byte b3 = b1 + 1;            // Error - not a constant expession
byte b4 = (byte) (b1 + 1);   // OK

```

When a constant expression is used as the condition in a `do`, `while` or `for`, then it affects the readability analysis. For example:

```java
while (false) {
    doSomething();           // Error - statenent not reachable
}
boolean flag = false;
while (flag) {
    doSomething();           // OK
}

```

(Note that this does not apply `if` statements.  The Java compiler allows the `then` or `else` block of an `if` statement to be unreachable.  This is the Java analog of conditional compilation in C and C++.)

Finally, `static final` fields in an class or interface with constant expression initializers are initialized eagerly.  Thus, it is guaranteed that these constants will be observed in the initialized state, even when there is a cycle in the class initialization dependency graph.

For more information, refer to [JLS 15.28. Constant Expressions](https://docs.oracle.com/javase/specs/jls/se8/html/jls-15.html#jls-15.27.3).



#### Remarks


For a reference on the operators that can be used in expressions, see [Operators](http://stackoverflow.com/documentation/java/176/operators).

