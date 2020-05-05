---
metaTitle: "Basic Control Structures"
description: "do...while Loop, For Each, Switch statement, Continue Statement in Java, If / Else If / Else Control, For Loops, Ternary Operator, While Loops, If / Else, Break, Try ... Catch ... Finally, Nested break / continue"
---

# Basic Control Structures



## do...while Loop


The `do...while` loop differs from other loops in that it is guaranteed to execute **at least once**.   It is also called the "post-test loop" structure because the conditional statement is performed after the main loop body.

```java
int i = 0;
do {
    i++;
    System.out.println(i);
} while (i < 100); // Condition gets checked AFTER the content of the loop executes.

```

In this example, the loop will run until the number `100` is printed (even though the condition is `i < 100` and not `i <= 100`), because the loop condition is evaluated **after** the loop executes.

With the guarantee of at least one execution, it is possible to declare variables outside of the loop and initialize them inside.

```java
String theWord;
Scanner scan = new Scanner(System.in);
do {
    theWord = scan.nextLine();
} while (!theWord.equals("Bird"));

System.out.println(theWord);

```

In this context, `theWord` is defined outside of the loop, but since it's guaranteed to have a value based on its natural flow, `theWord` will be initialized.



## For Each


With Java 5 and up, one can use for-each loops, also known as enhanced for-loops:

```java
List strings = new ArrayList();
        
strings.add("This");
strings.add("is");
strings.add("a for-each loop");
        
<b>for (String string : strings) {
    System.out.println(string);
}</b>
```

For each loops can be used to iterate over [Arrays](http://stackoverflow.com/documentation/java/99/arrays#t=201601170604583604413) and implementations of the [`Iterable`](https://docs.oracle.com/javase/8/docs/api/java/lang/Iterable.html) interface, the later includes [Collections](http://stackoverflow.com/documentation/java/90/collections#t=201601170605254314553) classes, such as `List` or `Set`.

The loop variable can be of any type that is assignable from the source type.

The loop variable for a enhanced for loop for `Iterable<T>` or `T[]` can be of type `S`, if

- `T extends S`
- both `T` and `S` are primitive types and assignable without a cast
- `S` is a primitive type and `T` can be converted to a type assignable to `S` after unboxing conversion.
- `T` is a primitive type and can be converted to `S` by autoboxing conversion.

**Examples:**

```java
T elements = ...
for (S s : elements) {
}

```

|T|S|Compiles
|---|---|---|---|---|---|---|---|---|---
|int[]|long|yes
|long[]|int|no
|`Iterable<Byte>`|long|yes
|`Iterable<String>`|CharSequence|yes
|`Iterable<CharSequence>`|String|no
|int[]|Long|no
|int[]|Integer|yes



## Switch statement


The `switch` statement is Java's multi-way branch statement. It is used to take the place of long `if`-`else if`-`else` chains, and make them more readable.  However, unlike `if` statements, one may not use inequalities; each value must be concretely defined.

There are three critical components to the `switch` statement:

- `case`: This is the value that is evaluated for equivalence with the argument to the `switch` statement.
- `default`:  This is an optional, catch-all expression, should none of the `case` statements evaluate to `true`.
- Abrupt completion of the `case` statement; usually `break`:  This is required to prevent the undesired evaluation of further `case` statements.

With the exception of `continue`, it is possible to use any statement which would cause the [abrupt completion of a statement](https://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html#jls-14.1).  This includes:

- `break`
- `return`
- `throw`

In the example below, a typical `switch` statement is written with four possible cases, including `default`.

```java
Scanner scan = new Scanner(System.in);
int i = scan.nextInt();
switch (i) {
    case 0:
        System.out.println("i is zero");
        break;
    case 1:
        System.out.println("i is one");
        break;
    case 2:
        System.out.println("i is two");
        break;
    default:
        System.out.println("i is less than zero or greater than two");
}

```

By omitting `break` or any statement which would an abrupt completion, we can leverage what are known as "fall-through" cases, which evaluate against several values.  This can be used to create ranges for a value to be successful against, but is still not as flexible as inequalities.

```java
Scanner scan = new Scanner(System.in);
int foo = scan.nextInt();
switch(foo) {
    case 1:
        System.out.println("I'm equal or greater than one");
    case 2:
    case 3:    
        System.out.println("I'm one, two, or three");
        break;
    default:
        System.out.println("I'm not either one, two, or three");
}

```

In case of `foo == 1` the output will be:

```java
I'm equal or greater than one
I'm one, two, or three

```

In case of `foo == 3` the output will be:

```java
I'm one, two, or three

```

The switch statement can also be used with `enum`s.

```java
enum Option {
    BLUE_PILL,
    RED_PILL
}

public void takeOne(Option option) {
    switch(option) {
        case BLUE_PILL:
            System.out.println("Story ends, wake up, believe whatever you want.");
            break;
        case RED_PILL:
            System.out.println("I show you how deep the rabbit hole goes.");
            break;
            
    }
}

```

The `switch` statement can also be used with `String`s.

```java
public void rhymingGame(String phrase) {
    switch (phrase) {
        case "apples and pears":
            System.out.println("Stairs");
            break;
        case "lorry":
            System.out.println("truck");
            break;
        default:
            System.out.println("Don't know any more");
    }
}

```



## Continue Statement in Java


The continue statement is used to skip the remaining steps in the current iteration
and start with the next loop iteration. The control goes from the `continue` statement to the step value (increment or decrement), if any.

```java
String[] programmers = {"Adrian", "Paul", "John", "Harry"};

    //john is not printed out
    for (String name : programmers) {
        if (name.equals("John"))
            continue;
        System.out.println(name);
    }

```

The `continue` statement can also make the control of the program shift to the step value (if any) of a named loop:

```java
Outer: // The name of the outermost loop is kept here as 'Outer'
for(int i = 0; i < 5; )
{
    for(int j = 0; j < 5; j++)
    {
        continue Outer;
    }
}

```



## If / Else If / Else Control


```java
if (i < 2) {
  System.out.println("i is less than 2");
} else if (i > 2) {
  System.out.println("i is more than 2");
} else {
  System.out.println("i is not less than 2, and not more than 2");
}

```

The `if` block will only run when `i` is 1 or less.

The `else if` condition is checked only if all the conditions before it (in previous `else if` constructs, and the parent `if` constructs) have been tested to `false`. In this example, the `else if` condition will only be checked if `i` is greater than or equal to 2.

If its result is `true`, its block is run, and any `else if` and `else` constructs after it will be skipped.

If none of the `if` and `else if` conditions have been tested to `true`, the `else` block at the end will be run.



## For Loops


```java
for (int i = 0; i < 100; i++) {
    System.out.println(i);
}

```

The three components of the `for` loop (separated by `;`) are variable declaration/initialization (here `int i = 0`), the condition (here `i < 100`), and the increment statement (here `i++`). The variable declaration is done once as if placed just inside the `{` on the first run. Then the condition is checked, if it is `true` the body of the loop will execute, if it is `false` the loop will stop. Assuming the loop continues, the body will execute and finally when the `}` is reached the increment statement will execute just before the condition is checked again.

The curly braces are optional (you can one line with a semicolon) if the loop contains just one statement. But, it's always recommended to use braces to avoid misunderstandings and bugs.

The `for` loop components are optional. If your business logic contains one of these parts, you can omit the corresponding component from your `for` loop.

```java
int i = obj.getLastestValue(); // i value is fetched from a method
    
for (; i < 100; i++) { // here initialization is not done
    System.out.println(i);
}

```

The `for (;;) { function-body }` structure is equal to a `while (true)` loop.

`Nested For Loops`

Any looping statement having another loop statement inside called nested loop. The same way for looping having more inner loop is called 'nested for loop'.

```

   for(;;){
        //Outer Loop Statements
        for(;;){
            //Inner Loop Statements
        }
        //Outer Loop Statements
    }

```

Nested for loop can be demonstrated to print triangle shaped numbers.

```java
for(int i=9;i>0;i--){//Outer Loop
    System.out.println();
    for(int k=i;k>0;k--){//Inner Loop -1
        System.out.print(" ");
    }
    for(int j=i;j<=9;j++){//Inner Loop -2
        System.out.print(" "+j);
    }
 }

```



## Ternary Operator


Sometimes you have to check for a condition and set the value of a variable.

For ex.

```java
String name;

if (A > B) {
    name = "Billy";
} else {
    name = "Jimmy";
}

```

This can be easily written in one line as

```java
String name = A > B ? "Billy" : "Jimmy";

```

The value of the variable is set to the value immediately after the condition, if the condition is true. If the condition is false, the second value will be given to the variable.



## While Loops


```java
int i = 0;
while (i < 100) { // condition gets checked BEFORE the loop body executes
    System.out.println(i);
    i++;
}

```

A `while` loop runs as long as the condition inside the parentheses is `true`.  This is also called the "pre-test loop" structure because the conditional statement must be met before the main loop body is performed every time.

The curly braces are optional if the loop contains just one statement, but some coding style conventions prefers having the braces regardless.



## If / Else


```java
int i = 2;
if (i < 2) {
  System.out.println("i is less than 2");
} else {
  System.out.println("i is greater than 2");
}

```

An `if` statement executes code conditionally depending on the result of the condition in parentheses. When condition in parentheses is true it will enter to the block of if statement which is defined by curly braces like `{` and `}`. opening bracket till the closing bracket is the scope of the if statement.

The `else` block is optional and can be omitted. It runs if the `if` statement is `false` and does not run if the `if` statement is true Because in that case `if` statement executes.

See also: Ternary If



## Break


The `break` statement ends a loop (like `for`, `while`) or the evaluation of a [switch statement](http://stackoverflow.com/documentation/java/118/basic-control-structures/614/switch-statements).

Loop:

```java
while(true) {
   if(someCondition == 5) {
       break;
   }
}

```

The loop in the example would run forever. But when `someCondition` equals `5` at some point of execution, then the loop ends.

If multiple loops are cascaded, only the most inner loop ends using `break`.



## Try ... Catch ... Finally


The `try { ... } catch ( ... ) { ... }` control structure is used for handling [Exceptions](http://stackoverflow.com/documentation/java/89/exceptions).

```java
String age_input = "abc";
try {
    int age = Integer.parseInt(age_input);
    if (age >= 18) {
        System.out.println("You can vote!");
    } else {
        System.out.println("Sorry, you can't vote yet.");
    }
} catch (NumberFormatException ex) {
    System.err.println("Invalid input.  '" + age_input + "' is not a valid integer.");
}

```

This would print:

> 
Invalid input.  'abc' is not a valid integer.


A `finally` clause can be added after the `catch`. The `finally` clause would always be executed, regardless of whether an exception was thrown.

`try { ... } catch ( ... ) { ... } finally { ... }`

```java
String age_input = "abc";
try {
    int age = Integer.parseInt(age_input);
    if (age >= 18) {
        System.out.println("You can vote!");
    } else {
        System.out.println("Sorry, you can't vote yet.");
    }
} catch (NumberFormatException ex) {
    System.err.println("Invalid input.  '" + age_input + "' is not a valid integer.");
} finally {
    System.out.println("This code will always be run, even if an exception is thrown");
}

```

This would print:

> 
<p>Invalid input.  'abc' is not a valid integer.<br />
This code will always be run, even if an exception is thrown</p>




## Nested break / continue


It's possible to `break` / `continue` to an outer loop by using label statements:

```java
outerloop:
for(...) {
    innerloop:
    for(...) {
        if(condition1)
            break outerloop;

        if(condition2)
            continue innerloop; // equivalent to: continue;
    }
}

```

There is no other use for labels in Java.



#### Remarks


All control structures, unless otherwise noted, make use of [block statements](https://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html).  These are denoted by curly braces `{}`.

This differs from [normal statements](https://docs.oracle.com/javase/specs/jls/se7/html/jls-14.html#jls-14.5), which do **not** require curly braces, but also come with a stiff caveat in that only the line **immediately following** the previous statement would be considered.

Thus, it is perfectly valid to write any of these control structures without curly braces, so long as only **one** statement follows the beginning, but it is ****strongly discouraged****, as it can lead to buggy implementations, or broken code.

Example:

```java
// valid, but discouraged
Scanner scan = new Scanner(System.in);
int val = scan.nextInt();
if(val % 2 == 0)
    System.out.println("Val was even!");


// invalid; will not compile
// note the misleading indentation here
for(int i = 0; i < 10; i++)
    System.out.println(i);
    System.out.println("i is currently: " + i);

```

