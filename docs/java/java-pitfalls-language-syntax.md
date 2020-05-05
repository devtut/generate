---
metaTitle: "Java - Java Pitfalls - Language syntax"
description: "Pitfall - Missing a ‘break’ in a 'switch' case, Pitfall - Leaving out braces: the dangling if and dangling else problems, Pitfall - Declaring classes with the same names as standard classes, Pitfall - Ignoring method visibility, Pitfall - Octal literals, Pitfall - Using '==' to test a boolean, Pitfall - Misplaced semicolons and missing braces, Pitfall - Wildcard imports can make your code fragile, Pitfall: Using 'assert' for argument or user input validation, Pitfall - Overloading instead of overriding, Pitfall of Auto-Unboxing Null Objects into Primitives"
---

# Java Pitfalls - Language syntax


Several Java programming language misusage might conduct a program to generate incorrect results despite being compiled correctly. This topic main purpose is to list common pitfalls with their causes, and to propose the correct way to avoid falling in such problems.



## Pitfall - Missing a ‘break’ in a 'switch' case


These Java issues can be very embarrassing, and sometimes remain undiscovered until run in production. Fallthrough behavior in switch statements is often useful; however, missing a “break” keyword when such behavior is not desired can lead to disastrous results. If you have forgotten to put a “break” in “case 0” in the code example below, the program will write “Zero” followed by “One”, since the control flow inside here will go through the entire “switch” statement until it reaches a “break”. For example:

```java
public static void switchCasePrimer() {
        int caseIndex = 0;
        switch (caseIndex) {
            case 0:
                System.out.println("Zero");
            case 1:
                System.out.println("One");
                break;
            case 2:
                System.out.println("Two");
                break;
            default:
                System.out.println("Default");
        }
}

```

In most cases, the cleaner solution would be to use interfaces and move code with specific behaviour into separate implementations (**composition over inheritance**)

If a switch-statement is unavoidable it is recommended to document "expected" fallthroughs if they occur. That way you show fellow developers that you are aware of the missing break, and that this is expected behaviour.

```java
switch(caseIndex) {
    [...]
    case 2:
        System.out.println("Two");
        // fallthrough
    default:
        System.out.println("Default");

```



## Pitfall - Leaving out braces: the "dangling if" and "dangling else" problems


The latest version of the Oracle Java style guide mandates that the "then" and "else" statements in an `if` statement should always be enclosed in "braces" or "curly brackets".  Similar rules apply to the bodies of various loop statements.

```java
if (a) {           // <- open brace
    doSomething();
    doSomeMore();
}                  // <- close brace

```

This is not actually required by Java language syntax.  Indeed, if the "then" part of an `if` statement is a single statement, it is legal to leave out the braces

```java
if (a)
    doSomething();

```

or even

```java
if (a) doSomething();

```

However there are dangers in ignoring Java style rules and leaving out the braces.  Specifically, you significantly increase the risk that code with faulty indentation will be misread.

**The "dangling if" problem:**

Consider the example code from above, rewritten without braces.

```java
if (a)
   doSomething();
   doSomeMore();

```

This code **seems to say** that the calls to `doSomething` and `doSomeMore` will both occur **if and only if** `a` is `true`.  In fact, the code is incorrectly indented.  The Java Language Specification that the `doSomeMore()` call is a separate statement following the `if` statement.  The correct indentation is as follows:

```java
if (a)
   doSomething();
doSomeMore();

```

**The "dangling else" problem**

A second problem appears when we add `else` to the mix.  Consider the following example with missing braces.

```java
if (a)
   if (b)
      doX();
   else if (c)
      doY(); 
else
   doZ();

```

The code above **seems to say** that `doZ` will be called when `a` is `false`.  In fact, the indentation is incorrect once again.  The correct indentation for the code is:

```java
if (a)
   if (b)
      doX();
   else if (c)
      doY(); 
   else
      doZ();

```

If the code was written according to the Java style rules, it would actually look like this:

```java
if (a) {
   if (b) {
      doX();
   } else if (c) {
      doY(); 
   } else {
      doZ();
   }
}

```

To illustrate why that is better, suppose that you had accidentally mis-indented the code.  You might end up with something like this:

```java
if (a) {                         if (a) {
   if (b) {                          if (b) {
      doX();                            doX();
   } else if (c) {                   } else if (c) {
      doY();                            doY();
} else {                         } else {
   doZ();                            doZ();
}                                    }
}                                }

```

But in both cases, the mis-indented code "looks wrong" to the eye of an experienced Java programmer.



## Pitfall - Declaring classes with the same names as standard classes


Sometimes, programmers who are new to Java make the mistake of defining a class with a name that is the same as a widely used class.  For example:

```java
package com.example;

/**
 * My string utilities
 */
public class String {
    ....
}

```

Then they wonder why they get unexpected errors.  For example:

```java
package com.example;

public class Test {
    public static void main(String[] args) {
        System.out.println("Hello world!");
    }
}

```

If you compile and then attempt to run the above classes you will get an error:

```java
$ javac com/example/*.java
$ java com.example.Test
Error: Main method not found in class test.Test, please define the main method as:
   public static void main(String[] args)
or a JavaFX application class must extend javafx.application.Application

```

Someone looking at the code for the `Test` class would see the declaration of `main` and look at its signature and wonder what the `java` command is complaining about.  But in fact, the `java` command is telling the truth.

When we declare a version of `String` in the same package as `Test`, this version takes precedence over the automatic import of `java.lang.String`.  Thus, the signature of the `Test.main` method is actually

```java
void main(com.example.String[] args) 

```

instead of

```java
void main(java.lang.String[] args)

```

and the `java` command will not recognize **that** as an entrypoint method.

Lesson: Do not define classes that have the same name as existing classes in `java.lang`, or other commonly used classes in the Java SE library.  If you do that, you are setting yourself open for all sorts of obscure errors.



## Pitfall - Ignoring method visibility


Even experienced Java developers tend to think that Java has only three protection modifiers. The language actually has four! The **package private** (a.k.a. default) level of visibility is often forgotten.

You should pay attention to what methods you make public. The public methods in an application are the application’s visible API. This should be as small and compact as possible, especially if you are writing a reusable library (see also the [SOLID](https://en.wikipedia.org/wiki/Open/closed_principle) principle). It is important to similarly consider the visibility of all methods, and to only use protected or package private access where appropriate.

When you declare methods that should be **private** as public, you expose the internal implementation details of the class.

A corollary to this is that you only [unit test](http://www.tutorialspoint.com/junit/junit_test_framework.htm) the public methods of your class - in fact you can **only** test public methods. It is bad practice to increase the visibility of private methods just to be able to run unit tests against those methods. Testing public methods that call the methods with more restrictive visibility should be sufficient to test an entire API. You should **never** expand your API with more public methods only to allow unit testing.



## Pitfall - Octal literals


Consider the following code snippet:

```java
// Print the sum of the numbers 1 to 10
int count = 0;
for (int i = 1; i < 010; i++) {    // Mistake here ....
    count = count + i;
}
System.out.println("The sum of 1 to 10 is " + count);

```

A Java beginner might be surprised to know that the above program prints the wrong answer.  It actually prints the sum of the numbers 1 to 8.

The reason is that an integer literal that starts with the digit zero ('0') is interpreted by the Java compiler as an octal literal, not a decimal literal as you might expect.  Thus, `010` is the octal number 10, which is 8 in decimal.



## Pitfall - Using '==' to test a boolean


Sometimes a new Java programmer will write code like this:

```java
public void check(boolean ok) {
    if (ok == true) {           // Note 'ok == true'
        System.out.println("It is OK");
    }
}

```

An experienced programmer would spot that as being clumsy and want to rewrite it as:

```java
public void check(boolean ok) {
    if (ok) {
       System.out.println("It is OK");
    }
}

```

However, there is more wrong with `ok == true` than simple clumsiness.  Consider this variation:

```java
public void check(boolean ok) {
    if (ok = true) {           // Oooops!
        System.out.println("It is OK");
    }
}

```

Here the programmer has mistyped `==` as `=` ... and now the code has a subtle bug.  The expression `x = true` unconditionally assigns `true` to `x` and then evaluates to `true`.  In other words, the `check` method will now print "It is OK" no matter what the parameter was.

The lesson here is to get out of the habit of using `== false` and `== true`.  In addition to being verbose, they make your coding more error prone.

Note: A possible alternative to `ok == true` that avoids the pitfall is to use [Yoda conditions](https://en.wikipedia.org/wiki/Yoda_conditions); i.e. put the literal on the left side of the relational operator, as in `true == ok`.  This works, but most programmers would probably agree that Yoda conditions look odd.  Certainly `ok` (or `!ok`) is more concise and more natural.



## Pitfall - Misplaced semicolons and missing braces


This is a mistake that causes real confusion for Java beginners, at least the first time that they do it. Instead of writing this:

```java
if (feeling == HAPPY)
    System.out.println("Smile");
else
    System.out.println("Frown");

```

they accidentally write this:

```java
if (feeling == HAPPY);
    System.out.println("Smile");
else
    System.out.println("Frown");

```

and are puzzled when the Java compiler tells them that the `else` is misplaced. The Java compiler with interpret the above as follows:

```java
if (feeling == HAPPY)
    /*empty statement*/ ;
System.out.println("Smile");   // This is unconditional
else                           // This is misplaced.  A statement cannot
                               // start with 'else'
System.out.println("Frown");

```

In other cases, there will be no be compilation errors, but the code won't do what the programmer intends. For example:

```java
for (int i = 0; i < 5; i++);
    System.out.println("Hello");

```

only prints "Hello" once. Once again, the spurious semicolon means that the body of the `for` loop is an empty statement. That means that the `println` call that follows is unconditional.

Another variation:

```java
for (int i = 0; i < 5; i++);
    System.out.println("The number is " + i);

```

This will give a "Cannot find symbol" error for `i`.  The presence of the spurious semicolon means that the `println` call is attempting to use `i` outside of its scope.

In those examples, there is a straight-forward solution: simply delete the spurious semicolon. However, there are some deeper lessons to be drawn from these examples:

<li>
The semicolon in Java is not "syntactic noise". The presence or absence of a semicolon can change the meaning of your program. Don't just add them at the end of every line.
</li>
<li>
Don't trust your code's indentation. In the Java language, extra whitespace at the beginning of a line is ignored by the compiler.
</li>
<li>
Use an automatic indenter. All IDEs and many simple text editors understand how to correctly indent Java code.
</li>
<li>
This is the most important lesson. Follow the latest Java style guidelines, and put braces around the "then" and "else" statements and the body statement of a loop. The open brace (`{`) should not be on a new line.
</li>

If the programmer followed the style rules then the `if` example with a misplaced semicolons would look like this:

```java
if (feeling == HAPPY); {
    System.out.println("Smile");
} else {
    System.out.println("Frown");
}

```

That looks odd to an experienced eye. If you auto-indented that code, it would probably look like this:

```java
if (feeling == HAPPY); {
                           System.out.println("Smile");
                       } else {
                           System.out.println("Frown");
                       }

```

which should stand out as wrong to even a beginner.



## Pitfall - Wildcard imports can make your code fragile


Consider the following partial example:

```java
import com.example.somelib.*;
import com.acme.otherlib.*;

public class Test {
    private Context x = new Context();   // from com.example.somelib
    ...
}

```

Suppose that when when you first developed the code against version 1.0 of `somelib` and version 1.0 of `otherlib`. Then at some later point, you need to upgrade your dependencies to a later versions, and you decide to use `otherlib` version 2.0.  Also suppose that one of the changes that they made to `otherlib` between 1.0 and 2.0 was to add a `Context` class.

Now when you recompile `Test`, you will get a compilation error telling you that `Context` is an ambiguous import.

If you are familiar with the codebase, this probably is just a minor inconvenience.  If not, then you have some work to do to address this problem, here and potentially elsewhere.

The problem here is the wildcard imports.  On the one hand, using wildcards can make your classes a few lines shorter.  On the other hand:

<li>
Upwards compatible changes to other parts of your codebase, to Java standard libraries or to 3rd party libraries can lead to compilation errors.
</li>
<li>
Readability suffers.  Unless you are using an IDE, figuring out which of the wildcard imports is pulling in a named class can be difficult.
</li>

The lesson is that it is a bad idea to use wildcard imports in code that needs to be long lived.  Specific (non-wildcard) imports are not much effort to maintain if you use an IDE, and the effort is worthwhile.



## Pitfall: Using 'assert' for argument or user input validation


A question that occasionally on StackOverflow is whether it is appropriate to use `assert` to validate arguments supplied to a method, or even inputs provided by the user.

The simple answer is that it is not appropriate.

Better alternatives include:

- Throwing an IllegalArgumentException using custom code.
- Using the `Preconditions` methods available in Google Guava library.
- Using the `Validate` methods available in Apache Commons Lang3 library.

This is what the [Java Language Specification (JLS 14.10, for Java 8)](https://docs.oracle.com/javase/specs/jls/se8/html/jls-14.html#jls-14.10) advises on this matter:

> 
Typically, assertion checking is enabled during program development and testing, and disabled for deployment, to improve performance.
Because assertions may be disabled, programs must not assume that the expressions contained in assertions will be evaluated. Thus, these boolean expressions should generally be free of side effects. Evaluating such a boolean expression should not affect any state that is visible after the evaluation is complete. It is not illegal for a boolean expression contained in an assertion to have a side effect, but it is generally inappropriate, as it could cause program behavior to vary depending on whether assertions were enabled or disabled.
In light of this, assertions should not be used for argument checking in public methods. Argument checking is typically part of the contract of a method, and this contract must be upheld whether assertions are enabled or disabled.
A secondary problem with using assertions for argument checking is that erroneous arguments should result in an appropriate run-time exception (such as `IllegalArgumentException`, `ArrayIndexOutOfBoundsException`, or `NullPointerException`). An assertion failure will not throw an appropriate exception. Again, it is not illegal to use assertions for argument checking on public methods, but it is generally inappropriate. It is intended that `AssertionError` never be caught, but it is possible to do so, thus the rules for try statements should treat assertions appearing in a try block similarly to the current treatment of throw statements.




## Pitfall - Overloading instead of overriding


Consider the following example:

```java
public final class Person {
    private final String firstName;
    private final String lastName;
   
    public Person(String firstName, String lastName) {
        this.firstName = (firstName == null) ? "" : firstName;
        this.lastName = (lastName == null) ? "" : lastName;
    }

    public boolean equals(String other) {
        if (!(other instanceof Person)) {
            return false;
        }
        Person p = (Person) other;
        return firstName.equals(p.firstName) &&
                lastName.equals(p.lastName);
    }

    public int hashcode() {
        return firstName.hashCode() + 31 * lastName.hashCode();
    }
}

```

This code is not going to behave as expected.  The problem is that the `equals` and `hashcode` methods for `Person` do not override the standard methods defined by `Object`.

- The `equals` method has the wrong signature.  It should be declared as `equals(Object)` not `equals(String)`.
- The `hashcode` method has the wrong name.  It should be `hashCode()` (note the capital **C**).

These mistakes mean that we have declared accidental overloads, and these won't be used if `Person` is used in a polymorphic context.

However, there is a simple way to deal with this (from Java 5 onwards).  Use the `@Override` annotation whenever you **intend** your method to be an override:

```java
public final class Person {
    ...

    @Override
    public boolean equals(String other) {
        ....
    }

    @Override
    public hashcode() {
        ....
    }
}

```

When we add an `@Override` annotation to a method declaration, the compiler will check that the method **does** override (or implement) a method declared in a superclass or interface.  So in the example above, the compiler will give us two compilation errors, which should be enough to alert us to the mistake.



## Pitfall of Auto-Unboxing Null Objects into Primitives


```java
public class Foobar {
    public static void main(String[] args) {

        // example: 
        Boolean ignore = null;
        if (ignore == false) {
            System.out.println("Do not ignore!");
        }
    }
}

```

The pitfall here is that `null` is compared to `false`. Since we're comparing a primitive `boolean` against a `Boolean`, Java attempts to **unbox** the the `Boolean` `Object` into a primitive equivalent, ready for comparison. However, since that value is `null`, a `NullPointerException` is thrown.

Java is incapable of comparing primitive types against `null` values, which causes a `NullPointerException` at runtime. Consider the primitive case of the condition `false == null`; this would generate a **compile time** error `incomparable types: int and <null>`.



#### Remarks


This topic is about specific aspects of the Java language syntax that are either error prone or that that should not be used in certain ways.

