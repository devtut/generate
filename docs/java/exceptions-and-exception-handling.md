---
metaTitle: "Java - Exceptions and exception handling"
description: "Catching an exception with try-catch, The try-with-resources statement, Custom Exceptions, Handling InterruptedException, Return statements in try catch block, The Java Exception Hierarchy - Unchecked and Checked Exceptions, Introduction, Creating and reading stacktraces, Throwing an exception, Advanced features of Exceptions, The try-finally and try-catch-finally statements, The 'throws' clause in a method declaration"
---

# Exceptions and exception handling




## Catching an exception with try-catch


An exception can be caught and handled using the `try...catch` statement.  (In fact `try` statements take other forms, as described in other examples about [`try...catch...finally`](http://stackoverflow.com/documentation/java/89/exceptions-and-exception-handling/25177/) and [`try-with-resources`](http://stackoverflow.com/documentation/java/89/exceptions-and-exception-handling/1581/).)

### Try-catch with one catch block

The most simple form looks like this:

```java
try {
    doSomething();
} catch (SomeException e) {
    handle(e);
}
// next statement

```

The behavior of a simple `try...catch` is as follows:

- The statements in the `try` block are executed.
- If no exception is thrown by the statements in the `try` block, then control passes to the next statement after the `try...catch`.
<li>If an exception is thrown within the `try` block.
<ul>
- The exception object is tested to see if it is an instance of `SomeException` or a subtype.
<li>If it is, then the `catch` block will **catch** the exception:
<ul>
- The variable `e` is bound to the exception object.
- The code within the `catch` block is executed.
- If that code throws an exception, then the newly thrown exception is propagated in place of the original one.
- Otherwise, control passes to the next statement after the `try...catch`.

### Try-catch with multiple catches

A `try...catch` can also have multiple `catch` blocks.  For example:

```java
try {
    doSomething();
} catch (SomeException e) {
    handleOneWay(e)
} catch (SomeOtherException e) {
    handleAnotherWay(e);
}
// next statement

```

If there are multiple `catch` blocks, they are tried one at a time starting with the first one, until a match is found for the exception.  The corresponding handler is executed (as above), and then control is passed to the next statement after the `try...catch` statement.  The `catch` blocks after the one that matches are always skipped, **even if the handler code throws an exception**.

The "top down" matching strategy has consequences for cases where the exceptions in the `catch` blocks are not disjoint.  For example:

```java
try {
    throw new RuntimeException("test");
} catch (Exception e) {
    System.out.println("Exception");
} catch (RuntimeException e) {
    System.out.println("RuntimeException");
}

```

This code snippet will output "Exception" rather than "RuntimeException".  Since `RuntimeException` is a subtype of `Exception`, the first (more general) `catch` will be matched.  The second (more specific) `catch` will never be executed.

The lesson to learn from this is that the most specific `catch` blocks (in terms of the exception types) should appear first, and the most general ones should be last.  (Some Java compilers will warn you if a `catch` can never be executed, but this is not a compilation error.)

### Multi-exception catch blocks

Starting with Java SE 7, a single `catch` block can handle a list of unrelated exceptions. The exception type are listed, separated with a vertical bar (`|`) symbol.  For example:

```java
try {
    doSomething();
} catch (SomeException | SomeOtherException e) {
    handleSomeException(e);
} 

```

The behavior of a multi-exception catch is a simple extension for the single-exception case.  The `catch` matches if the thrown exception matches (at least) one of the listed exceptions.

There is some additional subtlety in the specification.  The type of `e` is a synthetic **union** of the exception types in the list.  When the value of `e` is used, its static type is the least common supertype of the type union.  However, if `e` is rethrown within the `catch` block, the exception types that are thrown are the types in the union.  For example:

```java
public void method() throws IOException, SQLException
    try {
        doSomething();
    } catch (IOException | SQLException e) {
        report(e);
        throw e;
    }

```

In the above, `IOException` and `SQLException` are checked exceptions whose least common supertype is `Exception`.  This means that the `report` method must match `report(Exception)`.  However, the compiler knows that the `throw` can throw only an `IOException` or an `SQLException`.  Thus, `method` can be declared as `throws IOException, SQLException` rather than `throws Exception`.  (Which is a good thing: see [Pitfall - Throwing Throwable, Exception, Error or RuntimeException](http://stackoverflow.com/documentation/java/5381/java-pitfalls-exception-usage/18023/).)



## The try-with-resources statement


As the [try-catch-final statement](http://stackoverflow.com/documentation/java/89/exceptions-and-exception-handling/1581) example illustrates, resource cleanup using a `finally` clause requires a significant amount of "boiler-plate" code to implement the edge-cases correctly.  Java 7 provides a much simpler way to deal with this problem in the form of the **try-with-resources** statement.

### What is a resource?

Java 7 introduced the  `java.lang.AutoCloseable` interface to allow classes to be managed using the **try-with-resources** statement.  Instances of classes that implement `AutoCloseable` are referred to as **resources**.  These typically need to be disposed of in a timely fashion rather than relying on the garbage collector to dispose of them.

The `AutoCloseable` interface defines a single method:

```java
public void close() throws Exception

```

A `close()` method should dispose of the resource in an appropriate fashion.  The specification states that it should be safe to call the method on a resource that has already been disposed of.  In addition, classes that implement `Autocloseable` are **strongly encouraged** to declare the `close()` method to throw a more specific exception than `Exception`, or no exception at all.

A wide range of standard Java classes and interfaces implement `AutoCloseable`.  These include:

- `InputStream`, `OutputStream` and their subclasses
- `Reader`, `Writer` and their subclasses
- `Socket` and `ServerSocket` and their subclasses
- `Channel` and its subclasses, and
- the JDBC interfaces `Connection`, `Statement` and `ResultSet` and their subclasses.

Application and third party classes may do this as well.

### The basic try-with-resource statement

The syntax of a **try-with-resources** is based on classical **try-catch**, **try-finally** and **try-catch-finally** forms.  Here is an example of a "basic" form; i.e. the form without a `catch` or `finally`.

```java
try (PrintStream stream = new PrintStream("hello.txt")) {
    stream.println("Hello world!");
}

```

The resources to be manage are declared as variables in the `(...)` section after the `try` clause.  In the example above, we declare a resource variable `stream` and initialize it to a newly created `PrintStream`.

Once the resource variables have been initialized, the `try` block is executed.  When that completes, `stream.close()` will be called automatically to ensure that the resource does not leak.  Note that the `close()` call happens no matter how the block completes.

### The enhanced try-with-resource statements

The **try-with-resources** statement can be enhanced with `catch` and `finally` blocks, as with the pre-Java 7 **try-catch-finally** syntax.  The following code snippet adds a `catch` block to our previous one to deal with the `FileNotFoundException` that the `PrintStream` constructor can throw:

```java
try (PrintStream stream = new PrintStream("hello.txt")) {
    stream.println("Hello world!");
} catch (FileNotFoundException ex) {
    System.err.println("Cannot open the file");
} finally {
    System.err.println("All done");
}

```

If either the resource initialization or the try block throws the exception, then the `catch` block will be executed.  The `finally` block will always be executed, as with a conventional **try-catch-finally** statement.

There are a couple of things to note though:

- The resource variable is **out of scope** in the `catch` and `finally` blocks.
- The resource cleanup will happen before the statement tries to match the `catch` block.
- If the automatic resource cleanup threw an exception, then that **could** be caught in one of the `catch` blocks.

### Managing multiple resources

The code snippets above show a single resource being managed.  In fact, **try-with-resources** can manage multiple resources in one statement.  For example:

```java
try (InputStream is = new FileInputStream(file1);
     OutputStream os = new FileOutputStream(file2)) {
    // Copy 'is' to 'os'
}

```

This behaves as you would expect.  Both `is` and `os` are closed automatically at the end of the `try` block.  There are a couple of points to note:

- The initializations occur in the code order, and later resource variable initializers can use of the values of the earlier ones.
- All resource variables that were successfully initialized will be cleaned up.
- Resource variables are cleaned up in **reverse order** of their declarations.

Thus, in the above example, `is` is initialized before `os` and cleaned up after it, and `is` will be cleaned up if there is an exception while initializing `os`.

### Equivalence of try-with-resource and classical try-catch-finally

The Java Language Specification specifies the behavior of **try-with-resource** forms in terms of the classical **try-catch-finally** statement.  (Please refer to the JLS for the full details.)

For example, this basic **try-with-resource** :

```java
try (PrintStream stream = new PrintStream("hello.txt")) {
    stream.println("Hello world!");
}

```

is defined to be equivalent to this **try-catch-finally**:

```java
// Note that the constructor is not part of the try-catch statement
PrintStream stream = new PrintStream("hello.txt");

// This variable is used to keep track of the primary exception thrown
// in the try statement. If an exception is thrown in the try block,
// any exception thrown by AutoCloseable.close() will be suppressed.
Throwable primaryException = null;

// The actual try block
try {
    stream.println("Hello world!");
} catch (Throwable t) {
    // If an exception is thrown, remember it for the finally block
    primaryException = t;
    throw t;
} finally {
    if (primaryException == null) {
        // If no exception was thrown so far, exceptions thrown in close() will
        // not be caught and therefore be passed on to the enclosing code.
        stream.close();
    } else {
        // If an exception has already been thrown, any exception thrown in
        // close() will be suppressed as it is likely to be related to the
        // previous exception. The suppressed exception can be retrieved
        // using primaryException.getSuppressed().
        try {
            stream.close();
        } catch (Throwable suppressedException) {
            primaryException.addSuppressed(suppressedException);
        }
    }
}

```

(The JLS specifies that the actual `t` and `primaryException` variables will be invisible to normal Java code.)

The enhanced form of **try-with-resources** is specified as an equivalence with the basic form.  For example:

```java
try (PrintStream stream = new PrintStream(fileName)) {
    stream.println("Hello world!");
} catch (NullPointerException ex) {
    System.err.println("Null filename");
} finally {
    System.err.println("All done");    
}

```

is equivalent to:

```java
try {
    try (PrintStream stream = new PrintStream(fileName)) {
        stream.println("Hello world!");
    }
} catch (NullPointerException ex) {
    System.err.println("Null filename");
} finally {
    System.err.println("All done");    
}    

```



## Custom Exceptions


Under most circumstances, it is simpler from a code-design standpoint to use existing generic [`Exception`](https://docs.oracle.com/javase/8/docs/api/java/lang/Exception.html) classes when throwing exceptions. This is especially true if you only need the exception to carry a simple error message. In that case, [RuntimeException](https://docs.oracle.com/javase/7/docs/api/java/lang/RuntimeException.html) is usually preferred, since it is not a checked Exception. Other exception classes exist for common classes of errors:

- [UnsupportedOperationException](https://docs.oracle.com/javase/7/docs/api/java/lang/UnsupportedOperationException.html) - a certain operation is not supported
- [IllegalArgumentException](https://docs.oracle.com/javase/7/docs/api/java/lang/IllegalArgumentException.html) - an invalid parameter value was passed to a method
- [IllegalStateException](https://docs.oracle.com/javase/7/docs/api/java/lang/IllegalStateException.html) - your API has internally reached a condition that should never happen, or which occurs as a result of using your API in an invalid way

Cases where you **do** want to use a custom exception class include the following:

- You are writing an API or library for use by others, and you want to allow users of your API to be able to specifically catch and handle exceptions from your API, and **be able to differentiate those exceptions from other, more generic exceptions**.
- You are throwing exceptions for a **specific kind of error** in one part of your program, which you want to catch and handle in another part of your program, and you want to be able to differentiate these errors from other, more generic errors.

You can create your own custom exceptions by extending [`RuntimeException`](https://docs.oracle.com/javase/7/docs/api/java/lang/RuntimeException.html) for an unchecked exception, or checked exception by extending any [`Exception`](https://docs.oracle.com/javase/7/docs/api/java/lang/Exception.html) **which is not also subclass of RuntimeException**, because:

> 
Subclasses of Exception that are not also subclasses of RuntimeException are checked exceptions


```java
public class StringTooLongException extends RuntimeException {
    // Exceptions can have methods and fields like other classes
    // those can be useful to communicate information to pieces of code catching
    // such an exception
    public final String value;
    public final int maximumLength;

    public StringTooLongException(String value, int maximumLength){
        super(String.format("String exceeds maximum Length of %s: %s", maximumLength, value));
        this.value = value;
        this.maximumLength = maximumLength;
    }
}

```

Those can be used just as predefined exceptions:

```java
void validateString(String value){
    if (value.length() > 30){
        throw new StringTooLongException(value, 30);
    }
}

```

And the fields can be used where the exception is caught and handled:

```java
void anotherMethod(String value){
    try {
        validateString(value);
    } catch(StringTooLongException e){
        System.out.println("The string '" + e.value + 
                "' was longer than the max of " + e.maximumLength );
    }
}

```

Keep in mind that, according to [Oracle's Java Documentation](http://docs.oracle.com/javase/tutorial/essential/exceptions/runtime.html):

> 
[...] If a client can reasonably be expected to recover from an exception, make it a checked exception. If a client cannot do anything to recover from the exception, make it an unchecked exception.


More:

- [Why does RuntimeException not require an explicit exception handling?](http://stackoverflow.com/a/14995225/3502776)



## Handling InterruptedException


[`InterruptedException`](https://docs.oracle.com/javase/8/docs/api/java/lang/InterruptedException.html) is a confusing beast - it shows up in seemingly innocuous methods like [`Thread.sleep()`](https://docs.oracle.com/javase/7/docs/api/java/lang/Thread.html#sleep(long)), but handling it incorrectly leads to hard-to-manage code that behaves poorly in concurrent environments.

At its most basic, if an `InterruptedException` is caught it means someone, somewhere, called [`Thread.interrupt()`](https://docs.oracle.com/javase/8/docs/api/java/lang/Thread.html#interrupt--) on the thread your code is currently running in. You might be inclined to say "It's my code! I'll never interrupt it!" and therefore do something like this:

```java
// Bad. Don't do this.
try {
  Thread.sleep(1000);
} catch (InterruptedException e) {
  // disregard
}

```

But this is exactly the wrong way to handle an "impossible" event occurring. If you know your application will never encounter an `InterruptedException` you should treat such an event as a serious violation of your program's assumptions and exit as quickly as possible.

The proper way to handle an "impossible" interrupt is like so:

```java
// When nothing will interrupt your code
try {
  Thread.sleep(1000);
} catch (InterruptedException e) {
  Thread.currentThread().interrupt();
  throw new AssertionError(e);
}

```

This does two things; it first restores the interrupt status of the thread (as if the `InterruptedException` had not been thrown in the first place), and then it throws an `AssertionError` indicating the basic invariants of your application have been violated. If you know for certain that you'll never interrupt the thread this code runs in this is safe since the `catch` block should never be reached.

Using Guava's [`Uninterruptibles`](https://google.github.io/guava/releases/snapshot/api/docs/com/google/common/util/concurrent/Uninterruptibles.html) class helps simplify this pattern; calling [`Uninterruptibles.sleepUninterruptibly()`](https://google.github.io/guava/releases/snapshot/api/docs/com/google/common/util/concurrent/Uninterruptibles.html#sleepUninterruptibly(long,%20java.util.concurrent.TimeUnit)) disregards the interrupted state of a thread until the sleep duration has expired (at which point it's restored for later calls to inspect and throw their own `InterruptedException`). If you know you'll never interrupt such code this safely avoids needing to wrap your sleep calls in a try-catch block.

More often, however, you cannot guarantee that your thread will never be interrupted. In particular if you're writing code that will be executed by an [`Executor`](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/Executor.html) or some other thread-management it's critical that your code responds promptly to interrupts, otherwise your application will stall or even deadlock.

In such cases the best thing to do is generally to allow the `InterruptedException` to propagate up the call stack, adding a `throws InterruptedException` to each method in turn. This may seem kludgy but it's actually a desirable property - your method's signatures now indicates to callers that it will respond promptly to interrupts.

```java
// Let the caller determine how to handle the interrupt if you're unsure
public void myLongRunningMethod() throws InterruptedException {
  ...
}

```

In limited cases (e.g. while overriding a method that doesn't `throw` any checked exceptions) you can reset the interrupted status without raising an exception, expecting whatever code is executed next to handle the interrupt. This delays handling the interruption but doesn't suppress it entirely.

```java
// Suppresses the exception but resets the interrupted state letting later code
// detect the interrupt and handle it properly.
try {
  Thread.sleep(1000);
} catch (InterruptedException e) {
  Thread.currentThread().interrupt();
  return ...; // your expectations are still broken at this point - try not to do more work.
}

```



## Return statements in try catch block


Although it's bad practice, it's possible to add multiple return statements in a exception handling block:

```

public static int returnTest(int number){
    try{
        if(number%2 == 0) throw new Exception("Exception thrown");
        else return x;
    }
    catch(Exception e){
        return 3;
    }
    finally{
        return 7;
    }
}

```

This method will always return 7 since the finally block associated with the try/catch block is executed before anything is returned. Now, as finally has `return 7;`, this value supersedes the try/catch return values.

If the catch block returns a primitive value and that primitive value is subsequently changed in the finally block, the value returned in the catch block will be returned and the changes from the finally block will be ignored.

The example below will print "0", not "1".

```java
public class FinallyExample {

    public static void main(String[] args) {
        int n = returnTest(4);
        
        System.out.println(n);
    }

    public static int returnTest(int number) {
        
        int returnNumber = 0; 
        
        try {
            if (number % 2 == 0)
                throw new Exception("Exception thrown");
            else
                return returnNumber;
        } catch (Exception e) {
            return returnNumber;
        } finally {
            returnNumber = 1;
        }
    }
}

```



## The Java Exception Hierarchy - Unchecked and Checked Exceptions


All Java exceptions are instances of classes in the Exception class hierarchy.  This can be represented as follows:

<li>[`java.lang.Throwable`](https://docs.oracle.com/javase/8/docs/api/java/lang/Throwable.html) - This is the base class for all exception classes.  Its methods and constructors implement a range of functionality common to all exceptions.
<ul>
<li>[`java.lang.Exception`](https://docs.oracle.com/javase/8/docs/api/java/lang/Exception.html) - This is the superclass of all normal exceptions.
<ul>
- various standard and custom exception classes.
<li>[`java.lang.RuntimeException`](https://docs.oracle.com/javase/8/docs/api/java/lang/RuntimeException.html) - This the superclass of all normal exceptions that are **unchecked exceptions**.
<ul>
- various standard and custom runtime exception classes.

Notes:

1. The distinction between **checked** and **unchecked** exceptions is described below.
1. The `Throwable`, `Exception` and `RuntimeException` class should be treated as `abstract`; see [Pitfall - Throwing Throwable, Exception, Error or RuntimeException](http://stackoverflow.com/documentation/java/5381/java-pitfalls-exception-usage/18023/pitfall-throwing-throwable-exception-error-or-runtimeexception#t=201610240803173325134).
1. The `Error` exceptions are thrown by the JVM in situations where it would be unsafe or unwise for an application to attempt to recover.
1. It would be unwise to declare custom subtypes of `Throwable`.  Java tools and libraries may assume that `Error` and `Exception` are the only direct subtypes of `Throwable`, and misbehave if that assumption is incorrect.

### Checked versus Unchecked Exceptions

One of the criticisms of exception support in some programming languages is that is difficult to know which exceptions a given method or procedure might throw.  Given that an unhandled exception is liable to cause a program to crash, this can make exceptions a source of fragility.

The Java language addresses this concern with the checked exception mechanism.  First, Java classifies exceptions into two categories:

<li>
Checked exceptions typically represent anticipated events that an application should be able to deal with.  For instance, `IOException` and its subtypes represent error conditions that can occur in I/O operations.  Examples include, file opens failing because a file or directory does not exist, network reads and writes failing because a network connection has been broken and so on.
</li>
<li>
Unchecked exceptions typically represent unanticipated events that an application cannot deal with.  These are typically the result of a bug in the application.
</li>

<sup>(In the following, "thrown" refers to any exception thrown explicitly (by a `throw` statement), or implicitly (in a failed dereference, type cast and so on).  Similarly, "propagated" refers to an exception that was thrown in a nested call, and not caught within that call.  The sample code below will illustrate this.)</sup>

The second part of the checked exception mechanism is that there are restrictions on methods where a checked exception may occur:

- When a checked exception is thrown or propagated in a method, it **must** either be caught by the method, or listed in the method's `throws` clause.  (The significance of the `throws` clause is described in [this example](http://stackoverflow.com/documentation/java/89/exceptions-and-exception-handling/25209/).)
- When a checked exception is thrown or propagated in an initializer block, it must be caught the the block.
- A checked exception cannot be propagated by a method call in a field initialization expression. (There is no way to catch such an exception.)

In short, a checked exception must be either handled, or declared.

These restrictions do not apply to unchecked exceptions.  This includes all cases where an exception is thrown implicitly, since all such cases throw unchecked exceptions.

### Checked exception examples

These code snippets are intended to illustrate the checked exception restrictions.  In each case, we show a version of the code with a compilation error, and a second version with the error corrected.

```java
// This declares a custom checked exception.
public class MyException extends Exception {
    // constructors omitted.
}

// This declares a custom unchecked exception.
public class MyException2 extends RuntimeException {
    // constructors omitted.
}

```

The first example shows how explicitly thrown checked exceptions can be declared as "thrown" if they should not be handled in the method.

```java
// INCORRECT
public void methodThrowingCheckedException(boolean flag) {
    int i = 1 / 0;                // Compiles OK, throws ArithmeticException
    if (flag) {
        throw new MyException();  // Compilation error
    } else {
        throw new MyException2(); // Compiles OK
    }
}

// CORRECTED
public void methodThrowingCheckedException(boolean flag) throws MyException {
    int i = 1 / 0;                // Compiles OK, throws ArithmeticException
    if (flag) {
        throw new MyException();  // Compilation error
    } else {
        throw new MyException2(); // Compiles OK
    }
}

```

The second example shows how a propagated checked exception can be dealt with.

```java
// INCORRECT 
public void methodWithPropagatedCheckedException() {
    InputStream is = new FileInputStream("someFile.txt");  // Compilation error
    // FileInputStream throws IOException or a subclass if the file cannot 
    // be opened.  IOException is a checked exception.
    ...
}

// CORRECTED (Version A) 
public void methodWithPropagatedCheckedException() throws IOException {
    InputStream is = new FileInputStream("someFile.txt"); 
    ...
}

// CORRECTED (Version B) 
public void methodWithPropagatedCheckedException() {
    try {
        InputStream is = new FileInputStream("someFile.txt"); 
        ...
    } catch (IOException ex) {
        System.out.println("Cannot open file: " + ex.getMessage());
    }
}

```

The final example shows how to deal with a checked exception in a static field initializer.

```java
// INCORRECT
public class Test {
    private static final InputStream is = 
            new FileInputStream("someFile.txt");  // Compilation error
}

// CORRECTED
public class Test {
    private static final InputStream is;
    static {
        InputStream tmp = null;
        try {
            tmp = new FileInputStream("someFile.txt");
        } catch (IOException ex) {
            System.out.println("Cannot open file: " + ex.getMessage());
        }
        is = tmp;
    }
}

```

Note that in this last case, we also have to deal with the problems that `is` cannot be assigned to more than once, and yet also has to be assigned to, even in the case of an exception.



## Introduction


Exceptions are errors which occur when a program is executing. Consider the Java program below which divides two integers.

```java
class Division {
    public static void main(String[] args) {
 
        int a, b, result;
 
        Scanner input = new Scanner(System.in);
        System.out.println("Input two integers");
 
        a = input.nextInt();
        b = input.nextInt();
 
        result = a / b;
 
        System.out.println("Result = " + result);
    }
}

```

Now we compile and execute the above code, and see the output for an attempted division by zero:

```java
Input two integers
7 0
Exception in thread "main" java.lang.ArithmeticException: / by zero 
    at Division.main(Disivion.java:14)

```

Division by zero is an invalid operation that would produce a value that cannot be represented as an integer.  Java deals with this by **throwing an **exception****. In this case, the exception is an instance of the **ArithmeticException** class.

**Note:** The example on [creating and reading stack traces](http://stackoverflow.com/documentation/java/89/exceptions-and-exception-handling/1815/) explains what the output after the two numbers means.

The utility of an **exception** is the flow control that it allows. Without using exceptions, a typical solution to this problem may be to first check if `b == 0`:

```java
class Division {
    public static void main(String[] args) {
 
        int a, b, result;

        Scanner input = new Scanner(System.in);
        System.out.println("Input two integers");
 
        a = input.nextInt();
        b = input.nextInt();
 
        if (b == 0) {
            System.out.println("You cannot divide by zero.");
            return;
        }

        result = a / b;
 
        System.out.println("Result = " + result);
    }
}

```

This prints the message `You cannot divide by zero.` to the console and quits the program in a graceful way when the user tries to divide by zero. An equivalent way of dealing with this problem via **exception handling** would be to replace the `if` flow control with a `try-catch` block:

```java
...

a = input.nextInt();
b = input.nextInt();
 
try {
    result = a / b;
}
catch (ArithmeticException e) {
    System.out.println("An ArithmeticException occurred. Perhaps you tried to divide by zero.");
    return;
}
 
...  

```

A try catch block is executed as follows:

1. Begin executing the code in the `try` block.
1. If an **exception** occurs in the try block, immediately abort and check to see if this exception is **caught** by the `catch` block (in this case, when the Exception is an instance of `ArithmeticException`).
1. If the exception is **caught**, it is assigned to the variable `e` and the `catch` block is executed.
1. If either the `try` or `catch` block is completed (i.e. no uncaught exceptions occur during code execution) then continue to execute code below the `try-catch` block.

It is generally considered good practice to use **exception handling** as part of the normal flow control of an application where behavior would otherwise be undefined or unexpected. For instance, instead of returning `null` when a method fails, it is usually better practice to **throw an exception** so that the application making use of the method can define its own flow control for the situation via **exception handling** of the kind illustrated above. In some sense, this gets around the problem of having to return a particular **type**, as any one of multiple kinds of **exceptions** may be **thrown** to indicate the specific problem that occurred.

For more advice on how and how not to use exceptions, refer to [Java Pitfalls - Exception usage](http://stackoverflow.com/documentation/java/5381/java-pitfalls-exception-usage#t=20160818072550829097)



## Creating and reading stacktraces


When an exception object is created (i.e. when you `new` it), the `Throwable` constructor captures information about the context in which the exception was created.  Later on, this information can be output in the form of a stacktrace, which can be used to help diagnose the problem that caused the exception in the first place.

### Printing a stacktrace

Printing a stacktrace is simply a matter of calling the `printStackTrace()` method.  For example:

```java
try {
    int a = 0;
    int b = 0;
    int c = a / b;
} catch (ArithmeticException ex) {
    // This prints the stacktrace to standard output
    ex.printStackTrace();
}

```

The `printStackTrace()` method without arguments will print to the application's standard output; i.e. the current `System.out`.  There are also `printStackTrace(PrintStream)` and `printStackTrace(PrintWriter)` overloads that print to a specified `Stream` or `Writer`.

Notes:

<li>
The stacktrace does not include the details of the exception itself.  You can use the `toString()` method to get those details; e.g.

```java
   // Print exception and stacktrace
   System.out.println(ex);
   ex.printStackTrace();

```


</li>
<li>
Stacktrace printing should be used sparingly; see [Pitfall - Excessive or inappropriate stacktraces](http://stackoverflow.com/documentation/java/5381/java-pitfalls-exception-usage/19955/pitfall-excessive-or-inappropriate-stacktraces#t=201610200112090788291) .  It is often better to use a logging framework, and pass the exception object to be logged.
</li>

### Understanding a stacktrace

Consider the following simple program consisting of two classes in two files.  (We have shown the filenames and added line numbers for illustration purposes.)

```java
File: "Main.java"
1   public class Main {
2       public static void main(String[] args) {
3           new Test().foo();
4       }
5   }

File: "Test.java"
1   class Test {
2       public void foo() {
3           bar();
4       }
5   
6       public int bar() {
7           int a = 1;
8           int b = 0;
9           return a / b;
10      }

```

When these files are compiled and run, we will get the following output.

```java
Exception in thread "main" java.lang.ArithmeticException: / by zero
        at Test.bar(Test.java:9)
        at Test.foo(Test.java:3)
        at Main.main(Main.java:3)

```

Let us read this one line at a time to figure out what it is telling us.

Line #1 tells us that the thread called "main" has terminated due to an uncaught exception.  The full name of the exception is `java.lang.ArithmeticException`, and the exception message is "/ by zero".

If we look up the javadocs for this exception, it says:

> 
Thrown when an exceptional arithmetic condition has occurred. For example, an integer "divide by zero" throws an instance of this class.


Indeed, the message "/ by zero" is a strong hint that the cause of the exception is that some code has attempted to divide something by zero.  But what?

The remaining 3 lines are the stack trace.  Each line represents a method (or constructor) call on the call stack, and each one tells us three things:

- the name of the class and method that was being executed,
- the source code filename,
- the source code line number of the statement that was being executed

These lines of a stacktrace are listed with the frame for the current call at the top.  The top frame in our example above is in the `Test.bar` method, and at line 9 of the Test.java file.  That is the following line:

```

   return a / b;

```

If we look a couple of lines earlier in the file to where `b` is initialized, it is apparent that `b` will have the value zero.  We can say without any doubt that this is the cause of the exception.

If we needed to go further, we can see from the stacktrace that `bar()` was called from `foo()` at line 3 of Test.java, and that `foo()` was in turn called from `Main.main()`.

Note: The class and method names in the stack frames are the internal names for the classes and methods.  You will need to recognize the following unusual cases:

- A nested or inner class will look like "OuterClass$InnerClass".
- An anonymous inner class will look like "OuterClass$1", "OuterClass$2", etcetera.
- When code in a constructor, instance field initializer or an instance initializer block is being executed, the method name will be "".
- When code in a static field initializer or static initializer block is being executed, the method name will be "".

(In some versions of Java, the stacktrace formatting code will detect and elide repeated stackframe sequences, as can occur when an application fails due to excessive recursion.)

### Exception chaining and nested stacktraces

Exception chaining happens when a piece of code catches an exception, and then creates and throws a new one, passing the first exception as the cause.  Here is an example:

```java
File: Test,java
1   public class Test {
2      int foo() {
3           return 0 / 0;
4      }
5
6       public Test() {
7           try {
8               foo();
9           } catch (ArithmeticException ex) {
10              throw new RuntimeException("A bad thing happened", ex);
11          }
12      }
13
14      public static void main(String[] args) {
15          new Test();
16      }
17  }

```

When the above class is compiled and run, we get the following stacktrace:

```java
Exception in thread "main" java.lang.RuntimeException: A bad thing happened
        at Test.<init>(Test.java:10)
        at Test.main(Test.java:15)
Caused by: java.lang.ArithmeticException: / by zero
        at Test.foo(Test.java:3)
        at Test.<init>(Test.java:8)
        ... 1 more

```

The stacktrace starts with the class name, method and call stack for the exception that (in this case) caused the application to crash.  This is followed by a "Caused by:" line that reports the `cause` exception.  The class name and message are reported, followed by the cause exception's stack frames.  The trace ends with an  "... N more" which indicates that the last N frames are the same as for the previous exception.

The "Caused by:" is only included in the output when the primary exception's `cause` is not `null`).  Exceptions can be chained indefinitely, and in that case the stacktrace can have multiple "Caused by:" traces.

Note: the `cause` mechanism was only exposed in the `Throwable` API in Java 1.4.0.  Prior to that, exception chaining needed to be implemented by the application using a custom exception field to represent the cause, and a custom `printStackTrace` method.

### Capturing a stacktrace as a String

Sometimes, an application needs to be able to capture a stacktrace as a Java `String`, so that it can be used for other purposes.  The general approach for doing this is to create a temporary `OutputStream` or `Writer` that writes to an in-memory buffer and pass that to the `printStackTrace(...)`.

The [Apache Commons](http://commons.apache.org/proper/commons-lang/javadocs/api-2.6/org/apache/commons/lang/exception/ExceptionUtils.html#getStackTrace(java.lang.Throwable)) and [Guava](http://google.github.io/guava/releases/snapshot/api/docs/com/google/common/base/Throwables.html#getStackTraceAsString(java.lang.Throwable)) libraries provide utility methods for capturing a stacktrace as a String:

```java
org.apache.commons.lang.exception.ExceptionUtils.getStackTrace(Throwable)

com.google.common.base.Throwables.getStackTraceAsString(Throwable)

```

If you cannot use third party libraries in your code base, then the following method with do the task:

```

  /**
     * Returns the string representation of the stack trace.
     *
     * @param throwable the throwable
     * @return the string.
     */
    public static String stackTraceToString(Throwable throwable) {
        StringWriter stringWriter = new StringWriter();
        throwable.printStackTrace(new PrintWriter(stringWriter));
        return stringWriter.toString();
    }

```

Note that if your intention is to analyze the stacktrace, it is simpler to use `getStackTrace()` and `getCause()` than to attempt to parse a stacktrace.



## Throwing an exception


The following example shows the basics of throwing an exception:

```java
public void checkNumber(int number) throws IllegalArgumentException {
    if (number < 0) {
        throw new IllegalArgumentException("Number must be positive: " + number);
    }
}

```

The exception is thrown on the 3rd line.  This statement can be broken down into two parts:

<li>
`new IllegalArgumentException(...)` is creating an instance of the `IllegalArgumentException` class, with a message that describes the error that exception is reporting.
</li>
<li>
`throw ...` is then throwing the exception object.
</li>

When the exception is thrown, it causes the enclosing statements to **terminate abnormally** until the exception is **handled**.  This is described in other examples.

It is good practice to both create and throw the exception object in a single statement, as shown above.  It is also good practice to include a meaningful error message in the exception to help the programmer to understand the cause of the problem.  However, this is not necessarily the message that you should be showing to the end user.  (For a start, Java has no direct support for internationalizing exception messages.)

There are a couple more points to be made:

<li>
We have declared the `checkNumber` as `throws IllegalArgumentException`.  This was not strictly necessary, since `IllegalArgumentException` is a checked exception; see [The Java Exception Hierarchy - Unchecked and Checked Exceptions](http://stackoverflow.com/documentation/java/89/exceptions/3058/types-of-exceptions-unchecked-and-checked-exceptions#t=201610240630113832437).  However, it is good practice to do this, and also to include the exceptions thrown a method's javadoc comments.
</li>
<li>
Code immediately after a `throw` statement is **unreachable**.  Hence if we wrote this:

```java
 throw new IllegalArgumentException("it is bad");
 return;

```


the compiler would report a compilation error for the `return` statement.
</li>

### Exception chaining

Many standard exceptions have a constructor with a second `cause` argument in addition to the conventional `message` argument.  The `cause` allows you to chain exceptions.  Here is an example.

First we define an unchecked exception that our application is going throw when it encounters a non-recoverable error.  Note that we have included a constructor that accepts a `cause` argument.

```

   public class AppErrorException extends RuntimeException {
        public AppErrorException() {
            super();
        }

        public AppErrorException(String message) {
            super(message);
        }

        public AppErrorException(String message, Throwable cause) {
            super(message, cause);
        }
    }

```

Next, here is some code that illustrates exception chaining.

```

   public String readFirstLine(String file) throws AppErrorException {
        try (Reader r = new BufferedReader(new FileReader(file))) {
            String line = r.readLine();
            if (line != null) {
                return line;
            } else {
                throw new AppErrorException("File is empty: " + file);
            }
        } catch (IOException ex) {
            throw new AppErrorException("Cannot read file: " + file, ex);
        }
    }

```

The `throw` within the `try` block detects a problem and reports it via an exception with a simple message.  By contrast, the `throw` within the `catch` block is handling the `IOException` by wrapping it in a new (checked) exception.  However, it is not throwing away the original exception.  By passing the `IOException` as the `cause`, we record it so that it can be printed in the stacktrace, as explained in [Creating and reading stacktraces](http://stackoverflow.com/documentation/java/89/exceptions-and-exception-handling/1815/creating-and-reading-stacktraces#t=20161028160848944857).



## Advanced features of Exceptions


This example covers some advanced features and use-cases for Exceptions.

### Examining the callstack programmatically

The primary use of exception stacktraces is to provide information about an application error and its context so that the programmer can diagnose and fix the problem.  Sometimes it can be used for other things.  For example, a `SecurityManager` class may need to examine the call stack to decide whether the code that is making a call should be trusted.

You can use exceptions to examine the call stack programatically as follows:

```

   Exception ex = new Exception();   // this captures the call stack
    StackTraceElement[] frames = ex.getStackTrace();
    System.out.println("This method is " + frames[0].getMethodName());
    System.out.println("Called from method " + frames[1].getMethodName());

```

There are some important caveats on this:

<li>
The information available in a `StackTraceElement` is limited.  There is no more information available than is displayed by `printStackTrace`.  (The values of the local variables in the frame are not available.)
</li>
<li>
The javadocs for `getStackTrace()` state that a JVM is permitted to leave out frames:
<blockquote>
Some virtual machines may, under some circumstances, omit one or more stack frames from the stack trace. In the extreme case, a virtual machine that has no stack trace information concerning this throwable is permitted to return a zero-length array from this method.
</blockquote>
</li>

### Optimizing exception construction

As mentioned elsewhere, constructing an exception is rather expensive because it entails capturing and recording information about all stack frames on the current thread.  Sometimes, we know that that information is never going to be used for a given exception; e.g. the stacktrace will never be printed.  In that case, there is an implementation trick that we can use in a custom exception to cause the information to not be captured.

The stack frame information needed for stacktraces, is captured when the `Throwable` constructors call the `Throwable.fillInStackTrace()` method.  This method is `public`, which means that a subclass can override it.  The trick is to override the method inherited from `Throwable` with one that does nothing; e.g.

```

 public class MyException extends Exception {
      // constructors

      @Override 
      public void fillInStackTrace() {
          // do nothing
      }
  }

```

The problem with this approach is that an exception that overrides `fillInStackTrace()` can never capture the stacktrace, and is useless in scenarios where you need one.

### Erasing or replacing the stacktrace

In some situations, the stacktrace for an exception created in the normal way contains either incorrect information, or information that the developer does not want to reveal to the user.  For these scenarios, the `Throwable.setStackTrace` can be used to replace the array of `StackTraceElement` objects that holds the information.

For example, the following can be used to discard an exception's stack information:

```

exception.setStackTrace(new StackTraceElement[0]);

```

### Suppressed exceptions

Java 7 introduced the **try-with-resources** construct, and the associated concept of exception suppression.  Consider the following snippet:

```java
try (Writer w = new BufferedWriter(new FileWriter(someFilename))) {
    // do stuff
    int temp = 0 / 0;    // throws an ArithmeticException
}

```

When the exception is thrown, the `try` will call `close()` on the `w` which will flush any buffered output and then close the `FileWriter`.  But what happens if an `IOException` is thrown while flushing the output?

What happens is that any exception that is thrown while cleaning up a resource is **suppressed**.  The exception is caught, and added to the primary exception's suppressed exception list.  Next the **try-with-resources** will continue with the cleanup of the other resources.  Finally, primary exception will be rethrown.

A similar pattern occurs if an exception it thrown during the resource initialization, or if the `try` block completes normally.  The first exception thrown becomes the primary exception, and subsequent ones arising from cleanup are suppressed.

The suppressed exceptions can be retrieved from the primary exception object by calling `getSuppressedExceptions`.



## The try-finally and try-catch-finally statements


The `try...catch...finally` statement combines exception handling with clean-up code. The `finally` block contains code that will be executed in all circumstances. This makes them suitable for resource management, and other kinds of cleanup.

### Try-finally

Here is an example of the simpler (`try...finally`) form:

```java
try {
    doSomething();  
} finally {
    cleanUp();
}

```

The behavior of the `try...finally` is as follows:

- The code in the `try` block is executed.
<li>If no exception was thrown in the `try` block:
<ul>
- The code in the `finally` block is executed.
- If the `finally` block throws an exception, that exception is propagated.
- Otherwise, control passes to the next statement after the `try...finally`.

- The code in the `finally` block is executed.
- If the `finally` block throws an exception, that exception is propagated.
- Otherwise, the original exception continues to propagate.

The code within `finally` block will always be executed. (The only exceptions are if `System.exit(int)` is called, or if the JVM panics.) Thus a `finally` block is the correct place code that always needs to be executed; e.g. closing files and other resources or releasing locks.

### try-catch-finally

Our second example shows how `catch` and `finally` can be used together.  It also illustrates that cleaning up resources is not straightforward.

```java
// This code snippet writes the first line of a file to a string
String result = null;
Reader reader = null;
try {
    reader = new BufferedReader(new FileReader(fileName));
    result = reader.readLine();
} catch (IOException ex) {
    Logger.getLogger.warn("Unexpected IO error", ex);  // logging the exception
} finally {
    if (reader != null) {
        try {
            reader.close();
        } catch (IOException ex) {
            // ignore / discard this exception
        }
    }
}

```

The complete set of (hypothetical) behaviors of `try...catch...finally` in this example are too complicated to describe here.  The simple version is that the code in the `finally` block will always be executed.

Looking at this from the perspective of resource management:

- We declare the "resource" (i.e. `reader` variable) before the `try` block so that it will be in scope for the `finally` block.
- By putting the `new FileReader(...)`, the `catch` is able to handle any `IOError` exception from thrown when opening the file.
- We need a `reader.close()` in the `finally` block because there are some exception paths that we cannot intercept either in the `try` block or in `catch` block.
- However, since an exception **might** have been thrown before `reader` was initialized, we also need an explicit `null` test.
- Finally, the `reader.close()` call might (hypothetically) throw an exception.  We don't care about that, but if we don't catch the exception at source, we would need to deal with it further up the call stack.

Java 7 and later provide an alternative [try-with-resources syntax](http://stackoverflow.com/documentation/java/89/exceptions/1581/using-try-with-resources#t=2016072716372920995) which significantly simplifies resource clean-up.



## The 'throws' clause in a method declaration


Java's **checked exception** mechanism requires the programmer to declare that certain methods **could** throw specifed checked exceptions. This is done using the `throws` clause.  For example:

```java
public class OddNumberException extends Exception { // a checked exception
}

public void checkEven(int number) throws OddNumberException {
    if (number % 2 != 0) {
        throw new OddNumberException();
    }
}

```

The `throws OddNumberException` declares that a call to `checkEven` **could** throw an exception that is of type `OddNumberException`.

A `throws` clause can declare a list of types, and can include unchecked exceptions as well as checked exceptions.

```java
public void checkEven(Double number) 
        throws OddNumberException, ArithmeticException {
    if (!Double.isFinite(number)) {
        throw new ArithmeticException("INF or NaN");
    } else if (number % 2 != 0) {
        throw new OddNumberException();
    }
}

```

### What is the point of declaring unchecked exceptions as thrown?

The `throws` clause in a method declaration serves two purposes:

<li>
It tells the compiler which exceptions are thrown so that the compiler can report uncaught (checked) exceptions as errors.
</li>
<li>
It tells a programmer who is writing code that calls the method what exceptions to expect. For this purpose, it often makes to senses to include unchecked exceptions in a `throws` list.
</li>

Note: that the `throws` list is also used by the javadoc tool when generating API documentation, and by a typical IDE's "hover text" method tips.

### Throws and method overriding

The `throws` clause forms part of a method's signature for the purpose of method overriding. An override method can be declared with the same set of checked exceptions as thrown by the overridden method, or with a subset. However the override method cannot add extra checked exceptions. For example:

```java
@Override
public void checkEven(int number) throws NullPointerException // OK—NullPointerException is an unchecked exception
    ...

@Override
public void checkEven(Double number) throws OddNumberException // OK—identical to the superclass
    ...

class PrimeNumberException extends OddNumberException {}
class NonEvenNumberException extends OddNumberException {}

@Override
public void checkEven(int number) throws PrimeNumberException, NonEvenNumberException // OK—these are both subclasses

@Override
public void checkEven(Double number) throws IOExcepion         // ERROR

```

The reason for this rule is that if an overriden method can throw a checked exception that the overridden method could not throw, that would break type substitutability.



#### Syntax


<li>
<p>void someMethod() throws SomeException { } //method declaration, forces method
callers to catch if SomeException is a checked exception type</p>
</li>
<li>
try {

```java
someMethod(); //code that might throw an exception 

```


}
</li>
<li>
catch (SomeException e) {

```java
 System.out.println("SomeException was thrown!"); //code that will run if certain exception (SomeException) is thrown

```


}
</li>
<li>
finally {

```java
 //code that will always run, whether try block finishes or not

```


}
</li>

