---
metaTitle: "Common Java Pitfalls"
description: "Pitfall: using == to compare primitive wrappers objects such as Integer, Pitfall: using == to compare strings, Pitfall: forgetting to free resources, Pitfall: testing a file before attempting to open it., Pitfall: thinking of variables as objects, Pitfall: memory leaks, Pitfall: Not understanding that String is an immutable class, Pitfall: combining assignment and side-effects"
---

# Common Java Pitfalls


This topic outlines some of the common mistakes made by beginners in Java.

This includes any common mistakes in use of the Java language or understanding of the run-time environment.

Mistakes associated with specific APIs can be described in topics specific to those APIs. Strings are a special case; they're covered in the Java Language Specification. Details other than common mistakes can be described [in this topic on Strings](http://stackoverflow.com/documentation/java/109/strings).



## Pitfall: using == to compare primitive wrappers objects such as Integer


(This pitfall applies equally to all primitive wrapper types, but we will illustrate it for `Integer` and `int`.)

When working with `Integer` objects, it is tempting to use `==` to compare values, because that is what you would do with `int` values. And in some cases this will seem to work:

```java
Integer int1_1 = Integer.valueOf("1");
Integer int1_2 = Integer.valueOf(1);

System.out.println("int1_1 == int1_2: " + (int1_1 == int1_2));          // true
System.out.println("int1_1 equals int1_2: " + int1_1.equals(int1_2));   // true

```

Here we created two `Integer` objects with the value `1` and compare them (In this case we created one from a `String` and one from an `int` literal.  There are other alternatives). Also, we observe that the two comparison methods (`==` and `equals`) both yield `true`.

This behavior changes when we choose different values:

```java
Integer int2_1 = Integer.valueOf("1000");
Integer int2_2 = Integer.valueOf(1000);

System.out.println("int2_1 == int2_2: " + (int2_1 == int2_2));          // false
System.out.println("int2_1 equals int2_2: " + int2_1.equals(int2_2));   // true

```

In this case, only the `equals` comparison yields the correct result.

The reason for this difference in behavior is, that the JVM maintains a cache of `Integer` objects for the range -128 to 127. (The upper value can be overridden with the system property "java.lang.Integer.IntegerCache.high" or the JVM argument "-XX:AutoBoxCacheMax=size").  For values in this range, the `Integer.valueOf()` will return the cached value rather than creating a new one.

Thus, in the first example the `Integer.valueOf(1)` and `Integer.valueOf("1")` calls returned the same cached `Integer` instance.  By contrast, in the second example the `Integer.valueOf(1000)` and `Integer.valueOf("1000")` both created and returned new `Integer` objects.

The `==` operator for reference types tests for reference equality (i.e. the same object).  Therefore, in the first example `int1_1 == int1_2` is `true` because the references are the same.  In the second example `int2_1 == int2_2` is false because the references are different.



## Pitfall: using == to compare strings


A common mistake for Java beginners is to use the `==` operator to test if two strings are equal.  For example:

```java
public class Hello {
    public static void main(String[] args) {
        if (args.length > 0) {
            if (args[0] == "hello") {
                System.out.println("Hello back to you");
            } else {
                System.out.println("Are you feeling grumpy today?");
            }
        }
    }
}

```

The above program is supposed to test the first command line argument and print different messages when it and isn't the word "hello".  But the problem is that it won't work.  That program will output "Are you feeling grumpy today?" no matter what the first command line argument is.

In this particular case the `String` "hello" is put in the string pool while the `String` args[0] resides on the heap. This means there are two objects representing the same literal, each with its reference. Since `==` tests for references, not actual equality, the comparison will yield a false most of the times. This doesn't mean that it will always do so.

When you use `==` to test strings, what you are actually testing is if two `String` objects are the same Java object.  Unfortunately, that is not what string equality means in Java.  In fact, the correct way to test strings is to use the `equals(Object)` method.  For a pair of strings, we usually want to test if they consist of the same characters in the same order.

```java
public class Hello2 {
    public static void main(String[] args) {
        if (args.length > 0) {
            if (args[0].equals("hello")) {
                System.out.println("Hello back to you");
            } else {
                System.out.println("Are you feeling grumpy today?");
            }
        }
    }
}

```

But it actually gets worse.  The problem is that `==` **will** give the expected answer in some circumstances.  For example

```java
public class Test1 {
    public static void main(String[] args) {
        String s1 = "hello";
        String s2 = "hello";
        if (s1 == s2) {
            System.out.println("same");
        } else {
            System.out.println("different");
        }
    }
}

```

Interestingly, this will print "same", even though we are testing the strings the wrong way.  Why is that?  Because the [Java Language Specification (Section 3.10.5: String Literals)](https://docs.oracle.com/javase/specs/jls/se8/html/jls-3.html#jls-3.10.5) stipulates that any two string >>literals<< consisting of the same characters will actually be represented by the same Java object.  Hence, the `==` test will give true for equal literals.  (The string literals are "interned" and added to a shared "string pool" when your code is loaded, but that is actually an implementation detail.)

To add to the confusion, the Java Language Specification also stipulates that when you have a compile-time constant expression that concatenates two string literals, that is equivalent to a single literal.  Thus:

```

   public class Test1 {
    public static void main(String[] args) {
        String s1 = "hello";
        String s2 = "hel" + "lo";
        String s3 = " mum";
        if (s1 == s2) {
            System.out.println("1. same");
        } else {
            System.out.println("1. different");
        }
        if (s1 + s3 == "hello mum") {
            System.out.println("2. same");
        } else {
            System.out.println("2. different");
        }
    }
}

```

This will output "1. same" and "2. different".  In the first case, the `+` expression is evaluated at compile time and we compare one `String` object with itself.  In the second case, it is evaluated at run time and we compare two different `String` objects

In summary, using `==` to test strings in Java is almost always incorrect, but it is not guaranteed to give the wrong answer.



## Pitfall: forgetting to free resources


Every time a program opens a resource, such as a file or network connection, it is important to free the resource once you are done using it. Similar caution should be taken if any exception were to be thrown during operations on such resources. One could argue that the [`FileInputStream`](https://docs.oracle.com/javase/8/docs/api/java/io/FileInputStream.html) has a [finalizer](https://en.wikipedia.org/wiki/Finalizer) that invokes the `close()` method on a garbage collection event; however, since we can’t be sure when a garbage collection cycle will start, the input stream can consume computer resources for an indefinite period of time. The resource must be closed in a `finally` section of a try-catch block:

```java
private static void printFileJava6() throws IOException {
    FileInputStream input;
    try {
        input = new FileInputStream("file.txt");
        int data = input.read();
        while (data != -1){
            System.out.print((char) data);
            data = input.read();
        }
    } finally {
        if (input != null) {
            input.close();
        }
    }
}

```

Since Java 7 there is a really useful and neat statement introduced in Java 7 particularly for this case, called try-with-resources:

```java
private static void printFileJava7() throws IOException {
    try (FileInputStream input = new FileInputStream("file.txt")) {
        int data = input.read();
        while (data != -1){
            System.out.print((char) data);
            data = input.read();
        }
    }
}

```

The **try-with-resources** statement can be used with any object that implements the `Closeable` or `AutoCloseable` interface. It ensures that each resource is closed by the end of the statement. The difference between the two interfaces is, that the `close()` method of `Closeable` throws an `IOException` which has to be handled in some way.

In cases where the resource has already been opened but should be safely closed after use, one can assign it to a local variable inside the try-with-resources

```java
private static void printFileJava7(InputStream extResource) throws IOException {
    try (InputStream input = extResource) {
        ... //access resource
    }
}

```

The local resource variable created in the try-with-resources constructor is effectively final.



## Pitfall: testing a file before attempting to open it.


Some people recommend that you should apply various tests to a file before attempting to open it either to provide better diagnostics or avoid dealing with exceptions.  For example, this method attempts to check if `path` corresponds to a readable file:

```java
public static File getValidatedFile(String path) throws IOException {
    File f = new File(path);
    if (!f.exists()) throw new IOException("Error: not found: " + path);
    if (!f.isFile()) throw new IOException("Error: Is a directory: " + path);
    if (!f.canRead()) throw new IOException("Error: cannot read file: " + path);
    return f;
}

```

You might use the above method like this:

```java
File f = null;
try {
    f = getValidatedFile("somefile");
} catch (IOException ex) {
    System.err.println(ex.getMessage());
    return;
}
try (InputStream is = new FileInputStream(file)) {
    // Read data etc.
}

```

The first problem is in the signature for `FileInputStream(File)` because the compiler will still insist we catch `IOException` here, or further up the stack.

The second problem is that checks performed by `getValidatedFile` do not guarantee that the `FileInputStream` will succeed.

<li>
Race conditions: another thread or a separate process could rename the file, delete the file, or remove read access after the `getValidatedFile` returns.  That would lead to a "plain" `IOException` without the custom message.
</li>
<li>
There are edge cases not covered by those tests.  For example, on a system with SELinux in "enforcing" mode, an attempt to read a file can fail despite `canRead()` returning `true`.
</li>

The third problem is that the tests are inefficient.  For example, the `exists`, `isFile` and `canRead` calls will each make a [syscall](https://en.wikipedia.org/wiki/System_call) to perform the required check.  Another syscall is then made to open the file, which repeats the same checks behind the scenes.

In short, methods like `getValidatedFile` are misguided.  It is better to simply attempt to open the file and handle the exception:

```java
try (InputStream is = new FileInputStream("somefile")) {
    // Read data etc.
} catch (IOException ex) {
    System.err.println("IO Error processing 'somefile': " + ex.getMessage());
    return;
}

```

If you wanted to distinguish IO errors thrown while opening and reading, you could use a nested try / catch.  If you wanted to produce better diagnostics for open failures, you could perform the `exists`, `isFile` and `canRead` checks in the handler.



## Pitfall: thinking of variables as objects


No Java variable represents an object.

```java
String foo;   // NOT AN OBJECT

```

Neither does any Java array contain objects.

```java
String bar[] = new String[100];  // No member is an object.

```

If you mistakenly think of variables as objects, the actual behavior of the Java language will surprise you.

<li>
For Java variables which have a primitive type (such as `int` or `float`) the variable holds a copy of the value.  All copies of a primitive value are indistinguishable; i.e. there is only one `int` value for the number one.  Primitive values are not objects and they do not behave like objects.
</li>
<li>
For Java variables which have a reference type (either a class or an array type) the variable holds a reference.  All copies of a reference are indistinguishable.  References may point to objects, or they may be `null` which means that they point to no object.  However, they are not objects and they don't behave like objects.
</li>

Variables are not objects in either case, and they don't contain objects in either case.  They may contain **references to objects**, but that is saying something different.

### Example class

The examples that follow use this class, which represents a point in 2D space.

```java
public final class MutableLocation {
   public int x;
   public int y;

   public MutableLocation(int x, int y) {
       this.x = x;
       this.y = y;
   }

   public boolean equals(Object other) {
       if (!(other instanceof MutableLocation) {
           return false;
       }
       MutableLocation that = (MutableLocation) other;
       return this.x == that.x && this.y == that.y;
   }
}

```

An instance of this class is an object that has two fields `x` and `y` which have the type `int`.

We can have many instances of the `MutableLocation` class.  Some will represent the same locations in 2D space; i.e. the respective values of `x` and `y` will match.  Others will represent different locations.

### Multiple variables can point to the same object

```

MutableLocation here = new MutableLocation(1, 2);
 MutableLocation there = here;
 MutableLocation elsewhere = new MutableLocation(1, 2);

```

In the above, we have declared three variables `here`, `there` and `elsewhere` that can hold references to `MutableLocation` objects.

If you (incorrectly) think of these variables as being objects, then you are likely to misread the statements as saying:

> 
<ol>
- Copy the location "[1, 2]" to `here`
- Copy the location "[1, 2]" to `there`
- Copy the location "[1, 2]" to `elsewhere`
</ol>


From that, you are likely to infer we have three independent objects in the three variables.  In fact there are **only two objects created** by the above.  The variables `here` and `there` actually refer to the same object.

We can demonstrate this.  Assuming the variable declarations as above:

```java
System.out.println("BEFORE: here.x is " + here.x + ", there.x is " + there.x +
                   "elsewhere.x is " + elsewhere.x);
here.x = 42;
System.out.println("AFTER: here.x is " + here.x + ", there.x is " + there.x +
                   "elsewhere.x is " + elsewhere.x);

```

This will output the following:

```java
BEFORE: here.x is 1, there.x is 1, elsewhere.x is 1
AFTER: here.x is 42, there.x is 42, elsewhere.x is 1

```

We assigned a new value to `here.x` and it changed the value that we see via `there.x`.  They are referring to the same object.  But the value that we see via `elsewhere.x` has not changed, so `elsewhere` must refer to a different object.

If a variable was an object, then the assignment `here.x = 42` would not change `there.x`.

### The equality operator does NOT test that two objects are equal

Applying the equality (`==`) operator to reference values tests if the values refer to the same object.  It does **not** test whether two (different) objects are "equal" in the intuitive sense.

```

MutableLocation here = new MutableLocation(1, 2);
 MutableLocation there = here;
 MutableLocation elsewhere = new MutableLocation(1, 2);

 if (here == there) {
     System.out.println("here is there");
 }
 if (here == elsewhere) {
     System.out.println("here is elsewhere");
 }

```

This will print "here is there", but it won't print "here is elsewhere".  (The references in `here` and `elsewhere` are for two distinct objects.)

By contrast, if we call the `equals(Object)` method that we implemented above, we are going to test if two `MutableLocation` instances have an equal location.

```

if (here.equals(there)) {
     System.out.println("here equals there");
 }
 if (here.equals(elsewhere)) {
     System.out.println("here equals elsewhere");
 }

```

This will print both messages.  In particular, `here.equals(elsewhere)` returns `true` because the semantic criteria we chose for equality of two `MutableLocation` objects has been satisfied.

### Method calls do NOT pass objects at all

Java method calls use **pass by value**<sup>1</sup> to pass arguments and return a result.

When you pass a reference value to a method, you're actually passing a reference to an object **by value**, which means that it is creating a copy of the object reference.

As long as both object references are still pointing to the same object, you can modify that object from either reference, and this is what causes confusion for some.

However, you are **not** passing an object by reference<sup>2</sup>.  The distinction is that if the object reference copy is modified to point to another object, the original object reference will still point to the original object.

```java
void f(MutableLocation foo) {  
    foo = new MutableLocation(3, 4);   // Point local foo at a different object.
}

void g() {
    MutableLocation foo = MutableLocation(1, 2);
    f(foo);
    System.out.println("foo.x is " + foo.x); // Prints "foo.x is 1".
}

```

Neither are you passing a copy of the object.

```java
void f(MutableLocation foo) {  
    foo.x = 42;
}

void g() {
    MutableLocation foo = new MutableLocation(0, 0);
    f(foo);
    System.out.println("foo.x is " + foo.x); // Prints "foo.x is 42"
}

```

<sup>1 - In languages like Python and Ruby, the term "pass by sharing" is preferred for "pass by value" of an object / reference.</sup>

<sup>2 - The term "pass by reference" or "call by reference" has a very specific meaning in programming language terminology.  In effect, it means that you pass the address **of a variable or an array element**, so that when the called method assigns a new value to the formal argument, it changes the value in the original variable.  Java does not support this.  For a more fulsome description of different mechanisms for passing parameters, please refer to [https://en.wikipedia.org/wiki/Evaluation_strategy](https://en.wikipedia.org/wiki/Evaluation_strategy).</sup>



## Pitfall: memory leaks


Java manages memory automatically. You are not required to free memory manually. An object's memory on the heap may be freed by a garbage collector when the object is no longer **reachable** by a live thread.

However, you can prevent memory from being freed, by allowing objects to be reachable that are no longer needed. Whether you call this a memory leak or memory packratting, the result is the same -- an unnecessary increase in allocated memory.

Memory leaks in Java can happen in various ways, but the most common reason is everlasting object references, because the garbage collector can’t remove objects from the heap while there are still references to them.

**Static fields**

One can create such a reference by defining class with a `static` field containing some collection of objects, and forgetting to set that `static` field to `null` after the collection is no longer needed. `static` fields are considered GC roots and are never collected. Another issue is leaks in non-heap memory when [JNI](https://en.wikipedia.org/wiki/Java_Native_Interface) is used.

**Classloader leak**

By far, though, the most insidious type of memory leak is the [classloader leak](https://zeroturnaround.com/rebellabs/rjc201/). A classloader holds a reference to every class it has loaded, and every class holds a reference to its classloader. Every object holds a reference to its class as well. Therefore, if even a **single** object of a class loaded by a classloader is not garbage, not a single class that that classloader has loaded can be collected. Since each class also refers to its static fields, they cannot be collected either.

**Accumulation leak**
The accumulation leak example could look like the following:

```java
final ScheduledExecutorService scheduledExecutorService = Executors.newScheduledThreadPool(1);
final Deque<BigDecimal> numbers = new LinkedBlockingDeque<>();
final BigDecimal divisor = new BigDecimal(51);

scheduledExecutorService.scheduleAtFixedRate(() -> {
    BigDecimal number = numbers.peekLast();
    if (number != null && number.remainder(divisor).byteValue() == 0) {
        System.out.println("Number: " + number);
        System.out.println("Deque size: " + numbers.size());
    }
}, 10, 10, TimeUnit.MILLISECONDS);

scheduledExecutorService.scheduleAtFixedRate(() -> {
    numbers.add(new BigDecimal(System.currentTimeMillis()));
}, 10, 10, TimeUnit.MILLISECONDS);

try {
    scheduledExecutorService.awaitTermination(1, TimeUnit.DAYS);
} catch (InterruptedException e) {
    e.printStackTrace();
}

```

This example creates two scheduled tasks. The first task takes the last number from a deque called `numbers`, and, if the number is divisible by 51, it prints the number and the deque's size. The second task puts numbers into the deque. Both tasks are scheduled at a fixed rate, and they run every 10 ms.

If the code is executed, you’ll see that the size of the deque is permanently increasing. This will eventually cause the deque to be filled with objects that consume all available heap memory.

To prevent this while preserving the semantics of this program, we can use a different method for taking numbers from the deque: `pollLast`. Contrary to the method `peekLast`, `pollLast` returns the element and removes it from the deque while `peekLast` only returns the last element.



## Pitfall: Not understanding that String is an immutable class


New Java programmers often forget, or fail to fully comprehend, that the Java `String` class is immutable.  This leads to problems like the one in the following example:

```java
public class Shout {
    public static void main(String[] args) {
        for (String s : args) {
            s.toUpperCase();
            System.out.print(s);
            System.out.print(" ");
        }
        System.out.println();
    }
}

```

The above code is supposed to print command line arguments in upper case.  Unfortunately, it does not work, the case of the arguments is not changed.  The problem is this statement:

```java
s.toUpperCase();

```

You might think that calling `toUpperCase()` is going to change `s` to an uppercase string.  It doesn't.  It can't!  `String` objects are immutable.  They cannot be changed.

In reality, the `toUpperCase()` method **returns** a `String` object which is an uppercase version of the `String` that you call it on.  This will probably be a new `String` object, but if `s` was already all uppercase, the result could be the existing string.

So in order to use this method effectively, you need to use the object returned by the method call; for example:

```java
s = s.toUpperCase();

```

In fact, the "strings never change" rule applies to all `String` methods.  If you remember that, then you can avoid a whole category of beginner's mistakes.



## Pitfall: combining assignment and side-effects


Occasionally we see StackOverflow Java questions (and C or C++ questions) that ask what something like this:

```java
i += a[i++] + b[i--];

```

evaluates to ... for some known initial states of `i`, `a` and `b`.

Generally speaking:

- for Java the answer is always specified<sup>1</sup>, but non-obvious, and often difficult to figure out
- for C and C++ the answer is often unspecified.

Such examples are often used in exams or job interviews as an attempt to see if the student or interviewee understands how expression evaluation really works in the Java programming language.  This is arguably legitimate as a "test of knowledge", but that does not mean that you should ever do this in a real program.

To illustrate, the following seemingly simple example has appeared a few times in StackOverflow questions (like [this one](http://stackoverflow.com/questions/7911776)).  In some cases, it appears as a genuine mistake in someone's code.

```java
int a = 1;
a = a++;
System.out.println(a);    // What does this print.

```

Most programmers (including Java experts) reading those statements **quickly** would say that it outputs `2`.  In fact, it outputs `1`.  For a detailed explanation of why, please read [this Answer](http://stackoverflow.com/a/12033710/139985).

However the real takeaway from this and similar examples is that **any** Java statement that **both** assigns to **and** side-effects the same variable is going to be **at best** hard to understand, and **at worst** downright misleading.  You should avoid writing code like this.

<sup>1 - modulo potential issues with the [Java Memory Model](http://stackoverflow.com/documentation/java/6829/java-memory-model#t=201701070218120765838) if the variables or objects are visible to other threads.</sup>

