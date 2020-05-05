---
metaTitle: "Java - Java Pitfalls - Performance Issues"
description: "Pitfall - String concatenation in a loop does not scale, Pitfall - Using size() to test if a collection is empty is inefficient., Pitfall - Using 'new' to create primitive wrapper instances is inefficient, Pitfall - Interning strings so that you can use == is a bad idea, Pitfall - Efficiency concerns with regular expressions, Pitfall - Small reads / writes on unbuffered streams are inefficient, Pitfall - The overheads of creating log messages, Pitfall - Calling 'new String(String)' is inefficient, Pitfall - Calling System.gc() is inefficient, Pitfall - Over-use of primitive wrapper types is inefficient, Pitfall - Iterating a Map's keys can be inefficient"
---

# Java Pitfalls - Performance Issues


This topic describes a number of "pitfalls" (i.e. mistakes that novice java programmers make) that relate to Java application performance.



## Pitfall - String concatenation in a loop does not scale


Consider the following code as an illustration:

```java
public String joinWords(List<String> words) {
    String message = "";
    for (String word : words) {
        message = message + " " + word;
    }
    return message;
}

```

Unfortunate this code is inefficient if the `words` list is long.  The root of the problem is this statement:

```java
message = message + " " + word;

```

For each loop iteration, this statement creates a new `message` string containing a copy of all characters in the original `message` string with extra characters appended to it. This generates a lot of temporary strings, and does a lot of copying.

When we analyse `joinWords`, assuming that there are N words with an average length of M, we find that O(N) temporary strings are created and O(M.N<sup>2</sup>) characters will be copied in the process.  The N<sup>2</sup> component is particularly troubling.

The recommended approach for this kind of problem<sup>1</sup> is to use a `StringBuilder` instead of string concatenation as follows:

```java
public String joinWords2(List<String> words) {
    StringBuilder message = new StringBuilder();
    for (String word : words) {
        message.append(" ").append(word);
    }
    return message.toString();
}

```

The analysis of `joinWords2` needs to take account of the overheads of "growing" the `StringBuilder` backing array that holds the builder's characters.  However, it turns out that the number of new objects created is O(logN) and that the number of characters copied is O(M.N) characters.  The latter includes characters copied in the final `toString()` call.

(It may be possible to tune this further, by creating the `StringBuilder` with the correct capacity to start with.  However, the overall complexity remains the same.)

Returning to the original `joinWords` method, it turns out that the critical statement will be optimized by a typical Java compiler to something like this:

```

 StringBuilder tmp = new StringBuilder();
  tmp.append(message).append(" ").append(word);
  message = tmp.toString();

```

However, the Java compiler will not "hoist" the `StringBuilder` out of the loop, as we did by hand in the code for `joinWords2`.

Reference:

- ["Is Java's String '+' operator in a loop slow?"](http://outoffactserror.blogspot.com/2017/03/is-javas-string-operator-in-loop-slow.html)

<sup>1 - In Java 8 and later, the `Joiner` class can be used to solve this particular problem.  However, that is not what this example is **really supposed to be about**.</sup>



## Pitfall - Using size() to test if a collection is empty is inefficient.


The Java Collections Framework provides two related methods for all `Collection` objects:

- [`size()`](https://docs.oracle.com/javase/8/docs/api/java/util/Collection.html#size--) returns the number of entries in a `Collection`, and
- [`isEmpty()`](https://docs.oracle.com/javase/8/docs/api/java/util/Collection.html#isEmpty--) method returns true if (and only if) the `Collection` is empty.

Both methods can be used to test for collection emptiness.  For example:

```java
Collection<String> strings = new ArrayList<>();
boolean isEmpty_wrong = strings.size() == 0; // Avoid this
boolean isEmpty = strings.isEmpty();         // Best

```

While these approaches look the same, some collection implementations do not store the size.  For such a collection, the implementation of `size()` needs to calculate the size each time it is called.  For instance:

- A simple linked list class (but not the `java.util.LinkedList`) might need to traverse the list to count the elements.
- The `ConcurrentHashMap` class needs to sum the entries in all of the map's "segments".
- A lazy implementation of a collection might need to realize the entire collection in memory in order to count the elements.

By contrast, an `isEmpty()` method only needs to test if there is **at least one** element in the collection.  This does not entail counting the elements.

While `size() == 0` is not always less efficient that `isEmpty()`, it is inconceivable for a properly implemented `isEmpty()` to be less efficient than `size() == 0`.  Hence `isEmpty()` is preferred.



## Pitfall - Using 'new' to create primitive wrapper instances is inefficient


The Java language allows you to use `new` to create instances `Integer`, `Boolean` and so on, but it is generally a bad idea.  It is better to either use autoboxing (Java 5 and later) or the `valueOf` method.

```

Integer i1 = new Integer(1);      // BAD
 Integer i2 = 2;                   // BEST (autoboxing)
 Integer i3 = Integer.valueOf(3);  // OK

```

The reason that using `new Integer(int)` explicitly is a bad idea is that it creates a new object (unless optimized out by JIT compiler).  By contrast, when autoboxing or an explicit `valueOf` call are used, the Java runtime will try to reuse an `Integer` object from a cache of pre-existing objects.  Each time the runtime has a cache "hit", it avoids creating an object.  This also saves heap memory and reduces GC overheads caused by object churn.

Notes:

1. In recent Java implementations, autoboxing is implemented by calling `valueOf`, and there are caches for `Boolean`, `Byte`, `Short`, `Integer`, `Long` and `Character`.
1. The caching behavior for the integral types is mandated by the Java Language Specification.



## Pitfall - Interning strings so that you can use == is a bad idea


When some programmers see this advice:

> 
"Testing strings using `==` is incorrect (unless the strings are interned)"


their initial reaction is to intern strings so that they can use `==`.  (After all `==` is faster than calling `String.equals(...)`, isn't it.)

This is the wrong approach, from a number of perspectives:

### Fragility

First of all, you can only safely use `==` if you know that **all** of the `String` objects you are testing have been interned.  The JLS guarantees that String literals in your source code will have been interned.  However, none of the standard Java SE APIs guarantee to return interned strings, apart from `String.intern(String)` itself.  If you miss just one source of `String` objects that haven't been interned, your application will be unreliable.  That unreliability will manifest itself as false negatives rather than exceptions which is liable to make it harder to detect.

### Costs of using 'intern()'

Under the hood, interning works by maintaining a hash table that contains previously interned `String` objects.  Some kind of weak reference mechanism is used so that the interning hash table does not become a storage leak.  While the hash table is implemented in native code (unlike `HashMap`, `HashTable` and so on), the `intern` calls are still relatively costly in terms of CPU and memory used.

This cost has to be compared with the saving of we are going to get by using `==` instead of `equals`.  In fact, we are not going to break even unless each interned string is compared with other strings "a few" times.

(Aside: the few situations where interning is worthwhile tend to be about reducing the memory foot print of an application where the same strings recur many times, **and** those strings have a long lifetime.)

### The impact on garbage collection

In addition to the direct CPU and memory costs described above, interned Strings impact on the garbage collector performance.

For versions of Java prior to Java 7, interned strings are held in the "PermGen" space which is collected infrequently.  If PermGen needs to be collected, this (typically) triggers a full garbage collection.  If the PermGen space fills completely, the JVM crashes, even if there was free space in the regular heap spaces.

In Java 7, the string pool was moved out of "PermGen" into the normal heap.  However, the hash table is still going to be a long-lived data structure, which is going to cause any interned strings to be long-lived.  (Even if the interned string objects were allocated in Eden space they would most likely be promoted before they were collected.)

Thus in all cases, interning a string is going to prolong its lifetime relative to an ordinary string.  That will increase the garbage collection overheads over the lifetime of the JVM.

The second issue is that the hash table needs to use a weak reference mechanism of some kind to prevent string interning leaking memory.  But such a mechanism is more work for the garbage collector.

These garbage collection overheads are difficult to quantify, but there is little doubt that they do exist.  If you use `intern` extensively, they could be significant.

### The string pool hashtable size

According to [this source](http://java-performance.info/string-intern-in-java-6-7-8/), from Java 6 onwards, the string pool is implemented as fixed sized hash table with chains to deal with strings that hash to the same bucket.  In early releases of Java 6, the hash table had a (hard-wired) constant size.  A tuning parameter (`-XX:StringTableSize`) was added as a mid-life update to Java 6.  Then in a mid-life update to Java 7, the default size of the pool was changed from `1009` to `60013`.

The bottom line is that if you do intend to use `intern` intensively in your code, it is **advisable** to pick a version of Java where the hashtable size is tunable and make sure that you tune the size it appropriately.  Otherwise, the performance of `intern` is liable to degrade as the pool gets larger.

### Interning as a potential denial of service vector

The hashcode algorithm for strings is well-known.  If you intern strings supplied by malicious users or applications, this could be used as part of a denial of service (DoS) attack.  If the malicious agent arranges that all of the strings it provides have the same hash code, this could lead to an unbalanced hash table and `O(N)` performance for `intern` ... where `N` is the number of collided strings.

(There are simpler / more effective ways to launch a DoS attack against a service.  However, this vector could be used if the goal of the DoS attack is to break security, or to evade first-line DoS defences.)



## Pitfall - Efficiency concerns with regular expressions


Regular expression matching is a powerful tool (in Java, and in other contexts) but it does have some drawbacks.  One of these that regular expressions tends to be rather expensive.

### Pattern and Matcher instances should be reused

Consider the following example:

```java
/**
 * Test if all strings in a list consist of English letters and numbers.
 * @param strings the list to be checked
 * @return 'true' if an only if all strings satisfy the criteria
 * @throws NullPointerException if 'strings' is 'null' or a 'null' element.
 */
public boolean allAlphanumeric(List<String> strings) {
    for (String s : strings) {
        if (!s.matches("[A-Za-z0-9]*")) {
            return false;
        }  
    }
    return true;
}

```

This code is correct, but it is inefficient.  The problem is in the `matches(...)` call.  Under the hood, `s.matches("[A-Za-z0-9]*")` is equivalent to this:

```java
Pattern.matches(s, "[A-Za-z0-9]*")

```

which is in turn equivalent to

```java
Pattern.compile("[A-Za-z0-9]*").matcher(s).matches()

```

The `Pattern.compile("[A-Za-z0-9]*")` call parses the regular expression, analyze it, and construct a `Pattern` object that holds the data structure that will be used by the regex engine.  This is a non-trivial computation.  Then a `Matcher` object is created to wrap the `s` argument.  Finally we call `match()` to do the actual pattern matching.

The problem is that this work is all repeated for each loop iteration.  The solution is to restructure the code as follows:

```java
private static Pattern ALPHA_NUMERIC = Pattern.compile("[A-Za-z0-9]*");

public boolean allAlphanumeric(List<String> strings) {
    Matcher matcher = ALPHA_NUMERIC.matcher("");
    for (String s : strings) {
        matcher.reset(s);
        if (!matcher.matches()) {
            return false;
        }  
    }
    return true;
}

```

Note that the [javadoc](http://docs.oracle.com/javase/8/docs/api/java/util/regex/Pattern.html) for `Pattern` states:

> 
Instances of this class are immutable and are safe for use by multiple concurrent threads. Instances of the `Matcher` class are not safe for such use.


### Don't use match() when you should use find()

Suppose you want to test if a string `s` contains three or more digits in a row.  You cn express this in various ways including:

```

 if (s.matches(".*[0-9]{3}.*")) {
      System.out.println("matches");
  }

```

or

```

 if (Pattern.compile("[0-9]{3}").matcher(s).find()) {
      System.out.println("matches");
  }

```

The first one is more concise, but it is also likely to be less efficient.  On the face of it, the first version is going to try to match the entire string against the pattern.  Furthermore, since ".*" is a "greedy" pattern, the pattern matcher is likely to advance "eagerly" try to the end of the string, and backtrack until it finds a match.

By contrast, the second version will search from left to right and will stop searching as soon as it finds the 3 digits in a row.

### Use more efficient alternatives to regular expressions

Regular expressions are a powerful tool, but they should not be your only tool.  A lot of tasks can be done more efficiently in other ways.  For example:

```

Pattern.compile("ABC").matcher(s).find()

```

does the same thing as:

```

s.contains("ABC")

```

except that the latter is a lot more efficient.  (Even if you can amortize the cost of compiling the regular expression.)

Often, the non-regex form is more complicated.  For example, the test performed by  the `matches()` call the earlier `allAlplanumeric` method can be rewritten as:

```

public boolean matches(String s) {
     for (char c : s) {
         if ((c >= 'A' && c <= 'Z') ||
             (c >= 'a' && c <= 'z') ||
             (c >= '0' && c <= '9')) {
              return false;
         }
     }
     return true;
 }

```

Now that is more code than using a `Matcher`, but it is also going to be significantly faster.

### Catastrophic Backtracking

(This is potentially a problem with all implementations of regular expressions, but we will mention it here because it is a pitfall for `Pattern` usage.)

Consider this (contrived) example:

```java
Pattern pat = Pattern.compile("(A+)+B");
System.out.println(pat.matcher("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAB").matches());
System.out.println(pat.matcher("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAC").matches());

```

The first `println` call will quickly print `true`.  The second one will print `false`.  Eventually.  Indeed, if you experiment with the code above, you will see that each time you add an `A` before the `C`, the time take will double.

This is behavior is an example of **catastrophic backtracking**.  The pattern matching engine that implements the regex matching is fruitlessly trying all of the **possible** ways that the pattern **might** match.

Let us look at what `(A+)+B` actually means.  Superficially, it seems to say "one or more `A` characters followed by a `B` value", but in reality it says one or more groups, each of which consists of one or more `A` characters.  So, for example:

- 'AB' matches one way only: '(A)B'
- 'AAB' matches two ways: '(AA)B' or '(A)(A)B`
- 'AAAB' matches four ways: '(AAA)B' or '(AA)(A)B`or '(A)(AA)B` or '(A)(A)(A)B`
- and so on

In other words, the number of possible matches is 2<sup>N</sup> where N is the number of `A` characters.

The above example is clearly contrived, but patterns that exhibit this kind of performance characteristics (i.e. `O(2^N)` or `O(N^K)` for a large `K`) arise frequently when ill-considered regular expressions are used.  There are some standard remedies:

- Avoid nesting repeating patterns within other repeating patterns.
- Avoid using too many repeating patterns.
- Use non-backtracking repetition as appropriate.
- Don't use regexes for complicated parsing tasks.  (Write a proper parser instead.)

Finally, beware of situations where a user or an API client can supply a regex string with pathological characteristics.  That can lead to accidental or deliberate "denial of service".

References:

- The [Regular Expressions](http://stackoverflow.com/documentation/regex) tag, particularly [http://stackoverflow.com/documentation/regex/977/backtracking#t=201610010339131361163](http://stackoverflow.com/documentation/regex/977/backtracking#t=201610010339131361163) and [http://stackoverflow.com/documentation/regex/4527/when-you-should-not-use-regular-expressions#t=201610010339593564913](http://stackoverflow.com/documentation/regex/4527/when-you-should-not-use-regular-expressions#t=201610010339593564913)
- ["Regex Performance"](https://blog.codinghorror.com/regex-performance/) by Jeff Atwood.
- ["How to kill Java with a Regular Expression"](http://andreas.haufler.info/2013/09/how-to-kill-java-with-regular-expression.html) by Andreas Haufler.



## Pitfall - Small reads / writes on unbuffered streams are inefficient


Consider the following code to copy one file to another:

```java
import java.io.*;

public class FileCopy {

    public static void main(String[] args) throws Exception {
        try (InputStream is = new FileInputStream(args[0]);
             OutputStream os = new FileOutputStream(args[1])) {
           int octet;
           while ((octet = is.read()) != -1) {
               os.write(octet);
           }
        }
    }
}

```

(We have deliberated omitted normal argument checking, error reporting and so on because they are not relevant to **point** of this example.)

If you compile the above code and use it to copy a huge file, you will notice that it is very slow.  In fact, it will be at least a couple of orders of magnitude slower than the standard OS file copy utilities.

(**Add actual performance measurements here!**)

The primary reason that the example above is slow (in the large file case) is that it is performing one-byte reads and one-byte writes on unbuffered byte streams.  The simple way to improve performance is to wrap the streams with buffered streams.  For example:

```java
import java.io.*;

public class FileCopy {

    public static void main(String[] args) throws Exception {
        try (InputStream is = new BufferedInputStream(
                     new FileInputStream(args[0]));
             OutputStream os = new BufferedOutputStream(
                     new FileOutputStream(args[1]))) {
           int octet;
           while ((octet = is.read()) != -1) {
               os.write(octet);
           }
        }
    }
}

```

These small changes will improve data copy rate by **at least** a couple of orders of magnitude, depending on various platform-related factors.  The buffered stream wrappers cause the data to be read and written in larger chunks.  The instances both have buffers implemented as byte arrays.

<li>
With `is`, data is read from the file into the buffer a few kilobytes at a time.  When `read()` is called, the implementation will typically return a byte from the buffer.  It will only read from the underlying input stream if the buffer has been emptied.
</li>
<li>
The behavior for `os` is analogous.  Calls to `os.write(int)` write single bytes into the buffer.  Data is only written to the output stream when the buffer is full, or when `os` is flushed or closed.
</li>

### What about character-based streams?

As you should be aware, Java I/O provides different APIs for reading and writing binary and text data.

- `InputStream` and `OutputStream` are the base APIs for stream-based binary I/O
- `Reader` and `Writer` are the base APIs for stream-based text I/O.

For text I/O, `BufferedReader` and `BufferedWriter` are the equivalents for `BufferedInputStream` and `BufferedOutputStream`.

### Why do buffered streams make this much difference?

The real reason that buffered streams help performance is to do with the way that an application talks to the operating system:

<li>
Java method in a Java application, or native procedure calls in the JVM's native runtime libraries are fast.  They typically take a couple of machine instructions and have minimal performance impact.
</li>
<li>
By contrast, JVM runtime calls to the operating system are not fast.  They involve something known as a "syscall".  The typical pattern for a syscall is as follows:
<ol>
- Put the syscall arguments into registers.
- Execute a SYSENTER trap instruction.
- The trap handler switched to privileged state and changes the virtual memory mappings.  Then it dispatches to the code to handle the specific syscall.
- The syscall handler checks the arguments, taking care that it isn't being told to access memory that the user process should not see.
<li>The syscall specific work is performed.  In the case of a `read` syscall, this may involve:
<ol>
- checking that there is data to be read at the file descriptor's current position
- calling the file system handler to fetch the required data from disk (or wherever it is stored) into the buffer cache,
- copying data from the buffer cache to the JVM-supplied address
- adjusting thstream pointerse file descriptor position
</ol>
</li>
- Return from the syscall.  This entails changing VM mappings again and switching out of privileged state.
</ol>
</li>

As you can imagine, performing a single syscall can thousands of machine instructions. Conservatively, **at least** two orders of magnitude longer than a regular method call.  (Probably three or more.)

Given this, the reason that buffered streams make a big difference is that they drastically reduce the number of syscalls.  Instead of doing a syscall for each `read()` call, the buffered input stream reads a large amount of data into a buffer as required.  Most `read()` calls on the buffered stream do some simple bounds checking and return a `byte` that was read previously.  Similar reasoning applies in the output stream case, and also the character stream cases.

(Some people think that buffered I/O performance comes from the mismatch between the read request size and the size of a disk block, disk rotational latency and things like that.  In fact, a modern OS uses a number of strategies to ensure that the application **typically** doesn't need to wait for the disk.  This is not the real explanation.)

### Are buffered streams always a win?

Not always.  Buffered streams are definitely a win if your application is going to do lots of "small" reads or writes.   However, if your application only needs to perform large reads or writes to / from a large `byte[]`  or `char[]`, then buffered streams will give you no real benefits.  Indeed there might even be a (tiny) performance penalty.

### Is this the fastest way to copy a file in Java?

No it isn't.  When you use Java's stream-based APIs to copy a file, you incur the cost of at least one extra memory-to-memory copy of the data.  It is possible to avoid this if your use the NIO `ByteBuffer` and `Channel` APIs.  (**Add a link to a separate example here.**)



## Pitfall - The overheads of creating log messages


`TRACE` and `DEBUG` log levels are there to be able to convey high detail about the operation of the given code at runtime. Setting the log level above these is usually recommended, however some care must be taken for these statements to not affect performance even when seemingly "turned off".

Consider this log statement:

```java
// Processing a request of some kind, logging the parameters
LOG.debug("Request coming from " + myInetAddress.toString() 
          + " parameters: " + Arrays.toString(veryLongParamArray));

```

Even when the log level is set to `INFO`, arguments passed to `debug()` will be evaluated on each execution of the line. This makes it unnecessarily consuming on several counts:

- `String` concatenation: multiple `String` instances will be created
- `InetAddress` might even do a DNS lookup.
- the `veryLongParamArray` might be very long - creating a String out of it consumes memory, takes time

### Solution

Most logging framework provide means to create log messages using fix strings and object references. The log message will be evaluated only if the message is actually logged. Example:

```java
// No toString() evaluation, no string concatenation if debug is disabled
LOG.debug("Request coming from {} parameters: {}", myInetAddress, parameters));

```

This works very well as long as all parameters can be converted to strings using [String.valueOf(Object)](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#valueOf-java.lang.Object-). If the log message compuation is more complex, the log level can be checked before logging:

```java
if (LOG.isDebugEnabled()) {
    // Argument expression evaluated only when DEBUG is enabled
    LOG.debug("Request coming from {}, parameters: {}", myInetAddress,
              Arrays.toString(veryLongParamArray);
}

```

Here, `LOG.debug()` with the costly `Arrays.toString(Obect[])` computation is processed only when `DEBUG` is actually enabled.



## Pitfall - Calling 'new String(String)' is inefficient


Using `new String(String)` to duplicate a string is inefficient and almost always unnecessary.

- String objects are immutable, so there is no need to copy them to protect against changes.
- In some older versions of Java, `String` objects can share backing arrays with other `String` objects.  In those versions, it is possible to leak memory by creating a (small) substring of a (large) string and retaining it.  However, from Java 7 onwards, `String` backing arrays are not shared.

In the absence of any tangible benefit, calling `new String(String)` is simply wasteful:

- Making the copy takes CPU time.
- The copy uses more memory which increases the application's memoru footprint and / or increases GC overheads.
- Operations like `equals(Object)` and `hashCode()` can be slower if String objects are copied.



## Pitfall - Calling System.gc() is inefficient


It is (almost always) a bad idea to call `System.gc()`.

The javadoc for the `gc()` method specifies the following:

> 
"Calling the `gc` method suggests that the Java Virtual Machine expend effort toward recycling unused objects in order to make the memory they currently occupy available for quick reuse. When control returns from the method call, the Java Virtual Machine has made a best effort to reclaim space from all discarded objects."


There are a couple of important points that can be drawn from this:

<li>
The use of the word "suggests" rather than (say) "tells" means that the JVM is free to ignore the suggestion.  The default JVM behavior (recent releases) is to follow the suggestion, but this can be overridden by setting `-XX:+DisableExplicitGC` when when launching the JVM.
</li>
<li>
The phrase "a best effort to reclaim space from all discarded objects" implies that calling `gc` will trigger a "full" garbage collection.
</li>

So why is calling `System.gc()` a bad idea?

First, running a full garbage collection is expensive.  A full GC involves visiting and "marking" every object that is still reachable; i.e. every object that is not garbage.  If you trigger this when there isn't much garbage to be collected, then the GC does a lot of work for relatively little benefit.

Second, a full garbage collection is liable to disturb the "locality" properties of the objects that are not collected.  Objects that are allocated by the same thread at roughly the same time tend to be allocated close together in memory.  This is good.  Objects that are allocated at the same time are likely to be related; i.e. reference each other.  If your application uses those references, then the chances are that memory access will be faster because of various memory and page caching effects.  Unfortunately, a full garbage collection tend to move objects around so that objects that were once close are now further apart.

Third, running a full garbage collection is liable to make your application pause until the collection is complete.  While this is happening, your application will be non-responsive.

In fact, the best strategy is to let the JVM decide when to run the GC, and what kind of collection to run.  If you don't interfere, the JVM will choose a time and collection type that optimizes throughput or minimizes GC pause times.

At the beginning we said "... (almost always) a bad idea ...".  In fact there are a couple of scenarios where it **might** be a good idea:

<li>
If you are implementing a unit test for some code that is garbage collection sensitive (e.g. something involving finalizers or weak / soft / phantom references) then calling `System.gc()` may be necessary.
</li>
<li>
In some interactive applications, there can be particular points in time where the user won't care if there is a garbage collection pause.  One example is a game where there are natural pauses in the "play"; e.g. when loading a new level.
</li>



## Pitfall - Over-use of primitive wrapper types is inefficient


Consider these two pieces of code:

```java
int a = 1000;
int b = a + 1;

```

and

```java
Integer a = 1000;
Integer b = a + 1;

```

Question: Which version is more efficient?

Answer: The two versions look almost the identical, but the first version is a lot more efficient than the second one.

The second version is using a representation for the numbers that uses more space, and is relying on auto-boxing and auto-unboxing behind the scenes.  In fact the second version is directly equivalent to the following code:

```java
Integer a = Integer.valueOf(1000);               // box 1000
Integer b = Integer.valueOf(a.intValue() + 1);   // unbox 1000, add 1, box 1001

```

Comparing this to the other version that uses `int`, there are clearly three extra method calls when `Integer` is used.  In the case of `valueOf`, the calls are each going to create and initialize a new `Integer` object.  All of this extra boxing and unboxing work is likely to make the second version an order of magnitude slower than the first one.

In addition to that, the second version is allocating objects on the heap in each `valueOf` call.  While the space utilization is platform specific, it is likely to be in the region of 16 bytes for each `Integer` object.  By contrast, the `int` version needs zero extra heap space, assuming that `a` and `b` are local variables.

Another big reason why primitives are faster then their boxed equivalent is how their respective array types are laid out in memory.

If you take `int[]` and `Integer[]` as an example, in the case of an `int[]` the `int` **values** are contiguously laid out in memory. But in the case of an `Integer[]` it's not the values that are laid out, but references (pointers) to `Integer` objects, which in turn contain the actual `int` values.

Besides being an extra level of indirection, this can be a big tank when it comes to cache locality when iterating over the values. In the case of an `int[]` the CPU could fetch all the values in the array, into it's cache at once, because they are contiguous in memory. But in the case of an `Integer[]` the CPU potentially has to do an additional memory fetch for each element, since the array only contains references to the actual values.

In short, using primitive wrapper types is relatively expensive in both CPU and memory resources.  Using them unnecessarily is in efficient.



## Pitfall - Iterating a Map's keys can be inefficient


The following example code is slower than it needs to be :

```java
Map<String, String> map = new HashMap<>(); 
for (String key : map.keySet()) {
    String value = map.get(key);
    // Do something with key and value
}

```

That is because it requires a map lookup (the `get()` method) for each key in the map. This lookup may not be efficient (in a HashMap, it entails calling `hashCode` on the key, then looking up the correct bucket in internal data structures, and sometimes even calling `equals`). On a large map, this may not be a trivial overhead.

The correct way of avoiding this is to iterate on the map's entries, which is detailed in the [Collections topic](http://stackoverflow.com/documentation/java/90/collections/5856/iterating-over-collections#t=201608260922476000177)



#### Remarks


This topic describes some "micro" Java coding practices that are inefficient.  In most cases, the inefficiencies are relatively small, but it is still worth avoiding them is possible.

