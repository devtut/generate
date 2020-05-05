---
metaTitle: "Java - Java Memory Management"
description: "Setting the Heap, PermGen and Stack sizes, Garbage collection, Memory leaks in Java, Finalization, Manually triggering GC"
---

# Java Memory Management



## Setting the Heap, PermGen and Stack sizes


When a Java virtual machine starts, it needs to know how big to make the Heap, and the default size for thread stacks.  These can be specified using command-line options on the `java` command.  For versions of Java prior to Java 8, you can also specify the size of the PermGen region of the Heap.

Note that PermGen was removed in Java 8, and if you attempt to set the PermGen size the option will be ignored (with a warning message).

If you don't specify Heap and Stack sizes explicitly, the JVM will use defaults that are calculated in a version and platform specific way.  This may result in your application using too little or too much memory.  This is typically OK for thread stacks, but it can be problematic for a program that uses a lot of memory.

**Setting the Heap, PermGen and default Stack sizes:**

The following JVM options set the heap size:

- `-Xms<size>` - sets the initial heap size
- `-Xmx<size>` - sets the maximum heap size
- `-XX:PermSize<size>` - sets the initial PermGen size
- `-XX:MaxPermSize<size>` - sets the maximum PermGen size
- `-Xss<size>` - sets the default thread stack size

The `<size>` parameter can be a number of bytes, or can have a suffix of `k`, `m` or `g`.  The latter specify the size in kilobytes, megabytes and gigabytes respectively.

Examples:

```java
$ java -Xms512m -Xmx1024m JavaApp
$ java -XX:PermSize=64m -XX:MaxPermSize=128m JavaApp
$ java -Xss512k JavaApp

```

**Finding the default  sizes:**

The `-XX:+printFlagsFinal` option can be used to print the values of all  flags before starting the  JVM.  This can be used to print the defaults for the heap and stack size settings as follows:

<li>
For Linux, Unix, Solaris and Mac OSX
$ java -XX:+PrintFlagsFinal -version | grep -iE 'HeapSize|PermSize|ThreadStackSize'
</li>
<li>
For Windows:
<blockquote>
java -XX:+PrintFlagsFinal -version | findstr /i "HeapSize PermSize ThreadStackSize"
</blockquote>
</li>

The output of the above commands will resemble the following:

```java
uintx InitialHeapSize                          := 20655360        {product}
uintx MaxHeapSize                              := 331350016       {product}
uintx PermSize                                  = 21757952        {pd product}
uintx MaxPermSize                               = 85983232        {pd product}
 intx ThreadStackSize                           = 1024            {pd product}

```

The sizes are given in bytes.



## Garbage collection


### The C++ approach - new and delete

In a language like C++, the application program is responsible for managing the memory used by dynamically allocated memory.  When an object is created in the C++ heap using the `new` operator, there needs to be a corresponding use of the `delete` operator to dispose of the object:

<li>
If program forgets to `delete` an object and just "forgets" about it, the associated memory is lost to the application.  The term for this situation is a **memory leak**, and it too much memory leaks an application is liable to use more and more memory, and eventually crash.
</li>
<li>
On the other hand, if an application attempts to `delete` the same object twice, or use an object after it has been deleted, then the application is liable to crash due to problems with memory corruption
</li>

In a complicated C++ program, implementing memory management using `new` and `delete` can be time consuming.  Indeed, memory management is a common source of bugs.

### The Java approach - garbage collection

Java takes a different approach.  Instead of an explicit `delete` operator, Java provides an automatic mechanism known as garbage collection to reclaim the memory used by objects that are no longer needed.  The Java runtime system takes responsibility for finding the objects to be disposed of.  This task is performed by a component called a **garbage collector**, or GC for short.

At any time during the execution of a Java program, we can divide the set of all existing objects into two distinct subsets<sup>1</sup>:

<li>
Reachable objects are defined by the JLS as follows:
<blockquote>
A reachable object is any object that can be accessed in any potential continuing computation from any live thread.
</blockquote>
In practice, this means that there is a chain of references starting from an in-scope local variable or a `static` variable by which some code might be able to reach the object.
</li>
<li>
Unreachable objects are objects that **cannot possibly** be reached as above.
</li>

Any objects that are unreachable are **eligible** for garbage collection.  This does not mean that they **will** be garbage collected.  In fact:

- An unreachable object **does not** get collected immediately on becoming unreachable<sup>1</sup>.
- An unreachable object **may not ever** be garbage collected.

The Java language Specification gives a lot of latitude to a JVM implementation to decide when to collect unreachable objects.  It also (in practice) gives permission for a JVM implementation to be conservative in how it detects unreachable objects.

The one thing that the JLS guarantees is that no **reachable** objects will ever be garbage collected.

### What happens when an object becomes unreachable

First of all, nothing specifically happens when an object **becomes** unreachable.  Things only happen when the garbage collector runs **and** it detects that the object is unreachable.  Furthermore, it is common for a GC run to not detect all unreachable objects.

When the GC detects an unreachable object, the following events can occur.

<li>
If there are any `Reference` objects that refer to the object, those references will be cleared before the object is deleted.
</li>
<li>
If the object is **finalizable**, then it will be finalized.  This happens before the object is deleted.
</li>
<li>
The object can be deleted, and the memory it occupies can be reclaimed.
</li>

Note that there is a clear sequence in which the above events **can** occur, but nothing requires the garbage collector to perform the final deletion of any specific object in any specific time-frame.

### Examples of reachable and unreachable objects

Consider the following example classes:

```java
// A node in simple "open" linked-list.
public class Node {
    private static int counter = 0;

    public int nodeNumber = ++counter;
    public Node next;
}

public class ListTest {
    public static void main(String[] args) {
        test();                    // M1
        System.out.prinln("Done"); // M2
    }
    
    private static void test() {
        Node n1 = new Node();      // T1
        Node n2 = new Node();      // T2
        Node n3 = new Node();      // T3
        n1.next = n2;              // T4
        n2 = null;                 // T5
        n3 = null;                 // T6
    }
}

```

Let us examine what happens when `test()` is called.  Statements T1, T2 and T3 create `Node` objects, and the objects are all reachable via the `n1`, `n2` and `n3` variables respectively.  Statement T4 assigns the reference to the 2nd `Node` object to the `next` field of the first one.  When that is done, the 2nd `Node` is reachable via two paths:

```

n2 -> Node2
 n1 -> Node1, Node1.next -> Node2

```

In statement T5, we assign `null` to `n2`.  This breaks the first of the reachability chains for `Node2`, but the second one remains unbroken, so `Node2` is still reachable.

In statement T6, we assign `null` to `n3`.  This breaks the only reachability chain for `Node3`, which makes `Node3` unreachable.  However, `Node1` and `Node2` are both still reachable via the `n1` variable.

Finally, when the `test()` method returns, its local variables `n1`, `n2` and `n3` go out of scope, and therefore cannot be accessed by anything.  This breaks the remaining reachability chains for `Node1` and `Node2`, and all of the `Node` objects are nor unreachable and **eligible** for garbage collection.

<sup>1 - This is a simplification that ignores finalization, and `Reference` classes.</sup>
<sup>2 - Hypothetically, a Java implementation could do this, but the performance cost of doing this makes it impractical.</sup>



## Memory leaks in Java


In the [Garbage collection](http://stackoverflow.com/documentation/java/2804/java-memory-management/9473/garbage-collection#t=201610151259592490802) example, we implied that Java solves the problem of memory leaks.  This is not actually true.  A Java program can leak memory, though the causes of the leaks are rather different.

### Reachable objects can leak

Consider the following naive stack implementation.

```java
public class NaiveStack {
    private Object[] stack = new Object[100];
    private int top = 0;

    public void push(Object obj) {
        if (top >= stack.length) {
            throw new StackException("stack overflow");
        }
        stack[top++] = obj;
    }

    public Object pop() {
        if (top <= 0) {
            throw new StackException("stack underflow");
        }
        return stack[--top];
    }

    public boolean isEmpty() {
        return top == 0;
    }
}

```

When you `push` an object and then immediately `pop` it, there will still be a reference to the object in the `stack` array.

The logic of the stack implementation means that that reference cannot be returned to a client of the API.  If an object has been popped then we can prove that it cannot **"be accessed in any potential continuing computation from any live thread"**.  The problem is that a current generation JVM cannot prove this.  Current generation JVMs do not consider the logic of the program in determining whether references are reachable.  (For a start, it is not practical.)

But setting aside the issue of what **reachability** really means, we clearly have a situation here where the `NaiveStack` implementation is "hanging onto" objects that ought to be reclaimed.  That is a memory leak.

In this case, the solution is straightforward:

```

   public Object pop() {
        if (top <= 0) {
            throw new StackException("stack underflow");
        }
        Object popped = stack[--top];
        stack[top] = null;              // Overwrite popped reference with null.
        return popped;
    }

```

### Caches can be memory leaks

A common strategy for improving service performance is to cache results.  The idea is that you keep a record of common requests and their results in an in-memory data structure known as a cache.  Then, each time a request is made, you lookup the request in the cache.  If the lookup succeeds, you return the corresponding saved results.

This strategy can be very effective if implemented properly.  However, if implemented incorrectly, a cache can be a memory leak.  Consider the following example:

```java
public class RequestHandler {
    private Map<Task, Result> cache = new HashMap<>();

    public Result doRequest(Task task) {
        Result result = cache.get(task);
        if (result == null) {
            result == doRequestProcessing(task);
            cache.put(task, result);
        }
        return result;
    }
}

```

The problem with this code is that while any call to `doRequest` could add a new entry to the cache, there is nothing to remove them.  If the service is continually getting different tasks, then the cache will eventually consume all available memory.  This is a form of memory leak.

One approach to solving this is to use a cache with a maximum size, and throw out old entries when the cache exceeds the maximum.  (Throwing out the least recently used entry is a good strategy.)  Another approach is to build the cache using `WeakHashMap` so that the JVM can evict cache entries if the heap starts getting too full.



## Finalization


A Java object may declare a `finalize` method. This method is called just before Java releases the memory for the object. It will typically look like this:

```java
public class MyClass {
  
    //Methods for the class

    @Override
    protected void finalize() throws Throwable {
        // Cleanup code
    }
}

```

However, there some important caveats on the behavior of Java finalization.

- Java makes no guarantees about when a `finalize()` method will called.
- Java does not even guarantee that a `finalize()` method will be called some time during the running application's lifetime.
- The only thing that is guaranteed is that the method will be called before the object is deleted ... if the object is deleted.

The caveats above mean that it is a bad idea to rely on the `finalize` method to perform cleanup (or other) actions that must be performed in a timely fashion.  Over reliance on finalization can lead to storage leaks, memory leaks and other problems.

In short, there are very few situation where finalization is actually a good solution.

### Finalizers only run once

Normally, an object is deleted after it has been finalized.  However, this doesn't happen all of the time.  Consider the following example<sup>1</sup>:

```java
public class CaptainJack {
    public static CaptainJack notDeadYet = null;

    protected void finalize() {
        // Resurrection!
        notDeadYet = this;
    }
}

```

When an instance of `CaptainJack` becomes unreachable and the garbage collector attempts to reclaim it, the `finalize()` method will assign a reference to the instance to the `notDeadYet` variable.  That will make the instance reachable once more, and the garbage collector won't delete it.

Question:  Is Captain Jack immortal?

Answer: No.

The catch is the JVM will only run a finalizer on an object once in its lifetime.  If you assign `null` to `notDeadYet` causing a resurected instance to be unreachable once more, the garbage collector won't call `finalize()` on the object.

<sup>1 - See [https://en.wikipedia.org/wiki/Jack_Harkness](https://en.wikipedia.org/wiki/Jack_Harkness).</sup>



## Manually triggering GC


You can manually trigger the Garbage Collector by calling

```java
System.gc();

```

However, Java does not guarantee that the Garbage Collector has run when the call returns. This method simply "suggests" to the JVM (Java Virtual Machine) that you want it to run the garbage collector, but does not force it to do so.

It is generally considered a bad practice to attempt to manually trigger garbage collection. The JVM can be run with the `-XX:+DisableExplicitGC` option to disable calls to `System.gc()`. Triggering garbage collection by calling `System.gc()` can disrupt normal garbage management / object promotion activities of the specific garbage collector implementation in use by the JVM.



#### Remarks


In Java, objects are allocated in the heap, and heap memory is reclaimed by automatic garbage collection.  An application program cannot explicitly delete a Java object.

The basic principles of Java garbage collection are described in the [Garbage collection](http://stackoverflow.com/documentation/java/2804/java-memory-management/9473/garbage-collection#t=201610151259592490802) example.  Other examples describe finalization, how to trigger the garbage collector by hand, and the problem of storage leaks.

