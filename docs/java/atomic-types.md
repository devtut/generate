---
metaTitle: "Java - Atomic Types"
description: "Creating Atomic Types, Motivation for Atomic Types"
---

# Atomic Types


Java Atomic Types are simple mutable types that provide basic operations that are thread-safe and atomic without resorting to locking.  They are intended for use in cases where locking would be a concurrency bottleneck, or where there is risk of deadlock or livelock.



## Creating Atomic Types


For simple multi-threaded code, using [synchronization](http://stackoverflow.com/documentation/java/121/concurrent-programming-threads/7299/synchronization#t=201610211859175882372) is acceptable.  However, using synchronization does have a liveness impact, and as a codebase becomes more complex, the likelihood goes up that you will end up with [Deadlock](https://docs.oracle.com/javase/tutorial/essential/concurrency/deadlock.html), [Starvation, or Livelock](https://docs.oracle.com/javase/tutorial/essential/concurrency/starvelive.html).

In cases of more complex concurrency, using Atomic Variables is often a better alternative, as it allows an individual variable to be accessed in a thread-safe manner without the overhead of using synchronized methods or code blocks.

Creating an `AtomicInteger` type:

```java
AtomicInteger aInt = new AtomicInteger() // Create with default value 0

AtomicInteger aInt = new AtomicInteger(1) // Create with initial value 1

```

Similarly for other instance types.

```java
AtomicIntegerArray aIntArray = new AtomicIntegerArray(10) // Create array of specific length
AtomicIntegerArray aIntArray = new AtomicIntegerArray(new int[] {1, 2, 3}) // Initialize array with another array

```

Similarly for other atomic types.

There is a notable exception that there is no `float` and `double` types. These can be simulated through the use of `Float.floatToIntBits(float)` and `Float.intBitsToFloat(int)` for `float` as well as `Double.doubleToLongBits(double)` and `Double.longBitsToDouble(long)` for doubles.

If you are willing to use `sun.misc.Unsafe` you can use any primitive variable as atomic by using the atomic operation in `sun.misc.Unsafe`. All primitive types should be converted or encoded in int or longs to so use it in this way. For more on this see: [sun.misc.Unsafe](http://stackoverflow.com/documentation/java/6771/sun-misc-unsafe).



## Motivation for Atomic Types


The simple way to implement multi-threaded applications is to use Java's built-in synchronization and locking primitives; e.g. the `synchronized` keyword.  The following example shows how we might use `synchronized` to accumulate counts.

```java
public class Counters {
    private final int[] counters;

    public Counters(int nosCounters) {
        counters = new int[nosCounters];
    }

    /**
     * Increments the integer at the given index
     */
    public synchronized void count(int number) {
        if (number >= 0 && number < counters.length) {
            counters[number]++;
        }
    }

    /**
     * Obtains the current count of the number at the given index,
     * or if there is no number at that index, returns 0.
     */
    public synchronized int getCount(int number) {
        return (number >= 0 && number < counters.length) ? counters[number] : 0;
    }
}

```

This implementation will work correctly.  However, if you have a large number of threads making lots of simultaneous calls on the same `Counters` object, the synchronization is liable to be a bottleneck.  Specifically:

1. Each `synchronized` method call will start with the current thread acquiring the lock for the `Counters` instance.
1. The thread will hold the lock while it checks `number` value and updates the counter.
1. Finally, the it will release the lock, allowing other threads access.

If one thread attempts to acquire the lock while another one holds it, the attempting thread will be blocked (stopped) at step 1 until the lock is released.  If multiple threads are waiting, one of them will get it, and the others will continue to be blocked.

This can lead to a couple of problems:

<li>
If there is a lot of **contention** for the lock (i.e. lots of thread try to acquire it), then some threads can be blocked for a long time.
</li>
<li>
When a thread is blocked waiting for the lock, the operating system will typically try switch execution to a different thread. This **context switching** incurs a relatively large performance impact on the processor.
</li>
<li>
When there are multiple threads blocked on the same lock, there are no guarantees that any one of them will be treated "fairly" (i.e. each thread is guaranteed to be scheduled to run).  This can lead to **thread starvation**.
</li>

### How does one implement Atomic Types?

Let us start by rewriting the example above using `AtomicInteger` counters:

```java
public class Counters {
    private final AtomicInteger[] counters;

    public Counters(int nosCounters) {
        counters = new AtomicInteger[nosCounters];
        for (int i = 0; i < nosCounters; i++) {
            counters[i] = new AtomicInteger();
        }
    }

    /**
     * Increments the integer at the given index
     */
    public void count(int number) {
        if (number >= 0 && number < counters.length) {
            counters[number].incrementAndGet();
        }
    }

    /**
     * Obtains the current count of the object at the given index,
     * or if there is no number at that index, returns 0.
     */
    public int getCount(int number) {
        return (number >= 0 && number < counters.length) ? 
                counters[number].get() : 0;
    }
}

```

We have replaced the `int[]` with an `AtomicInteger[]`, and initialized it with an instance in each element.  We have also added calls to `incrementAndGet()` and `get()` in place of operations on `int` values.

But the most important thing is that we can remove the `synchronized` keyword because locking is no longer required.  This works because the `incrementAndGet()` and `get()` operations are **atomic** and **thread-safe**.  In this context, it means that:

<li>
Each counter in the array will only be **observable** in the either the "before" state for an operation (like an "increment") or in the "after" state.
</li>
<li>
Assuming that the operation occurs at time `T`, no thread will be able to see the "before" state after time `T`.
</li>

Furthermore, while two threads might actually attempt to update the same `AtomicInteger` instance at the same time, the implementations of the operations ensure that only one increment happens at a time on the given instance. This is done without locking, often resulting in better performance.

### How do Atomic Types work?

Atomic types typically rely on specialized hardware instructions in the instruction set of the target machine.  For example, Intel-based instruction sets provide a `CAS` ([Compare and Swap](https://en.wikipedia.org/wiki/Compare-and-swap)) instruction that will perform a specific sequence of memory operations atomically.

These low-level instructions are are used to implement higher-level operations in the APIs of the respective `AtomicXxx` classes.  For example, (again, in C-like pseudocode):

```java
private volatile num;

int increment() {
  while (TRUE) {
    int old = num;
    int new = old + 1;
    if (old == compare_and_swap(&num, old, new)) {
      return new;
    }
  }
}

```

If there is no contention on the `AtomicXxxx`, the `if` test will succeed and the loop will end immediately.  If there is contention, then the `if` will fail for all but one of the threads, and they will "spin" in the loop for a small number of cycles of the loop.  In practice, the spinning is orders of magnitude faster (except at **unrealistically high** levels of contention, where synchronized performs better than atomic classes because when the CAS operation fails, then the retry will only add more contention) than suspending the thread and switching to another one.

Incidentally, CAS instructions are typically used by the JVM to implement **uncontended locking**.  If the JVM can see that a lock is not currently locked, it will attempt to use a CAS to acquire the lock.  If the CAS succeeds, then there is no need to do the expensive thread scheduling, context switching and so on.  For more information on the techniques used, see [Biased Locking in HotSpot](https://blogs.oracle.com/dave/entry/biased_locking_in_hotspot).



#### Parameters


|Parameter|Description
|---|---|---|---|---|---|---|---|---|---
|set|Volatile set of the field
|get|Volatile read of the field
|lazySet|This is a store ordered operation of the field
|compareAndSet|If the value is the expeed value then sent it to the new value
|getAndSet|get the current value and update



#### Remarks


Many on essentially combinations of volatile reads or writes and [CAS](https://en.wikipedia.org/wiki/Compare-and-swap) operations. Best way to understand this is to look at the source code directly. E.g. [AtomicInteger](http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/java/util/concurrent/atomic/AtomicInteger.java), [Unsafe.getAndSet](http://grepcode.com/file/repository.grepcode.com/java/root/jdk/openjdk/8u40-b25/sun/misc/Unsafe.java#Unsafe.getAndSetInt%28java.lang.Object%2Clong%2Cint%29)

