---
metaTitle: "Java - Java Pitfalls - Threads and Concurrency"
description: "Pitfall: incorrect use of wait() / notify(), Pitfall - Extending 'java.lang.Thread', Pitfall - Too many threads makes an application slower., Pitfall - Thread creation is relatively expensive, Pitfall: Shared variables require proper synchronization"
---

# Java Pitfalls - Threads and Concurrency



## Pitfall: incorrect use of wait() / notify()


The methods `object.wait()`, `object.notify()` and `object.notifyAll()` are meant to be used in a very specific way.  (see [http://stackoverflow.com/documentation/java/5409/wait-notify#t=20160811161648303307](http://stackoverflow.com/documentation/java/5409/wait-notify#t=20160811161648303307) )

### The "Lost Notification" problem

One common beginner mistake is to unconditionally call `object.wait()`

```java
private final Object lock = new Object();

public void myConsumer() {
    synchronized (lock) {
        lock.wait();     // DON'T DO THIS!!
    }
    doSomething();
}

```

The reason this is wrong is that it depends on some other thread to call `lock.notify()` or `lock.notifyAll()`, but nothing guarantees that the other thread did not make that call **before** the consumer thread called `lock.wait()`.

`lock.notify()` and `lock.notifyAll()` do not do anything at all if some other thread is not **already** waiting for the notification.  The thread that calls `myConsumer()` in this example will hang forever if it is too late to catch the notification.

### The "Illegal Monitor State" bug

If you call `wait()` or `notify()` on an object without holding the lock, then the JVM will throw `IllegalMonitorStateException`.

```java
public void myConsumer() {
    lock.wait();      // throws exception
    consume();
}

public void myProducer() {
    produce();
    lock.notify();    // throws exception
}

```

(The design for `wait()` / `notify()` requires that the lock is held because this is necessary to avoid systemic race conditions.  If it was possible to call `wait()` or `notify()` without locking, then it would be impossible to implement the primary use-case for these primitives: waiting for a condition to occur.)

### Wait / notify is too low-level

The **best** way to avoid problems with `wait()` and `notify()` is to not use them.  Most synchronization problems can be solved by using the higher-level synchronization objects (queues, barriers, semaphores, etc.) that are available in the `java.utils.concurrent` package.



## Pitfall - Extending 'java.lang.Thread'


The javadoc for the `Thread` class shows two ways to define and use a thread:

Using a custom thread class:

```

class PrimeThread extends Thread {
     long minPrime;
     PrimeThread(long minPrime) {
         this.minPrime = minPrime;
     }

     public void run() {
         // compute primes larger than minPrime
          . . .
     }
 }

 PrimeThread p = new PrimeThread(143);
 p.start();

```

Using a `Runnable`:

```

class PrimeRun implements Runnable {
     long minPrime;
     PrimeRun(long minPrime) {
         this.minPrime = minPrime;
     }

     public void run() {
         // compute primes larger than minPrime
          . . .
     }
 }

 PrimeRun p = new PrimeRun(143);
 new Thread(p).start();

```

(Source: [`java.lang.Thread` javadoc](http://docs.oracle.com/javase/8/docs/api/java/lang/Thread.html).)

The custom thread class approach works, but it has a few problems:

<li>
It is awkward to use `PrimeThread` in a context that uses a classic thread pool, an executor, or the ForkJoin framework.  (It is not impossible, because `PrimeThread` indirectly implements `Runnable`, but using a custom `Thread` class as a `Runnable` is certainly clumsy, and may not be viable ... depending on other aspects of the class.)
</li>
<li>
There is more opportunity for mistakes in other methods.  For example, if you declared a `PrimeThread.start()` without delegating to `Thread.start()`, you would end up with a "thread" that ran on the current thread.
</li>

The approach of putting the thread logic into a `Runnable` avoids these problems.  Indeed, if you use an anonymous class (Java 1.1 onwards) to implement the `Runnable` the result is more succinct, and more readable than the examples above.

```

final long minPrime = ...
 new Thread(new Runnable() {
     public void run() {
         // compute primes larger than minPrime
          . . .
     }
 }.start();

```

With a lambda expression (Java 8 onwards), the above example would become even more elegant:

```

final long minPrime = ...
 new Thread(() -> {
    // compute primes larger than minPrime
     . . .
 }).start();

```



## Pitfall - Too many threads makes an application slower.


A lot of people who are new to multi-threading think that using threads automatically make an application go faster. In fact, it is a lot more complicated than that.  But one thing that we can state with certainty is that for any computer there is a limit on the number of threads that can be run at the same time:

- A computer has a fixed number of **cores** (or **hyperthreads**).
- A Java thread has to be **scheduled** to a core or hyperthread in order to run.
- If there are more runnable Java threads than (available) cores / hyperthreads, some of them must wait.

This tells us that simply creating more and more Java threads **cannot** make the application go faster and faster.  But there are other considerations as well:

<li>
Each thread requires an off-heap memory region for its thread stack.  The typical (default) thread stack size is 512Kbytes or 1Mbytes.  If you have a significant number of threads, the memory usage can be significant.
</li>
<li>
Each active thread will refer to a number of objects in the heap.  That increases the working set of **reachable** objects, which impacts on garbage collection and on physical memory usage.
</li>
<li>
The overheads of switching between threads is non-trivial.  It typically entails a switch into the OS kernel space to make a thread scheduling decision.
</li>
<li>
The overheads of thread synchronization and inter-thread signaling (e.g. wait(), notify() / notifyAll) **can be** significant.
</li>

Depending on the details of your application, these factors generally mean that there is a "sweet spot" for the number of threads.  Beyond that, adding more threads gives minimal performance improvement, and can make performance worse.

If your application create for each new task, then an unexpected increase in the workload (e.g. a high request rate) can lead to catastrophic behavior.

A better way to deal with this is to use bounded thread pool whose size you can control (statically or dynamically).  When there is too much work to do, the application needs to queue the requests.  If you use an `ExecutorService`, it will take care of the thread pool management and task queuing.



## Pitfall - Thread creation is relatively expensive


Consider these two micro-benchmarks:

The first benchmark simply creates, starts and joins threads.  The thread's `Runnable` does no work.

```java
public class ThreadTest {
    public static void main(String[] args) throws Exception {
        while (true) {
            long start = System.nanoTime();
            for (int i = 0; i < 100_000; i++) {
                Thread t = new Thread(new Runnable() {
                        public void run() {
                }});
                t.start();
                t.join();
            }
            long end = System.nanoTime();
            System.out.println((end - start) / 100_000.0);
        }
    }
}

$ java ThreadTest 
34627.91355
33596.66021
33661.19084
33699.44895
33603.097
33759.3928
33671.5719
33619.46809
33679.92508
33500.32862
33409.70188
33475.70541
33925.87848
33672.89529
^C

```

On a typical modern PC running Linux with 64bit Java 8 u101, this benchmark shows an average time taken to create, start and join thread of between 33.6 and 33.9 microseconds.

The second benchmark does the equivalent to the first but using an `ExecutorService` to submit tasks and a `Future` to rendezvous with the end of the task.

```java
import java.util.concurrent.*;

public class ExecutorTest {
    public static void main(String[] args) throws Exception {
        ExecutorService exec = Executors.newCachedThreadPool();
        while (true) {
            long start = System.nanoTime();
            for (int i = 0; i < 100_000; i++) {
                Future<?> future = exec.submit(new Runnable() {
                    public void run() {
                    }
                });
                future.get();
            }
            long end = System.nanoTime();
            System.out.println((end - start) / 100_000.0);
        }
    }
}

$ java ExecutorTest
6714.66053
5418.24901
5571.65213
5307.83651
5294.44132
5370.69978
5291.83493
5386.23932
5384.06842
5293.14126
5445.17405
5389.70685
^C

```

As you can see, the averages are between 5.3 and 5.6 microseconds.

While the actual times will depend on a variety of factors, the difference between these two results is significant.  It is clearly faster to use a thread pool to recycle threads than it is to create new threads.



## Pitfall: Shared variables require proper synchronization


Consider this example:

```java
public class ThreadTest implements Runnable {
   
    private boolean stop = false;
    
    public void run() {
        long counter = 0;
        while (!stop) {
            counter = counter + 1;
        }
        System.out.println("Counted " + counter);
    }

    public static void main(String[] args) {
        ThreadTest tt = new ThreadTest();
        new Thread(tt).start();    // Create and start child thread
        Thread.sleep(1000);
        tt.stop = true;            // Tell child thread to stop.
    }
}

```

The intent of this program is intended to start a thread, let it run for 1000 milliseconds, and then cause it to stop by setting the `stop` flag.

### Will it work as intended?

Maybe yes, may be no.

An application does not necessarily stop when the `main` method returns.  If another thread has been created, and that thread has not been marked as a daemon thread, then the application will continue to run after the main thread has ended.  In this example, that means that the application will keep running until child thread ends.  That should happens when `tt.stop` is set to `true`.

But that is actually not strictly true.  In fact, the child thread will stop after it has **observed** `stop` with the value `true`.   Will that happen?  Maybe yes, maybe no.

The Java Language Specification **guarantees** that memory reads and writes made in a thread are visible to that thread, as per the order of the statements in the source code.  However, in general, this is NOT guaranteed when one thread writes and another thread (subsequently) reads.  To get guaranteed visibility, there needs to be a chain of **happens-before** relations between a write and a subsequent read.  In the example above, there is no such chain for the update to the `stop` flag, and therefore it is not guaranteed that the child thread will see `stop` change to `true`.

**(Note to authors: There should be a separate Topic on the Java Memory Model to go into the deep technical details.)**

### How do we fix the problem?

In this case, there are two simple ways to ensure that the `stop` update is visible:

<li>
Declare `stop` to be `volatile`; i.e.

```java
 private volatile boolean stop = false;

```


For a `volatile` variable, the JLS specifies that there is a **happens-before** relation between a write by one thread and a later read by a second thread.
</li>
<li>
Use a mutex to synchronize as follows:
</li>

```java
public class ThreadTest implements Runnable {
   
    private boolean stop = false;
    
    public void run() {
        long counter = 0;
        while (true) {
            synchronize (this) {
                if (stop) {
                    break;
                }
            }
            counter = counter + 1;
        }
        System.out.println("Counted " + counter);
    }

    public static void main(String[] args) {
        ThreadTest tt = new ThreadTest();
        new Thread(tt).start();    // Create and start child thread
        Thread.sleep(1000);
        synchronize (tt) {
            tt.stop = true;        // Tell child thread to stop.
        }
    }
}

```

In addition to ensuring that there is mutual exclusion, the JLS specifies that there is a **happens-before** relation between the releasing a mutex in one thread and gaining the same mutex in a second thread.

### But isn't assignment atomic?

Yes it is!

However, that fact does not mean that the effects of update will be visible simultaneously to all threads.  Only a proper chain of **happens-before** relations will guarantee that.

### Why did they do this?

Programmers doing multi-threaded programming in Java for the first time find the Memory Model is challenging.  Programs behave in an unintuitive way because the natural expectation is that writes are visible uniformly.  So why the Java designers design the Memory Model this way.

It actually comes down to a compromise between performance and ease of use (for the programmer).

A modern computer architecture consists of multiple processors (cores) with individual register sets.  Main memory is accessible either to all processors or to groups of processors.  Another property of modern computer hardware is that access to registers is typically orders of magnitude faster to access than access to main memory.  As the number of cores scales up, it is easy to see that reading and writing to main memory can become a system's main performance bottleneck.

This mismatch is addressed by implementing one or more levels of memory caching between the processor cores and main memory.  Each core access memory cells via its cache.  Normally, a main memory read only happens when there is a cache miss, and a main memory write only happens when a cache line needs to be flushed.  For an application where each core's working set of memory locations will fit into its cache, the core speed is no longer limited by main memory speed / bandwidth.

But that gives us a new problem when multiple cores are reading and writing shared variables.  The latest version of a variable may sit in one core's cache.  Unless the that core flushes the cache line to main memory, AND other cores invalidate their cached copy of older versions, some of them are liable to see stale versions of the variable.  But if the caches were flushed to memory each time there is a cache write ("just in case" there was a read by another core) that would consume main memory bandwidth unnecessarily.

The standard solution used at the hardware instruction set level is to provide instructions for cache invalidation and a cache write-through, and leave it to the compiler to decide when to use them.

Returning to Java. the Memory Model is designed so that the Java compilers are not required to issue cache invalidation and write-through instructions where they are not really needed.  The assumption is that the programmer will use an appropriate synchronization mechanism (e.g. primitive mutexes, `volatile`, higher-level concurrency classes and so on) to indicate that it needs memory visibility.  In the absence of a **happens-before** relation, the Java compilers are free to **assume** that no cache operations (or similar) are required.

This has significant performance advantages for multi-threaded applications, but the downside is that writing correct multi-threaded applications is not a simple matter.  The programmer **does** have to understand what he or she is doing.

### Why can't I reproduce this?

There are a number of reasons why problems like this are difficult to reproduce:

<li>
As explained above, the consequence of not dealing with memory visibility issues problems properly is **typically** that your compiled application does not handle the memory caches correctly.  However, as we alluded to above, memory caches often get flushed anyway.
</li>
<li>
When you change the hardware platform, the characteristics of the memory caches may change.  This can lead to different behavior if your application does not synchronize correctly.
</li>
<li>
You may be observing the effects of **serendipitous** synchronization.  For example, if you add traceprints, their is typically some synchronization happening behind the scenes in the I/O streams that causes cache flushes.  So adding traceprints **often** causes the application to behave differently.
</li>
<li>
Running an application under a debugger causes it to be compiled differently by the JIT compiler.  Breakpoints and single stepping exacerbate this.  These effects will often change the way an application behaves.
</li>

These things make bugs that are due to inadequate synchronization particularly difficult to solve.

