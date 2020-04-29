---
metaTitle: "Concurrent Programming (Threads)"
description: "Callable and Future, Basic Multithreading, CountDownLatch, Locks as Synchronisation aids, Synchronization, Creating basic deadlocked system, Runnable Object, Semaphore, Atomic operations, Creating a java.lang.Thread instance, Exclusive write / Concurrent read access, Producer-Consumer, Using ThreadLocal, Visualizing read/write barriers while using synchronized / volatile, Multiple producer/consumer example with shared global queue, Get status of all threads started by your program excluding system threads, Pausing Execution, Add two `int` arrays using a Threadpool, Thread Interruption / Stopping Threads"
---

# Concurrent Programming (Threads)


Concurrent computing is a form of computing in which several computations are executed concurrently instead of sequentially. Java language is designed to support [concurrent programming](https://en.wikipedia.org/wiki/Java_concurrency) through the usage of threads. Objects and resources can be accessed by multiple threads; each thread can potentially access any object in the program and the programmer must ensure read and write access to objects is properly synchronized between threads.



## Callable and Future


While `Runnable` provides a means to wrap code to be executed in a different thread, it has a limitation in that it cannot return a result from the execution. The only way to get some return value from the execution of a `Runnable` is to assign the result to a variable accessible in a scope outside of the `Runnable`.

`Callable` was introduced in Java 5 as a peer to `Runnable`. `Callable` is essentially the same except it has a `call` method instead of `run`. The `call` method has the additional capability to return a result and is also allowed to throw checked exceptions.

**The result from a **Callable task submission** is available to be tapped via a <strong>Future**</strong>

`Future` can be considered a container of sorts that houses the result of the `Callable` computation. Computation of the callable can carry on in another thread, and any attempt to tap the result of a `Future` will block and will only return the result once it is available.

**Callable Interface**

```java
public interface Callable<V> {
    V call() throws Exception;
}

```

**Future**

```java
interface Future<V> {
    V get();
    V get(long timeout, TimeUnit unit);
    boolean cancel(boolean mayInterruptIfRunning);
    boolean isCancelled();
    boolean isDone();
}

```

**Using Callable and Future example:**

```java
public static void main(String[] args) throws Exception {
    ExecutorService es = Executors.newSingleThreadExecutor();
          
    System.out.println("Time At Task Submission : " + new Date());
    Future<String> result = es.submit(new ComplexCalculator());
    // the call to Future.get() blocks until the result is available.So we are in for about a 10 sec wait now
    System.out.println("Result of Complex Calculation is : " + result.get());
    System.out.println("Time At the Point of Printing the Result : " + new Date());
}

```

**Our Callable that does a lengthy computation**

```java
public class ComplexCalculator implements Callable<String> {

    @Override
    public String call() throws Exception {
        // just sleep for 10 secs to simulate a lengthy computation
        Thread.sleep(10000);            
        System.out.println("Result after a lengthy 10sec calculation");
        return "Complex Result"; // the result 
    }
}

```

**Output**

```java
Time At Task Submission : Thu Aug 04 15:05:15 EDT 2016
Result after a lengthy 10sec calculation
Result of Complex Calculation is : Complex Result
Time At the Point of Printing the Result : Thu Aug 04 15:05:25 EDT 2016

```

**Other operations permitted on Future**

While `get()` is the method to extract the actual result Future has provision

- `get(long timeout, TimeUnit unit)` defines maximum time period during current thread will wait for a result;
- To cancel the task call `cancel(mayInterruptIfRunning)`. The flag `mayInterrupt` indicates that task should be interrupted if it was started and is running right now;
- To check if task is completed/finished by calling `isDone()`;
- To check if the lengthy task were cancelled `isCancelled()`.



## Basic Multithreading


If you have many tasks to execute, and all these tasks are not dependent of the result of the precedent ones, you can use **Multithreading** for your computer to do all this tasks at the same time using more processors if your computer can. This can make your program execution **faster** if you have some big independent tasks.

```java
class CountAndPrint implements Runnable {

    private final String name;

    CountAndPrint(String name) {
        this.name = name;
    }

    /** This is what a CountAndPrint will do */
    @Override
    public void run() {
        for (int i = 0; i < 10000; i++) {
            System.out.println(this.name + ": " + i);
        }
    }

    public static void main(String[] args) {
        // Launching 4 parallel threads
        for (int i = 1; i <= 4; i++) {
            // `start` method will call the `run` method 
            // of CountAndPrint in another thread
            new Thread(new CountAndPrint("Instance " + i)).start();
        }

        // Doing some others tasks in the main Thread
        for (int i = 0; i < 10000; i++) {
            System.out.println("Main: " + i);
        }
    }
}

```

The code of the run method of the various `CountAndPrint` instances will execute in non predictable order. A snippet of a sample execution might look like this:

```java
Instance 4: 1
Instance 2: 1
Instance 4: 2
Instance 1: 1
Instance 1: 2
Main: 1
Instance 4: 3
Main: 2
Instance 3: 1
Instance 4: 4
...

```



## CountDownLatch


[CountDownLatch](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/CountDownLatch.html)

> 
A synchronization aid that allows one or more threads to wait until a set of operations being performed in other threads completes.


1. A `CountDownLatch` is initialized with a given count.
1. The await methods block until the current count reaches zero due to invocations of the `countDown()` method, after which all waiting threads are released and any subsequent invocations of await return immediately.
1. This is a one-shot phenomenon—the count cannot be reset. If you need a version that resets the count, consider using a `CyclicBarrier`.

Key Methods:

```java
public void await() throws InterruptedException

```

> 
Causes the current thread to wait until the latch has counted down to zero, unless the thread is interrupted.


```java
public void countDown()

```

> 
Decrements the count of the latch, releasing all waiting threads if the count reaches zero.


Example:

```java
import java.util.concurrent.*;

class DoSomethingInAThread implements Runnable {
    CountDownLatch latch;
    public DoSomethingInAThread(CountDownLatch latch) {
        this.latch = latch;
    } 
    public void run() {
        try {
            System.out.println("Do some thing");
            latch.countDown();
        } catch(Exception err) {
            err.printStackTrace();
        }
    }
}

public class CountDownLatchDemo {
    public static void main(String[] args) {
        try {
            int numberOfThreads = 5;
            if (args.length < 1) {
                System.out.println("Usage: java CountDownLatchDemo numberOfThreads");
                return;
            }
            try {
                numberOfThreads = Integer.parseInt(args[0]);
            } catch(NumberFormatException ne) {
            
            }
            CountDownLatch latch = new CountDownLatch(numberOfThreads);
            for (int n = 0; n < numberOfThreads; n++) {
                Thread t = new Thread(new DoSomethingInAThread(latch));
                t.start();
            }
            latch.await();
            System.out.println("In Main thread after completion of " + numberOfThreads + " threads");
        } catch(Exception err) {
            err.printStackTrace();
        }
    }
}

```

output:

```java
java CountDownLatchDemo 5
Do some thing
Do some thing
Do some thing
Do some thing
Do some thing
In Main thread after completion of 5 threads

```

Explanation:

1. `CountDownLatch` is initialized with a counter of 5 in Main thread
1. Main thread is waiting by using `await()` method.
1. Five instances of `DoSomethingInAThread` have been created. Each instance decremented the counter with `countDown()` method.
1. Once the counter becomes zero, Main thread will resume



## Locks as Synchronisation aids


Prior to Java 5's concurrent package introduction threading was more low level.The introduction of this package provided several higher level concurrent programming aids/constructs.

Locks are thread synchronisation mechanisms that essentially serve the same purpose as synchronized blocks or key words.

**Intrinsic Locking**

```java
int count = 0; // shared among multiple threads

public void doSomething() {
    synchronized(this) {
        ++count; // a non-atomic operation
    }
}

```

**Synchronisation using Locks**

```java
int count = 0; // shared among multiple threads

Lock lockObj = new ReentrantLock();
public void doSomething() {
    try {
        lockObj.lock();
        ++count; // a non-atomic operation
    } finally {    
        lockObj.unlock(); // sure to release the lock without fail
    }
}

```

Locks also have functionality available that intrinsic locking does not offer, such as locking but remaining responsive to interruption, or trying to lock, and not block when unable to.

**Locking, responsive to interruption**

```java
class Locky {
    int count = 0; // shared among multiple threads

    Lock lockObj = new ReentrantLock();

    public void doSomething() {
        try {
            try {
                lockObj.lockInterruptibly();
                ++count; // a non-atomic operation
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt(); // stopping
            }
        } finally {
            if (!Thread.currentThread().isInterrupted()) {
                lockObj.unlock(); // sure to release the lock without fail
            }
        }
    }
}

```

**Only do something when able to lock**

```java
public class Locky2 {
    int count = 0; // shared among multiple threads

    Lock lockObj = new ReentrantLock();

    public void doSomething() {
        boolean locked = lockObj.tryLock(); // returns true upon successful lock
        if (locked) {
            try {
                ++count; // a non-atomic operation
            } finally {
                lockObj.unlock(); // sure to release the lock without fail
            }
        }
    }
}

```

There are several variants of lock available.For more details refer the api docs [here](https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/locks/package-summary.html)



## Synchronization


In Java, there is a built-in language-level locking mechanism: the `synchronized` block, which can use any Java object as an intrinsic lock (i.e. every Java object may have a monitor associated with it).

Intrinsic locks provide atomicity to groups of statements. To understand what that means for us, let's have a look at an example where `synchronized` is useful:

```java
private static int t = 0;
private static Object mutex = new Object();

public static void main(String[] args) {
    ExecutorService executorService = Executors.newFixedThreadPool(400); // The high thread count is for demonstration purposes.
    for (int i = 0; i < 100; i++) {
        executorService.execute(() -> {
            synchronized (mutex) {
                t++;
                System.out.println(MessageFormat.format("t: {0}", t));
            }
        });
    }
    executorService.shutdown();
}

```

In this case, if it weren't for the `synchronized` block, there would have been multiple concurrency issues involved. The first one would be with the post increment operator (it isn't atomic in itself), and the second would be that we would be observing the value of t after an arbitrary amount of other threads has had the chance to modify it. However, since we acquired an intrinsic lock, there will be no race conditions here and the output will contain numbers from 1 to 100 in their normal order.

Intrinsic locks in Java are **mutexes** (i.e. mutual execution locks). Mutual execution means that if one thread has acquired the lock, the second will be forced to wait for the first one to release it before it can acquire the lock for itself. Note: An operation that may put the thread into the wait (sleep) state is called a **blocking operation**. Thus, acquiring a lock is a blocking operation.

Intrinsic locks in Java are **reentrant**. This means that if a thread attempts to acquire a lock it already owns, it will not block and it will successfully acquire it. For instance, the following code will **not** block when called:

```java
public void bar(){
    synchronized(this){
        ...
    }
}
public void foo(){
    synchronized(this){
        bar();
    }
}

```

Beside `synchronized` blocks, there are also `synchronized` methods.

The following blocks of code are practically equivalent (even though the bytecode seems to be different):

<li>
`synchronized` block on `this`:

```java
public void foo() {
    synchronized(this) {
        doStuff();
    }
}

```


</li>
<li>
`synchronized` method:

```java
 public synchronized void foo() {
     doStuff();
 }

```


</li>

Likewise for `static` methods, this:

```java
class MyClass {
    ...
    public static void bar() {
        synchronized(MyClass.class) {
            doSomeOtherStuff();
        }
    }
}

```

has the same effect as this:

```java
class MyClass {
    ...
    public static synchronized void bar() {
        doSomeOtherStuff();
    }
}

```



## Creating basic deadlocked system


A deadlock occurs when two competing actions wait for the other to finish, and thus neither ever does.
In java there is one lock associated with each object.
To avoid concurrent modification done by multiple threads on single object we can use `synchronized`
keyword, but everything comes at a cost.
Using `synchronized` keyword wrongly can lead to stuck systems called as deadlocked system.

Consider there are 2 threads working on 1 instance,
Lets call threads as First and Second, and lets say we have 2 resources R1 and R2.
First acquires R1 and also needs R2 for its completion
while Second acquires R2 and needs R1 for completion.

so say at time t=0,

First has R1 and Second has R2.
now First is waiting for R2 while Second is waiting for R1.
this wait is indefinite and this leads to deadlock.

```java
public class Example2 {
    
    public static void main(String[] args) throws InterruptedException {
        final DeadLock dl = new DeadLock();
        Thread t1 = new Thread(new Runnable() {
    
            @Override
            public void run() {
                // TODO Auto-generated method stub
                dl.methodA();
            }
        });
   
        Thread t2 = new Thread(new Runnable() {
    
            @Override
            public void run() {
                // TODO Auto-generated method stub
                try {
                    dl.method2();
                } catch (InterruptedException e) {
                    // TODO Auto-generated catch block
                    e.printStackTrace();
                }
            }
        });
        t1.setName("First");
        t2.setName("Second");
        t1.start();
        t2.start();
    }
}

class DeadLock {
    
    Object mLock1 = new Object();
    Object mLock2 = new Object();
    

    public void methodA() {
        System.out.println("methodA wait for mLock1  " + Thread.currentThread().getName());
        synchronized (mLock1) {
            System.out.println("methodA mLock1 acquired   " + Thread.currentThread().getName());
            try {
                Thread.sleep(100);
                method2();
            } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }
        }
    }
    public void method2() throws InterruptedException {
        System.out.println("method2 wait for mLock2  " + Thread.currentThread().getName());
        synchronized (mLock2) {
            System.out.println("method2  mLock2 acquired   " + Thread.currentThread().getName());
            Thread.sleep(100);
            method3();
        }
    }
    public void method3() throws InterruptedException {
        System.out.println("method3  mLock1  "+ Thread.currentThread().getName());
        synchronized (mLock1) {
            System.out.println("method3   mLock1 acquired  " + Thread.currentThread().getName());
        }
    }
}

```

Output of this program:

```java
methodA wait for mLock1  First
method2 wait for mLock2  Second
method2  mLock2 acquired   Second
methodA mLock1 acquired   First
method3  mLock1  Second
method2 wait for mLock2  First

```



## Runnable Object


The `Runnable` interface defines a single method, `run()`, meant to contain the code executed in the thread.

The `Runnable` object is passed to the `Thread` constructor. And Thread's `start()` method is called.

**Example**

```java
public class HelloRunnable implements Runnable {

    @Override
    public void run() {
        System.out.println("Hello from a thread");
    }

    public static void main(String[] args) {
        new Thread(new HelloRunnable()).start();
    }
}

```

**Example in Java8:**

```java
public static void main(String[] args) {
    Runnable r = () -> System.out.println("Hello world");
    new Thread(r).start();
}

```

**Runnable vs Thread subclass**

A `Runnable` object employment is more general, because the `Runnable` object can subclass a class other than `Thread`.

`Thread` subclassing is easier to use in simple applications, but is limited by the fact that your task class must be a descendant of `Thread`.

A `Runnable` object is applicable to the high-level thread management APIs.



## Semaphore


A Semaphore is a high-level synchronizer that maintains a set of **permits** that can be acquired and released by threads. A Semaphore can be imagined as a counter of **permits** that will be decremented when a thread acquires, and incremented when a thread releases. If the amount of **permits** is `0` when a thread attempts to acquire, then the thread will block until a permit is made available (or until the thread is interrupted).

A semaphore is initialized as:

`Semaphore semaphore = new Semaphore(1); // The int value being the number of permits`

The Semaphore constructor accepts an additional boolean parameter for fairness. When set false, this class makes no guarantees about the order in which threads acquire permits. When fairness is set true, the semaphore guarantees that threads invoking any of the acquire methods are selected to obtain permits in the order in which their invocation of those methods was processed. It is declared in the following manner:

`Semaphore semaphore = new Semaphore(1, true);`

Now let's look at an example from javadocs, where Semaphore is used to control access to a pool of items. A Semaphore is used in this example to provide blocking functionality in order to ensure that there are always items to be obtained when `getItem()` is called.

```java
class Pool {
    /*
     * Note that this DOES NOT bound the amount that may be released!
     * This is only a starting value for the Semaphore and has no other
     * significant meaning UNLESS you enforce this inside of the
     * getNextAvailableItem() and markAsUnused() methods
     */
    private static final int MAX_AVAILABLE = 100;
    private final Semaphore available = new Semaphore(MAX_AVAILABLE, true);

    /**
     * Obtains the next available item and reduces the permit count by 1. 
     * If there are no items available, block.
     */
    public Object getItem() throws InterruptedException {
        available.acquire();
        return getNextAvailableItem();
    }

    /**
     * Puts the item into the pool and add 1 permit.
     */
    public void putItem(Object x) {
        if (markAsUnused(x))
            available.release();
    }

    private Object getNextAvailableItem() {
        // Implementation
    }

    private boolean markAsUnused(Object o) {
        // Implementation
    }
}

```



## Atomic operations


An atomic operation is an operation that is executed "all at once", without any chance of other threads observing or modifying state during the atomic operation's execution.

Lets consider a **BAD EXAMPLE**.

```java
private static int t = 0;

public static void main(String[] args) {
    ExecutorService executorService = Executors.newFixedThreadPool(400); // The high thread count is for demonstration purposes.
    for (int i = 0; i < 100; i++) {
        executorService.execute(() -> {
            t++;
            System.out.println(MessageFormat.format("t: {0}", t));
        });
    }
    executorService.shutdown();
}

```

In this case, there are two issues. The first issue is that the post increment operator is **not** atomic. It is comprised of multiple operations: get the value, add 1 to the value, set the value. That's why if we run the example, it is likely that we won't see `t: 100` in the output - two threads may concurrently get the value, increment it, and set it: let's say the value of t is 10, and two threads are incrementing t. Both threads will set the value of t to 11, since the second thread observes the value of t before the first thread had finished incrementing it.

The second issue is with how we are observing t. When we are printing the value of t, the value may have already been changed by a different thread after this thread's increment operation.

To fix those issues, we'll use the [`java.util.concurrent.atomic.AtomicInteger`](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/atomic/AtomicInteger.html),
which has many atomic operations for us to use.

```java
private static AtomicInteger t = new AtomicInteger(0);

public static void main(String[] args) {
    ExecutorService executorService = Executors.newFixedThreadPool(400); // The high thread count is for demonstration purposes.
    for (int i = 0; i < 100; i++) {
        executorService.execute(() -> {
            int currentT = t.incrementAndGet();
            System.out.println(MessageFormat.format("t: {0}", currentT));
        });
    }
    executorService.shutdown();
}

```

The `incrementAndGet` method of [`AtomicInteger`](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/atomic/AtomicInteger.html) atomically increments and returns the new value, thus eliminating the previous race condition. Please note that in this example the lines will still be out of order because we make no effort to sequence the `println` calls and that this falls outside the scope of this example, since it would require synchronization and the goal of this example is to show how to use `AtomicInteger` to eliminate race conditions concerning state.



## Creating a java.lang.Thread instance


There are two main approaches to creating a thread in Java. In essence, creating a thread is as easy as writing the code that will be executed in it. The two approaches differ in where you define that code.

In Java, a thread is represented by an object - an instance of [java.lang.Thread](https://docs.oracle.com/javase/8/docs/api/java/lang/Thread.html) or its subclass. So the first approach is to create that subclass and override the **run()** method.

**Note**: I'll use **Thread** to refer to the [java.lang.Thread](https://docs.oracle.com/javase/8/docs/api/java/lang/Thread.html) class and **thread** to refer to the logical concept of threads.

```java
class MyThread extends Thread {
    @Override
    public void run() {
        for (int i = 0; i < 10; i++) {
            System.out.println("Thread running!");
        }
    }
}

```

Now since we've already defined the code to be executed, the thread can be created simply as:

```java
MyThread t = new MyThread();

```

The [Thread](https://docs.oracle.com/javase/8/docs/api/java/lang/Thread.html) class also contains a constructor accepting a string, which will be used as the thread's name. This can be particulary useful when debugging a multi thread program.

```java
class MyThread extends Thread {
    public MyThread(String name) {
        super(name);
    }
    
    @Override
    public void run() {
        for (int i = 0; i < 10; i++) {
            System.out.println("Thread running! ");
        }
    }
}

MyThread t = new MyThread("Greeting Producer");

```

The second approach is to define the code using [java.lang.Runnable](https://docs.oracle.com/javase/8/docs/api/java/lang/Runnable.html) and its only method **run()**. The [Thread](https://docs.oracle.com/javase/8/docs/api/java/lang/Thread.html) class then allows you to execute that method in a separated thread. To achieve this, create the thread using a constructor accepting an instance of the [Runnable](https://docs.oracle.com/javase/8/docs/api/java/lang/Runnable.html) interface.

```java
Thread t = new Thread(aRunnable);

```

This can be very powerful when combined with lambdas or methods references (Java 8 only):

```java
Thread t = new Thread(operator::hardWork);

```

You can specify the thread's name, too.

```java
Thread t = new Thread(operator::hardWork, "Pi operator");

```

Practicaly speaking, you can use both approaches without worries. However the [general wisdom](http://stackoverflow.com/questions/541487/implements-runnable-vs-extends-thread) says to use the latter.

For every of the four mentioned constructors, there is also an alternative accepting an instance of [java.lang.ThreadGroup](https://docs.oracle.com/javase/8/docs/api/java/lang/ThreadGroup.html) as the first parameter.

```java
ThreadGroup tg = new ThreadGroup("Operators");
Thread t = new Thread(tg, operator::hardWork, "PI operator");

```

The [ThreadGroup](https://docs.oracle.com/javase/8/docs/api/java/lang/ThreadGroup.html) represents a set of threads. You can only add a [Thread](https://docs.oracle.com/javase/8/docs/api/java/lang/Thread.html) to a [ThreadGroup](https://docs.oracle.com/javase/8/docs/api/java/lang/ThreadGroup.html) using a [Thread](https://docs.oracle.com/javase/8/docs/api/java/lang/Thread.html)'s constructor. The [ThreadGroup](https://docs.oracle.com/javase/8/docs/api/java/lang/ThreadGroup.html) can then be used to manage all its [Thread](https://docs.oracle.com/javase/8/docs/api/java/lang/Thread.html)s together, as well as the [Thread](https://docs.oracle.com/javase/8/docs/api/java/lang/Thread.html) can gain information from its [ThreadGroup](https://docs.oracle.com/javase/8/docs/api/java/lang/ThreadGroup.html).

So to sumarize, the [Thread](https://docs.oracle.com/javase/8/docs/api/java/lang/Thread.html) can be created with one of these public constructors:

```java
Thread()
Thread(String name)
Thread(Runnable target)
Thread(Runnable target, String name)
Thread(ThreadGroup group, String name)
Thread(ThreadGroup group, Runnable target)
Thread(ThreadGroup group, Runnable target, String name)
Thread(ThreadGroup group, Runnable target, String name, long stackSize)

```

The last one allows us to define desired stack size for the new thread.

Often the code readability suffers when creating and configuring many Threads with same properties or from the same pattern. That's when [java.util.concurrent.ThreadFactory](https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/ThreadFactory.html) can be used. This interface allows you to encapsulate the procedure of creating the thread through the factory pattern and its only method **newThread(Runnable)**.

```java
class WorkerFactory implements ThreadFactory {
    private int id = 0;

    @Override
    public Thread newThread(Runnable r) {
        return new Thread(r, "Worker " + id++);
    }
}

```



## Exclusive write / Concurrent read access


It is sometimes required for a process to concurrently write and read the same "data".

The `ReadWriteLock` interface, and its `ReentrantReadWriteLock` implementation allows for an access pattern that can be described as follow :

1. There can be any number of concurrent readers of the data. If there is at least one reader access granted, then no writer access is possible.
1. There can be at most one single writer to the data. If there is a writer access granted, then no reader can access the data.

An implementation could look like :

```java
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;
public class Sample {

// Our lock. The constructor allows a "fairness" setting, which guarantees the chronology of lock attributions.
protected static final ReadWriteLock RW_LOCK = new ReentrantReadWriteLock();

// This is a typical data that needs to be protected for concurrent access
protected static int data = 0;

/** This will write to the data, in an exclusive access */
public static void writeToData() {
    RW_LOCK.writeLock().lock();
    try {
        data++;
    } finally {
        RW_LOCK.writeLock().unlock();
    }
}

public static int readData() {
    RW_LOCK.readLock().lock();
    try {
        return data;
    } finally {
        RW_LOCK.readLock().unlock();
    }
}

}

```

**NOTE 1** : This precise use case has a cleaner solution using `AtomicInteger`, but what is described here is an access pattern, that works regardless of the fact that data here is an integer that as an Atomic variant.

**NOTE 2** : The lock on the reading part is really needed, although it might not look so to the casual reader. Indeed, if you do not lock on the reader side, any number of things can go wrong, amongst which :

1. The writes of primitive values are not guaranteed to be atomic on all JVMs, so the reader could see e.g. only 32bits of a 64bits write if `data` were a 64bits long type
1. The visibility of the write from a thread that did not perform it is guaranteed by the JVM only if we establish **Happen Before relationship** between the writes and the reads. This relationship is established when both readers and writers use their respective locks, but not otherwise

In case higher performance is required, an under certain types of usage, there is a faster lock type available, called the `StampedLock`, that amongst other things implements an optimistic lock mode. This lock works very differently from the `ReadWriteLock`, and this sample is not transposable.



## Producer-Consumer


A simple example of producer-consumer problem solution. Notice that JDK classes (`AtomicBoolean` and `BlockingQueue`) are used for synchronization, which reduces the chance of creating an invalid solution. Consult Javadoc for various types of [BlockingQueue](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/BlockingQueue.html); choosing different implementation may drastically change the behavior of this example (like [DelayQueue](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/DelayQueue.html) or [Priority Queue](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/PriorityBlockingQueue.html)).

```java
public class Producer implements Runnable {

    private final BlockingQueue<ProducedData> queue;

    public Producer(BlockingQueue<ProducedData> queue) {
        this.queue = queue;
    }

    public void run() {
        int producedCount = 0;
        try {
            while (true) {
                producedCount++;
                //put throws an InterruptedException when the thread is interrupted
                queue.put(new ProducedData());
            }
        } catch (InterruptedException e) {
            // the thread has been interrupted: cleanup and exit
            producedCount--;
            //re-interrupt the thread in case the interrupt flag is needeed higher up
            Thread.currentThread().interrupt();
        }
        System.out.println("Produced " + producedCount + " objects");
    }
}

public class Consumer implements Runnable {

    private final BlockingQueue<ProducedData> queue;

    public Consumer(BlockingQueue<ProducedData> queue) {
        this.queue = queue;
    }

    public void run() {
        int consumedCount = 0;
        try {
            while (true) {
                //put throws an InterruptedException when the thread is interrupted
                ProducedData data = queue.poll(10, TimeUnit.MILLISECONDS);
                // process data
                consumedCount++;
            }
        } catch (InterruptedException e) {
            // the thread has been interrupted: cleanup and exit
            consumedCount--;
            //re-interrupt the thread in case the interrupt flag is needeed higher up
            Thread.currentThread().interrupt();
        }
        System.out.println("Consumed " + consumedCount + " objects");
    }
}


public class ProducerConsumerExample {
    static class ProducedData {    
        // empty data object
    }

    public static void main(String[] args) throws InterruptedException {
        BlockingQueue<ProducedData> queue = new ArrayBlockingQueue<ProducedData>(1000);
        // choice of queue determines the actual behavior: see various BlockingQueue implementations

        Thread producer = new Thread(new Producer(queue));
        Thread consumer = new Thread(new Consumer(queue));

        producer.start();
        consumer.start();

        Thread.sleep(1000);
        producer.interrupt();
        Thread.sleep(10);
        consumer.interrupt();
    }
}

```



## Using ThreadLocal


A useful tool in Java Concurrency is `ThreadLocal` – this allows you to have a variable that will be unique to a given thread. Thus, if the same code runs in different threads, these executions will not share the value, but instead each thread has its own variable that is **local to the thread**.

For example, this is frequently used to establish the context (such as authorization information) of handling a request in a servlet. You might do something like this:

```java
private static final ThreadLocal<MyUserContext> contexts = new ThreadLocal<>();

public static MyUserContext getContext() {
    return contexts.get(); // get returns the variable unique to this thread
}

public void doGet(...) {
    MyUserContext context = magicGetContextFromRequest(request); 
    contexts.put(context); // save that context to our thread-local - other threads
                           // making this call don't overwrite ours
    try {
        // business logic
    } finally {
        contexts.remove(); // 'ensure' removal of thread-local variable
    }
}

```

Now, instead of passing `MyUserContext` into every single method, you can instead use `MyServlet.getContext()` where you need it. Now of course, this does introduce a variable that needs to be documented, but it’s thread-safe, which eliminates a lot of the downsides to using such a highly scoped variable.

The key advantage here is that every thread has its own thread local variable in that `contexts` container. As long as you use it from a defined entry point (like demanding that each servlet maintains its context, or perhaps by adding a servlet filter) you can rely on this context being there when you need it.



## Visualizing read/write barriers while using synchronized / volatile


As we know that we should use `synchronized` keyword to make execution of a method or block exclusive. But few of us may not be aware of one more important aspect of using `synchronized` and `volatile` keyword: **apart from making a unit of code atomic, it also provides read / write barrier**. What is this read / write barrier? Let's discuss this using an example:

```java
class Counter {

  private Integer count = 10;

  public synchronized void incrementCount() {
    count++;
  }

  public Integer getCount() {
    return count;
  }
}

```

Let's suppose a thread **A** calls `incrementCount()` first then another thread **B** calls `getCount()`. In this scenario there is no guarantee that B will see updated value of `count`. It may still see `count` as `10`, even it is also possible that it never sees updated value of `count` ever.

To understand this behavior we need to understand how Java memory model integrates with hardware architecture. In Java, each thread has it's own thread stack. This stack contains: method call stack and local variable created in that thread. In a multi core system, it is quite possible that two threads are running concurrently in separate cores. In such scenario it is possible that part of a thread's stack lies inside register / cache of a core. If inside a thread, an object is accessed using `synchronized` (or `volatile`) keyword, after `synchronized` block that thread syncs it's local copy of that variable with the main memory. This creates a read / write barrier and makes sure that the thread is seeing the latest value of that object.

But in our case, since thread B has not used synchronized access to `count`, it might be refering value of `count` stored in register and may never see updates from thread A. To make sure that B sees latest value of count we need to make `getCount()` synchronized as well.

```java
public synchronized Integer getCount() {
  return count;
}

```

Now when thread A is done with updating `count` it unlocks `Counter` instance, at the same time creates write barrier and flushes all changes done inside that block to the main memory. Similarly when thread B acquires lock on the same instance of `Counter`, it enters into read barrier and reads value of `count` from main memory and sees all updates.

[<img src="https://i.stack.imgur.com/tnFLB.png" alt="visibility" />](https://i.stack.imgur.com/tnFLB.png)

Same visibility effect goes for `volatile` read / writes as well. All variables updated prior to write to `volatile` will be flushed to main memory and all reads after `volatile` variable read will be from main memory.



## Multiple producer/consumer example with shared global queue


Below code showcases multiple Producer/Consumer program. Both Producer and Consumer threads share same global queue.

```java
import java.util.concurrent.*;
import java.util.Random;

public class ProducerConsumerWithES {
    public static void main(String args[]) {
        BlockingQueue<Integer> sharedQueue = new LinkedBlockingQueue<Integer>();
         
        ExecutorService pes = Executors.newFixedThreadPool(2);
        ExecutorService ces = Executors.newFixedThreadPool(2);
          
        pes.submit(new Producer(sharedQueue, 1));
        pes.submit(new Producer(sharedQueue, 2));
        ces.submit(new Consumer(sharedQueue, 1));
        ces.submit(new Consumer(sharedQueue, 2));
         
        pes.shutdown();
        ces.shutdown();
    }
}

/* Different producers produces a stream of integers continuously to a shared queue, 
which is shared between all Producers and consumers */

class Producer implements Runnable {
    private final BlockingQueue<Integer> sharedQueue;
    private int threadNo;
    private Random random = new Random();
    public Producer(BlockingQueue<Integer> sharedQueue,int threadNo) {
        this.threadNo = threadNo;
        this.sharedQueue = sharedQueue;
    }
    @Override
    public void run() {
        // Producer produces a continuous stream of numbers for every 200 milli seconds
        while (true) {
            try {
                int number = random.nextInt(1000);
                System.out.println("Produced:" + number + ":by thread:"+ threadNo);
                sharedQueue.put(number);
                Thread.sleep(200);
            } catch (Exception err) {
                err.printStackTrace();
            }
        }
    }
}
/* Different consumers consume data from shared queue, which is shared by both producer and consumer threads */
class Consumer implements Runnable {
    private final BlockingQueue<Integer> sharedQueue;
    private int threadNo;
    public Consumer (BlockingQueue<Integer> sharedQueue,int threadNo) {
        this.sharedQueue = sharedQueue;
        this.threadNo = threadNo;
    }
    @Override
    public void run() {
        // Consumer consumes numbers generated from Producer threads continuously
        while(true){
            try {
                int num = sharedQueue.take();
                System.out.println("Consumed: "+ num + ":by thread:"+threadNo);
            } catch (Exception err) {
               err.printStackTrace();
            }
        }
    }   
}

```

output:

```java
Produced:69:by thread:2
Produced:553:by thread:1
Consumed: 69:by thread:1
Consumed: 553:by thread:2
Produced:41:by thread:2
Produced:796:by thread:1
Consumed: 41:by thread:1
Consumed: 796:by thread:2
Produced:728:by thread:2
Consumed: 728:by thread:1

```

and so on ................

Explanation:

1. `sharedQueue`, which is a `LinkedBlockingQueue` is shared among all Producer and Consumer threads.
1. Producer threads produces one integer for every 200 milli seconds continuously and append it to `sharedQueue`
1. `Consumer` thread consumes integer from `sharedQueue` continuously.
<li>This program is implemented with-out explicit `synchronized` or `Lock` constructs.
[BlockingQueue](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/BlockingQueue.html) is the key to achieve it.</li>

> 
BlockingQueue implementations are designed to be used primarily for producer-consumer queues.


> 
BlockingQueue implementations are thread-safe. All queuing methods achieve their effects atomically using internal locks or other forms of concurrency control.




## Get status of all threads started by your program excluding system threads


Code snippet:

```java
import java.util.Set;

public class ThreadStatus {
    public static void main(String args[]) throws Exception {
        for (int i = 0; i < 5; i++){
            Thread t = new Thread(new MyThread());
            t.setName("MyThread:" + i);
            t.start();
        }
        int threadCount = 0;
        Set<Thread> threadSet = Thread.getAllStackTraces().keySet();
        for (Thread t : threadSet) {
            if (t.getThreadGroup() == Thread.currentThread().getThreadGroup()) {
                System.out.println("Thread :" + t + ":" + "state:" + t.getState());
                ++threadCount;
            }
        }
        System.out.println("Thread count started by Main thread:" + threadCount);
    }
}

class MyThread implements Runnable {
    public void run() {
        try {
            Thread.sleep(2000);
        } catch(Exception err) {
            err.printStackTrace();
        }
    }
}

```

Output:

```java
Thread :Thread[MyThread:1,5,main]:state:TIMED_WAITING
Thread :Thread[MyThread:3,5,main]:state:TIMED_WAITING
Thread :Thread[main,5,main]:state:RUNNABLE
Thread :Thread[MyThread:4,5,main]:state:TIMED_WAITING
Thread :Thread[MyThread:0,5,main]:state:TIMED_WAITING
Thread :Thread[MyThread:2,5,main]:state:TIMED_WAITING
Thread count started by Main thread:6

```

Explanation:

`Thread.getAllStackTraces().keySet()` returns all `Thread`s including application threads and system threads. If you are interested only in status of Threads, started by your application, iterate the `Thread` set by checking Thread Group of a particular thread against your main program thread.

In absence of above ThreadGroup condition, the program returns status of below System Threads:

```java
Reference Handler
Signal Dispatcher
Attach Listener
Finalizer

```



## Pausing Execution


`Thread.sleep` causes the current thread to suspend execution for a specified period. This is an efficient means of making processor time available to the other threads of an application or other applications that might be running on a computer system. There are two overloaded `sleep` methods in the Thread class.<br><br>
One that specifies the sleep time to the millisecond

```java
public static void sleep(long millis) throws InterruptedException

```

One that specifies the sleep time to the nanosecond

```java
public static void sleep(long millis, int nanos)

```

Pausing Execution for 1 second

```java
Thread.sleep(1000);

```

It is important to note that this is a hint to the operating system's kernel's scheduler. This may not necessarily be precise, and some implementations do not even consider the nanosecond parameter (possibly rounding to the nearest millisecond).

It is recommended to enclose a call to `Thread.sleep` in try/catch and catch `InterruptedException`.



## Add two `int` arrays using a Threadpool


A Threadpool has a Queue of tasks, of which each will be executed on one these Threads.

The following example shows how to add two `int` arrays using a Threadpool.

```java
int[] firstArray = { 2, 4, 6, 8 };
int[] secondArray = { 1, 3, 5, 7 };
int[] result = { 0, 0, 0, 0 };

ExecutorService pool = Executors.newCachedThreadPool(); 

// Setup the ThreadPool:
// for each element in the array, submit a worker to the pool that adds elements
for (int i = 0; i < result.length; i++) {
    final int worker = i;
    pool.submit(() -> result[worker] = firstArray[worker] + secondArray[worker] );
}

// Wait for all Workers to finish:
try {
    // execute all submitted tasks
    pool.shutdown();
    // waits until all workers finish, or the timeout ends
    pool.awaitTermination(12, TimeUnit.SECONDS);
}
catch (InterruptedException e) {
    pool.shutdownNow(); //kill thread
}

System.out.println(Arrays.toString(result));

```

Notes:

<li>
This example is purely illustrative.  In practice, there won't be any speedup by using threads for a task this small.  A slowdown is likely, since the overheads of task creation and scheduling will swamp the time taken to run a task.
</li>
<li>
If you were using Java 7 and earlier, you would use anonymous classes instead of lambdas to implement the tasks.
</li>



## Thread Interruption / Stopping Threads


Each Java Thread has an interrupt flag, which is initially false. Interrupting a thread, is essentially nothing more than setting that flag to true. The code running on that thread can check the flag on occasion and act upon it. The code can also ignore it completely.
But why would each Thread have such a flag? After all, having a boolean flag on a thread is something we can just organize ourselves, if and when we need it. Well, there are methods that behave in a special way when the thread they're running on is interrupted. These methods are called blocking methods. These are methods that put the thread in the WAITING or TIMED_WAITING state. When a thread is in this state, interrupting it, will cause an InterruptedException to be thrown on the interrupted thread, rather than the interrupt flag being set to true, and the thread becomes RUNNABLE again. Code that invokes a blocking method is forced to deal with the InterruptedException, since it is a checked exception. So, and hence its name, an interrupt can have the effect of interrupting a WAIT, effectively ending it. Note that not all methods that are somehow waiting (e.g. blocking IO) respond to interruption in that way, as they don't put the thread in a waiting state.
Lastly a thread that has its interrupt flag set, that enters a blocking method (i.e. tries to get into a waiting state), will immediately throw an InterruptedException and the interrupt flag will be cleared.

Other than these mechanics, Java does not assign any special semantic meaning to interruption. Code is free to interpret an interrupt any way it likes. But most often interruption is used to signal to a thread it should stop running at its earliest convenience. But, as should be clear from the above, it is up to the code on that thread to react to that interruption appropriately in order to stop running. Stopping a thread is a collaboration.
When a thread is interrupted its running code can be several levels deep into the stacktrace. Most code doesn't call a blocking method, and finishes timely enough to not delay the stopping of the thread unduly. The code that should mostly be concerned with being responsive to interruption, is code that is in a loop handling tasks until there are none left, or until a flag is set signalling it to stop that loop. Loops that handle possibly infinite tasks (i.e. they keep running in principle) should check the interrupt flag in order to exit the loop. For finite loops the semantics may dictate that all tasks must be finished before ending, or it may be appropriate to leave some tasks unhandled.
Code that calls blocking methods will be forced to deal with the InterruptedException. If at all semantically possible, it can simply propagate the InterruptedException and declare to throw it. As such it becomes a blocking method itself in regard to its callers. If it cannot propagate the exception, it should at the very least set the interrupted flag, so callers higher up the stack also know the thread was interrupted. In some cases the method needs to continue waiting regardless of the InterruptedException, in which case it must delay setting the interrupted flag until after it is done waiting, this may involve setting a local variable, which is to be checked prior to exiting the method to then interrupt its thread.

**Examples :**

**Example of code that stops handling tasks upon interruption**

```java
class TaskHandler implements Runnable {
    
    private final BlockingQueue<Task> queue;

    TaskHandler(BlockingQueue<Task> queue) {
        this.queue = queue;
    }

    @Override
    public void run() {
        while (!Thread.currentThread().isInterrupted()) { // check for interrupt flag, exit loop when interrupted
            try {
                Task task = queue.take(); // blocking call, responsive to interruption
                handle(task);
            } catch (InterruptedException e) {
                Thread.currentThread().interrupt(); // cannot throw InterruptedException (due to Runnable interface restriction) so indicating interruption by setting the flag
            }
        }
    }
    
    private void handle(Task task) {
        // actual handling
    }
}

```

**Example of code that delays setting the interrupt flag until completely done :**

```java
class MustFinishHandler implements Runnable {

    private final BlockingQueue<Task> queue;

    MustFinishHandler(BlockingQueue<Task> queue) {
        this.queue = queue;
    }

    @Override
    public void run() {
        boolean shouldInterrupt = false;
        
        while (true) {
            try {
                Task task = queue.take();
                if (task.isEndOfTasks()) {
                    if (shouldInterrupt) {
                        Thread.currentThread().interrupt();
                    }
                    return;
                }
                handle(task);
            } catch (InterruptedException e) {
                shouldInterrupt = true; // must finish, remember to set interrupt flag when we're done
            }
        }
    }

    private void handle(Task task) {
        // actual handling
    }
}

```

**Example of code that has a fixed list of tasks but may quit early when interrupted**

```java
class GetAsFarAsPossible implements Runnable {

    private final List<Task> tasks = new ArrayList<>();

    @Override
    public void run() {
        for (Task task : tasks) {
            if (Thread.currentThread().isInterrupted()) {
                return;
            }
            handle(task);
        }
    }

    private void handle(Task task) {
        // actual handling
    }
}

```



#### Remarks


Related topic(s) on StackOverflow:

- [Atomic Types](http://stackoverflow.com/documentation/java/5963/atomic-types)
- [Executor, ExecutorService and Thread pools](http://stackoverflow.com/documentation/java/143/executors)
- [Extending `Thread` versus implementing `Runnable`](http://stackoverflow.com/documentation/java/5567/java-pitfalls-threads-and-concurrency/19768/pitfall-extending-java-lang-thread)

