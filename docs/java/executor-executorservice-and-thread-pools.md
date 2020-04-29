---
metaTitle: "Executor, ExecutorService and Thread pools"
description: "ThreadPoolExecutor, Retrieving value from computation - Callable, submit() vs execute() exception handling differences, Fire and Forget - Runnable Tasks, Handle Rejected Execution, Use cases for different types of concurrency constructs, Wait for completion of all tasks in ExecutorService, Use cases for different types of ExecutorService, Scheduling tasks to run at a fixed time, after a delay or repeatedly, Using Thread Pools"
---

# Executor, ExecutorService and Thread pools


The [Executor](https://docs.oracle.com/javase/7/docs/api/java/util/concurrent/Executor.html) interface in Java provides a way of decoupling task submission from the mechanics of how each task will be run, including details of thread use, scheduling, etc. An Executor is normally used instead of explicitly creating threads. With Executors, developers won't have to significantly rewrite their code to be able to easily tune their program's task-execution policy.



## ThreadPoolExecutor


A common Executor used is the `ThreadPoolExecutor`, which takes care of Thread handling. You can configure the minimal amount of Threads the executor always has to maintain when there's not much to do (it's called core size) and a maximal Thread size to which the Pool can grow, if there is more work to do. Once the workload declines, the Pool slowly reduces the Thread count again until it reaches min size.

```java
ThreadPoolExecutor pool = new ThreadPoolExecutor(
    1,                                     // keep at least one thread ready, 
                                           // even if no Runnables are executed
    5,                                     // at most five Runnables/Threads
                                           // executed in parallel
    1, TimeUnit.MINUTES,                   // idle Threads terminated after one
                                           // minute, when min Pool size exceeded
    new ArrayBlockingQueue<Runnable>(10)); // outstanding Runnables are kept here

pool.execute(new Runnable() {
    @Override public void run() {
        //code to run
    }
});

```

**Note** If you configure the `ThreadPoolExecutor` with an **unbounded** queue, then the thread count will not exceed `corePoolSize` since new threads are only created if the queue is full:

ThreadPoolExecutor with all parameters:

```java
ThreadPoolExecutor(int corePoolSize, int maximumPoolSize, long keepAliveTime, 
TimeUnit unit, BlockingQueue<Runnable> workQueue, ThreadFactory threadFactory,
RejectedExecutionHandler handler)

```

from [JavaDoc](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadPoolExecutor.html)

> 
If there are more than corePoolSize but less than maximumPoolSize threads running, a new thread will be created only if the queue is full.


Advantages:

<li>
BlockingQueue size can be controlled and out-of-memory scenarios can be avoided. Application performance won't be degraded with limited bounded queue size.
</li>
<li>
You can use existing or create new Rejection Handler policies.
<ol>
<li>
In the default ThreadPoolExecutor.AbortPolicy, the handler throws a runtime RejectedExecutionException upon rejection.
</li>
<li>
In `ThreadPoolExecutor.CallerRunsPolicy`, the thread that invokes execute itself runs the task. This provides a simple feedback control mechanism that will slow down the rate that new tasks are submitted.
</li>
<li>
In `ThreadPoolExecutor.DiscardPolicy`, a task that cannot be executed is simply dropped.
</li>
<li>
In `ThreadPoolExecutor.DiscardOldestPolicy`, if the executor is not shut down, the task at the head of the work queue is dropped, and then execution is retried (which can fail again, causing this to be repeated.)
</li>

Custom `ThreadFactory` can be configured, which is useful :

1. To set a more descriptive thread name
1. To set thread daemon status
1. To set thread priority

[Here](https://github.com/manjunathshetty/java-samples/tree/master/java-threadpool-executor-sample) is a example of how to use ThreadPoolExecutor



## Retrieving value from computation - Callable


If your computation produces some return value which later is required, a simple Runnable task isn't sufficient. For such cases you can use `ExecutorService.submit(`[`Callable`](http://stackoverflow.com/documentation/java/121/concurrent-programming-threads/18630/callable-and-future#t=201609161547021506502)`<T>)` which returns a value after execution completes.

The Service will return a [`Future`](http://stackoverflow.com/documentation/java/121/concurrent-programming-threads/18630/callable-and-future#t=201609161547021506502) which you can use to retrieve the result of the task execution.

```java
// Submit a callable for execution
ExecutorService pool = anExecutorService;
Future<Integer> future = pool.submit(new Callable<Integer>() {
    @Override public Integer call() {
        //do some computation
        return new Random().nextInt();
    }
});    
// ... perform other tasks while future is executed in a different thread

```

When you need to get the result of the future, call `future.get()`

<li>
Wait indefinitely for future to finish with a result.

```java
  try {
      // Blocks current thread until future is completed
      Integer result = future.get(); 
  catch (InterruptedException || ExecutionException e) {
      // handle appropriately
  }

```


</li>
<li>
Wait for future to finish, but no longer than specified time.

```java
  try {
      // Blocks current thread for a maximum of 500 milliseconds.
      // If the future finishes before that, result is returned,
      // otherwise TimeoutException is thrown.
      Integer result = future.get(500, TimeUnit.MILLISECONDS); 
  catch (InterruptedException || ExecutionException || TimeoutException e) {
      // handle appropriately
  }

```


</li>

If the result of a scheduled or running task is no longer required, you can call `Future.cancel(boolean)` to cancel it.

- Calling `cancel(false)` will just remove the task from the queue of tasks to be run.
- Calling `cancel(true)` will **also** interrupt the task if it is currently running.



## submit() vs execute() exception handling differences


Generally execute() command is used for fire and forget calls (without need of analyzing the result) and submit() command is used for analyzing the result of Future object.

We should be aware of key difference of Exception Handling mechanisms between these two commands.

Exceptions from submit() are swallowed by framework if you did not catch them.

Code example to understand the difference:

**Case 1: submit the Runnable with execute() command, which reports the Exception.**

```java
import java.util.concurrent.*;
import java.util.*;

public class ExecuteSubmitDemo {
    public ExecuteSubmitDemo() {
        System.out.println("creating service");
        ExecutorService service = Executors.newFixedThreadPool(2);
        //ExtendedExecutor service = new ExtendedExecutor();
        for (int i = 0; i < 2; i++){
            service.execute(new Runnable(){
                 public void run(){
                    int a = 4, b = 0;
                    System.out.println("a and b=" + a + ":" + b);
                    System.out.println("a/b:" + (a / b));
                    System.out.println("Thread Name in Runnable after divide by zero:"+Thread.currentThread().getName());
                 }
            });
        }
        service.shutdown();
    }
    public static void main(String args[]){
        ExecuteSubmitDemo demo = new ExecuteSubmitDemo();
    }
}

class ExtendedExecutor extends ThreadPoolExecutor {

   public ExtendedExecutor() { 
       super(1, 1, 60, TimeUnit.SECONDS, new ArrayBlockingQueue<Runnable>(100));
   }
   // ...
   protected void afterExecute(Runnable r, Throwable t) {
     super.afterExecute(r, t);
     if (t == null && r instanceof Future<?>) {
       try {
         Object result = ((Future<?>) r).get();
       } catch (CancellationException ce) {
           t = ce;
       } catch (ExecutionException ee) {
           t = ee.getCause();
       } catch (InterruptedException ie) {
           Thread.currentThread().interrupt(); // ignore/reset
       }
     }
     if (t != null)
       System.out.println(t);
   }
 }

```

output:

```java
creating service
a and b=4:0
a and b=4:0
Exception in thread "pool-1-thread-1" Exception in thread "pool-1-thread-2" java.lang.ArithmeticException: / by zero
        at ExecuteSubmitDemo$1.run(ExecuteSubmitDemo.java:15)
        at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1145)
        at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:615)
        at java.lang.Thread.run(Thread.java:744)
java.lang.ArithmeticException: / by zero
        at ExecuteSubmitDemo$1.run(ExecuteSubmitDemo.java:15)
        at java.util.concurrent.ThreadPoolExecutor.runWorker(ThreadPoolExecutor.java:1145)
        at java.util.concurrent.ThreadPoolExecutor$Worker.run(ThreadPoolExecutor.java:615)
        at java.lang.Thread.run(Thread.java:744)

```

**Case 2: Replace execute() with submit() :**  `service.submit(new Runnable(){`
In this case, Exceptions are swallowed by framework since run() method did not catch them explicitly.

output:

```java
creating service
a and b=4:0
a and b=4:0

```

**Case 3: Change the newFixedThreadPool to ExtendedExecutor**

```java
//ExecutorService service = Executors.newFixedThreadPool(2);
 ExtendedExecutor service = new ExtendedExecutor(); 

```

output:

```java
creating service
a and b=4:0
java.lang.ArithmeticException: / by zero
a and b=4:0
java.lang.ArithmeticException: / by zero

```

I have demonstrated this example to cover two topics : Use your custom ThreadPoolExecutor and handle Exectpion with custom ThreadPoolExecutor.

**Other simple solution to above problem :**  When you are using normal ExecutorService & submit command, get the Future object from submit() command call get() API on Future. Catch the three exceptions, which have been quoted in afterExecute method implementation. Advantage of custom ThreadPoolExecutor over this approach : You have to handle Exception handling mechanism in only one place - Custom ThreadPoolExecutor.



## Fire and Forget - Runnable Tasks


Executors accept a `java.lang.Runnable` which contains (potentially computationally or otherwise long-running or heavy) code to be run in another Thread.

Usage would be:

```java
Executor exec = anExecutor;
exec.execute(new Runnable() {
    @Override public void run() {
        //offloaded work, no need to get result back
    }
});

```

Note that with this executor, you have no means to get any computed value back.<br />
With Java 8, one can utilize lambdas to shorten the code example.

```java
Executor exec = anExecutor;
exec.execute(() -> {
    //offloaded work, no need to get result back
});

```



## Handle Rejected Execution


If

1. you try to submit tasks to a shutdown Executor or
1. the queue is saturated (only possible with bounded ones) and maximum number of Threads has been reached,

`RejectedExecutionHandler.rejectedExecution(Runnable, ThreadPoolExecutor)` will be called.

The default behavior is that you'll get a RejectedExecutionException thrown at the caller.
But there are more predefined behaviors available:

- **ThreadPoolExecutor.AbortPolicy** (default, will throw REE)
- **ThreadPoolExecutor.CallerRunsPolicy** (executes task on caller's thread - **blocking it**)
- **ThreadPoolExecutor.DiscardPolicy** (silently discard task)
- **ThreadPoolExecutor.DiscardOldestPolicy** (silently discard **oldest** task in queue and retry execution of the new task)

You can set them using one of the ThreadPool [constructors](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadPoolExecutor.html#ThreadPoolExecutor-int-int-long-java.util.concurrent.TimeUnit-java.util.concurrent.BlockingQueue-java.util.concurrent.RejectedExecutionHandler-):

```java
public ThreadPoolExecutor(int corePoolSize,
                      int maximumPoolSize,
                      long keepAliveTime,
                      TimeUnit unit,
                      BlockingQueue<Runnable> workQueue,
                      RejectedExecutionHandler handler) // <--

public ThreadPoolExecutor(int corePoolSize,
                      int maximumPoolSize,
                      long keepAliveTime,
                      TimeUnit unit,
                      BlockingQueue<Runnable> workQueue,
                      ThreadFactory threadFactory,
                      RejectedExecutionHandler handler) // <--

```

You can as well implement your own behavior by extending [RejectedExecutionHandler](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/RejectedExecutionHandler.html#rejectedExecution-java.lang.Runnable-java.util.concurrent.ThreadPoolExecutor-) interface:

```java
void rejectedExecution(Runnable r, ThreadPoolExecutor executor)

```



## Use cases for different types of concurrency constructs


<li>
[ExecutorService](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ExecutorService.html)
`ExecutorService executor = Executors.newFixedThreadPool(50);`
It is simple and easy to use. It hides low level details of `ThreadPoolExecutor`.
I prefer this one when number of `Callable/Runnable` tasks are small in number and piling of tasks in unbounded queue does not increase  memory & degrade the performance of the system. If you have `CPU/Memory` constraints, I prefer to use `ThreadPoolExecutor` with capacity constraints & `RejectedExecutionHandler` to handle rejection of tasks.
</li>
<li>
[CountDownLatch](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/CountDownLatch.html)
`CountDownLatch` will be initialized with a given count. This count is decremented by calls to the `countDown()` method.  Threads waiting for this count to reach zero can call one of the `await()` methods. Calling `await()` blocks the thread until the count reaches zero. **This class enables a java thread to wait until other set of threads completes their tasks.**
**Use cases:**
<ol>
<li>
Achieving Maximum Parallelism: Sometimes we want to start a number of threads at the same time to achieve maximum parallelism
</li>
<li>
Wait N threads to completes before start execution
</li>
<li>
Deadlock detection.
</li>

<li>
[ThreadPoolExecutor](http://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadPoolExecutor.html) : It provides more control. If application is constrained by number of pending Runnable/Callable tasks, you can use bounded queue by setting the max capacity. Once the queue reaches maximum capacity, you can define RejectionHandler. Java provides four types of `RejectedExecutionHandler` [policies](http://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadPoolExecutor.html).
<ol>
<li>
`ThreadPoolExecutor.AbortPolicy`, the handler throws a runtime RejectedExecutionException upon rejection.
</li>
<li>
ThreadPoolExecutor.CallerRunsPolicy`, the thread that invokes execute itself runs the task. This provides a simple feedback control mechanism that will slow down the rate that new tasks are submitted.
</li>
<li>
In `ThreadPoolExecutor.DiscardPolicy`, a task that cannot be executed is simply dropped.
</li>
<li>
`ThreadPoolExecutor.DiscardOldestPolicy`, if the executor is not shut down, the task at the head of the work queue is dropped, and then execution is retried (which can fail again, causing this to be repeated.)
</li>

If you want to simulate `CountDownLatch` behaviour, you can use `invokeAll()` method.

<li>
One more mechanism you did not quote is [ForkJoinPool](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ForkJoinPool.html)
<p>The `ForkJoinPool` was added to Java in Java 7. The `ForkJoinPool` is similar to
the Java `ExecutorService` but with one difference. The `ForkJoinPool` makes it
easy for tasks to split their work up into smaller tasks which are then
submitted to the `ForkJoinPool` too. Task stealing happens in `ForkJoinPool`   when free worker threads steal tasks from busy worker thread queue.</p>
Java 8 has introduced one more API in [ExecutorService](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ExecutorService.html) to create work stealing pool. You don't have to create `RecursiveTask` and `RecursiveAction` but still can use `ForkJoinPool`.

```java
public static ExecutorService newWorkStealingPool()

```


<blockquote>
Creates a work-stealing thread pool using all available processors as its target parallelism level.
</blockquote>
By default, it will take number of CPU cores as parameter.
</li>

All these four mechanism are complimentary to each other. Depending on level of granularity you want to control, you have to chose right ones.



## Wait for completion of all tasks in ExecutorService


Let's have a look at various options to wait for completion of tasks submitted to [Executor](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/Executor.html)

<li>[ExecutorService](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ExecutorService.html) `invokeAll()`
<blockquote>
Executes the given tasks, returning a list of Futures holding their status and results when everything is completed.
</blockquote>
</li>

Example:

```java
import java.util.concurrent.*;
import java.util.*;

public class InvokeAllDemo{
    public InvokeAllDemo(){
        System.out.println("creating service");
        ExecutorService service = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());
        
        List<MyCallable> futureList = new ArrayList<MyCallable>();
        for (int i = 0; i < 10; i++){
            MyCallable myCallable = new MyCallable((long)i);
            futureList.add(myCallable);
        }
        System.out.println("Start");
        try{
            List<Future<Long>> futures = service.invokeAll(futureList);  
        } catch(Exception err){
            err.printStackTrace();
        }
        System.out.println("Completed");
        service.shutdown();
    }
    public static void main(String args[]){
        InvokeAllDemo demo = new InvokeAllDemo();
    }
    class MyCallable implements Callable<Long>{
        Long id = 0L;
        public MyCallable(Long val){
            this.id = val;
        }
        public Long call(){
            // Add your business logic
            return id;
        }
    }
}

```


<li>
[CountDownLatch](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/CountDownLatch.html)
<blockquote>
A synchronization aid that allows one or more threads to wait until a set of operations being performed in other threads completes.
</blockquote>
<blockquote>
A **CountDownLatch** is initialized with a given count. The await methods block until the current count reaches zero due to invocations of the `countDown()` method, after which all waiting threads are released and any subsequent invocations of await return immediately. This is a one-shot phenomenon -- the count cannot be reset. If you need a version that resets the count, consider using a **CyclicBarrier**.
</blockquote>
</li>
<li>
[ForkJoinPool](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ForkJoinPool.html) or `newWorkStealingPool()` in [Executors](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/Executors.html)
</li>
<li>
Iterate through all `Future` objects created after submitting to `ExecutorService`
</li>
<li>
Recommended way of shutdown from oracle documentation page of [ExecutorService](http://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ExecutorService.html):

```java
void shutdownAndAwaitTermination(ExecutorService pool) {
    pool.shutdown(); // Disable new tasks from being submitted
    try {
      // Wait a while for existing tasks to terminate
      if (!pool.awaitTermination(60, TimeUnit.SECONDS)) {
        pool.shutdownNow(); // Cancel currently executing tasks
        // Wait a while for tasks to respond to being cancelled
        if (!pool.awaitTermination(60, TimeUnit.SECONDS))
            System.err.println("Pool did not terminate");
      }
    } catch (InterruptedException ie) {
      // (Re-)Cancel if current thread also interrupted
      pool.shutdownNow();
      // Preserve interrupt status
      Thread.currentThread().interrupt();
    }

```


`shutdown():`  Initiates an orderly shutdown in which previously submitted tasks are executed, but no new tasks will be accepted.
`shutdownNow():`Attempts to stop all actively executing tasks, halts the processing of waiting tasks, and returns a list of the tasks that were awaiting execution.
In above example, if your tasks are taking more time to complete, you can change if condition to while condition
Replace

```java
if (!pool.awaitTermination(60, TimeUnit.SECONDS))

```


with

```java
while(!pool.awaitTermination(60, TimeUnit.SECONDS)) {
  Thread.sleep(60000);

```


}
</li>

> 
A **CountDownLatch** is initialized with a given count. The await methods block until the current count reaches zero due to invocations of the `countDown()` method, after which all waiting threads are released and any subsequent invocations of await return immediately. This is a one-shot phenomenon -- the count cannot be reset. If you need a version that resets the count, consider using a **CyclicBarrier**.




## Use cases for different types of ExecutorService


[Executors](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/Executors.html) returns different type of ThreadPools catering to specific need.

<li>
`public static ExecutorService newSingleThreadExecutor()`
<blockquote>
Creates an Executor that uses a single worker thread operating off an unbounded queue
</blockquote>
There is a difference between `newFixedThreadPool(1)` and `newSingleThreadExecutor()` as the java doc says for the latter:
<blockquote>
Unlike the otherwise equivalent newFixedThreadPool(1) the returned executor is guaranteed not to be reconfigurable to use additional threads.
</blockquote>
<p>Which means that a `newFixedThreadPool` can be reconfigured later in the program by: `((ThreadPoolExecutor) fixedThreadPool).setMaximumPoolSize(10)`
This is not possible for `newSingleThreadExecutor`</p>
Use cases:
<ol>
1. You want to execute the submitted tasks in a sequence.
1. You need only one Thread to handle all your request

> 
Unlike the otherwise equivalent newFixedThreadPool(1) the returned executor is guaranteed not to be reconfigurable to use additional threads.


Cons:

1. Unbounded queue is harmful

`public static ExecutorService newFixedThreadPool(int nThreads)`

> 
Creates a thread pool that reuses a fixed number of threads operating off a shared unbounded queue. At any point, at most nThreads threads will be active processing tasks. If additional tasks are submitted when all threads are active, they will wait in the queue until a thread is available


Use cases:

1. Effective use of available cores. Configure `nThreads` as `Runtime.getRuntime().availableProcessors()`
1. When you decide that number of thread should not exceed a number in the thread pool

Cons:

1. Unbounded queue is harmful.

`public static ExecutorService newCachedThreadPool()`

> 
Creates a thread pool that creates new threads as needed, but will reuse previously constructed threads when they are available


Use cases:

1. For short-lived asynchronous tasks

Cons:

1. Unbounded queue is harmful.
1. Each new task will create a new thread if all existing threads are busy. If the task is taking long duration, more number of threads will be created,which will degrade the performance of the system. Alternative in this case: `newFixedThreadPool`

<li>
`public static ScheduledExecutorService newScheduledThreadPool(int corePoolSize)`
<blockquote>
Creates a thread pool that can schedule commands to run after a given delay, or to execute periodically.
</blockquote>
Use cases:
<ol>
1. Handling recurring events with delays, which will happen in future at certain interval of times

Cons:

1. Unbounded queue is harmful.

5.`public static ExecutorService newWorkStealingPool()`

> 
Creates a work-stealing thread pool using all available processors as its target parallelism level


Use cases:

1. For divide and conquer type of problems.
1. Effective use of idle threads. Idle threads steals tasks from busy threads.

Cons:

1. Unbounded queue size is harmful.

You can see one common drawbacks in all these ExecutorService : unbounded queue. This will be addressed with [ThreadPoolExecutor](https://docs.oracle.com/javase/8/docs/api/java/util/concurrent/ThreadPoolExecutor.html#execute-java.lang.Runnable-)

```java
ThreadPoolExecutor(int corePoolSize, int maximumPoolSize, long keepAliveTime, 
TimeUnit unit, BlockingQueue<Runnable> workQueue, ThreadFactory threadFactory,
RejectedExecutionHandler handler)

```

With `ThreadPoolExecutor`, you can

1. Control Thread pool size dynamically
1. Set the capacity for `BlockingQueue`
1. Define `RejectionExecutionHander` when queue is full
1. `CustomThreadFactory` to add some additional functionality during Thread creation `(public Thread newThread(Runnable r)`



## Scheduling tasks to run at a fixed time, after a delay or repeatedly


The `ScheduledExecutorService` class provides a methods for scheduling single or repeated tasks in a number of ways.  The following code sample assume that `pool` has been declared and initialized as follows:

```java
ScheduledExecutorService pool = Executors.newScheduledThreadPool(2);

```

In addition to the normal `ExecutorService` methods, the `ScheduledExecutorService` API adds 4 methods that schedule tasks and return `ScheduledFuture` objects.  The latter can be used to retrieve results (in some cases) and cancel tasks.

### Starting a task after a fixed delay

The following example schedules a task to start after ten minutes.

```java
ScheduledFuture<Integer> future = pool.schedule(new Callable<>() {
        @Override public Integer call() {
            // do something
            return 42;
        }
    }, 
    10, TimeUnit.MINUTES);

```

### Starting tasks at a fixed rate

The following example schedules a task to start after ten minutes, and then repeatedly at a rate of once every one minute.

```java
ScheduledFuture<?> future = pool.scheduleAtFixedRate(new Runnable() {
        @Override public void run() {
            // do something
        }
    }, 
    10, 1, TimeUnit.MINUTES);

```

Task execution will continue according to the schedule until the `pool` is shut down, the `future` is canceled, or one of the tasks encounters an exception.

It is guaranteed that the tasks scheduled by a given `scheduledAtFixedRate` call will not overlap in time.  If a task takes longer than the prescribed period, then  the next and subsequent task executions may start late.

### Starting tasks with a fixed delay

The following example schedules a task to start after ten minutes, and then repeatedly with a delay of one minute between one task ending and the next one starting.

```java
ScheduledFuture<?> future = pool.scheduleWithFixedDelay(new Runnable() {
        @Override public void run() {
            // do something
        }
    }, 
    10, 1, TimeUnit.MINUTES);

```

Task execution will continue according to the schedule until the `pool` is shut down, the `future` is canceled, or one of the tasks encounters an exception.



## Using Thread Pools


Thread Pools are used mostly calling methods in `ExecutorService`.

The following methods can be used to submit work for execution:

|Method|Description
|------
|`submit`|Executes a the submitted work and return a future which can be used to get the result
|`execute`|Execute the task sometime in the future without getting any return value
|`invokeAll`|Execute a list of tasks and return a list of Futures
|`invokeAny`|Executes all the but return only the result of one that has been successful (without exceptions)

Once you are done with the Thread Pool you can call `shutdown()` to terminate the Thread Pool. This executes all pending tasks. To wait for all tasks to execute you can can loop around `awaitTermination` or `isShutdown()`.



#### Remarks


**Pitfalls**

- When you schedule a task for repeated execution, depending on the ScheduledExecutorService used, your task might be suspended from any further execution, if an execution of your task causes an exception which isn't handled. See [Mother F**k the ScheduledExecutorService!](http://code.nomad-labs.com/2011/12/09/mother-fk-the-scheduledexecutorservice)

