---
metaTitle: "Threading"
description: "Avoiding Reading and Writing Data Simultaneously, Creating and Starting a Second Thread, Parallel.ForEach Loop, Simple Complete Threading Demo, Deadlocks (hold resource and wait), Creating One Thread Per Processor, Simple Complete Threading Demo using Tasks, Explicit Task Parallism, Implicit Task Parallelism, Starting a thread with parameters, Deadlocks (two threads waiting on eachother)"
---

# Threading



## Avoiding Reading and Writing Data Simultaneously


Sometimes, you want your threads to simultaneously share data. When this happens it is important to be aware of the code and lock any parts that could go wrong. A simple example of two threads counting is shown below.

Here is some dangerous (incorrect) code:

```cs
using System.Threading;

class MainClass 
{    
    static int count { get; set; }

    static void Main() 
    {
        for (int i = 1; i <= 2; i++)
        {
            var thread = new Thread(ThreadMethod);
            thread.Start(i);
            Thread.Sleep(500);
        }
    }

    static void ThreadMethod(object threadNumber) 
    {
        while (true)
        {
            var temp = count;
            System.Console.WriteLine("Thread " + threadNumber + ": Reading the value of count.");
            Thread.Sleep(1000);
            count = temp + 1;
            System.Console.WriteLine("Thread " + threadNumber + ": Incrementing the value of count to:" + count);
            Thread.Sleep(1000);
        }
    }
}

```

You'll notice, instead of counting 1,2,3,4,5... we count 1,1,2,2,3...

To fix this problem, we need to **lock** the value of count, so that multiple different threads cannot read and write to it at the same time. With the addition of a lock and a key, we can prevent the threads from accessing the data simultaneously.

```cs
using System.Threading;

class MainClass
{

    static int count { get; set; } 
    static readonly object key = new object();

    static void Main()
    {
        for (int i = 1; i <= 2; i++)
        {
            var thread = new Thread(ThreadMethod);
            thread.Start(i);
            Thread.Sleep(500);
        }
    }

    static void ThreadMethod(object threadNumber)
    {
        while (true)
        {
            lock (key) 
            {
                var temp = count;
                System.Console.WriteLine("Thread " + threadNumber + ": Reading the value of count.");
                Thread.Sleep(1000);
                count = temp + 1;
                System.Console.WriteLine("Thread " + threadNumber + ": Incrementing the value of count to:" + count);
            }
            Thread.Sleep(1000);
        }
    }
}

```



## Creating and Starting a Second Thread


If you're doing multiple long calculations, you can run them at the same time on different threads on your computer. To do this, we make a new **Thread** and have it point to a different method.

```cs
using System.Threading;

class MainClass {
    static void Main() {
        var thread = new Thread(Secondary);
        thread.Start();
    }

    static void Secondary() {
        System.Console.WriteLine("Hello World!");
    }
}

```



## Parallel.ForEach Loop


If you have a foreach loop that you want to speed up and you don't mind what order the output is in, you can convert it to a parallel foreach loop by doing the following:

```cs
using System;
using System.Threading;
using System.Threading.Tasks;

public class MainClass {

    public static void Main() {
        int[] Numbers = new int[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
        // Single-threaded
        Console.WriteLine("Normal foreach loop: ");
        foreach (var number in Numbers) {
            Console.WriteLine(longCalculation(number));
        }
        // This is the Parallel (Multi-threaded solution)
        Console.WriteLine("Parallel foreach loop: ");
        Parallel.ForEach(Numbers, number => {
            Console.WriteLine(longCalculation(number));
        });
    }

    private static int longCalculation(int number) {
        Thread.Sleep(1000); // Sleep to simulate a long calculation
        return number * number;
    }
}

```



## Simple Complete Threading Demo


```cs
class Program
{
    static void Main(string[] args)
    {
        // Create 2 thread objects.  We're using delegates because we need to pass 
        // parameters to the threads.  
        var thread1 = new Thread(new ThreadStart(() => PerformAction(1)));
        var thread2 = new Thread(new ThreadStart(() => PerformAction(2)));

        // Start the threads running 
        thread1.Start();
        // NB: as soon as the above line kicks off the thread, the next line starts; 
        // even if thread1 is still processing.
        thread2.Start();

        // Wait for thread1 to complete before continuing
        thread1.Join();
        // Wait for thread2 to complete before continuing
        thread2.Join();

        Console.WriteLine("Done");
        Console.ReadKey();
    }

    // Simple method to help demonstrate the threads running in parallel.
    static void PerformAction(int id)
    {
        var rnd = new Random(id);
        for (int i = 0; i < 100; i++)
        {
            Console.WriteLine("Thread: {0}: {1}", id, i);
            Thread.Sleep(rnd.Next(0, 1000));
        }
    }
}

```



## Deadlocks (hold resource and wait)


A deadlock is what occurs when two or more threads are waiting for eachother to complete or to release a resource in such a way that they wait forever.

If thread1 holds a lock on resource A and is waiting for resource B to be released while thread2 holds resource B and is waiting for resource A to be released, they are deadlocked.

Clicking button1 for the following example code will cause your application to get into aforementioned deadlocked state and hang

```cs
private void button_Click(object sender, EventArgs e)
{
    DeadlockWorkers workers = new DeadlockWorkers();
    workers.StartThreads();
    textBox.Text = workers.GetResult();
}

private class DeadlockWorkers
{
    Thread thread1, thread2;

    object resourceA = new object();
    object resourceB = new object();

    string output;

    public void StartThreads()
    {
        thread1 = new Thread(Thread1DoWork);
        thread2 = new Thread(Thread2DoWork);
        thread1.Start();
        thread2.Start();
    }

    public string GetResult()
    {
        thread1.Join();
        thread2.Join();
        return output;
    }

    public void Thread1DoWork()
    {
        Thread.Sleep(100);
        lock (resourceA)
        {
            Thread.Sleep(100);
            lock (resourceB)
            {
                output += "T1#";
            }
        }
    }

    public void Thread2DoWork()
    {
        Thread.Sleep(100);
        lock (resourceB)
        {
            Thread.Sleep(100);
            lock (resourceA)
            {
                output += "T2#";
            }
        }
    }
}

```

To avoid being deadlocked this way, one can use Monitor.TryEnter(lock_object, timeout_in_milliseconds) to check if a lock is held on an object already. If Monitor.TryEnter does not succeed in acquiring a lock on lock_object before timeout_in_milliseconds, it returns false, giving the thread a chance to release other held resources and yielding, thus giving other threads a chance to complete as in this slightly modified version of the above:

```cs
private void button_Click(object sender, EventArgs e)
{
    MonitorWorkers workers = new MonitorWorkers();
    workers.StartThreads();
    textBox.Text = workers.GetResult();
}

private class MonitorWorkers
{
    Thread thread1, thread2;

    object resourceA = new object();
    object resourceB = new object();

    string output;

    public void StartThreads()
    {
        thread1 = new Thread(Thread1DoWork);
        thread2 = new Thread(Thread2DoWork);
        thread1.Start();
        thread2.Start();
    }

    public string GetResult()
    {
        thread1.Join();
        thread2.Join();
        return output;
    }

    public void Thread1DoWork()
    {
        bool mustDoWork = true;
        Thread.Sleep(100);
        while (mustDoWork)
        {
            lock (resourceA)
            {
                Thread.Sleep(100);
                if (Monitor.TryEnter(resourceB, 0))
                {
                    output += "T1#";
                    mustDoWork = false;
                    Monitor.Exit(resourceB);
                }
            }
            if (mustDoWork) Thread.Yield();
        }
    }

    public void Thread2DoWork()
    {
        Thread.Sleep(100);
        lock (resourceB)
        {
            Thread.Sleep(100);
            lock (resourceA)
            {
                output += "T2#";
            }
        }
    }
}

```

Note that this workaround relies on thread2 being stubborn about its locks and thread1 being willing to yield, such that thread2 always take precedence. Also note that thread1 has to redo the work it did after locking resource A, when it yields. Therefore be careful when implementing this approach with more than one yielding thread, as you'll then run the risk of entering a so-called livelock - a state which would occur if two threads kept doing the first bit of their work and then yield mutually, starting over repeatedly.



## Creating One Thread Per Processor


> 
`Environment.ProcessorCount` Gets the number of **logical** processors on the current machine.


The CLR will then schedule each thread to a logical processor, this theoretically could mean each thread on a different logical processor, all threads on a single logical processor or some other combination.

```cs
using System;
using System.Threading;

class MainClass {
    static void Main() {
        for (int i = 0; i < Environment.ProcessorCount; i++) {
            var thread = new Thread(Secondary);
            thread.Start(i);
        }
        
    }

    static void Secondary(object threadNumber) {
        System.Console.WriteLine("Hello World from thread: " + threadNumber);
    }
}

```



## Simple Complete Threading Demo using Tasks


```cs
class Program
{
    static void Main(string[] args)
    {
        // Run 2 Tasks.  
        var task1 = Task.Run(() => PerformAction(1)));
        var task2 = Task.Run(() => PerformAction(2)));

        // Wait (i.e. block this thread) until both Tasks are complete.
        Task.WaitAll(new [] { task1, task2 });
        
        Console.WriteLine("Done");
        Console.ReadKey();
    }

    // Simple method to help demonstrate the threads running in parallel.
    static void PerformAction(int id)
    {
        var rnd = new Random(id);
        for (int i = 0; i < 100; i++)
        {
            Console.WriteLine("Task: {0}: {1}", id, i);
            Thread.Sleep(rnd.Next(0, 1000));
        }
    }
}

```



## Explicit Task Parallism


```cs

   private static void explicitTaskParallism()
    {
        Thread.CurrentThread.Name = "Main";

        // Create a task and supply a user delegate by using a lambda expression. 
        Task taskA = new Task(() => Console.WriteLine($"Hello from task {nameof(taskA)}."));
        Task taskB = new Task(() => Console.WriteLine($"Hello from task {nameof(taskB)}."));

        // Start the task.
        taskA.Start();
        taskB.Start();

        // Output a message from the calling thread.
        Console.WriteLine("Hello from thread '{0}'.",
                          Thread.CurrentThread.Name);
        taskA.Wait();
        taskB.Wait();
        Console.Read();
    }

```



## Implicit Task Parallelism


```cs

   private static void Main(string[] args)
    {
        var a = new A();
        var b = new B();
        //implicit task parallelism
        Parallel.Invoke(
            () => a.DoSomeWork(),
            () => b.DoSomeOtherWork()
            );

      }

```



## Starting a thread with parameters


using System.Threading;

```cs
class MainClass {
    static void Main() {
        var thread = new Thread(Secondary);
        thread.Start("SecondThread");
    }

    static void Secondary(object threadName) {
        System.Console.WriteLine("Hello World from thread: " + threadName);
    }
}

```



## Deadlocks (two threads waiting on eachother)


A deadlock is what occurs when two or more threads are waiting for eachother to complete or to release a resource in such a way that they wait forever.

A typical scenario of two threads waiting on eachother to complete is when a Windows Forms GUI thread waits for a worker thread and the worker thread attempts to invoke an object managed by the GUI thread.
Observe that with this code exmaple, clicking button1 will cause the program to hang.

```cs
private void button1_Click(object sender, EventArgs e)
{
    Thread workerthread= new Thread(dowork);
    workerthread.Start();
    workerthread.Join();
    // Do something after
}

private void dowork()
{
    // Do something before
    textBox1.Invoke(new Action(() => textBox1.Text = "Some Text"));
    // Do something after
}

```

`workerthread.Join()` is a call that blocks the calling thread until workerthread completes.
`textBox1.Invoke(invoke_delegate)` is a call that blocks the calling thread until the GUI thread has processed invoke_delegate, but this call causes deadlocks if the GUI thread is already waiting for the calling thread to complete.

To get around this, one can use a non-blocking way of invoking the textbox instead:

```cs
private void dowork()
{
    // Do work
    textBox1.BeginInvoke(new Action(() => textBox1.Text = "Some Text"));
    // Do work that is not dependent on textBox1 being updated first
}

```

However, this will cause trouble if you need to run code that is dependent on the textbox being updated first. In that case, run that as part of the invoke, but be aware that this will make it run on the GUI thread.

```cs
private void dowork()
{
    // Do work
    textBox1.BeginInvoke(new Action(() => {
        textBox1.Text = "Some Text";
        // Do work dependent on textBox1 being updated first, 
        // start another worker thread or raise an event
    }));
    // Do work that is not dependent on textBox1 being updated first
}

```

Alternatively start af whole new thread and let that one do the waiting on the GUI thread, so that workerthread might complete.

```cs
private void dowork()
{
    // Do work
    Thread workerthread2 = new Thread(() =>
    {
        textBox1.Invoke(new Action(() => textBox1.Text = "Some Text"));
        // Do work dependent on textBox1 being updated first, 
        // start another worker thread or raise an event
    });
    workerthread2.Start();
    // Do work that is not dependent on textBox1 being updated first
}

```

To minimize the risk of running into a deadlock of mutual waiting, always avoid circular references between threads when possible. A hierarchy of threads where lower-ranking threads only leave messages for higher-ranking threads and never waiting on them will not run into this kind of issue. However, it would still be vulnerable to deadlocks based on resource locking.



#### Remarks


A **thread** is a part of a program that can execute independently of other parts. It can perform tasks simultaneously with other threads. **Multithreading** is a feature that enables programs to perform concurrent processing so that more than one operation can be done at a time.

For example, you can use threading to update a timer or counter in the background while simultaneously performing other tasks in the foreground.

Multithreaded applications are more responsive to user input and are also easily  scalable, because the developer can add threads as and when the workload increases.

By default, a C# program has one thread -  the main program thread. However, secondary threads can be created and used to execute code in parallel with the primary thread. Such threads are called worker threads.

To control the operation of a thread, the CLR delegates a function to the operating system known as Thread Scheduler. A thread scheduler assures that all the threads are allocated proper execution time. It also checks that the threads that are blocked or locked do not consume much of the CPU time.

The .NET Framework `System.Threading` namespace makes using threads easier. System.Threading enables multithreading by providing a number of classes and interfaces. Apart from providing types and classes for a particular thread, it also defines types to hold a collection of threads, timer class and so on. It also provides its support by allowing synchronized access to shared data.

`Thread` is the main class in the `System.Threading` namespace. Other classes include `AutoResetEvent`, `Interlocked`, `Monitor`, `Mutex`, and `ThreadPool`.

Some of the delegates that are present in the `System.Threading` namespace include
`ThreadStart`, `TimerCallback`, and `WaitCallback`.

Enumerations in `System.Threading` namespace include `ThreadPriority`, `ThreadState`,
and `EventResetMode`.

In .NET Framework 4 and later versions, multithreaded programming is made easier and simpler through the `System.Threading.Tasks.Parallel` and `System.Threading.Tasks.Task` classes, Parallel LINQ (PLINQ), new concurrent collection classes in the `System.Collections.Concurrent` namespace, and a new task-based programming model.

