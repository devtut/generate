---
metaTitle: ".NET Framework - Task Parallel Library (TPL)"
description: "Basic producer-consumer loop (BlockingCollection), Parallel.Invoke, Task: basic instantiation and Wait, Task.WhenAll, Parallel.ForEach, Parallel.For, Task: Returning a value, Task: WaitAll and variable capturing, Task: WaitAny, Task: handling exceptions (using Wait), Task: handling exceptions (without using Wait), Task: cancelling using CancellationToken, Task.WhenAny, Flowing execution context with AsyncLocal, Parallel.ForEach in VB.NET"
---

# Task Parallel Library (TPL)



## Basic producer-consumer loop (BlockingCollection)


```dotnet
var collection = new BlockingCollection<int>(5);
var random = new Random();

var producerTask = Task.Run(() => {
    for(int item=1; item<=10; item++) 
    {
        collection.Add(item);
        Console.WriteLine("Produced: " + item);
        Thread.Sleep(random.Next(10,1000));
    }
    collection.CompleteAdding();
    Console.WriteLine("Producer completed!");
});

```

It is worth noting that if you do not call `collection.CompleteAdding();`, you are able to keep adding to the collection even if your consumer task is running. Just call `collection.CompleteAdding();` when you are sure there are no more additions. This functionality can be used to make a Multiple Producer to a Single Consumer pattern where you have multiple sources feeding items into the BlockingCollection and a single consumer pulling items out and doing something with them. If your BlockingCollection is empty before you call complete adding, the Enumerable from `collection.GetConsumingEnumerable()` will block until a new item is added to the collection or BlockingCollection.CompleteAdding(); is called and the queue is empty.

```dotnet
var consumerTask = Task.Run(() => {
    foreach(var item in collection.GetConsumingEnumerable())
    {
        Console.WriteLine("Consumed: " + item);
        Thread.Sleep(random.Next(10,1000));
    }
    Console.WriteLine("Consumer completed!");
});
  
Task.WaitAll(producerTask, consumerTask);
       
Console.WriteLine("Everything completed!");

```



## Parallel.Invoke


```dotnet
var actions = Enumerable.Range(1, 10).Select(n => new Action(() =>
{
    Console.WriteLine("I'm task " + n);
    if((n & 1) == 0)
        throw new Exception("Exception from task " + n);
})).ToArray();

try
{
    Parallel.Invoke(actions);
}
catch(AggregateException ex)
{
    foreach(var inner in ex.InnerExceptions)
        Console.WriteLine("Task failed: " + inner.Message);
}

```



## Task: basic instantiation and Wait


A task can be created by directly instantiating the `Task` class...

```dotnet
var task = new Task(() =>
{
    Console.WriteLine("Task code starting...");
    Thread.Sleep(2000);
    Console.WriteLine("...task code ending!");
});

Console.WriteLine("Starting task...");
task.Start();
task.Wait();
Console.WriteLine("Task completed!");

```

...or by using the static `Task.Run` method:

```dotnet
Console.WriteLine("Starting task...");
var task = Task.Run(() =>
{
    Console.WriteLine("Task code starting...");
    Thread.Sleep(2000);
    Console.WriteLine("...task code ending!");
});
task.Wait();
Console.WriteLine("Task completed!");

```

Note that only in the first case it is necessary to explicitly invoke `Start`.



## Task.WhenAll


```dotnet
var random = new Random();
IEnumerable<Task<int>> tasks = Enumerable.Range(1, 5).Select(n => Task.Run(() =>
{
    Console.WriteLine("I'm task " + n);
    return n;
}));

Task<int[]> task = Task.WhenAll(tasks);
int[] results = await task;

Console.WriteLine(string.Join(",", results.Select(n => n.ToString())));
// Output: 1,2,3,4,5

```



## Parallel.ForEach


This example uses `Parallel.ForEach` to calculate the sum of the numbers between 1 and 10000 by using multiple threads. To achieve thread-safety, `Interlocked.Add` is used to sum the numbers.

```dotnet
using System.Threading;

int Foo()
{
    int total = 0;
    var numbers = Enumerable.Range(1, 10000).ToList();
    Parallel.ForEach(numbers, 
        () => 0, // initial value,
        (num, state, localSum) => num + localSum,
        localSum => Interlocked.Add(ref total, localSum));
    return total; // total = 50005000
}

```



## Parallel.For


This example uses `Parallel.For` to calculate the sum of the numbers between 1 and 10000 by using multiple threads. To achieve thread-safety, `Interlocked.Add` is used to sum the numbers.

```dotnet
using System.Threading;

int Foo()
{
    int total = 0;
    Parallel.For(1, 10001, 
        () => 0, // initial value,
        (num, state, localSum) => num + localSum,
        localSum => Interlocked.Add(ref total, localSum));
    return total; // total = 50005000
}

```



## Task: Returning a value


Task that return a value has return type of `Task< TResult >` where TResult is the type of value that needs to be returned. You can query the outcome of a Task by its Result property.

```dotnet
Task<int> t = Task.Run(() => 
    {
        int sum = 0;

        for(int i = 0; i < 500; i++)
            sum += i;

        return sum;
    });

Console.WriteLine(t.Result); // Outuput 124750

```

If the Task execute asynchronously than awaiting the Task returns it's result.

```dotnet
public async Task DoSomeWork()
{
    WebClient client = new WebClient();
    // Because the task is awaited, result of the task is assigned to response
    string response = await client.DownloadStringTaskAsync("http://somedomain.com");
}

```



## Task: WaitAll and variable capturing


```dotnet
var tasks = Enumerable.Range(1, 5).Select(n => new Task<int>(() =>
{
    Console.WriteLine("I'm task " + n);
    return n;
})).ToArray();

foreach(var task in tasks) task.Start();
Task.WaitAll(tasks);

foreach(var task in tasks)
    Console.WriteLine(task.Result);

```



## Task: WaitAny


```dotnet
var allTasks = Enumerable.Range(1, 5).Select(n => new Task<int>(() => n)).ToArray();
var pendingTasks = allTasks.ToArray();

foreach(var task in allTasks) task.Start();

while(pendingTasks.Length > 0)
{
    var finishedTask = pendingTasks[Task.WaitAny(pendingTasks)];
    Console.WriteLine("Task {0} finished", finishedTask.Result);
    pendingTasks = pendingTasks.Except(new[] {finishedTask}).ToArray();
}

Task.WaitAll(allTasks);

```

**Note:** The final `WaitAll` is necessary becasue `WaitAny` does not cause exceptions to be observed.



## Task: handling exceptions (using Wait)


```dotnet
var task1 = Task.Run(() =>
{
    Console.WriteLine("Task 1 code starting...");
    throw new Exception("Oh no, exception from task 1!!");
});

var task2 = Task.Run(() =>
{
    Console.WriteLine("Task 2 code starting...");
    throw new Exception("Oh no, exception from task 2!!");
});

Console.WriteLine("Starting tasks...");
try
{
    Task.WaitAll(task1, task2);
}
catch(AggregateException ex)
{
    Console.WriteLine("Task(s) failed!");
    foreach(var inner in ex.InnerExceptions)
        Console.WriteLine(inner.Message);
}

Console.WriteLine("Task 1 status is: " + task1.Status); //Faulted
Console.WriteLine("Task 2 status is: " + task2.Status); //Faulted

```



## Task: handling exceptions (without using Wait)


```dotnet
var task1 = Task.Run(() =>
{
    Console.WriteLine("Task 1 code starting...");
    throw new Exception("Oh no, exception from task 1!!");
});

var task2 = Task.Run(() =>
{
    Console.WriteLine("Task 2 code starting...");
    throw new Exception("Oh no, exception from task 2!!");
});

var tasks = new[] {task1, task2};

Console.WriteLine("Starting tasks...");
while(tasks.All(task => !task.IsCompleted));

foreach(var task in tasks)
{
    if(task.IsFaulted)
        Console.WriteLine("Task failed: " +
            task.Exception.InnerExceptions.First().Message);
}

Console.WriteLine("Task 1 status is: " + task1.Status); //Faulted
Console.WriteLine("Task 2 status is: " + task2.Status); //Faulted

```



## Task: cancelling using CancellationToken


```dotnet
var cancellationTokenSource = new CancellationTokenSource();
var cancellationToken = cancellationTokenSource.Token;

var task = new Task((state) =>
    {
        int i = 1;
        var myCancellationToken = (CancellationToken)state;
        while(true)
        {
            Console.Write("{0} ", i++);
            Thread.Sleep(1000);
            myCancellationToken.ThrowIfCancellationRequested();
        }
    },
    cancellationToken: cancellationToken,
    state: cancellationToken);

Console.WriteLine("Counting to infinity. Press any key to cancel!");
task.Start();
Console.ReadKey();

cancellationTokenSource.Cancel();
try
{
    task.Wait();
}
catch(AggregateException ex)
{
    ex.Handle(inner => inner is OperationCanceledException);
}

Console.WriteLine($"{Environment.NewLine}You have cancelled! Task status is: {task.Status}");
//Canceled

```

As an alternative to `ThrowIfCancellationRequested`, the cancellation request can be detected with `IsCancellationRequested` and a `OperationCanceledException` can be thrown manually:

```dotnet
//New task delegate
int i = 1;
var myCancellationToken = (CancellationToken)state;
while(!myCancellationToken.IsCancellationRequested)
{
    Console.Write("{0} ", i++);
    Thread.Sleep(1000);
}
Console.WriteLine($"{Environment.NewLine}Ouch, I have been cancelled!!");
throw new OperationCanceledException(myCancellationToken);

```

Note how the cancellation token is passed to the task constructor in the `cancellationToken` parameter. This is needed so that the task transitions to the `Canceled` state, not to the `Faulted` state, when `ThrowIfCancellationRequested` is invoked. Also, for the same reason, the cancellation token is explicitly supplied in the constructor of `OperationCanceledException` in the second case.



## Task.WhenAny


```dotnet
var random = new Random();
IEnumerable<Task<int>> tasks = Enumerable.Range(1, 5).Select(n => Task.Run(async() =>
{
    Console.WriteLine("I'm task " + n);
    await Task.Delay(random.Next(10,1000));
    return n;
}));

Task<Task<int>> whenAnyTask = Task.WhenAny(tasks);
Task<int> completedTask = await whenAnyTask;
Console.WriteLine("The winner is: task " + await completedTask);

await Task.WhenAll(tasks);
Console.WriteLine("All tasks finished!");

```



## Flowing execution context with AsyncLocal


When you need to pass some data from the parent task to its children tasks, so it logically flows with the execution, use `AsyncLocal` [class](https://msdn.microsoft.com/en-us/library/dn906268(v=vs.110).aspx):

```dotnet
void Main()
{
    AsyncLocal<string> user = new AsyncLocal<string>();
    user.Value = "initial user";
    
    // this does not affect other tasks - values are local relative to the branches of execution flow
    Task.Run(() => user.Value = "user from another task"); 
    
    var task1 = Task.Run(() =>
    {
        Console.WriteLine(user.Value); // outputs "initial user"
        Task.Run(() =>
        {
            // outputs "initial user" - value has flown from main method to this task without being changed
            Console.WriteLine(user.Value);
        }).Wait();

        user.Value = "user from task1";

        Task.Run(() =>
        {
            // outputs "user from task1" - value has flown from main method to task1
            // than value was changed and flown to this task.
            Console.WriteLine(user.Value);
        }).Wait();
    });
    
    task1.Wait();
    
    // ouputs "initial user" - changes do not propagate back upstream the execution flow    
    Console.WriteLine(user.Value); 
}

```

**Note:** As can be seen from the example above `AsynLocal.Value` has `copy on read` semantic, but if you flow some reference type and change its properties you will affect other tasks. Hence, best practice with `AsyncLocal` is to use value types or immutable types.



## Parallel.ForEach in VB.NET


```dotnet
For Each row As DataRow In FooDataTable.Rows
    Me.RowsToProcess.Add(row)
Next

Dim myOptions As ParallelOptions = New ParallelOptions()
myOptions.MaxDegreeOfParallelism = environment.processorcount

Parallel.ForEach(RowsToProcess, myOptions, Sub(currentRow, state)
                                               ProcessRowParallel(currentRow, state)
                                           End Sub)

```



#### Remarks


### Purpose And  Use Cases

The purpose of the Task Parallel Library is to simplify the process of writing  and maintaining multithreaded and parallel code.

Some Use Cases*:

- Keeping a UI responsive by running background work on separate task
- Distributing workload
- Allowing a client application to send and receive requests at the same time (rest, TCP/UDP, ect)
- Reading and/or writing multiple files at once

*Code should be considered on a case by case basis for multithreading. For example, if a loop only  has a few iterations or only does a small amount of the work, the overhead for parallelism may outweigh the benefits.

**TPL with .Net 3.5**

The TPL is also available for .Net 3.5 included in a NuGet package, it is called Task Parallel Library.

