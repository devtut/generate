---
metaTitle: "Async-Await"
description: "Await operator and async keyword, Concurrent calls, Try/Catch/Finally, Returning a Task without await, Web.config setup to target 4.5 for correct async behaviour., Async/await will only improve performance if it allows the machine to do additional work, Simple consecutive calls, Blocking on async code can cause deadlocks"
---

# Async-Await


In C#, a method declared `async` won't block within a synchronous process, in case of you're using I/O based operations (e.g. web access, working with files, ...). The result of such async marked methods may be awaited via the use of the `await`keyword.



## Await operator and async keyword


`await` operator and `async` keyword come together:

> 
<p>The asynchronous method in which **await** is used must be modified by
the **async** keyword.</p>


The opposite is not always true: you can mark a method as `async` without using `await` in its body.

What `await` actually does is to suspend execution of the code until the awaited task completes; any task can be awaited.

**Note:** you cannot await for async method which returns nothing (void).

Actually, the word 'suspends' is a bit misleading because not only the execution stops, but the thread may become free for executing other operations. Under the hood, `await` is implemented by a bit of compiler magic: it splits a method into two parts - before and after `await`. The latter part is executed when the awaited task completes.

If we ignore some important details, the compiler roughly does this for you:

```cs
public async Task<TResult> DoIt()
{
    // do something and acquire someTask of type Task<TSomeResult>  
    var awaitedResult = await someTask;
    // ... do something more and produce result of type TResult
    return result;
}

```

becomes:

```cs
public Task<TResult> DoIt()
{
    // ...
    return someTask.ContinueWith(task => {
        var result = ((Task<TSomeResult>)task).Result;
        return DoIt_Continuation(result);
    });
}

private TResult DoIt_Continuation(TSomeResult awaitedResult)
{
    // ...
}

```

Any usual method can be turned into async in the following way:

```cs
await Task.Run(() => YourSyncMethod());

```

This can be advantageous when you need to execute a long running method on the UI thread without freezing the UI.

But there is a very important remark here: **Asynchronous does not always mean concurrent (parallel or even multi-threaded).** Even on a single thread, `async`-`await` still allows for asynchronous code. For example, see this custom [task scheduler](https://msdn.microsoft.com/en-us/library/system.threading.tasks.taskscheduler(v=vs.110).aspx). Such a 'crazy' task scheduler can simply turn tasks into functions which are called within message loop processing.

We need to ask ourselves: What thread will execute the continuation of our method `DoIt_Continuation`?

By default the `await` operator schedules the execution of continuation with the current [Synchronization context](https://msdn.microsoft.com/en-us/library/system.threading.synchronizationcontext(v=vs.110).aspx). It means that by default for WinForms and WPF continuation runs in the UI thread. If, for some reason, you need to change this behavior, use [method](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.configureawait(v=vs.110).aspx) `Task.ConfigureAwait()`:

```cs
await Task.Run(() => YourSyncMethod()).ConfigureAwait(continueOnCapturedContext: false);

```



## Concurrent calls


It is possible to await multiple calls concurrently by first invoking the awaitable tasks and **then** awaiting them.

```cs
public async Task RunConcurrentTasks()
{
    var firstTask = DoSomethingAsync();
    var secondTask = DoSomethingElseAsync();

    await firstTask;
    await secondTask;
}

```

Alternatively, `Task.WhenAll` can be used to group multiple tasks into a single `Task`, which completes when all of its passed tasks are complete.

```cs
public async Task RunConcurrentTasks()
{
    var firstTask = DoSomethingAsync();
    var secondTask = DoSomethingElseAsync();

    await Task.WhenAll(firstTask, secondTask);
}

```

You can also do this inside a loop, for example:

```cs
List<Task> tasks = new List<Task>();
while (something) {
    // do stuff
    Task someAsyncTask = someAsyncMethod();
    tasks.Add(someAsyncTask);
}

await Task.WhenAll(tasks);

```

To get results from a task after awaiting multiple tasks with Task.WhenAll, simply await the task again. Since the task is already completed  it will just return the result back

```cs
var task1 = SomeOpAsync();
var task2 = SomeOtherOpAsync();

await Task.WhenAll(task1, task2);

var result = await task2;

```

Also, the `Task.WhenAny` can be used to execute multiple tasks in parallel, like the `Task.WhenAll` above, with the difference that this method will complete when **any** of the supplied tasks will be completed.

```cs
public async Task RunConcurrentTasksWhenAny()
{
    var firstTask = TaskOperation("#firstTask executed");
    var secondTask = TaskOperation("#secondTask executed");
    var thirdTask = TaskOperation("#thirdTask executed");
    await Task.WhenAny(firstTask, secondTask, thirdTask);
}

```

The `Task` returned by `RunConcurrentTasksWhenAny` will complete when any of `firstTask`, `secondTask`, or `thirdTask` completes.



## Try/Catch/Finally


As of C# 6.0, the `await` keyword can now be used within a `catch` and `finally` block.

```cs
try {
   var client = new AsyncClient();
   await client.DoSomething();
} catch (MyException ex) {
   await client.LogExceptionAsync();
   throw;
} finally {
   await client.CloseAsync();
}

```

Prior to C# 6.0, you would need to do something along the lines of the following. Note that 6.0 also cleaned up the null checks with the [Null Propagating operator](http://stackoverflow.com/documentation/c%23/24/c-6-features/51/null-propagation#t=201511271308000980289).

```cs
AsynClient client;
MyException caughtException;
try {
     client = new AsyncClient();
     await client.DoSomething();
} catch (MyException ex) {
     caughtException = ex;
}

if (client != null) {
    if (caughtException != null) {
       await client.LogExceptionAsync();
    }
    await client.CloseAsync();
    if (caughtException != null) throw caughtException;
}

```

Please note that if you await a task not created by `async` (e.g. a task created by `Task.Run`), some debuggers may break on exceptions thrown by the task even when it is seemingly handled by the surrounding try/catch. This happens because the debugger considers it to be unhandled with respect to user code. In Visual Studio, there is an option called ["Just My Code"](https://msdn.microsoft.com/en-us/library/dn457346.aspx), which can be disabled to prevent the debugger from breaking in such situations.



## Returning a Task without await


Methods that perform asynchronous operations don't need to use `await` if:

- There is only one asynchronous call inside the method
- The asynchronous call is at the end of the method
- Catching/handling exception that may happen within the Task is not necessary

Consider this method that returns a `Task`:

```cs
public async Task<User> GetUserAsync(int id)
{
    var lookupKey = "Users" + id;

    return await dataStore.GetByKeyAsync(lookupKey);
}

```

If `GetByKeyAsync` has the same signature as `GetUserAsync` (returning a `Task<User>`), the method can be simplified:

```cs
public Task<User> GetUserAsync(int id)
{
    var lookupKey = "Users" + id;

    return dataStore.GetByKeyAsync(lookupKey);
}

```

In this case, the method doesn't need to be marked `async`, even though it's preforming an asynchronous operation. The Task returned by `GetByKeyAsync` is passed directly to the calling method, where it will be `await`ed.

**Important**: Returning the `Task` instead of awaiting it, changes the exception behavior of the method, as it won't throw the exception inside the method which starts the task but in the method which awaits it.

```cs
public Task SaveAsync()
{
    try {
        return dataStore.SaveChangesAsync();
    }
    catch(Exception ex)
    {
        // this will never be called
        logger.LogException(ex);
    }
}

// Some other code calling SaveAsync()

// If exception happens, it will be thrown here, not inside SaveAsync()
await SaveAsync();

```

This will improve performance as it will save the compiler the generation of an extra **async** state machine.



## Web.config setup to target 4.5 for correct async behaviour.


The web.config system.web.httpRuntime must target 4.5 to ensure the thread will renter the request context before resuming your async method.

```cs
<httpRuntime targetFramework="4.5" />

```

Async and await have undefined behavior on ASP.NET prior to 4.5. Async / await will resume on an arbitrary thread that may not have the request context. Applications under load will randomly fail with null reference exceptions accessing the HttpContext after the await. [Using HttpContext.Current in WebApi is dangerous because of async](http://stackoverflow.com/questions/24956178/using-httpcontext-current-in-webapi-is-dangerous-because-of-async)



## Async/await will only improve performance if it allows the machine to do additional work


Consider the following code:

```cs
public async Task MethodA()
{
     await MethodB();
     // Do other work
}

public async Task MethodB()
{
     await MethodC();
     // Do other work
}

public async Task MethodC()
{
     // Or await some other async work
     await Task.Delay(100);
}

```

This will not perform any better than

```cs
public void MethodA()
{
     MethodB();
     // Do other work
}

public void MethodB()
{
     MethodC();
     // Do other work
}

public void MethodC()
{
     Thread.Sleep(100);
}

```

The primary purpose of async/await is to allow the machine to do additional work - for example, to allow the calling thread to do other work while it's waiting for a result from some I/O operation. In this case, the calling thread is never allowed to do more work than it would have been able to do otherwise, so there's no performance gain over simply calling `MethodA()`, `MethodB()`, and `MethodC()` synchronously.



## Simple consecutive calls


```cs
public async Task<JobResult> GetDataFromWebAsync()
{
  var nextJob = await _database.GetNextJobAsync();
  var response = await _httpClient.GetAsync(nextJob.Uri);
  var pageContents = await response.Content.ReadAsStringAsync();
  return await _database.SaveJobResultAsync(pageContents);
}

```

The main thing to note here is that while every `await`-ed method is called asynchronously - and for the time of that call the control is yielded back to the system - the flow inside the method is linear and does not require any special treatment due to asynchrony. If any of the methods called fail, the exception will be processed "as expected", which in this case means that the method execution will be aborted and the exception will be going up the stack.



## Blocking on async code can cause deadlocks


It is a bad practice to block on async calls as it can cause deadlocks in environments that have a synchronization context. The best practice is to use async/await "all the way down." For example, the following Windows Forms code causes a deadlock:

```cs
private async Task<bool> TryThis()
{
    Trace.TraceInformation("Starting TryThis");
    await Task.Run(() =>
    {
        Trace.TraceInformation("In TryThis task");
        for (int i = 0; i < 100; i++)
        {
            // This runs successfully - the loop runs to completion
            Trace.TraceInformation("For loop " + i);
            System.Threading.Thread.Sleep(10);
        }
    });

    // This never happens due to the deadlock
    Trace.TraceInformation("About to return");
    return true;
}

// Button click event handler
private void button1_Click(object sender, EventArgs e)
{
    // .Result causes this to block on the asynchronous call
    bool result = TryThis().Result;
    // Never actually gets here
    Trace.TraceInformation("Done with result");
}

```

Essentially, once the async call completes, it waits for the synchronization context to become available. However, the event handler "holds on" to the synchronization context while it's waiting for the `TryThis()` method to complete, thus causing a circular wait.

To fix this, code should be modified to

```cs
private async void button1_Click(object sender, EventArgs e)
{
  bool result = await TryThis();
  Trace.TraceInformation("Done with result");
}

```

Note: event handlers are the only place where `async void` should be used (because you can't await an `async void` method).



#### Remarks


An `async` method can return `void`, `Task` or `Task<T>`.

The return type `Task` will wait for the method to finish and the result will be `void`. `Task<T>` will return a value from type `T` after the method completes.

`async` methods should return `Task` or `Task<T>`, as opposed to `void`, in almost all circumstances. `async void` methods cannot be `await`ed, which leads to a variety of problems. The only scenario where an `async` should return `void` is in the case of an event handler.

`async`/`await` works by transforming your `async` method into a state machine.  It does this by creating a structure behind the scenes which stores the current state and any context (like local variables), and exposes a `MoveNext()` method to advance states (and run any associated code) whenever an awaited awaitable completes.

