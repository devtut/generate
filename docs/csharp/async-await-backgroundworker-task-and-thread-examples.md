---
metaTitle: "Async/await, Backgroundworker, Task and Thread Examples"
description: "ASP.NET Configure Await, Async/await, BackgroundWorker, Task, Thread, Task run and forget extension"
---

# Async/await, Backgroundworker, Task and Thread Examples




## ASP.NET Configure Await


When ASP.NET handles a request, a thread is assigned from the thread pool and a **request context** is created. The request context contains information about the current request which can be accessed through the static `HttpContext.Current` property. The request context for the request is then assigned to the thread handling the request.

A given request context **may only be active on one thread at a time**.

When execution reaches `await`, the thread handling a request is returned to the thread pool while the asynchronous method runs and the request context is free for another thread to use.

```cs
public async Task<ActionResult> Index()
{
    // Execution on the initially assigned thread
    var products = await dbContext.Products.ToListAsync();

    // Execution resumes on a "random" thread from the pool
    // Execution continues using the original request context.
    return View(products);
}

```

When the task completes the thread pool assigns another thread to continue execution of the request. The request context is then assigned to this thread. This may or may not be the original thread.

### Blocking

When the result of an `async` method call is waited for **synchronously** deadlocks can arise. For example the following code will result in a deadlock when `IndexSync()` is called:

```cs
public async Task<ActionResult> Index()
{
    // Execution on the initially assigned thread
    List<Product> products = await dbContext.Products.ToListAsync();

    // Execution resumes on a "random" thread from the pool
    return View(products);
}

public ActionResult IndexSync()
{
    Task<ActionResult> task = Index();

    // Block waiting for the result synchronously
    ActionResult result = Task.Result;

    return result;       
}

```

This is because, by default the awaited task, in this case `db.Products.ToListAsync()` will capture the context (in the case of ASP.NET the request context) and try to use it once it has completed.

When the entire call stack is asynchronous there is no problem because, once `await` is reached the original thread is release, freeing the request context.

When we block synchronously using `Task.Result` or `Task.Wait()` (or other blocking methods) the original thread is still active and retains the request context. The awaited method still operates asynchronously and once the callback tries to run, i.e. once the awaited task has returned, it attempts to obtain the request context.

Therefore the deadlock arises because while the blocking thread with the request context is waiting for the asynchronous operation to complete, the asynchronous operation is trying to obtain the request context in order to complete.

### ConfigureAwait

By default calls to an awaited task will capture the current context and attempt to resume execution on the context once complete.

By using `ConfigureAwait(false)` this can be prevented and deadlocks can be avoided.

```cs
public async Task<ActionResult> Index()
{
    // Execution on the initially assigned thread
    List<Product> products = await dbContext.Products.ToListAsync().ConfigureAwait(false);

    // Execution resumes on a "random" thread from the pool without the original request context
    return View(products);
}

public ActionResult IndexSync()
{
    Task<ActionResult> task = Index();

    // Block waiting for the result synchronously
    ActionResult result = Task.Result;

    return result;       
}

```

This can avoid deadlocks when it is necessary to block on asynchronous code, however this comes at the cost of losing the context in the continuation (code after the call to await).

In ASP.NET this means that if your code following a call to `await someTask.ConfigureAwait(false);` attempts to access information from the context, for example `HttpContext.Current.User` then the information has been lost. In this case the `HttpContext.Current` is null. For example:

```cs
public async Task<ActionResult> Index()
{
    // Contains information about the user sending the request
    var user = System.Web.HttpContext.Current.User;

    using (var client = new HttpClient())
    {
        await client.GetAsync("http://google.com").ConfigureAwait(false);
    }

    // Null Reference Exception, Current is null
    var user2 = System.Web.HttpContext.Current.User;

    return View();
}

```

If `ConfigureAwait(true)` is used (equivalent to having no ConfigureAwait at all) then both `user` and `user2` are populated with the same data.

For this reason it is often recommended to use `ConfigureAwait(false)` in library code where the context is no longer used.



## Async/await


See below for a simple example of how to use async/await to do some time intensive stuff in a background process while maintaining the option of doing some other stuff that do not need to wait on the time intensive stuff to complete.

However, if you need to work with the result of the time intensive method later, you can do this by awaiting the execution.

```cs
public async Task ProcessDataAsync()
{
    // Start the time intensive method
    Task<int> task = TimeintensiveMethod(@"PATH_TO_SOME_FILE");

    // Control returns here before TimeintensiveMethod returns
    Console.WriteLine("You can read this while TimeintensiveMethod is still running.");

    // Wait for TimeintensiveMethod to complete and get its result
    int x = await task;
    Console.WriteLine("Count: " + x);
}

private async Task<int> TimeintensiveMethod(object file)
{
    Console.WriteLine("Start TimeintensiveMethod.");

    // Do some time intensive calculations...
    using (StreamReader reader = new StreamReader(file.ToString()))
    {
        string s = await reader.ReadToEndAsync();

        for (int i = 0; i < 10000; i++)
            s.GetHashCode();
    }
    Console.WriteLine("End TimeintensiveMethod.");

    // return something as a "result"
    return new Random().Next(100);
}

```



## BackgroundWorker


See below for a simple example of how to use a `BackgroundWorker` object to perform time-intensive operations in a background thread.

You need to:

1. Define a worker method that does the time-intensive work and call it from an event handler for the `DoWork` event of a `BackgroundWorker`.
1. Start the execution with `RunWorkerAsync`. Any argument required by the worker method  attached to `DoWork` can be passed in via the `DoWorkEventArgs` parameter to `RunWorkerAsync`.

In addition to the `DoWork` event the `BackgroundWorker` class also defines two events that should be used for interacting with the user interface. These are optional.

- The `RunWorkerCompleted` event is triggered when the `DoWork` handlers have completed.
- The `ProgressChanged` event is triggered when the `ReportProgress` method is called.

```cs
public void ProcessDataAsync()
{
    // Start the time intensive method
    BackgroundWorker bw = new BackgroundWorker();
    bw.DoWork += BwDoWork;
    bw.RunWorkerCompleted += BwRunWorkerCompleted;
    bw.RunWorkerAsync(@"PATH_TO_SOME_FILE");

    // Control returns here before TimeintensiveMethod returns
    Console.WriteLine("You can read this while TimeintensiveMethod is still running.");
}

// Method that will be called after BwDoWork exits
private void BwRunWorkerCompleted(object sender, RunWorkerCompletedEventArgs e)
{
    // we can access possible return values of our Method via the Parameter e
    Console.WriteLine("Count: " + e.Result);
}

// execution of our time intensive Method
private void BwDoWork(object sender, DoWorkEventArgs e)
{
    e.Result = TimeintensiveMethod(e.Argument);
}

private int TimeintensiveMethod(object file)
{
    Console.WriteLine("Start TimeintensiveMethod.");

    // Do some time intensive calculations...
    using (StreamReader reader = new StreamReader(file.ToString()))
    {
        string s = reader.ReadToEnd();

       for (int i = 0; i < 10000; i++)
            s.GetHashCode();
    }
    Console.WriteLine("End TimeintensiveMethod.");

    // return something as a "result"
    return new Random().Next(100);
}

```



## Task


See below for a simple example of how to use a `Task` to do some time intensive stuff in a background process.

All you need to do is wrap your time intensive method in a `Task.Run()` call.

```cs
public void ProcessDataAsync()
{
    // Start the time intensive method
    Task<int> t = Task.Run(() => TimeintensiveMethod(@"PATH_TO_SOME_FILE"));

    // Control returns here before TimeintensiveMethod returns
    Console.WriteLine("You can read this while TimeintensiveMethod is still running.");

    Console.WriteLine("Count: " + t.Result);
}

private int TimeintensiveMethod(object file)
{
    Console.WriteLine("Start TimeintensiveMethod.");

    // Do some time intensive calculations...
    using (StreamReader reader = new StreamReader(file.ToString()))
    {
        string s = reader.ReadToEnd();

        for (int i = 0; i < 10000; i++)
            s.GetHashCode();
    }
    Console.WriteLine("End TimeintensiveMethod.");

    // return something as a "result"
    return new Random().Next(100);
}

```



## Thread


See below for a simple example of how to use a `Thread` to do some time intensive stuff in a background process.

```cs
public async void ProcessDataAsync()
{
    // Start the time intensive method
    Thread t = new Thread(TimeintensiveMethod);

    // Control returns here before TimeintensiveMethod returns
    Console.WriteLine("You can read this while TimeintensiveMethod is still running.");
}

private void TimeintensiveMethod()
{
    Console.WriteLine("Start TimeintensiveMethod.");

    // Do some time intensive calculations...
    using (StreamReader reader = new StreamReader(@"PATH_TO_SOME_FILE"))
    {
        string v = reader.ReadToEnd();

        for (int i = 0; i < 10000; i++)
            v.GetHashCode();
    }
    Console.WriteLine("End TimeintensiveMethod.");
}

```

As you can see we can not return a value from our `TimeIntensiveMethod` because `Thread` expects a void Method as its parameter.

To get a return value from a `Thread` use either an event or the following:

```cs
int ret;
Thread t= new Thread(() => 
{
    Console.WriteLine("Start TimeintensiveMethod.");

    // Do some time intensive calculations...
    using (StreamReader reader = new StreamReader(file))
    {
        string s = reader.ReadToEnd();

        for (int i = 0; i < 10000; i++)
            s.GetHashCode();
    }
    Console.WriteLine("End TimeintensiveMethod.");

    // return something to demonstrate the coolness of await-async
    ret = new Random().Next(100);
});

t.Start();
t.Join(1000);
Console.Writeline("Count: " + ret);

```



## Task "run and forget" extension


In certain cases  (e.g. logging) it might be useful to run task and do not await for the result. The following extension allows to run task and continue execution of the rest code:

```cs
public static class TaskExtensions
{
    public static async void RunAndForget(
        this Task task, Action<Exception> onException = null)
    {
        try
        {
            await task;
        }
        catch (Exception ex)
        {
            onException?.Invoke(ex);
        }
    }
}

```

The result is awaited only inside the extension method. Since `async`/`await` is used, it is possible to catch an exception and call an optional method for handling it.

An example how to use the extension:

```cs
var task = Task.FromResult(0); // Or any other task from e.g. external lib.
task.RunAndForget(
    e =>
    {
        // Something went wrong, handle it.
    });

```



#### Remarks


To run any of these examples just call them like that:

```cs
static void Main()
{
    new Program().ProcessDataAsync();
    Console.ReadLine();
}

```

