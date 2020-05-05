---
metaTitle: "C# | Task Parallel Library"
description: "Parallel.ForEach, Parallel.For, Parallel.Invoke, A cancellable polling Task using CancellationTokenSource, Async version of PingUrl, An async cancellable polling Task that waits between iterations"
---

# Task Parallel Library



## Parallel.ForEach


An example that uses Parallel.ForEach loop to ping a given array of website urls.

```cs
static void Main()
{
    string [] urls = 
    {
        "www.stackoverflow.com", 
        "www.google.net", 
        "www.facebook.com", 
        "www.twitter.com"
    };
    
    System.Threading.Tasks.Parallel.ForEach(urls, url =>
    {
        var ping = new System.Net.NetworkInformation.Ping();

        var result = ping.Send(url);

        if (result.Status == System.Net.NetworkInformation.IPStatus.Success)
        {
            Console.WriteLine(string.Format("{0} is online", url));
        }
    });
}

```



## Parallel.For


An example that uses Parallel.For loop to ping a given array of website urls.

```cs
static void Main()
{
    string [] urls = 
    {
        "www.stackoverflow.com", 
        "www.google.net", 
        "www.facebook.com", 
        "www.twitter.com"
    };

    System.Threading.Tasks.Parallel.For(0, urls.Length, i =>
    {
        var ping = new System.Net.NetworkInformation.Ping();

        var result = ping.Send(urls[i]);

        if (result.Status == System.Net.NetworkInformation.IPStatus.Success)
        {
            Console.WriteLine(string.Format("{0} is online", urls[i]));
        }
    });
}

```



## Parallel.Invoke


Invoking methods or actions in parallel (Parallel region)

```cs
static void Main()
{
    string [] urls = 
    {
        "www.stackoverflow.com", 
        "www.google.net", 
        "www.facebook.com", 
        "www.twitter.com"
    };
    
    System.Threading.Tasks.Parallel.Invoke(
        () => PingUrl(urls[0]),
        () => PingUrl(urls[1]),
        () => PingUrl(urls[2]),
        () => PingUrl(urls[3])
    );
}

void PingUrl(string url)
{
    var ping = new System.Net.NetworkInformation.Ping();
    
    var result = ping.Send(url);
    
    if (result.Status == System.Net.NetworkInformation.IPStatus.Success)
    {
        Console.WriteLine(string.Format("{0} is online", url));
    }
}

```



## A cancellable polling Task using CancellationTokenSource


```cs
public class Foo
{
    private CancellationTokenSource _cts;

    public Foo()
    {
        this._cts = new CancellationTokenSource();
    }

    public void StartExecution()
    {
        Task.Factory.StartNew(this.OwnCodeCancelableTask, this._cts.Token);
    }

    public void CancelExecution()
    {
        this._cts.Cancel();
    }

    /// <summary>
    /// "Infinite" loop with no delays. Writing to a database while pulling from a buffer for example.
    /// </summary>
    /// <param name="taskState">The cancellation token from our _cts field, passed in the StartNew call</param>
    private void OwnCodeCancelableTask(object taskState)
    {
        var token = (CancellationToken) taskState; //Our cancellation token passed from StartNew();

        while ( !token.IsCancellationRequested )
        {
            Console.WriteLine("Do your task work in this loop");
        }
    }
}

```



## Async version of PingUrl


```cs

   static void Main(string[] args)
    {
        string url = "www.stackoverflow.com";
        var pingTask = PingUrlAsync(url);
        Console.WriteLine($"Waiting for response from {url}");
        Task.WaitAll(pingTask);            
        Console.WriteLine(pingTask.Result);
    }

    static async Task<string> PingUrlAsync(string url)
    {
        string response = string.Empty;
        var ping = new System.Net.NetworkInformation.Ping();

        var result = await ping.SendPingAsync(url);

        await Task.Delay(5000); //simulate slow internet

        if (result.Status == System.Net.NetworkInformation.IPStatus.Success)
        {
            response = $"{url} is online";
        }

        return response;
    }

```



## An async cancellable polling Task that waits between iterations


```cs
public class Foo
{
    private const int TASK_ITERATION_DELAY_MS = 1000;
    private CancellationTokenSource _cts;

    public Foo()
    {
        this._cts = new CancellationTokenSource();
    }

    public void StartExecution()
    {
        Task.Factory.StartNew(this.OwnCodeCancelableTask_EveryNSeconds, this._cts.Token);
    }

    public void CancelExecution()
    {
        this._cts.Cancel();
    }

    /// <summary>
    /// "Infinite" loop that runs every N seconds. Good for checking for a heartbeat or updates.
    /// </summary>
    /// <param name="taskState">The cancellation token from our _cts field, passed in the StartNew call</param>
    private async void OwnCodeCancelableTask_EveryNSeconds(object taskState)
    {
        var token = (CancellationToken)taskState;

        while (!token.IsCancellationRequested)
        {
            Console.WriteLine("Do the work that needs to happen every N seconds in this loop");

            // Passing token here allows the Delay to be cancelled if your task gets cancelled.
            await Task.Delay(TASK_ITERATION_DELAY_MS, token);
        }
    }
}

```

