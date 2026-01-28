---
metaTitle: "C# | Synchronization Context in Async-Await"
description: "Pseudocode for async/await keywords, Disabling synchronization context, Why SynchronizationContext is so important?"
---

# Synchronization Context in Async-Await



## Pseudocode for async/await keywords


Consider a simple asynchronous method:

```cs
async Task Foo()
{
    Bar();
    await Baz();
    Qux();
}

```

Simplifying, we can say that this code actually means the following:

```cs
Task Foo()
{
    Bar();
    Task t = Baz();
    var context = SynchronizationContext.Current;
    t.ContinueWith(task) =>
    {
        if (context == null)
            Qux();
        else
            context.Post((obj) => Qux(), null);
    }, TaskScheduler.Current);

    return t;
}

```

It means that `async`/`await` keywords use current synchronization context if it exists. I.e. you can write library code that would work correctly in UI, Web, and Console applications.

[Source article](https://blogs.msdn.microsoft.com/pfxteam/2012/01/20/await-synchronizationcontext-and-console-apps/).



## Disabling synchronization context


To disable synchronization context you should call the [`ConfigureAwait`](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task.configureawait(v=vs.110).aspx) method:

```cs
async Task() Foo()
{
    await Task.Run(() => Console.WriteLine("Test"));
}

. . .

Foo().ConfigureAwait(false);

```

> 
ConfigureAwait provides a means to avoid the default SynchronizationContext capturing behavior; passing false for the flowContext parameter prevents the SynchronizationContext from being used to resume execution after the await.


Quote from [It's All About the SynchronizationContext](https://msdn.microsoft.com/en-us/magazine/gg598924.aspx).



## Why SynchronizationContext is so important?


Consider this example:

```cs
private void button1_Click(object sender, EventArgs e)
{
    label1.Text = RunTooLong();
}

```

This method will freeze UI application until the `RunTooLong` will be completed. The application will be unresponsive.

You can try run inner code asynchronously:

```cs
private void button1_Click(object sender, EventArgs e)
{
    Task.Run(() => label1.Text = RunTooLong());
}

```

But this code won't execute because inner body may be run on non-UI thread and [it shouldn't change UI properties directly](https://nnish.com/2010/03/14/accessing-wpf-controls-on-a-non-ui-thread/):

```cs
private void button1_Click(object sender, EventArgs e)
{
    Task.Run(() =>
    {
        var label1Text = RunTooLong();

        if (label1.InvokeRequired)
            lable1.BeginInvoke((Action) delegate() { label1.Text = label1Text; });
        else
            label1.Text = label1Text;
    });
}

```

Now don't forget always to use this pattern. Or, try [`SynchronizationContext.Post`](https://lostechies.com/gabrielschenker/2009/01/23/synchronizing-calls-to-the-ui-in-a-multi-threaded-application/) that will make it for you:

```cs
private void button1_Click(object sender, EventArgs e)
{
    Task.Run(() =>
    {
        var label1Text = RunTooLong();
        SynchronizationContext.Current.Post((obj) =>
        {
            label1.Text = label1    Text);
        }, null);
    });
}

```

