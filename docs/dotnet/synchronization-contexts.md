---
metaTitle: ".NET Framework - Synchronization Contexts"
description: "Execute code on the UI thread after performing background work"
---

# Synchronization Contexts



## Execute code on the UI thread after performing background work


This example shows how to update a UI component from a background thread by using a `SynchronizationContext`

```dotnet
void Button_Click(object sender, EventArgs args)
{
    SynchronizationContext context = SynchronizationContext.Current;
    Task.Run(() =>
    {
        for(int i = 0; i < 10; i++) 
        {
            Thread.Sleep(500); //simulate work being done
            context.Post(ShowProgress, "Work complete on item " + i);
        }
    }
}

void UpdateCallback(object state)
{
    // UI can be safely updated as this method is only called from the UI thread
    this.MyTextBox.Text = state as string;
}

```

In this example, if you tried to directly update `MyTextBox.Text` inside the `for` loop, you would get a threading error. By posting the `UpdateCallback` action to the `SynchronizationContext`, the text box is updated on the same thread as the rest of the UI.

In practice, progress updates should be performed using an instance of `System.IProgress<T>`. The default implementation `System.Progress<T>` automatically captures the synchronisation context it is created on.



#### Remarks


A Synchronization Context is an abstraction that allows consuming to code to pass units of work to a scheduler, without requiring awareness of how the work will be scheduled.

Synchronization contexts are traditionally used to ensure that code is run on a specific thread. In WPF and Winforms applications, a `SynchronizationContext` representing the UI thread is provided by the presentation framework. In this way `SynchronizationContext` can be thought of as a producer-consumer pattern for delegates. A worker thread will **produce** executable code (the delegate) and queue it or **consumption** by the UI message loop.

The Task Parallel Library provides features for automatically capturing and using synchronization contexts.

