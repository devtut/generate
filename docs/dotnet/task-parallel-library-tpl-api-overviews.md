---
metaTitle: ".NET Framework - Task Parallel Library (TPL) API Overviews"
description: "Perform work in response to a button click and update the UI"
---

# Task Parallel Library (TPL) API Overviews



## Perform work in response to a button click and update the UI


This example demonstrates how you can respond to a button click by performing some work on a worker thread and then update the user interface to indicate completion

```dotnet
void MyButton_OnClick(object sender, EventArgs args)
{
    Task.Run(() => // Schedule work using the thread pool
        {
            System.Threading.Thread.Sleep(5000); // Sleep for 5 seconds to simulate work.
        })
    .ContinueWith(p => // this continuation contains the 'update' code to run on the UI thread
    {
        this.TextBlock_ResultText.Text = "The work completed at " + DateTime.Now.ToString()
    },
    TaskScheduler.FromCurrentSynchronizationContext()); // make sure the update is run on the UI thread.

}

```



#### Remarks


The Task Parallel Library is set of public types and APIs that dramatically simplify the process of adding parallelism and concurrency to an application. .Net. TPL was introduced in .Net 4 and is the recommended way to write multi threaded and parallel code.

TPL takes care of work scheduling, thread affinity, cancellation support, state management, and load balancing so that the programmer can focus on solving problems rather than spending time on common low level details.

