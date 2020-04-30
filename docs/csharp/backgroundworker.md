---
metaTitle: "BackgroundWorker"
description: "Using a BackgroundWorker to complete a task., Assigning Event Handlers to a BackgroundWorker, Creating a new BackgroundWorker instance, Assigning Properties to a BackgroundWorker"
---

# BackgroundWorker



## Using a BackgroundWorker to complete a task.


The following example demonstrates the use of a BackgroundWorker to update a WinForms ProgressBar. The backgroundWorker will update the value of the progress bar without blocking the UI thread, thus showing a reactive UI while work is done in the background.

```cs
namespace BgWorkerExample
{
    public partial class Form1 : Form
{

    //a new instance of a backgroundWorker is created.
    BackgroundWorker bgWorker = new BackgroundWorker();
    
    public Form1()
    {
        InitializeComponent();

        prgProgressBar.Step = 1;

        //this assigns event handlers for the backgroundWorker
        bgWorker.DoWork += bgWorker_DoWork;
        bgWorker.RunWorkerCompleted += bgWorker_WorkComplete;

        //tell the backgroundWorker to raise the "DoWork" event, thus starting it.
        //Check to make sure the background worker is not already running.
        if(!bgWorker.IsBusy)
            bgWorker.RunWorkerAsync();
        
    }

    private void bgWorker_DoWork(object sender, DoWorkEventArgs e)
    {
        //this is the method that the backgroundworker will perform on in the background thread.
        /* One thing to note! A try catch is not necessary as any exceptions will terminate the backgroundWorker and report 
          the error to the "RunWorkerCompleted" event */
        CountToY();    
    }

    private void bgWorker_WorkComplete(object sender, RunWorkerCompletedEventArgs e)
    {
        //e.Error will contain any exceptions caught by the backgroundWorker
        if (e.Error != null)
        {
            MessageBox.Show(e.Error.Message);
        }
        else
        {
            MessageBox.Show("Task Complete!");
            prgProgressBar.Value = 0;
        }
    }

    // example method to perform a "long" running task.
    private void CountToY()
    {
        int x = 0;

        int maxProgress = 100;
        prgProgressBar.Maximum = maxProgress;
        

        while (x < maxProgress)
        {
            System.Threading.Thread.Sleep(50);
            Invoke(new Action(() => { prgProgressBar.PerformStep(); }));
            x += 1;
        }
    }


}

```

**<h3>The result is the following...</h3>**

[<img src="http://i.stack.imgur.com/xGryX.png" alt="enter image description here" />](http://i.stack.imgur.com/xGryX.png)
[<img src="http://i.stack.imgur.com/CRarn.png" alt="enter image description here" />](http://i.stack.imgur.com/CRarn.png)



## Assigning Event Handlers to a BackgroundWorker


Once the instance of the BackgroundWorker has been declared, it must be given properties and event handlers for the tasks it performs.

```cs

   /* This is the backgroundworker's "DoWork" event handler. This 
       method is what will contain all the work you 
       wish to have your program perform without blocking the UI. */

    bgWorker.DoWork += bgWorker_DoWork;


    /*This is how the DoWork event method signature looks like:*/
    private void bgWorker_DoWork(object sender, DoWorkEventArgs e)
    {
        // Work to be done here   
        // ...
        // To get a reference to the current Backgroundworker:
        BackgroundWorker worker = sender as BackgroundWorker;
        // The reference to the BackgroundWorker is often used to report progress
        worker.ReportProgress(...);
    }

    /*This is the method that will be run once the BackgroundWorker has completed its tasks */

    bgWorker.RunWorkerCompleted += bgWorker_CompletedWork;

    /*This is how the RunWorkerCompletedEvent event method signature looks like:*/
    private void bgWorker_CompletedWork(object sender, RunWorkerCompletedEventArgs e)
    {
        // Things to be done after the backgroundworker has finished
    }

   /* When you wish to have something occur when a change in progress 
     occurs, (like the completion of a specific task) the "ProgressChanged" 
     event handler is used. Note that ProgressChanged events may be invoked
     by calls to bgWorker.ReportProgress(...) only if bgWorker.WorkerReportsProgress
     is set to true.  */

     bgWorker.ProgressChanged += bgWorker_ProgressChanged;

    /*This is how the ProgressChanged event method signature looks like:*/
    private void bgWorker_ProgressChanged(object sender, ProgressChangedEventArgs e)
    {
        // Things to be done when a progress change has been reported

        /* The ProgressChangedEventArgs gives access to a percentage,
         allowing for easy reporting of how far along a process is*/
        int progress = e.ProgressPercentage;
    }

```



## Creating a new BackgroundWorker instance


A BackgroundWorker is commonly used to perform tasks, sometimes time consuming, without blocking the UI thread.

```cs
// BackgroundWorker is part of the ComponentModel namespace.
using System.ComponentModel;

namespace BGWorkerExample 
{
     public partial class ExampleForm : Form 
     {

      // the following creates an instance of the BackgroundWorker named "bgWorker"
      BackgroundWorker bgWorker = new BackgroundWorker();

      public ExampleForm() { ...

```



## Assigning Properties to a BackgroundWorker


This allows the BackgroundWorker to be cancelled in between tasks

```cs
bgWorker.WorkerSupportsCancellation = true;

```

This allows the worker to report progress between completion of tasks...

```cs
bgWorker.WorkerReportsProgress = true;

//this must also be used in conjunction with the ProgressChanged event

```



#### Syntax


<li>
`bgWorker.CancellationPending //returns whether the bgWorker was cancelled during its operation`
</li>
<li>
`bgWorker.IsBusy //returns true if the bgWorker is in the middle of an operation`
</li>
<li>
`bgWorker.ReportProgress(int x) //Reports a change in progress. Raises the "ProgressChanged" event`
</li>
<li>
`bgWorker.RunWorkerAsync() //Starts the BackgroundWorker by raising the "DoWork" event`
</li>
<li>
`bgWorker.CancelAsync() //instructs the BackgroundWorker to stop after the completion of a task.`
</li>



#### Remarks


Performing long-running operations within the UI thread can cause your application to become unresponsive, appearing to the user that it has stopped working.  It is preferred that these tasks be run on a background thread.  Once complete, the UI can be updated.

Making changes to the UI during the BackgroundWorker's operation requires invoking the changes to the UI thread, typically by using the [Control.Invoke](https://msdn.microsoft.com/en-us/library/system.windows.forms.control.invoke(v=vs.110).aspx) method on the control you are updating.  Neglecting to do so will cause your program to throw an exception.

The BackgroundWorker is typically only used in Windows Forms applications.  In WPF applications, [Tasks](https://msdn.microsoft.com/en-us/library/system.threading.tasks.task(v=vs.110).aspx) are used to offload work onto background threads (possibly in combination with [async/await](https://msdn.microsoft.com/en-us/library/mt674882.aspx)).  Marshalling updates onto the UI thread is typically done  automatically, when the property being updated implements [INotifyPropertyChanged](https://msdn.microsoft.com/en-us/library/windows/apps/windows.ui.xaml.data.inotifypropertychanged.aspx), or manually by using the UI thread's [Dispatcher](https://msdn.microsoft.com/en-us/library/system.windows.threading.dispatcher(v=vs.110).aspx).

