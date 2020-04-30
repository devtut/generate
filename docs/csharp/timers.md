---
metaTitle: "Timers"
description: "Multithreaded Timers, Creating an Instance of a Timer, Assigning the Tick event handler to a Timer, Example: Using a Timer to perform a simple countdown."
---

# Timers



## Multithreaded Timers


`System.Threading.Timer` - Simplest multithreaded timer. Contains two methods and one constructor.

Example:
A timer calls the DataWrite method, which writes "multithread executed..." after five
seconds have elapsed, and then every second after that until the user presses Enter:

```cs
using System;
using System.Threading;
class Program
{
  static void Main()
  {
    // First interval = 5000ms; subsequent intervals = 1000ms
    Timer timer = new Timer (DataWrite, "multithread executed...", 5000, 1000);
    Console.ReadLine();
    timer.Dispose(); // This both stops the timer and cleans up.
  }

  static void DataWrite (object data)
  {
    // This runs on a pooled thread
    Console.WriteLine (data); // Writes "multithread executed..."
  }
}

```

Note : Will post a separate section for disposing multithreaded timers.

`Change` - This method can be called when you would like change the timer interval.

`Timeout.Infinite` - If you want to fire just once. Specify this in the last argument of the constructor.

`System.Timers` - Another timer class provided by .NET Framework. It wraps the `System.Threading.Timer`.

### Features:

- `IComponent` - Allowing it to be sited in the Visual Studio’s Designer’s component tray
- `Interval` property instead of a `Change` method
- `Elapsed` `event` instead of a callback `delegate`
- `Enabled` property to start and stop the timer (`default value = false`)
- `Start` & `Stop` methods in case if you get confused by `Enabled` property (above point)
- `AutoReset` - for indicating a recurring event (`default value = true`)
<li>`SynchronizingObject` property with `Invoke` and `BeginInvoke` methods for
safely calling methods on WPF elements and Windows Forms controls</li>

Example representing all the above features:

```cs
using System;
using System.Timers; // Timers namespace rather than Threading
class SystemTimer
{
  static void Main()
  {
    Timer timer = new Timer(); // Doesn't require any args
    timer.Interval = 500;
    timer.Elapsed += timer_Elapsed; // Uses an event instead of a delegate
    timer.Start(); // Start the timer
    Console.ReadLine();
    timer.Stop(); // Stop the timer
    Console.ReadLine();
    timer.Start(); // Restart the timer
    Console.ReadLine();
    timer.Dispose(); // Permanently stop the timer
 }

 static void timer_Elapsed(object sender, EventArgs e)
 {
   Console.WriteLine ("Tick");
 }
}

```

`Multithreaded timers` - use the thread pool to allow a few threads to serve many
timers. It means that callback method or `Elapsed` event may trigger on a different
thread each time it is called.

`Elapsed` - this event always fires on time—regardless of whether the previous `Elapsed` event finished executing. Because of this, callbacks or event handlers must be thread-safe.
The accuracy of multithreaded timers depends on the OS, and is typically
in the 10–20 ms.

`interop` - when ever you need greater accuracy use this and call the Windows multimedia timer. This has accuracy down to 1 ms and it is defined in `winmm.dll`.

`timeBeginPeriod` - Call this first to inform OS that you need high timing accuracy

`timeSetEvent` - call this after `timeBeginPeriod` to start a multimedia timer.

`timeKillEvent` - call this when you are done, this stops the timer

`timeEndPeriod` - Call this to inform the OS that you no longer need high timing accuracy.

You can find complete examples on the Internet that use the multimedia timer by searching for the keywords `dllimport` `winmm.dll` `timesetevent`.



## Creating an Instance of a Timer


Timers are used to perform tasks at specific intervals of time (Do X every Y seconds)
Below is an example of creating a new instance of a Timer.

**NOTE**: This applies to Timers using WinForms. If using WPF, you may want to look into `DispatcherTimer`

```cs

   using System.Windows.Forms; //Timers use the Windows.Forms namespace

    public partial class Form1 : Form
    {

        Timer myTimer = new Timer(); //create an instance of Timer named myTimer

    
        public Form1()
        {
            InitializeComponent();
        }

    }

```



## Assigning the "Tick" event handler to a Timer


All actions performed in a timer are handled in the "Tick" event.

```cs
public partial class Form1 : Form
{

    Timer myTimer = new Timer();

    
    public Form1()
    {
        InitializeComponent();

        myTimer.Tick += myTimer_Tick; //assign the event handler named "myTimer_Tick"
    }

    private void myTimer_Tick(object sender, EventArgs e)
    {
        // Perform your actions here.
    }
}

```



## Example: Using a Timer to perform a simple countdown.


```cs

   public partial class Form1 : Form
    {

    Timer myTimer = new Timer();
    int timeLeft = 10;
    
        public Form1()
        {
            InitializeComponent();

            //set properties for the Timer
            myTimer.Interval = 1000;
            myTimer.Enabled = true;

            //Set the event handler for the timer, named "myTimer_Tick"
            myTimer.Tick += myTimer_Tick;

            //Start the timer as soon as the form is loaded
            myTimer.Start();

            //Show the time set in the "timeLeft" variable
            lblCountDown.Text = timeLeft.ToString();

        }

        private void myTimer_Tick(object sender, EventArgs e)
        {
            //perform these actions at the interval set in the properties.
            lblCountDown.Text = timeLeft.ToString();
            timeLeft -= 1;

            if (timeLeft < 0)
            {
                myTimer.Stop();
            }
        }
    }

```

Results in...

[<img src="http://i.stack.imgur.com/VZlnr.png" alt="enter image description here" />](http://i.stack.imgur.com/VZlnr.png)[<img src="http://i.stack.imgur.com/30t8F.png" alt="enter image description here" />](http://i.stack.imgur.com/30t8F.png)

And so on...



#### Syntax


- `myTimer.Interval` -  sets how often the "Tick" event is called (in milliseconds)
- `myTimer.Enabled` - boolean value that sets the timer to be enabled / disabled
- `myTimer.Start()` - Starts the timer.
- `myTimer.Stop()` - Stops the timer.



#### Remarks


If using Visual Studio, Timers can be added as a control directly to your form from the toolbox.

