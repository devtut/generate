---
metaTitle: ".NET Framework - VB Forms"
description: "Hello World in VB.NET Forms, For Beginners, Forms Timer"
---

# VB Forms



## Hello World in VB.NET Forms


To show a message box when the form has been shown:

```dotnet
Public Class Form1
    Private Sub Form1_Shown(sender As Object, e As EventArgs) Handles MyBase.Shown
        MessageBox.Show("Hello, World!")
    End Sub
End Class
To show a message box before the form has been shown:

Public Class Form1
    Private Sub Form1_Load(sender As Object, e As EventArgs) Handles MyBase.Load
        MessageBox.Show("Hello, World!")
    End Sub
End Class

```

Load() will be called first, and only once, when the form first loads. Show() will be called every time the user launches the form. Activate() will be called every time the user makes the form active.

Load() will execute before Show() is called, but be warned: calling msgBox() in show can cause that msgBox() to execute before Load() is finished. **It is generally a bad idea to depend on event ordering between Load(), Show(), and similar.**



## For Beginners


Some things all beginners should know / do that will help them have a good start with VB .Net:

Set the following Options:

```dotnet
'can be permanently set
' Tools / Options / Projects and Soluntions / VB Defaults
Option Strict On
Option Explicit On
Option Infer Off

Public Class Form1

End Class

```

[Use &, not + for string concatenation.](https://msdn.microsoft.com/en-us/library/te2585xw.aspx?f=255&MSPPError=-2147217396)  [Strings](https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx) should be studied in some detail as they are widely used.

Spend some time understanding [Value and Reference Types](https://msdn.microsoft.com/en-us/library/t63sy5hs.aspx).

Never use [Application.DoEvents](https://msdn.microsoft.com/en-us/library/system.windows.forms.application.doevents%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396).  Pay attention to the 'Caution'.  When you reach a point where this seems like something you must use, ask.

The [documentation](https://social.msdn.microsoft.com/Search/en-US?query=vb%20.net&emptyWatermark=true&searchButtonTooltip=Search%20MSDN&ac=2) is your friend.



## Forms Timer


The [Windows.Forms.Timer](https://msdn.microsoft.com/en-us/library/system.windows.forms.timer(v=vs.110).aspx) component can be used to provide the user information that is **not** time critical.  Create a form with one button, one label, and a Timer component.

For example it could be used to show the user the time of day periodically.

```dotnet
'can be permanently set
' Tools / Options / Projects and Soluntions / VB Defaults
Option Strict On
Option Explicit On
Option Infer Off

Public Class Form1

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Button1.Enabled = False
        Timer1.Interval = 60 * 1000 'one minute intervals
        'start timer
        Timer1.Start()
        Label1.Text = DateTime.Now.ToLongTimeString
    End Sub

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        Label1.Text = DateTime.Now.ToLongTimeString
    End Sub
End Class

```

But this timer is not suited for timing.  An example would be using it for a countdown.  In this example we will simulate a countdown to three minutes.  This may very well be one of the most boringly important examples here.

```dotnet
'can be permanently set
' Tools / Options / Projects and Soluntions / VB Defaults
Option Strict On
Option Explicit On
Option Infer Off

Public Class Form1

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Button1.Enabled = False
        ctSecs = 0 'clear count
        Timer1.Interval = 1000 'one second in ms.
        'start timers
        stpw.Reset()
        stpw.Start()
        Timer1.Start()
    End Sub

    Dim stpw As New Stopwatch
    Dim ctSecs As Integer

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        ctSecs += 1
        If ctSecs = 180 Then 'about 2.5 seconds off on my PC!
            'stop timing
            stpw.Stop()
            Timer1.Stop()
            'show actual elapsed time
            'Is it near 180?
            Label1.Text = stpw.Elapsed.TotalSeconds.ToString("n1")
        End If
    End Sub
End Class

```

After button1 is clicked, about three minutes pass and label1 shows the results.  Does label1 show 180?  Probably not.  On my machine it showed 182.5!

The reason for the discrepancy is in the documentation, "The Windows Forms Timer component is single-threaded, and is limited to an accuracy of 55 milliseconds."  This is why it shouldn't be used for timing.

By using the timer and stopwatch a little differently we can obtain better results.

```dotnet
'can be permanently set
' Tools / Options / Projects and Soluntions / VB Defaults
Option Strict On
Option Explicit On
Option Infer Off

Public Class Form1

    Private Sub Button1_Click(sender As Object, e As EventArgs) Handles Button1.Click
        Button1.Enabled = False
        Timer1.Interval = 100 'one tenth of a second in ms.
        'start timers
        stpw.Reset()
        stpw.Start()
        Timer1.Start()
    End Sub

    Dim stpw As New Stopwatch
    Dim threeMinutes As TimeSpan = TimeSpan.FromMinutes(3)

    Private Sub Timer1_Tick(sender As Object, e As EventArgs) Handles Timer1.Tick
        If stpw.Elapsed >= threeMinutes Then '0.1 off on my PC!
            'stop timing
            stpw.Stop()
            Timer1.Stop()
            'show actual elapsed time
            'how close?
            Label1.Text = stpw.Elapsed.TotalSeconds.ToString("n1")
        End If
    End Sub
End Class

```

There are other timers that can be used as needed.  This [search](https://social.msdn.microsoft.com/Search/en-US?query=vb%20.net%20windows%20timers&emptyWatermark=true&searchButtonTooltip=Search%20MSDN&ac=5#refinementChanges=117&pageNumber=1&showMore=false) should help in that regard.

