---
metaTitle: "Visual Basic .NET - Threading"
description: "Performing thread-safe calls using Control.Invoke(), Performing thread-safe calls using Async/Await"
---

# Threading




## Performing thread-safe calls using Control.Invoke()


Using the `Control.Invoke()` method you may move the execution of a method or function from a background thread to the thread that the control was created on, which is usually the UI (User Interface) thread. By doing so your code will be queued to run on the control's thread instead, which removes the possibility of concurrency.

The `Control.InvokeRequired` property should also be checked in order to determine whether you need to invoke, or if the code is already running on the same thread as the control.

The `Invoke()` method takes a delegate as its first parameter. A delegate holds the reference, parameter list and return type to another method.

In Visual Basic 2010 (10.0) or higher, **lambda expressions** can be used to create a delegate method on the fly:

```vb
If LogTextBox.InvokeRequired = True Then
    LogTextBox.Invoke(Sub() LogTextBox.AppendText("Check passed"))
Else
    LogTextBox.AppendText("Check passed")
End If

```

Whereas in Visual Basic 2008 (9.0) or lower, you have to declare the delegate on your own:

```vb
Delegate Sub AddLogText(ByVal Text As String)

If LogTextBox.InvokeRequired = True Then
    LogTextBox.Invoke(New AddLogText(AddressOf UpdateLog), "Check passed")
Else
    UpdateLog("Check passed")
End If

Sub UpdateLog(ByVal Text As String)
    LogTextBox.AppendText(Text)
End Sub

```



## Performing thread-safe calls using Async/Await


If we try to change an object on the UI thread from a different thread we will get a cross-thread operation exception:

```vb
Private Sub Button_Click(sender As Object, e As EventArgs) Handles MyButton.Click
    ' Cross thread-operation exception as the assignment is executed on a different thread
    ' from the UI one:
    Task.Run(Sub() MyButton.Text = Thread.CurrentThread.ManagedThreadId)
End Sub

```

Before **VB 14.0** and **.NET 4.5** the solution was invoking the assignment on and object living on the UI thread:

```vb
Private Sub Button_Click(sender As Object, e As EventArgs) Handles MyButton.Click
    ' This will run the conde on the UI thread:
    MyButton.Invoke(Sub() MyButton.Text = Thread.CurrentThread.ManagedThreadId)
End Sub

```

With **VB 14.0**, we can run a `Task` on a different thread and then have the context restored once the execution is complete and then perform the assignment with Async/Await:

```vb
Private Async Sub Button_Click(sender As Object, e As EventArgs) Handles MyButton.Click
    ' This will run the code on a different thread then the context is restored
    ' so the assignment happens on the UI thread:
    MyButton.Text = Await Task.Run(Function() Thread.CurrentThread.ManagedThreadId)
End Sub

```

