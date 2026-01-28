---
metaTitle: "C# | Read & Understand Stacktraces"
description: "Stack trace for a simple NullReferenceException in Windows Forms"
---

# Read & Understand Stacktraces


A stack trace is a great aid when debugging a program. You will get a stack trace when your program throws an Exception, and sometimes when the program terminates abnormally.



## Stack trace for a simple NullReferenceException in Windows Forms


Let's create a small piece of code that throws an exception:

```cs
private void button1_Click(object sender, EventArgs e)
{
    string msg = null;
    msg.ToCharArray();
}

```

If we execute this, we get the following Exception and stack trace:

```cs
System.NullReferenceException: "Object reference not set to an instance of an object."
   at WindowsFormsApplication1.Form1.button1_Click(Object sender, EventArgs e) in F:\WindowsFormsApplication1\WindowsFormsApplication1\Form1.cs:line 29
   at System.Windows.Forms.Control.OnClick(EventArgs e)
   at System.Windows.Forms.Button.OnClick(EventArgs e)
   at System.Windows.Forms.Button.OnMouseUp(MouseEventArgs mevent)

```

The stack trace goes on like that, but this part will suffice for our purposes.

At the top of the stack trace we see the line:

> 
at WindowsFormsApplication1.Form1.button1_Click(Object sender, EventArgs e) in F:\WindowsFormsApplication1\WindowsFormsApplication1\Form1.cs:line 29


This is the most important part. It tells us the **exact** line where the Exception occurred: line 29 in Form1.cs .<br />
So, this is where you begin your search.

The second line is

> 
at System.Windows.Forms.Control.OnClick(EventArgs e)


This is the method that called `button1_Click`. So now we know that `button1_Click`, where the error occurred, was called from `System.Windows.Forms.Control.OnClick`.

We can continue like this; the third line is

> 
at System.Windows.Forms.Button.OnClick(EventArgs e)


This is, in turn, the code that called `System.windows.Forms.Control.OnClick`.

The stack trace is the list of functions that was called until your code encountered the Exception.
And by following this, you can figure out which execution path your code followed until it ran into trouble!

Note that the stack trace includes calls from the .Net system; you don't normally need to follow all Microsofts `System.Windows.Forms` code to find out what went wrong, only the code that belongs to your own application.

So, why is this called a "stack trace"?<br />
Because, every time a program calls a method, it keeps track of where it was. It has a data structure called the "stack", where it dumps its last location.<br />
If it is done executing the method, it looks on the stack to see where it was before it called the method - and continues from there.

So the stack lets the computer know where it left off, before calling a new method.

But it also serves as a debugging help. Like a detective tracing the steps that a criminal took when committing their crime, a programmer can use the stack to trace the steps a program took before it crashed.

