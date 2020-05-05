---
metaTitle: "C# | Diagnostics"
description: "Redirecting log output with TraceListeners, Debug.WriteLine"
---

# Diagnostics



## Redirecting log output with TraceListeners


You can redirect the debug output to a text file by adding a TextWriterTraceListener to the Debug.Listeners collection.

```cs
public static void Main(string[] args)
{
    TextWriterTraceListener myWriter = new TextWriterTraceListener(@"debug.txt");
    Debug.Listeners.Add(myWriter);
    Debug.WriteLine("Hello");

    myWriter.Flush();
}

```

You can redirect the debug output to a console application's out stream using a ConsoleTraceListener.

```cs
public static void Main(string[] args)
{
    ConsoleTraceListener myWriter = new ConsoleTraceListener();
    Debug.Listeners.Add(myWriter);
    Debug.WriteLine("Hello");
}

```



## Debug.WriteLine


Writes to the trace listeners in the Listeners collection when the application is compiled in debug configuration.

```cs
public static void Main(string[] args)
{
    Debug.WriteLine("Hello");
}

```

In Visual Studio or Xamarin Studio this will appear in the Application Output window. This is due to the presence of the [default trace listener](https://msdn.microsoft.com/en-us/library/system.diagnostics.defaulttracelistener(v=vs.110).aspx) in the TraceListenerCollection.

