---
metaTitle: "Memory management"
description: "Use SafeHandle when wrapping unmanaged resources, Unmanaged Resources"
---

# Memory management



## Use SafeHandle when wrapping unmanaged resources


When writing wrappers for unmanaged resources, you should subclass `SafeHandle` rather than trying to implement `IDisposable` and a finalizer yourself. Your `SafeHandle` subclass should be as small and simple as possible to minimize the chance of a handle leak. This likely means that your SafeHandle implementation would an internal implementation detail of a class which wraps it to provide a usable API. This class ensures that, even if a program leaks your `SafeHandle` instance, your unmanaged handle is released.

```dotnet
using System.Runtime.InteropServices;

class MyHandle : SafeHandle
{
    public override bool IsInvalid => handle == IntPtr.Zero;
    public MyHandle() : base(IntPtr.Zero, true)
    { }

    public MyHandle(int length) : this()
    {
        SetHandle(Marshal.AllocHGlobal(length));
    }

    protected override bool ReleaseHandle()
    {
        Marshal.FreeHGlobal(handle);
        return true;
    }
}

```

Disclaimer: This example is an attempt to show how to guard a managed resource with `SafeHandle` which implements `IDisposable` for you and configures finalizers appropriately. It is very contrived and likely pointless to allocate a chunk of memory in this manner.



## Unmanaged Resources


When we talk about the GC and the "heap", we're really talking about what's called the **managed heap**. Objects on the **managed heap** can access resources not on the managed heap, for example, when writing to or reading from a file. Unexpected behavior can occur when, a file is opened for reading and then an exception occurs, preventing the file handle from closing as it normally would. For this reason, .NET requires that unmanaged resources implement the `IDisposable` interface. This interface has a single method called `Dispose` with no parameters:

```dotnet
public interface IDisposable
{
    Dispose();
} 

```

When handling unmanaged resources, you should make sure that they are properly disposed. You can do this by explicitly calling `Dispose()` in a `finally` block, or with a `using` statement.

```dotnet
StreamReader sr; 
string textFromFile;
string filename = "SomeFile.txt";
try 
{
    sr = new StreamReader(filename);
    textFromFile = sr.ReadToEnd();
}
finally
{
    if (sr != null) sr.Dispose();
}

```

or

```dotnet
string textFromFile;
string filename = "SomeFile.txt";

using (StreamReader sr = new Streamreader(filename))
{
    textFromFile = sr.ReadToEnd();
}

```

The latter is the preferred method, and is automatically expanded to the former during compilation.



#### Remarks


Performance-critical applications in managed .NET applications can be severely impacted by the GC. When the GC runs, all other threads are suspended until it completes. For this reason, it is recommended to carefully evaluate the GC processes and determine how to minimize when it runs.

