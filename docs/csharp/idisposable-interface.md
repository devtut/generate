---
metaTitle: "IDisposable interface"
description: "In a class that contains only managed resources, In a class with managed and unmanaged resources, IDisposable, Dispose, using keyword, In an inherited class with managed resources"
---

# IDisposable interface



## In a class that contains only managed resources


Managed resources are resources that the runtime's garbage collector is aware and under control of. There are many classes available in the BCL, for example, such as a `SqlConnection` that is a wrapper class for an unmanaged resource. These classes already implement the `IDisposable` interface -- it's up to your code to clean them up when you are done.

It's not necessary to implement a finalizer if your class only contains managed resources.

```cs
public class ObjectWithManagedResourcesOnly : IDisposable
{
    private SqlConnection sqlConnection = new SqlConnection();

    public void Dispose()
    {
        sqlConnection.Dispose();
    }
}

```



## In a class with managed and unmanaged resources


It's important to let finalization ignore managed resources. The finalizer runs on another thread -- it's possible that the managed objects don't exist anymore by the time the finalizer runs. Implementing a protected `Dispose(bool)` method is a common practice to ensure managed resources do not have their `Dispose` method called from a finalizer.

```cs
public class ManagedAndUnmanagedObject : IDisposable
{
    private SqlConnection sqlConnection = new SqlConnection();
    private UnmanagedHandle unmanagedHandle = Win32.SomeUnmanagedResource();
    private bool disposed;

    public void Dispose()
    {
        Dispose(true); // client called dispose
        GC.SuppressFinalize(this); // tell the GC to not execute the Finalizer
    }

    protected virtual void Dispose(bool disposeManaged)
    {
        if (!disposed)
        {
            if (disposeManaged)
            {
                if (sqlConnection != null)
                {
                    sqlConnection.Dispose();
                }
            }

            unmanagedHandle.Release();

            disposed = true;
        }
    }

    ~ManagedAndUnmanagedObject()
    {
        Dispose(false);
    }
}

```



## IDisposable, Dispose


.NET Framework defines a interface for types requiring a tear-down method:

```cs
public interface IDisposable
{
  void Dispose();
}

```

`Dispose()` is primarily used for cleaning up resources, like unmanaged references. However, it can also be useful to force the disposing of other resources even though they are managed. Instead of waiting for the GC to eventually also clean up your database connection, you can make sure it's done in your own `Dispose()` implementation.

```cs
public void Dispose()
{
   if (null != this.CurrentDatabaseConnection)
   {
       this.CurrentDatabaseConnection.Dispose();
       this.CurrentDatabaseConnection = null;
   }
}

```

When you need to directly access unmanaged resources such as unmanaged pointers or win32 resources, create a class inheriting from `SafeHandle` and use that class’s conventions/tools to do so.



## using keyword


When an object implements the `IDisposable` interface, it can be created within the `using` syntax:

```cs
using (var foo = new Foo())
{
    // do foo stuff
} // when it reaches here foo.Dispose() will get called

public class Foo : IDisposable
{
    public void Dispose()
    {
        Console.WriteLine("dispose called");
    }
}

```

[View demo](https://dotnetfiddle.net/StEPc2)

`using` is [syntatic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for a `try/finally` block; the above usage would  roughly translate into:

```cs
{
    var foo = new Foo();
    try
    {
        // do foo stuff
    }
    finally
    {
        if (foo != null)
            ((IDisposable)foo).Dispose();
    }
}

```



## In an inherited class with managed resources


It's fairly common that you may create a class that implements `IDisposable`, and then derive classes that also contain managed resources. It is recommendeded to mark the `Dispose` method with the `virtual` keyword so that clients have the ability to cleanup any resources they may own.

```cs
public class Parent : IDisposable
{
    private ManagedResource parentManagedResource = new ManagedResource();

    public virtual void Dispose()
    {
        if (parentManagedResource != null)
        {
            parentManagedResource.Dispose();
        }
    }
}

public class Child : Parent
{
    private ManagedResource childManagedResource = new ManagedResource();

    public override void Dispose()
    {
        if (childManagedResource != null)
        {
            childManagedResource.Dispose();
        }
        //clean up the parent's resources
        base.Dispose();
    }
}

```



#### Remarks


<li>
It's up to clients of the class implementing `IDisposable` to make sure they call the `Dispose` method when they are finished using the object. There is nothing in the CLR that directly searches objects for a `Dispose` method to invoke.
</li>
<li>
It's not necessary to implement a finalizer if your object only contains managed resources. Be sure to call `Dispose` on all of the objects that your class uses when you implement your own `Dispose` method.
</li>
<li>
It's recommended to make the class safe against multiple calls to `Dispose`, although it should ideally be called only once. This can be achieved by adding a `private bool` variable to your class and setting the value to `true` when the `Dispose` method has run.
</li>

