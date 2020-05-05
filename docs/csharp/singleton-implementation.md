---
metaTitle: "C# | Singleton Implementation"
description: "Statically Initialized Singleton, Lazy, thread-safe Singleton (using Lazy<T>), Lazy, thread-safe Singleton (using Double Checked Locking), Lazy, thread safe singleton (for .NET 3.5 or older, alternate implementation), Disposing of the Singleton instance when it is no longer needed"
---

# Singleton Implementation




## Statically Initialized Singleton


```cs
public class Singleton
{
    private readonly static Singleton instance = new Singleton();
    private Singleton() { }
    public static Singleton Instance => instance;
}

```

This implementation is thread-safe because in this case `instance` object is initialized in the static constructor. The CLR already ensures that all static constructors are executed thread-safe.

Mutating `instance` is not a thread-safe operation, therefore the `readonly` attribute guarantees immutability after initialization.



## Lazy, thread-safe Singleton (using Lazy<T>)


.Net 4.0 type Lazy guarantees thread-safe object initialization, so this type could be used to make Singletons.

```cs
public class LazySingleton
{
    private static readonly Lazy<LazySingleton> _instance =
        new Lazy<LazySingleton>(() => new LazySingleton());
 
    public static LazySingleton Instance
    {
        get { return _instance.Value; }
    }

    private LazySingleton() { }
}

```

Using `Lazy<T>` will make sure that the object is only instantiated when it is used somewhere in the calling code.

A simple usage will be like:

```cs
using System;
                    
public class Program
{
    public static void Main()
    {
        var instance = LazySingleton.Instance;
    }
}

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/oHVpK3)



## Lazy, thread-safe Singleton (using Double Checked Locking)


This thread-safe version of a singleton was necessary in the early versions of .NET where `static` initialization was not guaranteed to be thread-safe. In more modern versions of the framework a [statically initialized singleton](http://stackoverflow.com/documentation/c%23/1192/singleton-implementation/3863/statically-initialized-singleton) is usually preferred because it is very easy to make implementation mistakes in the following pattern.

```cs
public sealed class ThreadSafeSingleton
{
   private static volatile ThreadSafeSingleton instance;
   private static object lockObject = new Object();

   private ThreadSafeSingleton()
   {
   }

   public static ThreadSafeSingleton Instance
   {
      get 
      {
         if (instance == null) 
         {
            lock (lockObject) 
            {
               if (instance == null)
               {
                  instance = new ThreadSafeSingleton();
               }
            }
         }

         return instance;
      }
   }
}

```

Notice that the `if (instance == null)` check is done twice: once before the lock is acquired, and once afterwards. This implementation would still be thread-safe even without the first null check. However, that would mean that a lock would be acquired **every time** the instance is requested, and that would cause performance to suffer. The first null check is added so that the lock is not acquired unless it's necessary. The second null check makes sure that only the first thread to acquire the lock then creates the instance. The other threads will find the instance to be populated and skip ahead.



## Lazy, thread safe singleton (for .NET 3.5 or older, alternate implementation)


Because in .NET 3.5 and older you don't have [`Lazy<T>`](https://msdn.microsoft.com/en-us/library/dd642331(v=vs.110).aspx) class you use the following pattern:

```cs
public class Singleton
{
    private Singleton() // prevents public instantiation
    {
    }

    public static Singleton Instance
    {
        get
        {
            return Nested.instance;
        }
    }
    
    private class Nested
    {
        // Explicit static constructor to tell C# compiler
        // not to mark type as beforefieldinit
        static Nested()
        {
        }

        internal static readonly Singleton instance = new Singleton();
    }
}

```

This is inspired from [Jon Skeet's blog post](http://www.yoda.arachsys.com/csharp/singleton.html).

Because the `Nested` class is nested and private the instantiation of the singleton instance will not be triggered by accessing other members of the `Sigleton` class (such as a public readonly property, for example).



## Disposing of the Singleton instance when it is no longer needed


Most examples show instantiating and holding a `LazySingleton` object until the owning application has terminated, even if that object is no longer needed by the application. A solution to this is to implement `IDisposable` and set the object instance to null as follows:

```cs
public class LazySingleton : IDisposable
{
    private static volatile Lazy<LazySingleton> _instance;
    private static volatile int _instanceCount = 0;
    private bool _alreadyDisposed = false;

public static LazySingleton Instance
{
    get
    {
        if (_instance == null)
            _instance = new Lazy<LazySingleton>(() => new LazySingleton());
        _instanceCount++;
        return _instance.Value;
    }
}

private LazySingleton() { }

// Public implementation of Dispose pattern callable by consumers.
public void Dispose()
{ 
    if (--_instanceCount == 0) // No more references to this object.
    {       
       Dispose(true);
       GC.SuppressFinalize(this);           
    }
}

// Protected implementation of Dispose pattern.
protected virtual void Dispose(bool disposing)
{
    if (_alreadyDisposed) return; 
  
    if (disposing) 
    {
        _instance = null; // Allow GC to dispose of this instance.
        // Free any other managed objects here.
    }
  
    // Free any unmanaged objects here.
    _alreadyDisposed = true;
}

```

The above code disposes of the instance prior to application termination but only if consumers call `Dispose()` on the object after every use. Since there is no guarantee that this will happen or a way to force it, there is also no guarantee that the instance will ever be disposed. But if this class is being used internally then it's easier to ensure that the `Dispose()` method is being called after each use. An example follows:

```cs
public class Program
{
    public static void Main()
    {
        using (var instance = LazySingleton.Instance)
        {
            // Do work with instance
        }
    }
}

```

Please note that this example is **not thread-safe**.

