---
metaTitle: ".NET Framework - Garbage Collection"
description: "A basic example of (garbage) collection, Live objects and dead objects - the basics, Multiple dead objects, Weak References, Dispose() vs. finalizers, Proper disposal and finalization of objects"
---

# Garbage Collection


In .Net, objects created with new() are allocated on the managed heap. These objects are never explicitly finalized by the program that uses them; instead, this process is controlled by the .Net Garbage Collector.

Some of the examples below are "lab cases" to show the Garbage Collector at work and some significant details of its behavior, while other focus on how to prepare classes for proper handling by the Garbage Collector.



## A basic example of (garbage) collection


Given the following class:

```dotnet
public class FinalizableObject 
{
    public FinalizableObject()
    {
        Console.WriteLine("Instance initialized");
    }

    ~FinalizableObject()
    {
        Console.WriteLine("Instance finalized");
    }
}

```

A program that creates an instance, even without using it:

```dotnet
new FinalizableObject(); // Object instantiated, ready to be used

```

Produces the following output:

```dotnet
<namespace>.FinalizableObject initialized

```

If nothing else happens, the object is not finalized until the program ends (which frees all objects on the managed heap, finalizing these in the process).

It is possible to force the Garbage Collector to run at a given point, as follows:

```dotnet
new FinalizableObject(); // Object instantiated, ready to be used
GC.Collect();

```

Which produces the following result:

```dotnet
<namespace>.FinalizableObject initialized
<namespace>.FinalizableObject finalized

```

This time, as soon as the Garbage Collector was invoked, the unused (aka "dead") object was finalized and freed from the managed heap.



## Live objects and dead objects - the basics


Rule of thumb: when garbage collection occurs, "live objects" are those still in use, while "dead objects" are those no longer used (any variable or field referencing them, if any, has gone out of scope before the collection occurs).

In the following example (for convenience, FinalizableObject1 and FinalizableObject2 are subclasses of FinalizableObject from the example above and thus inherit the initialization / finalization message behavior):

```dotnet
var obj1 = new FinalizableObject1(); // Finalizable1 instance allocated here
var obj2 = new FinalizableObject2(); // Finalizable2 instance allocated here
obj1 = null; // No more references to the Finalizable1 instance 
GC.Collect();

```

The output will be:

```dotnet
<namespace>.FinalizableObject1 initialized
<namespace>.FinalizableObject2 initialized
<namespace>.FinalizableObject1 finalized

```

At the time when the Garbage Collector is invoked, FinalizableObject1 is a dead object and gets finalized, while FinalizableObject2 is a live object and it is kept on the managed heap.



## Multiple dead objects


What if two (or several) otherwise dead objects reference one another? This is shown in the example below, supposing that OtherObject is a public property of FinalizableObject:

```dotnet
var obj1 = new FinalizableObject1(); 
var obj2 = new FinalizableObject2();
obj1.OtherObject = obj2;
obj2.OtherObject = obj1;
obj1 = null; // Program no longer references Finalizable1 instance
obj2 = null; // Program no longer references Finalizable2 instance
// But the two objects still reference each other
GC.Collect();

```

This produces the following output:

```dotnet
<namespace>.FinalizedObject1 initialized
<namespace>.FinalizedObject2 initialized
<namespace>.FinalizedObject1 finalized
<namespace>.FinalizedObject2 finalized

```

The two objects are finalized and freed from the managed heap despite referencing each other (because no other reference exists to any of them from an actually live object).



## Weak References


Weak references are... references, to other objects (aka "targets"), but "weak" as they do not prevent those objects from being garbage-collected. In other words, weak references do not count when the Garbage Collector evaluates objects as "live" or "dead".

The following code:

```dotnet
var weak = new WeakReference<FinalizableObject>(new FinalizableObject());
GC.Collect();

```

Produces the output:

```dotnet
<namespace>.FinalizableObject initialized
<namespace>.FinalizableObject finalized

```

The object is freed from the managed heap despite being referenced by the WeakReference variable (still in scope when the Garbage collector was invoked).

Consequence #1: at any time, it is unsafe to assume whether a WeakReference target is still allocated on the managed heap or not.

Consequence #2: whenever a program needs to access the target of a Weakreference, code should be provided for both cases, of the target being still allocated or not. The method to access the target is TryGetTarget:

```dotnet
var target = new object(); // Any object will do as target
var weak = new WeakReference<object>(target); // Create weak reference
target = null; // Drop strong reference to the target

// ... Many things may happen in-between

// Check whether the target is still available
if(weak.TryGetTarget(out target))
{
    // Use re-initialized target variable
    // To do whatever the target is needed for
}
else
{
    // Do something when there is no more target object
    // The target variable value should not be used here
}

```

The generic version of WeakReference is available since .Net 4.5. All framework versions provide a non-generic, untyped version that is built in the same way and checked as follows:

```dotnet
var target = new object(); // Any object will do as target
var weak = new WeakReference(target); // Create weak reference
target = null; // Drop strong reference to the target

// ... Many things may happen in-between

// Check whether the target is still available
if (weak.IsAlive)
{
    target = weak.Target;

    // Use re-initialized target variable
    // To do whatever the target is needed for
}
else
{
    // Do something when there is no more target object
    // The target variable value should not be used here
}

```



## Dispose() vs. finalizers


Implement Dispose() method (and declare the containing class as IDisposable) as a means to ensure any memory-heavy resources are freed as soon as the object is no longer used. The "catch" is that there is no strong guarantee the the Dispose() method would ever be invoked (unlike finalizers that always get invoked at the end of the life of the object).

One scenario is a program calling Dispose() on objects it explicitly creates:

```dotnet
private void SomeFunction()
{
    // Initialize an object that uses heavy external resources
    var disposableObject = new ClassThatImplementsIDisposable();

    // ... Use that object

    // Dispose as soon as no longer used
    disposableObject.Dispose();

    // ... Do other stuff 

    // The disposableObject variable gets out of scope here
    // The object will be finalized later on (no guarantee when)
    // But it no longer holds to the heavy external resource after it was disposed
}

```

Another scenario is declaring a class to be instantiated by the framework. In this case the new class usually inherits a base class, for instance in MVC one creates a controller class as a subclass of System.Web.Mvc.ControllerBase. When the base class implements IDisposable interface, this is a good hint that Dispose() would be invoked properly by the framework - but again there is no strong guarantee.

Thus Dispose() is not a substitute for a finalizer; instead, the two should be used for different purposes:

- A finalizer eventually frees resources to avoid memory leaks that would occur otherwise
- Dispose() frees resources (possibly the same ones) as soon as these are no longer needed, to ease pressure on overall memory allocation.



## Proper disposal and finalization of objects


As Dispose() and finalizers are aimed to different purposes, a class managing external memory-heavy resources should implement both of them. The consequence is writing the class so that it handles well two possible scenarios:

- When only the finalizer is invoked
- When Dispose() is invoked first and later the finalizer is invoked as well

One solution is writing the cleanup code in such a way that running it once or twice would produce the same result as running it only once. Feasibility depends on the nature of the cleanup, for instance:

- Closing an already closed database connection would probably have no effect so it works
- Updating some "usage count" is dangerous and would produce a wrong result when called twice instead of once.

A safer solution is ensuring by design that the cleanup code is called once and only once whatever the external context. This can be achieved the "classic way" using a dedicated flag:

```dotnet
public class DisposableFinalizable1: IDisposable
{
    private bool disposed = false;

    ~DisposableFinalizable1() { Cleanup(); }

    public void Dispose() { Cleanup(); }

    private void Cleanup()
    {
        if(!disposed)
        {
            // Actual code to release resources gets here, then
            disposed = true;
        }
    }
}

```

Alternately, the Garbage Collector provides a specific method SuppressFinalize() that allows skipping the finalizer after Dispose has been invoked:

```dotnet
public class DisposableFinalizable2 : IDisposable
{
    ~DisposableFinalizable2() { Cleanup(); }

    public void Dispose()
    {
        Cleanup();
        GC.SuppressFinalize(this);
    }

    private void Cleanup()
    {
        // Actual code to release resources gets here
    }
}

```



#### Remarks


The Garbage Collector is aimed to lower the program cost in terms of allocated memory, but doing so has a cost in terms of processing time. In order to achieve a good overall compromise, there are a number of optimizations that should be taken into consideration while programming with the Garbage Collector in mind:

- If the Collect() method is to be explicitly invoked (which should not often be the case anyway), consider using the "optimized" mode which finalizes dead object only when memory is actually needed
- Instead of invoking the Collect() method, consider using the AddMemoryPressure() and RemoveMemoryPressure() methods, which trigger a memory collection only if actually needed
- A memory collection is not guaranteed to finalize all dead objects; instead, the Garbage Collector manages 3 "generations", an object sometimes "surviving" from a generation into the next one
- Several threading models may apply, depending on various factors including setup fine tuning, resulting in different degrees of interference between the Garbage Collector thread and the other application thread(s)

