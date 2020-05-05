---
metaTitle: "C# | Garbage Collector in .Net"
description: "Weak References, Large Object Heap compaction"
---

# Garbage Collector in .Net



## Weak References


In .NET, the GC allocates objects when there are no references left to them.  Therefore, while an object can still be reached from code (there is a strong reference to it), the GC will not allocate this object. This can become a problem if there are a lot of large objects.

A weak reference is a reference, that allows the GC to collect the object while still allowing to access the object. A weak reference is valid only during the indeterminate amount of time until the object is collected when no strong references exist. When you use a weak reference, the application can still obtain a strong reference to the object, which prevents it from being collected. So weak references can be useful for holding on to large objects that are expensive to initialize, but should be available for garbage collection if they are not actively in use.

Simple usage:

```cs
WeakReference reference = new WeakReference(new object(), false);

GC.Collect();

object target = reference.Target;
if (target != null)
  DoSomething(target);

```

So weak references could be used to maintain, for example, a cache of objects. However, it is important to remember that there is always the risk that the garbage collector will get to the object before a strong reference is reestablished.

Weak references are also handy for avoiding memory leaks. A typical use case is with events.

Suppose we have some handler to an event on a source:

```cs
Source.Event += new EventHandler(Handler)

```

This code registers an event handler and creates a strong reference from the event source to the listening object. If the source object has a longer lifetime than the listener, and the listener doesn't need the event anymore when there are no other references to it, using normal .NET events causes a memory leak: the source object holds listener objects in memory that should be garbage collected.

In this case, it may be a good idea is to use the [Weak Event Pattern](https://msdn.microsoft.com/en-us/library/aa970850(v=vs.110).aspx).

Something like:

```cs
public static class WeakEventManager
    {
    public static void SetHandler<S, TArgs>(
    Action<EventHandler<TArgs>> add,
    Action<EventHandler<TArgs>> remove,
    S subscriber,
    Action<S, TArgs> action)
    where TArgs : EventArgs
    where S : class
        {
            var subscrWeakRef = new WeakReference(subscriber);
            EventHandler<TArgs> handler = null;

            handler = (s, e) =>
            {
                var subscrStrongRef = subscrWeakRef.Target as S;
                if (subscrStrongRef != null)
                {
                    action(subscrStrongRef, e);
                }
                else
                {
                    remove(handler);
                    handler = null;
                }
            };

            add(handler);
        }
    }

```

and used like this:

```

EventSource s = new EventSource();
 Subscriber subscriber = new Subscriber();
 WeakEventManager.SetHandler<Subscriber, SomeEventArgs>(a => s.Event += a, r => s.Event -= r, subscriber, (s,e) => { s.HandleEvent(e); });

```

In this case of course we have some restrictions - the event must be a

```cs
public event EventHandler<SomeEventArgs> Event;

```

As [MSDN](https://msdn.microsoft.com/en-us/library/ms404247(v=vs.110).aspx#Anchor_1) suggests:

<li>Use long weak references only when necessary as the state of the
object is unpredictable after finalization.</li>
<li>Avoid using weak references to small objects because the pointer
itself may be as large or larger.</li>
<li>Avoid using weak references as an automatic solution to memory
management problems. Instead, develop an effective caching policy for
handling your application's objects.</li>



## Large Object Heap compaction


By default the Large Object Heap is not compacted unlike the classic Object Heap which [can lead to memory fragmentation](https://www.simple-talk.com/dotnet/.net-framework/the-dangers-of-the-large-object-heap/) and further, can lead to `OutOfMemoryException`s

Starting with .NET 4.5.1 there is [an option](https://msdn.microsoft.com/en-us/library/system.runtime.gcsettings.largeobjectheapcompactionmode(v=vs.110).aspx) to explicitly compact the Large Object Heap (along with a garbage collection):

```cs
GCSettings.LargeObjectHeapCompactionMode = GCLargeObjectHeapCompactionMode.CompactOnce;
GC.Collect();   

```

Just as any explicit garbage collection request (it's called request because the CLR is not forced to conduct it) use with care and by default avoid it if you can since it can de-calibrate `GC`s statistics, decreasing its performance.

