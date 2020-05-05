---
metaTitle: "C# | Lock Statement"
description: "Throwing exception in a lock statement, Simple usage, Return in a lock statement, Anti-Patterns and gotchas, Using instances of Object for lock"
---

# Lock Statement




## Throwing exception in a lock statement


Following code will release the lock. There will be no problem. Behind the scenes lock statement works as `try finally`

```cs
lock(locker)
{
    throw new Exception();
}

```

More can be seen in the [C# 5.0 Specification](https://msdn.microsoft.com/en-us/library/aa664735%28VS.71%29.aspx?f=255&MSPPError=-2147217396):

A `lock` statement of the form

```cs
lock (x) ...

```

where `x` is an expression of a **reference-type**, is precisely equivalent to

```cs
bool __lockWasTaken = false;
try {
    System.Threading.Monitor.Enter(x, ref __lockWasTaken);
    ...
}
finally {
    if (__lockWasTaken) System.Threading.Monitor.Exit(x);
}

```

except that `x` is only evaluated once.



## Simple usage


Common usage of `lock` is a critical section.

In the following example `ReserveRoom` is supposed to be called from different threads. Synchronization with `lock` is the simplest way to prevent race condition here. Method body is surrounded with `lock` which ensures that two or more threads cannot execute it simultaneously.

```cs
public class Hotel
{
    private readonly object _roomLock = new object();

    public void ReserveRoom(int roomNumber)
    {
        // lock keyword ensures that only one thread executes critical section at once
        // in this case, reserves a hotel room of given number
        // preventing double bookings
        lock (_roomLock)
        {
            // reserve room logic goes here
        }
    }
}

```

If a thread reaches `lock`-ed block while another thread is running within it, the former will wait another to exit the block.

> 
<p>Best practice is to define a private object to lock on, or a private
static object variable to protect data common to all instances.</p>




## Return in a lock statement


Following code will release lock.

```cs
lock(locker)
{
    return 5;
}

```

For a detailed explanation, [this SO answer](http://stackoverflow.com/a/266718/1519458) is recommended.



## Anti-Patterns and gotchas


### Locking on an stack-allocated / local variable

One of the fallacies while using `lock` is the usage of local objects as locker in a function. Since these local object instances will differ on each call of the function, `lock` will not perform as expected.

```cs
List<string> stringList = new List<string>();

public void AddToListNotThreadSafe(string something)
{
    // DO NOT do this, as each call to this method 
    // will lock on a different instance of an Object.
    // This provides no thread safety, it only degrades performance.
    var localLock = new Object();
    lock(localLock)
    {
        stringList.Add(something);
    }
}

// Define object that can be used for thread safety in the AddToList method
readonly object classLock = new object();

public void AddToList(List<string> stringList, string something)
{
    // USE THE classLock instance field to achieve a 
    // thread-safe lock before adding to stringList
    lock(classLock)
    {
        stringList.Add(something);
    }
}

```

### Assuming that locking restricts access to the synchronizing object itself

If one thread calls: `lock(obj)` and another thread calls `obj.ToString()` second thread is not going to be blocked.

```cs
object obj = new Object();
 
public void SomeMethod()
{
     lock(obj)
    {
       //do dangerous stuff 
    }
 }

 //Meanwhile on other tread 
 public void SomeOtherMethod()
 {
   var objInString = obj.ToString(); //this does not block
 }

```

### Expecting subclasses to know when to lock

Sometimes base classes are designed such that their subclasses are required to use a lock when accessing certain protected fields:

```cs
public abstract class Base
{
    protected readonly object padlock;
    protected readonly List<string> list;

    public Base()
    {
        this.padlock = new object();
        this.list = new List<string>();
    }

    public abstract void Do();
}

public class Derived1 : Base
{
    public override void Do()
    {
        lock (this.padlock)
        {
            this.list.Add("Derived1");
        }
    }
}

public class Derived2 : Base
{
    public override void Do()
    {
        this.list.Add("Derived2"); // OOPS! I forgot to lock!
    }
}

```

It is much safer to **encapsulate locking** by using a [Template Method](https://en.wikipedia.org/wiki/Template_method_pattern):

```cs
public abstract class Base
{
    private readonly object padlock; // This is now private
    protected readonly List<string> list;

    public Base()
    {
        this.padlock = new object();
        this.list = new List<string>();
    }

    public void Do()
    {
        lock (this.padlock) {
            this.DoInternal();
        }
    }

    protected abstract void DoInternal();
}

public class Derived1 : Base
{
    protected override void DoInternal()
    {
        this.list.Add("Derived1"); // Yay! No need to lock
    }
}

```

### Locking on a boxed ValueType variable does not synchronize

In the following example, a private variable is implicitly boxed as it's supplied as an `object` argument to a function, expecting a monitor resource to lock at.
The boxing occurs just prior to calling the IncInSync function, so the boxed instance corresponds to a different heap object each time the function is called.

```cs
public int Count { get; private set; }

private readonly int counterLock = 1;

public void Inc()
{
    IncInSync(counterLock);
}

private void IncInSync(object monitorResource)
{
    lock (monitorResource)
    {
        Count++;
    }
}

```

Boxing occurs in the `Inc` function:

```cs
BulemicCounter.Inc:
IL_0000:  nop         
IL_0001:  ldarg.0     
IL_0002:  ldarg.0     
IL_0003:  ldfld       UserQuery+BulemicCounter.counterLock
IL_0008:  box         System.Int32**
IL_000D:  call        UserQuery+BulemicCounter.IncInSync
IL_0012:  nop         
IL_0013:  ret         

```

It does not mean that a boxed ValueType can't be used for monitor locking at all:

```cs
private readonly object counterLock = 1;

```

Now boxing occurs in constructor, which is fine for locking:

```cs
IL_0001:  ldc.i4.1    
IL_0002:  box         System.Int32
IL_0007:  stfld       UserQuery+BulemicCounter.counterLock

```

### Using locks unnecessarily when a safer alternative exists

A very common pattern is to use a private `List` or `Dictionary` in a thread safe class and lock every time it is accessed:

```cs
public class Cache
{
    private readonly object padlock;
    private readonly Dictionary<string, object> values;

    public WordStats()
    {
        this.padlock = new object();
        this.values = new Dictionary<string, object>();
    }
    
    public void Add(string key, object value)
    {
        lock (this.padlock)
        {
            this.values.Add(key, value);
        }
    }

    /* rest of class omitted */
}

```

If there are multiple methods accessing the `values` dictionary, the code can get very long and, more importantly, locking all the time obscures its **intent**. Locking is also very easy to forget and lack of proper locking can cause very hard to find bugs.

By using a [`ConcurrentDictionary`](https://msdn.microsoft.com/en-us/library/dd287191%28v=vs.110%29.aspx?f=255&MSPPError=-2147217396), we can avoid locking completely:

```cs
public class Cache
{
    private readonly ConcurrentDictionary<string, object> values;

    public WordStats()
    {
        this.values = new ConcurrentDictionary<string, object>();
    }
    
    public void Add(string key, object value)
    {
        this.values.Add(key, value);
    }

    /* rest of class omitted */
}

```

Using concurrent collections also improves performance because [all of them employ lock-free techniques](https://blogs.msdn.microsoft.com/pfxteam/2010/01/26/faq-are-all-of-the-new-concurrent-collections-lock-free/) to some extent.



## Using instances of Object for lock


When using C#'s inbuilt `lock` statement an instance of some type is needed, but its state does not matter. An instance of `object` is perfect for this:

```cs
public class ThreadSafe {
  private static readonly object locker = new object();


  public void SomeThreadSafeMethod() {
    lock (locker) {
      // Only one thread can be here at a time.
    }
  }
}

```

**NB**. instances of `Type` should not be used for this (in the code above `typeof(ThreadSafe)`) because instances of `Type` are shared across AppDomains and thus the extent of the lock can expectedly include code it shouldn't (eg. if `ThreadSafe` is loaded into two AppDomains in the same process then locking on its `Type` instance would mutually lock).



#### Syntax


- lock (obj) {}



#### Remarks


Using the `lock` statement you can control different threads' access to code within the code block. It is commonly used to prevent race conditions, for example multiple threads reading and removing items from a collection. As locking forces threads to wait for other threads to exit a code block it can cause delays that could be solved with other synchronization methods.

MSDN

> 
<p>The lock keyword marks a statement block as a critical section by
obtaining the mutual-exclusion lock for a given object, executing a
statement, and then releasing the lock.</p>
<p>The lock keyword ensures that one thread does not enter a critical
section of code while another thread is in the critical section. If
another thread tries to enter a locked code, it will wait, block,
until the object is released.</p>
<p>Best practice is to define a **private** object to lock on, or a <strong>private
static</strong> object variable to protect data common to all instances.</p>


In C# 5.0 and later, the `lock` statement is equivalent to:

```cs
bool lockTaken = false;
try 
{
    System.Threading.Monitor.Enter(refObject, ref lockTaken);
    // code 
}
finally 
{
    if (lockTaken)
        System.Threading.Monitor.Exit(refObject);
}

```

For C# 4.0 and earlier, the `lock` statement is equivalent to:

```cs
System.Threading.Monitor.Enter(refObject);
try 
{
    // code
}
finally 
{
     System.Threading.Monitor.Exit(refObject);
}

```

