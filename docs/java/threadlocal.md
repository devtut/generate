---
metaTitle: "ThreadLocal"
description: "Basic ThreadLocal usage, ThreadLocal Java 8 functional initialization, Multiple threads with one shared object"
---

# ThreadLocal



## Basic ThreadLocal usage


Java `ThreadLocal` is used to create thread local variables. It is known that threads of an Object share it’s variables, so the variable is not thread safe. We can use synchronization for thread safety but if we want to avoid synchronization,ThreadLocal allows us to create variables which are local to the thread, i.e. only that thread can read or write to those variables, so the other threads executing the same piece of code will not be able to access each others ThreadLocal variables.

This can be usedwe can use `ThreadLocal` variables.
in situations where you have a thread pool like for example in a web service. For example, Creating a `SimpleDateFormat` object every time for every request is time consuming and a Static one cannot be created as `SimpleDateFormat` is not thread safe, so we can create a ThreadLocal so that we can perform thread safe operations without the overhead of creating `SimpleDateFormat` every time.

The below piece of code shows how it can be used:

Every thread has it’s own `ThreadLocal` variable and they can use it’s `get()` and `set()` methods to get the default value or change it’s value local to Thread.

`ThreadLocal` instances are typically private static fields in classes that wish to associate state with a thread.

Here is a small example showing use of ThreadLocal in java program and proving that every thread has it’s own copy of `ThreadLocal` variable.

```java
package com.examples.threads;

import java.text.SimpleDateFormat;
import java.util.Random;

public class ThreadLocalExample implements Runnable{

    // SimpleDateFormat is not thread-safe, so give one to each thread
 // SimpleDateFormat is not thread-safe, so give one to each thread
    private static final ThreadLocal<SimpleDateFormat> formatter = new ThreadLocal<SimpleDateFormat>(){
        @Override
        protected SimpleDateFormat initialValue()
        {
            return new SimpleDateFormat("yyyyMMdd HHmm");
        }
    };
    
    public static void main(String[] args) throws InterruptedException {
        ThreadLocalExample obj = new ThreadLocalExample();
        for(int i=0 ; i<10; i++){
            Thread t = new Thread(obj, ""+i);
            Thread.sleep(new Random().nextInt(1000));
            t.start();
        }
    }

    @Override
    public void run() {
        System.out.println("Thread Name= "+Thread.currentThread().getName()+" default Formatter = "+formatter.get().toPattern());
        try {
            Thread.sleep(new Random().nextInt(1000));
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        
        formatter.set(new SimpleDateFormat());
        
        System.out.println("Thread Name= "+Thread.currentThread().getName()+" formatter = "+formatter.get().toPattern());
    }

}

```

**Output:**

`Thread Name= 0 default Formatter = yyyyMMdd HHmm`

`Thread Name= 1 default Formatter = yyyyMMdd HHmm`

`Thread Name= 0 formatter = M/d/yy h:mm a`

`Thread Name= 2 default Formatter = yyyyMMdd HHmm`

`Thread Name= 1 formatter = M/d/yy h:mm a`

`Thread Name= 3 default Formatter = yyyyMMdd HHmm`

`Thread Name= 4 default Formatter = yyyyMMdd HHmm`

`Thread Name= 4 formatter = M/d/yy h:mm a`

`Thread Name= 5 default Formatter = yyyyMMdd HHmm`

`Thread Name= 2 formatter = M/d/yy h:mm a`

`Thread Name= 3 formatter = M/d/yy h:mm a`

`Thread Name= 6 default Formatter = yyyyMMdd HHmm`

`Thread Name= 5 formatter = M/d/yy h:mm a`

`Thread Name= 6 formatter = M/d/yy h:mm a`

`Thread Name= 7 default Formatter = yyyyMMdd HHmm`

`Thread Name= 8 default Formatter = yyyyMMdd HHmm`

`Thread Name= 8 formatter = M/d/yy h:mm a`

`Thread Name= 7 formatter = M/d/yy h:mm a`

`Thread Name= 9 default Formatter = yyyyMMdd HHmm`

`Thread Name= 9 formatter = M/d/yy h:mm a`

As we can see from the output that Thread-0 has changed the value of formatter but still thread-2 default formatter is same as the initialized value.



## ThreadLocal Java 8 functional initialization


```java
public static class ThreadLocalExample
{
    private static final ThreadLocal<SimpleDateFormat> format = 
        ThreadLocal.withInitial(() -> new SimpleDateFormat("yyyyMMdd_HHmm"));

    public String formatDate(Date date)
    {
        return format.get().format(date);
    }
}

```



## Multiple threads with one shared object


In this example we have only one object but it is shared between/executed on different threads. Ordinary usage of fields to save state would not be possible because the other thread would see that too (or probably not see).

```java
public class Test {
    public static void main(String[] args) {
        Foo foo = new Foo();
        new Thread(foo, "Thread 1").start();
        new Thread(foo, "Thread 2").start();
    }
}

```

In Foo we count starting from zero. Instead of saving the state to a field we store our current number in the ThreadLocal object which is statically accessible.
Note that the synchronization in this example is not related to the usage of ThreadLocal but rather ensures a better console output.

```java
public class Foo implements Runnable {
    private static final int ITERATIONS = 10;
    private static final ThreadLocal<Integer> threadLocal = new ThreadLocal<Integer>() {
        @Override
        protected Integer initialValue() {
            return 0;
        }
    };

    @Override
    public void run() {
        for (int i = 0; i < ITERATIONS; i++) {
            synchronized (threadLocal) {
                //Although accessing a static field, we get our own (previously saved) value.
                int value = threadLocal.get();
                System.out.println(Thread.currentThread().getName() + ": " + value);
                
                //Update our own variable
                threadLocal.set(value + 1);

                try {
                    threadLocal.notifyAll();
                    if (i < ITERATIONS - 1) {
                        threadLocal.wait();
                    }
                } catch (InterruptedException ex) {
                }
            }
        }
    }
}

```

From the output we can see that each thread counts for itself and does not use the value of the other one:

```java
Thread 1: 0
Thread 2: 0
Thread 1: 1
Thread 2: 1
Thread 1: 2
Thread 2: 2
Thread 1: 3
Thread 2: 3
Thread 1: 4
Thread 2: 4
Thread 1: 5
Thread 2: 5
Thread 1: 6
Thread 2: 6
Thread 1: 7
Thread 2: 7
Thread 1: 8
Thread 2: 8
Thread 1: 9
Thread 2: 9

```



#### Remarks


Best used for objects which depend on internals during invoking a call, but are stateless otherwise, like `SimpleDateFormat`, `Marshaller`

For `Random` ThreadLocal usage, consider using `ThreadLocalRandom`

