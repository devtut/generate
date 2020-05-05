---
metaTitle: "Java - Java Pitfalls - Exception usage"
description: "Pitfall - Catching Throwable, Exception, Error or RuntimeException, Pitfall - Ignoring or squashing exceptions, Pitfall - Throwing Throwable, Exception, Error or RuntimeException, Pitfall - Using exceptions for normal flowcontrol, Pitfall - Catching InterruptedException, Pitfall - Excessive or inappropriate stacktraces, Pitfall - Directly subclassing `Throwable`"
---

# Java Pitfalls - Exception usage


Several Java programming language misusage might conduct a program to generate incorrect results despite being compiled correctly. This topic main purpose is to list common **pitfalls** related to **exception handling**, and to propose the correct way to avoid having such pitfalls.



## Pitfall - Catching Throwable, Exception, Error or RuntimeException


A common thought pattern for inexperienced Java programmers is that exceptions are "a problem" or "a burden" and the best way to deal with this is catch them all<sup>1</sup> as soon as possible.  This leads to code like this:

```java
....
try {
    InputStream is = new FileInputStream(fileName);
    // process the input
} catch (Exception ex) {
    System.out.println("Could not open file " + fileName);
}

```

The above code has a significant flaw.  The `catch` is actually going to catch more exceptions than the programmer is expecting.  Suppose that the value of the `fileName` is `null`, due to a bug elsewhere in the application.  This will cause the `FileInputStream` constructor to throw a `NullPointerException`.  The handler will catch this, and report to the user:

```

   Could not open file null

```

which is unhelpful and confusing.  Worse still, suppose that the it was the "process the input" code that threw the unexpected exception (checked or unchecked!).  Now the user will get the misleading message for a problem that didn't occur while opening the file, and may not be related to I/O at all.

The root of the problem is that the programmer has coded a handler for `Exception`. This is almost always a mistake:

- Catching `Exception` will catch all checked exceptions, and most unchecked exceptions as well.
- Catching `RuntimeException` will catch most unchecked exceptions.
- Catching `Error` will catch unchecked exceptions that signal JVM internal errors.  These errors are generally not recoverable, and should not be caught.
- Catching `Throwable` will catch all possible exceptions.

The problem with catching too broad a set of exceptions is that the handler typically cannot handle all of them appropriately.  In the case of the `Exception` and so on, it is difficult for the programmer to predict what **could** be caught; i.e. what to expect.

In general, the correct solution is to deal with the exceptions that **are** thrown.  For example, you can catch them and handle them in situ:

```java
try {
    InputStream is = new FileInputStream(fileName);
    // process the input
} catch (FileNotFoundException ex) {
    System.out.println("Could not open file " + fileName);
}

```

or you can declare them as `thrown` by the enclosing method.

There are very few situations where catching `Exception` is appropriate.  The only one that arises commonly is something like this:

```java
public static void main(String[] args) {
    try {
        // do stuff
    } catch (Exception ex) {
        System.err.println("Unfortunately an error has occurred. " +
                           "Please report this to X Y Z");
        // Write stacktrace to a log file.
        System.exit(1);
    }
}

```

Here we genuinely want to deal with all exceptions, so catching `Exception` (or even `Throwable`) is correct.

<sup>1 - Also known as [Pokemon Exception Handling](http://c2.com/cgi/wiki?PokemonExceptionHandling).</sup>



## Pitfall - Ignoring or squashing exceptions


This example is about deliberately ignoring or "squashing" exceptions.  Or to be more precise, it is about how to catch and handle an exception in a way that ignores it.  However, before we describe how to do this, we should first point out that squashing exceptions is generally not the correct way to deal with them.

Exceptions are usually thrown (by something) to notify other parts of the program that some significant (i.e. "exceptional") event has occurred.  Generally (though not always) an exception means that something has gone wrong.  If you code your program to squash the exception, there is a fair chance that the problem will reappear in another form.  To make things worse, when you squash the exception, you are throwing away the information in the exception object and its associated stack trace.  That is likely to make it harder to figure out what the original source of the problem was.

In practice, exception squashing frequently happens when you use an IDE's auto-correction feature to "fix" a compilation error caused by an unhandled exception.  For example, you might see code like this:

```java
try {
    inputStream = new FileInputStream("someFile");
} catch (IOException e) {
    /* add exception handling code here */
}

```

Clearly, the programmer has accepted the IDE's suggestion to make the compilation error go away, but the suggestion was inappropriate.  (If the file open has failed, the program should most likely do something about it.  With the above "correction", the program is liable to fail later; e.g. with a `NullPointerException` because `inputStream` is now `null`.)

Having said that, here is an example of deliberately squashing an exception.  (For the purposes of argument, assume that we have determined that an interrupt while showing the selfie is harmless.)  The comment tells the reader that we squashed the exception deliberately, and why we did that.

```java
try {
    selfie.show();
} catch (InterruptedException e) {
    // It doesn't matter if showing the selfie is interrupted.
}

```

Another conventional way to highlight that we are **deliberately** squashing an exception without saying why is to indicate this with the exception variable's name, like this:

```java
try { 
    selfie.show(); 
} catch (InterruptedException ignored) {  }

```

Some IDEs (like IntelliJ IDEA) won't display a warning about the empty catch block if the variable name is set to `ignored`.



## Pitfall - Throwing Throwable, Exception, Error or RuntimeException


While catching the `Throwable`, `Exception`, `Error` and `RuntimeException` exceptions is bad, throwing them is even worse.

The basic problem is that when your application needs to handle exceptions, the presence of the top level exceptions make it hard to discriminate between different error conditions.  For example

```java
try {
    InputStream is = new FileInputStream(someFile);  // could throw IOException
    ...
    if (somethingBad) {
        throw new Exception();  // WRONG
    }
} catch (IOException ex) {
    System.err.println("cannot open ...");
} catch (Exception ex) {
    System.err.println("something bad happened");  // WRONG
}

```

The problem is that because we threw an `Exception` instance, we are forced to catch it.  However as described in another example, catching `Exception` is bad.  In this situation, it becomes difficult to discriminate between the "expected" case of an `Exception` that gets thrown if `somethingBad` is `true`, and the unexpected case where we actually catch an unchecked exception such as `NullPointerException`.

If the top-level exception is allowed to propagate, we run into other problems:

- We now have to remember all of the different reasons that we threw the top-level, and discriminate / handle them.
- In the case of `Exception` and `Throwable` we also need to add these exceptions to the `throws` clause of methods if we want the exception to propagate.  This is problematic, as described below.

In short, don't throw these exceptions.  Throw a more specific exception that more closely describes the "exceptional event" that has happened.  If you need to, define and use a custom exception class.

### Declaring Throwable or Exception in a method's "throws" is problematic.

It is tempting to replace a long list of thrown exceptions in a method's `throws` clause with `Exception` or even `Throwable.  This is a bad idea:

1. It forces the caller to handle (or propagate) `Exception`.
1. We can no longer rely on the compiler to tell us about specific checked exceptions that need to be handled.
1. Handling `Exception` properly is difficult.  It is hard to know what actual exceptions may be caught, and if you don't know what could be caught, it is hard to know what recovery strategy is appropriate.
1. Handling `Throwable` is even harder, since now you also have to cope with potential failures that should never be recovered from.

This advice means that certain other patterns should be avoided.  For example:

```java
try {
    doSomething();
} catch (Exception ex) {
    report(ex);
    throw ex;
}

```

The above attempts to log all exceptions as they pass, without definitively handling them.  Unfortunately, prior to Java 7, the `throw ex;` statement caused the compiler to think that any `Exception` could be thrown.  That could force you to declare the enclosing method as `throws Exception`.  From Java 7 onwards, the compiler knows that the set of exceptions that could be (re-thrown) there is smaller.



## Pitfall - Using exceptions for normal flowcontrol


There is a mantra that some Java experts are wont to recite:

> 
"Exceptions should only be used for exceptional cases."


(For example: [http://programmers.stackexchange.com/questions/184654](http://programmers.stackexchange.com/questions/184654) )

The essence of this is that is it is a bad idea (in Java) to use exceptions and exception handling to implement normal flow control.  For example, compare these two ways of dealing with a parameter that could be null.

```java
public String truncateWordOrNull(String word, int maxLength) {
    if (word == null) {
        return "";
    } else {
        return word.substring(0, Math.min(word.length(), maxLength));
    }
}

public String truncateWordOrNull(String word, int maxLength) {
    try {
        return word.substring(0, Math.min(word.length(), maxLength));
    } catch (NullPointerException ex) {
        return "";
    }
}

```

In this example, we are (by design) treating the case where `word` is `null` as if it is an empty word.  The two versions deal with `null` either using conventional **if ... else** and or **try ... catch**.  How should we decide which version is better?

The first criterion is readability.  While readability is hard to quantify objectively, most programmers would agree that the essential meaning of the first version is easier to discern.  Indeed, in order to truly understand the second form, you need to understand that a `NullPointerException` cannot be thrown by the `Math.min` or `String.substring` methods.

The second criterion is efficiency.  In releases of Java prior to Java 8, the second version is significantly (orders of magnitude) slower than the first version.  In particular, the construction of an exception object entails capturing and recording the stackframes, just in case the stacktrace is required.

On the other hand, there are many situations where using exceptions is more readable, more efficient and (sometimes) more correct than using conditional code to deal with "exceptional" events.  Indeed, there are rare situations where it is necessary to use them for "non-exceptional" events; i.e. events that occur relatively frequently.  For the latter, it is worth looking at ways to reduce the overheads of creating exception objects.



## Pitfall - Catching InterruptedException


As already pointed out in other pitfalls, catching all exceptions by using

```java
try {
    // Some code
} catch (Exception) {
    // Some error handling
}

```

Comes with a lot of different problems. But one perticular problem is that it can lead to deadlocks as it breaks the interrupt system when writing multi-threaded applications.

If you start a thread you usually also need to be able to stop it abruptly for various reasons.

```java
Thread t = new Thread(new Runnable() {
    public void run() {
         while (true) {
             //Do something indefinetely
         }
    }
}

t.start();

//Do something else

// The thread should be canceld if it is still active. 
// A Better way to solve this is with a shared variable that is tested 
// regularily by the thread for a clean exit, but for this example we try to 
// forcibly interrupt this thread.
if (t.isAlive()) {
   t.interrupt();
   t.join();
}

//Continue with program

```

The `t.interrupt()` will raise an InterruptedException in that thread, than is intended to shut down the thread. But what if the Thread needs to clean up some resources before its completely stopped? For this it can catch the InterruptedException and do some cleanup.

```

Thread t = new Thread(new Runnable() {
    public void run() {
        try {
            while (true) {
                //Do something indefinetely
            }
        } catch (InterruptedException ex) {
            //Do some quick cleanup

            // In this case a simple return would do. 
            // But if you are not 100% sure that the thread ends after 
            // catching the InterruptedException you will need to raise another 
            // one for the layers surrounding this code.                
            Thread.currentThread().interrupt(); 
        }
    }
}

```

But if you have a catch-all expression in your code, the InterruptedException will be caught by it as well and the interruption will not continue. Which in this case could lead to a deadlock as the parent thread waits indefinitely for this thead to stop with `t.join()`.

```

Thread t = new Thread(new Runnable() {
    public void run() {
        try {
            while (true) {
                try {
                    //Do something indefinetely
                }
                catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        } catch (InterruptedException ex) {
            // Dead code as the interrupt exception was already caught in
            // the inner try-catch           
            Thread.currentThread().interrupt(); 
        }
    }
}

```

So it is better to catch Exceptions individually, but if you insist on using a catch-all, at least catch the InterruptedException individually beforehand.

```java
Thread t = new Thread(new Runnable() {
    public void run() {
        try {
            while (true) {
                try {
                    //Do something indefinetely
                } catch (InterruptedException ex) {
                    throw ex; //Send it up in the chain
                } catch (Exception ex) {
                    ex.printStackTrace();
                }
            }
        } catch (InterruptedException ex) {
            // Some quick cleanup code 
    
            Thread.currentThread().interrupt(); 
        }
    }
}

```



## Pitfall - Excessive or inappropriate stacktraces


One of the more annoying things that programmers can do is to scatter calls to `printStackTrace()` throughout their code.

The problem is that the `printStackTrace()` is going to write the stacktrace to standard output.

<li>
For an application that is intended for end-users who are not Java programmers, a stacktrace is uninformative at best, and alarming at worst.
</li>
<li>
For a server-side application, the chances are that nobody will look at the standard output.
</li>

A better idea is to not call `printStackTrace` directly, or if you do call it, do it in a way that the stack trace is written to a log file or error file rather than to the end-user's console.

One way to do this is to use a logging framework, and pass the exception object as a parameter of the log event.  However, even logging the exception can be harmful if done injudiciously.  Consider the following:

```java
public void method1() throws SomeException {
    try {
        method2();
        // Do something
    } catch (SomeException ex) {
        Logger.getLogger().warn("Something bad in method1", ex);
        throw ex;
    }
}

public void method2() throws SomeException {
    try {
        // Do something else
    } catch (SomeException ex) {
        Logger.getLogger().warn("Something bad in method2", ex);
        throw ex;
    }
}

```

If the exception is thrown in `method2`, you are likely to see two copies of the same stacktrace in the logfile, corresponding to the same failure.

In short, either log the exception or re-throw it further (possibly wrapped with another exception).  Don't do both.



## Pitfall - Directly subclassing `Throwable`


`Throwable` has two direct subclasses, `Exception` and `Error`. While it's possible to create a new class that extends `Throwable` directly, this is inadvisable as many applications assume only `Exception` and `Error` exist.

More to the point there is no practical benefit to directly subclassing `Throwable`, as the resulting class is, in effect, simply a checked exception. Subclassing `Exception` instead will result in the same behavior, but will more clearly convey your intent.

