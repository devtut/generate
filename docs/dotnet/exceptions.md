---
metaTitle: ".NET Framework - Exceptions"
description: "Catching and rethrowing caught exceptions, Using a finally block, Exception Filters, Rethrowing an exception within a catch block, Throwing an exception from a different method while preserving its information, Catching an exception"
---

# Exceptions




## Catching and rethrowing caught exceptions


When you want to catch an exception and do something, but you can't continue execution of the current block of code because of the exception, you may want to rethrow the exception to the next exception handler in the call stack. There are good ways and bad ways to do this.

```dotnet
private static void AskTheUltimateQuestion()
{
    try
    {
        var x = 42;
        var y = x / (x - x); // will throw a DivideByZeroException

        // IMPORTANT NOTE: the error in following string format IS intentional
        // and exists to throw an exception to the FormatException catch, below
        Console.WriteLine("The secret to life, the universe, and everything is {1}", y); 
    }
    catch (DivideByZeroException)
    {
        // we do not need a reference to the exception
        Console.WriteLine("Dividing by zero would destroy the universe.");

        // do this to preserve the stack trace:
        throw;
    }
    catch (FormatException ex)
    {
        // only do this if you need to change the type of the Exception to be thrown 
        // and wrap the inner Exception

        // remember that the stack trace of the outer Exception will point to the
        // next line

        // you'll need to examine the InnerException property to get the stack trace
        // to the line that actually started the problem

        throw new InvalidOperationException("Watch your format string indexes.", ex);
    }
    catch (Exception ex)
    {
        Console.WriteLine("Something else horrible happened. The exception: " + ex.Message);

        // do not do this, because the stack trace will be changed to point to
        // this location instead of the location where the exception
        // was originally thrown:
        throw ex; 
    }
}

static void Main()
{
    try
    {
        AskTheUltimateQuestion();
    }
    catch
    {
        // choose this kind of catch if you don't need any information about 
        // the exception that was caught

        // this block "eats" all exceptions instead of rethrowing them
    }
}

```

You can filter by exception type and even by exception properties (new in C# 6.0, a bit longer available in VB.NET (citation needed)):

[Documentation/C#/new features](http://stackoverflow.com/documentation/c%23/24/c-sharp-6-0-features/46/exception-filters)



## Using a finally block


The `finally { ... }` block of a `try-finally` or `try-catch-finally` will always execute, regardless of whether an exception occurred or not (except when a `StackOverflowException` has been thrown or call has been made to `Environment.FailFast()`).

It can be utilized to free or clean up resources acquired in the `try { ... }` block safely.

```dotnet
Console.Write("Please enter a filename: ");
string filename = Console.ReadLine();

Stream fileStream = null;

try
{
    fileStream = File.Open(filename);
}
catch (FileNotFoundException)
{
    Console.WriteLine("File '{0}' could not be found.", filename);
}
finally
{
    if (fileStream != null)
    {
        fileStream.Dispose();
    }
}

```



## Exception Filters


Since C# 6.0 exceptions can be filtered using the `when` operator.

This is similar to using a simple `if` but does not unwind the stack if the condition inside the `when` is not met.

**Example**

```dotnet
try
{ 
  // ...
}
catch (Exception e) when (e.InnerException != null) // Any condition can go in here.
{
  // ...
}

```

The same info can be found in the [C# 6.0 Features](http://stackoverflow.com/documentation/c%23/24/c-sharp-6-0-features) here: [Exception filters](http://stackoverflow.com/documentation/c%23/24/c-sharp-6-0-features/46/exception-filters#t=201607211048375185447)



## Rethrowing an exception within a catch block


Within a `catch` block the `throw` keyword can be used on its own, without specifying an exception value, to **rethrow** the exception which was just caught.  Rethrowing an exception allows the original exception to continue up the exception handling chain, preserving its call stack or associated data:

```dotnet
try {...}
catch (Exception ex) {
  // Note: the ex variable is *not* used
  throw;
}

```

A common anti-pattern is to instead `throw ex`, which has the effect of limiting the next exception handler's view of the stack trace:

```dotnet
try {...}
catch (Exception ex) {
  // Note: the ex variable is thrown
  //  future stack traces of the exception will not see prior calls
  throw ex;  
}

```

In general using `throw ex` isn't desirable, as future exception handlers which inspect the stack trace will only be able to see calls as far back as `throw ex`.  By omitting the `ex` variable, and using the `throw` keyword alone the original exception will ["bubble-up"](http://stackoverflow.com/questions/4065893/about-throw-and-exception-bubbling).



## Throwing an exception from a different method while preserving its information


Occasionally you'd want to catch an exception and throw it from a different thread or method while preserving the original exception stack. This can be done with `ExceptionDispatchInfo`:

```dotnet
using System.Runtime.ExceptionServices;

void Main()
{
    ExceptionDispatchInfo capturedException = null;
    try
    {
        throw new Exception();
    }
    catch (Exception ex)
    {
        capturedException = ExceptionDispatchInfo.Capture(ex);
    }
    
    Foo(capturedException);
}

void Foo(ExceptionDispatchInfo exceptionDispatchInfo)
{
    // Do stuff

    if (capturedException != null)
    {
        // Exception stack trace will show it was thrown from Main() and not from Foo()
        exceptionDispatchInfo.Throw();
    }
}

```



## Catching an exception


Code can and should throw exceptions in exceptional circumstances. Examples of this include:

- Attempting to [read past the end of a stream](https://msdn.microsoft.com/en-us/library/system.io.endofstreamexception(v=vs.110).aspx)
- [Not having necessary permissions](https://msdn.microsoft.com/en-us/library/system.unauthorizedaccessexception(v=vs.110).aspx) to access a file
- Attempting to perform an invalid operation, such as [dividing by zero](https://msdn.microsoft.com/en-us/library/system.dividebyzeroexception(v=vs.110).aspx)
- [A timeout occurring](https://msdn.microsoft.com/en-us/library/system.net.webexception.aspx) when downloading a file from the internet

The caller can handle these exceptions by "catching" them, and should only do so when:

- It can actually resolve the exceptional circumstance or recover appropriately, or;
- It can provide additional context to the exception that would be useful if the exception needs to be re-thrown (re-thrown exceptions are caught by exception handlers further up the call stack)

It should be noted that choosing **not** to catch an exception is perfectly valid if the intention is for it to be handled at a higher level.

Catching an exception is done by wrapping the potentially-throwing code in a `try { ... }` block as follows, and catching the exceptions it's able to handle in a `catch (ExceptionType) { ... }` block:

```dotnet
Console.Write("Please enter a filename: ");
string filename = Console.ReadLine();

Stream fileStream;

try
{
    fileStream = File.Open(filename);
}
catch (FileNotFoundException)
{
    Console.WriteLine("File '{0}' could not be found.", filename);
}

```



#### Remarks


Related:

- [MSDN: Exceptions and Exception Handling (C# Programming Guide)](https://msdn.microsoft.com/en-us/library/ms173160.aspx)
- [MSDN: Handling and Throwing Exceptions](https://msdn.microsoft.com/en-us/library/5b2yeyab.aspx)
- [MSDN: CA1031: Do not catch general exception types](https://msdn.microsoft.com/en-us/library/ms182137.aspx)
- [MSDN: try-catch (C# Reference)](https://msdn.microsoft.com/en-us/library/0yd65esw.aspx)

