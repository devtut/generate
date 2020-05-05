---
metaTitle: "C# | Exception Handling"
description: "Creating Custom Exceptions, Finally block, Best Practices, Exception Anti-patterns, Basic Exception Handling, Handling specific exception types, Aggregate exceptions / multiple exceptions from one method, Throwing an exception, Unhandled and Thread Exception, Using the exception object, Implementing IErrorHandler for WCF Services, Nesting of Exceptions & try catch blocks."
---

# Exception Handling




## Creating Custom Exceptions


You are allowed to implement custom exceptions that can be thrown just like any other exception. This makes sense when you want to make your exceptions distinguishable from other errors during runtime.

In this example we will create a custom exception for clear handling of problems the application may have while parsing a complex input.

### Creating Custom Exception Class

To create a custom exception create a sub-class of `Exception`:

```cs
public class ParserException : Exception
{
    public ParserException() : 
      base("The parsing went wrong and we have no additional information.") { }
}

```

Custom exception become very useful when you want to provide additional information to the catcher:

```cs
public class ParserException : Exception
{
    public ParserException(string fileName, int lineNumber) : 
      base($"Parser error in {fileName}:{lineNumber}") 
    {
      FileName = fileName;
      LineNumber = lineNumber;
    }
    public string FileName {get; private set;}
    public int LineNumber {get; private set;}    
}

```

Now, when you `catch(ParserException x)` you will have additional semantics to  fine-tune exception handling.

Custom classes can implement the following features to support additional scenarios.

### re-throwing

During the parsing process, the original exception is still of interest. In this example it is a `FormatException` because the code attempts to parse a piece of string, which is expected to be a number. In this case the custom exception should support the inclusion of the '**InnerException**':

```cs
//new constructor:
ParserException(string msg, Exception inner) : base(msg, inner) {
}

```

### serialization

In some cases your exceptions may have to cross AppDomain boundaries. This is the case if your parser is running in its own AppDomain to support hot reloading of new parser configurations. In Visual Studio, you can use `Exception` template to generate code like this.

```cs
[Serializable]
public class ParserException : Exception
{
    // Constructor without arguments allows throwing your exception without
    // providing any information, including error message. Should be included
    // if your exception is meaningful without any additional details. Should
    // set message by calling base constructor (default message is not helpful).
    public ParserException()
        : base("Parser failure.")
    {}

    // Constructor with message argument allows overriding default error message.
    // Should be included if users can provide more helpful messages than
    // generic automatically generated messages.
    public ParserException(string message) 
        : base(message)
    {}

    // Constructor for serialization support. If your exception contains custom
    // properties, read their values here.
    protected ParserException(SerializationInfo info, StreamingContext context) 
        : base(info, context)
    {}
}

```

### Using the ParserException

```cs
try
{
    Process.StartRun(fileName)
}
catch (ParserException ex)
{
    Console.WriteLine($"{ex.Message} in ${ex.FileName}:${ex.LineNumber}");
}
catch (PostProcessException x) 
{
    ...
}

```

You may also use custom exceptions for catching and wrapping exceptions. This way many different errors can be converted into a single error type that is more useful to the application:

```cs
try
{
    int foo = int.Parse(token);
}
catch (FormatException ex)
{
    //Assuming you added this constructor
    throw new ParserException(
      $"Failed to read {token} as number.", 
      FileName, 
      LineNumber, 
      ex);
}

```

When handling exceptions by raising your own custom exceptions, you should generally include a reference the original exception in the `InnerException` property, as shown above.

### Security Concerns

If exposing the reason for the exception might compromise security by allowing users to see the inner workings of your application it can be a bad idea to wrap the inner exception. This might apply if you are creating a class library that will be used by others.

Here is how you could raise a custom exception without wrapping the inner exception:

```cs
try
{
  // ...
}
catch (SomeStandardException ex)
{
  // ...
  throw new MyCustomException(someMessage);
}

```

### Conclusion

When raising a custom exception (either with wrapping or with an unwrapped new exception), you should raise an exception that is meaningful to the caller. For instance, a user of a class library may not know much about how that library does its internal work. The exceptions that are thrown by the dependencies of the class library are not meaningful. Rather, the user wants an exception that is relevant to how the class library is using those dependencies in an erroneous way.

```cs
try
{
  // ...
}
catch (IOException ex)
{
  // ...
  throw new StorageServiceException(@"The Storage Service encountered a problem saving
your data. Please consult the inner exception for technical details. 
If you are not able to resolve the problem, please call 555-555-1234 for technical       
assistance.", ex);
}

```



## Finally block


```cs
try
{
    /* code that could throw an exception */
}
catch (Exception)
{
    /* handle the exception */
}
finally
{
    /* Code that will be executed, regardless if an exception was thrown / caught or not */
}

```

The `try / catch / finally` block can be very handy when reading from files.<br />
For example:

```cs
FileStream f = null;

try
{
    f = File.OpenRead("file.txt");
    /* process the file here */
}
finally
{
    f?.Close(); // f may be null, so use the null conditional operator.
}

```

A try block must be followed by either a `catch` or a `finally` block. However, since there is no catch block, the execution will cause termination. Before termination, the statements inside the finally block will be executed.

In the file-reading we could have used a `using` block as `FileStream` (what `OpenRead` returns) implements `IDisposable`.

Even if there is a `return` statement in `try` block, the `finally` block will usually execute; there are a few cases where it will not:

- When a [StackOverflow occurs](https://msdn.microsoft.com/en-us/library/system.stackoverflowexception(v=vs.110).aspx).
- [`Environment.FailFast`](https://msdn.microsoft.com/en-us/library/system.environment.failfast.aspx)
- The application process is killed, usually by an external source.



## Best Practices


### Cheatsheet

|DO|DON'T
|---|---|---|---|---|---|---|---|---|---
|Control flow with control statements|Control flow with exceptions
|Keep track of ignored (absorbed) exception by logging|Ignore exception
|Repeat exception by using `throw`|Re-throw exception - `throw new ArgumentNullException()` or `throw ex`
|Throw predefined system exceptions|Throw custom exceptions similar to predefined system exceptions
|Throw custom/predefined exception if it is crucial to application logic|Throw custom/predefined exceptions to state a warning in flow
|Catch exceptions that you want to handle|Catch every exception

### DO NOT manage business logic with exceptions.

Flow control should NOT be done by exceptions. Use conditional statements instead. If a control can be done with `if-else` statement clearly, don't use exceptions because it reduces readability and performance.

Consider the following snippet by Mr. Bad Practices:

```cs
// This is a snippet example for DO NOT
object myObject;
void DoingSomethingWithMyObject()
{
    Console.WriteLine(myObject.ToString());
}

```

When execution reaches `Console.WriteLine(myObject.ToString());` application will throw an NullReferenceException. Mr. Bad Practices realized that `myObject` is null and edited his snippet to catch & handle `NullReferenceException`:

```cs
// This is a snippet example for DO NOT
object myObject;
void DoingSomethingWithMyObject()
{
    try
    {
        Console.WriteLine(myObject.ToString());
    }
    catch(NullReferenceException ex)
    {
        // Hmmm, if I create a new instance of object and assign it to myObject:
        myObject = new object();
        // Nice, now I can continue to work with myObject
        DoSomethingElseWithMyObject();
    }
}

```

Since previous snippet only covers logic of exception, what should I do if `myObject` is not null at this point? Where should I cover this part of logic? Right after `Console.WriteLine(myObject.ToString());`? How about after the `try...catch` block?

How about Mr. Best Practices? How would he handle this?

```cs
// This is a snippet example for DO
object myObject;
void DoingSomethingWithMyObject()
{
    if(myObject == null)
        myObject = new object();
    
    // When execution reaches this point, we are sure that myObject is not null
    DoSomethingElseWithMyObject();
}

```

Mr. Best Practices achieved same logic with fewer code and a clear & understandable logic.

### DO NOT re-throw Exceptions

Re-throwing exceptions is expensive. It negatively impact performance. For code that routinely fails, you can use design patterns to minimize performance issues. [This topic](https://msdn.microsoft.com/en-us/library/ms229009(v=vs.100).aspx) describes two design patterns that are useful when exceptions might significantly impact performance.

### DO NOT absorb exceptions with no logging

```cs
try
{
    //Some code that might throw an exception
}
catch(Exception ex)
{
    //empty catch block, bad practice
}

```

Never swallow exceptions. Ignoring exceptions will save that moment but will create a chaos for maintainability later. When logging exceptions, you should always log the exception instance so that the complete stack trace is logged and not the exception message only.

```cs
try
{
    //Some code that might throw an exception
}
catch(NullException ex)
{
    LogManager.Log(ex.ToString());
}

```

### Do not catch exceptions that you cannot handle

Many resources, such as [this one](http://c2.com/cgi/wiki?DontCatchExceptions), strongly urge you to consider why you are catching an exception in the place that you are catching it. You should only catch an exception if you can handle it at that location. If you can do something there to help mitigate the problem, such as trying an alternative algorithm, connecting to a backup database, trying another filename, waiting 30 seconds and trying again, or notifying an administrator, you can catch the error and do that. If there is nothing that you can plausibly and reasonably do, just "let it go" and let the exception be handled at a higher level. If the exception is sufficiently catastrophic and there is no reasonable option other than for the entire program to crash because of the severity of the problem, then let it crash.

```cs
try
{
    //Try to save the data to the main database.
}
catch(SqlException ex)
{
    //Try to save the data to the alternative database.
}
//If anything other than a SqlException is thrown, there is nothing we can do here. Let the exception bubble up to a level where it can be handled.

```



## Exception Anti-patterns


### Swallowing Exceptions

One should always re-throw exception in the following way:

```cs
try
{
    ...
}
catch (Exception ex)
{
    ...
    throw;
}

```

Re-throwing an exception like below will obfuscate the original exception and will lose the original stack trace. One should never do this! The stack trace prior to the catch and rethrow will be lost.

```cs
try
{
    ...
}
catch (Exception ex)
{
    ...
    throw ex;
}

```

### Baseball Exception Handling

One should not use exceptions as a [substitute for normal flow control constructs](http://c2.com/cgi/wiki?DontUseExceptionsForFlowControl) like if-then statements and while loops. This anti-pattern is sometimes called [Baseball Exception Handling](http://www.stackprinter.com/questions/new-programming-jargon-you-coined.html).

Here is an example of the anti-pattern:

```cs
try
{
    while (AccountManager.HasMoreAccounts())
    {
        account = AccountManager.GetNextAccount();
        if (account.Name == userName)
        {
            //We found it
            throw new AccountFoundException(account);
        }
    }
}
catch (AccountFoundException found)
{
    Console.Write("Here are your account details: " + found.Account.Details.ToString());
}

```

Here is a better way to do it:

```cs
Account found = null;
while (AccountManager.HasMoreAccounts() && (found==null))
{
    account = AccountManager.GetNextAccount();
    if (account.Name == userName)
    {
        //We found it
        found = account;
    }
}
Console.Write("Here are your account details: " + found.Details.ToString());

```

### catch (Exception)

There are almost no (some say none!) reasons to catch the generic exception type in your code. You should catch only the exception types you expect to happen, because you hide bugs in your code otherwise.

```cs
try 
{
     var f = File.Open(myfile);
     // do something
}
catch (Exception x)
{
     // Assume file not found
     Console.Write("Could not open file");
     // but maybe the error was a NullReferenceException because of a bug in the file handling code?
}

```

Better do:

```cs
try 
{
     var f = File.Open(myfile);
     // do something which should normally not throw exceptions
}
catch (IOException)
{
     Console.Write("File not found");
}
// Unfortunatelly, this one does not derive from the above, so declare separatelly
catch (UnauthorizedAccessException) 
{
     Console.Write("Insufficient rights");
}

```

If any other exception happens, we purposedly let the application crash, so it directly steps in the debugger and we can fix the problem. We mustn't ship a program where any other exceptions than these happen anyway, so it's not a problem to have a crash.

The following is a bad example, too, because it uses exceptions to work around a programming error. That's not what they're designed for.

```cs
public void DoSomething(String s)
{
     if (s == null)
         throw new ArgumentNullException(nameof(s));
     // Implementation goes here
}

try 
{    
     DoSomething(myString);
}
catch(ArgumentNullException x)
{
    // if this happens, we have a programming error and we should check
    // why myString was null in the first place.
}

```



## Basic Exception Handling


```cs
try
{
    /* code that could throw an exception */
}
catch (Exception ex)
{
    /* handle the exception */
}

```

Note that handling all exceptions with the same code is often not the best approach.<br />
This is commonly used when any inner exception handling routines fail, as a last resort.



## Handling specific exception types


```cs
try
{
    /* code to open a file */
}
catch (System.IO.FileNotFoundException)
{
    /* code to handle the file being not found */
}
catch (System.IO.UnauthorizedAccessException)
{
    /* code to handle not being allowed access to the file */
}
catch (System.IO.IOException)
{
    /* code to handle IOException or it's descendant other than the previous two */
}
catch (System.Exception)
{
    /* code to handle other errors */
}

```

Be careful that exceptions are evaluated in order and inheritance is applied. So you need to start with the most specific ones and end with their ancestor.
At any given point, only one catch block will get executed.



## Aggregate exceptions / multiple exceptions from one method


Who says you cannot throw multiple exceptions in one method. If you are not used to playing around with AggregateExceptions you may be tempted to create your own data-structure to represent many things going wrong. There are of course were another data-structure that is not an exception would be more ideal such as the results of a validation. Even if you do play with AggregateExceptions you may be on the receiving side and always handling them not realizing they can be of use to you.

It is quite plausible to have a method execute and even though it will be a failure as a whole you will want to highlight multiple things that went wrong in the exceptions that are thrown. As an example this behavior can be seen with how Parallel methods work were a task broken into multiple threads and any number of them could throw exceptions and this needs to be reported. Here is a silly example of how you could benefit from this:

```cs

   public void Run()
    {
        try
        {
            this.SillyMethod(1, 2);
        }
        catch (AggregateException ex)
        {
            Console.WriteLine(ex.Message);
            foreach (Exception innerException in ex.InnerExceptions)
            {
                Console.WriteLine(innerException.Message);
            }
        }
    }

    private void SillyMethod(int input1, int input2)
    {
        var exceptions = new List<Exception>();

        if (input1 == 1)
        {
            exceptions.Add(new ArgumentException("I do not like ones"));
        }
        if (input2 == 2)
        {
            exceptions.Add(new ArgumentException("I do not like twos"));
        }
        if (exceptions.Any())
        {
            throw new AggregateException("Funny stuff happended during execution", exceptions);
        }
    }

```



## Throwing an exception


Your code can, and often should, throw an exception when something unusual has happened.

```cs
public void WalkInto(Destination destination)
{
    if (destination.Name == "Mordor")
    {
        throw new InvalidOperationException("One does not simply walk into Mordor.");
    }
    // ... Implement your normal walking code here.
}

```



## Unhandled and Thread Exception


**AppDomain.UnhandledException**
This event provides notification of uncaught exceptions.It allows the application to log information about the exception before the system default handler reports the exception to the user and terminates the application.If sufficient information about the state of the application is available, other actions may be undertaken — such as saving program data for later recovery.Caution is advised, because program data can become corrupted when exceptions are not handled.

```cs

   /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    private static void Main(string[] args)
    {
        AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);            
    }

```

**Application.ThreadException**
This event allows your Windows Forms application to handle otherwise unhandled exceptions that occur in Windows Forms threads. Attach your event handlers to the ThreadException event to deal with these exceptions, which will leave your application in an unknown state. Where possible, exceptions should be handled by a structured exception handling block.

```cs

   /// <summary>
    /// The main entry point for the application.
    /// </summary>
    [STAThread]
    private static void Main(string[] args)
    {
        AppDomain.CurrentDomain.UnhandledException += new UnhandledExceptionEventHandler(UnhandledException);
        Application.ThreadException += new ThreadExceptionEventHandler(ThreadException);
    }

```

And finally exception handling

```cs
static void UnhandledException(object sender, UnhandledExceptionEventArgs e)
    {
        Exception ex = (Exception)e.ExceptionObject;
        // your code
    }

static void ThreadException(object sender, ThreadExceptionEventArgs e)
    {
        Exception ex = e.Exception;
        // your code
    }

```



## Using the exception object


You are allowed to create and throw exceptions in your own code.
Instantiating an exception is done the same way that any other C# object.

```cs
Exception ex = new Exception();

// constructor with an overload that takes a message string
Exception ex = new Exception("Error message"); 

```

You can then use the `throw` keyword to raise the exception:

```cs
try
{
    throw new Exception("Error");
}
catch (Exception ex)
{
    Console.Write(ex.Message); // Logs 'Error' to the output window
} 

```

**Note:** If you're throwing a new exception inside a catch block, ensure that the original exception is passed as "inner exception", e.g.

```cs
void DoSomething() 
{
    int b=1; int c=5;
    try
    {
        var a = 1; 
        b = a - 1;
        c = a / b;
        a = a / c;
    }        
    catch (DivideByZeroException dEx) when (b==0)
    {
        // we're throwing the same kind of exception
        throw new DivideByZeroException("Cannot divide by b because it is zero", dEx);
    }
    catch (DivideByZeroException dEx) when (c==0)
    {
        // we're throwing the same kind of exception
        throw new DivideByZeroException("Cannot divide by c because it is zero", dEx);
    }
}

void Main()
{    
    try
    {
        DoSomething();
    }
    catch (Exception ex)
    {
        // Logs full error information (incl. inner exception)
        Console.Write(ex.ToString()); 
    }    
}

```

In this case it is assumed that the exception cannot be handled but some useful information is added to the message (and the original exception can still be accessed via `ex.InnerException` by an outer exception block).

It will show something like:

> 
<p>System.DivideByZeroException: Cannot divide by b because it is zero ---> System.DivideByZeroException: Attempted to divide by zero. <br/>
at UserQuery.g__DoSomething0_0() in C:[...]\LINQPadQuery.cs:line 36 <br/>
--- End of inner exception stack trace --- <br/>
at UserQuery.g__DoSomething0_0() in C:[...]\LINQPadQuery.cs:line 42 <br/>
at UserQuery.Main() in C:[...]\LINQPadQuery.cs:line 55 <br/></p>


If you're trying this example in LinqPad, you'll notice that the line numbers aren't very meaningful (they don't always help you). But passing a helpful error text as suggested above oftentimes significantly reduces the time to track down the location of the error, which is in this example clearly the line

> 
c = a / b;


in function `DoSomething()`.

**[Try it in .NET Fiddle](https://dotnetfiddle.net/Widget/JLUXXY)**



## Implementing IErrorHandler for WCF Services


Implementing IErrorHandler for WCF services is a great way to centralize error handling and logging. The implementation shown here should catch any unhandled exception that is thrown as a result of a call to one of your WCF services. Also shown in this example is how to return a custom object, and how to return JSON rather than the default XML.

Implement IErrorHandler:

```cs
using System.ServiceModel.Channels;
using System.ServiceModel.Dispatcher;
using System.Runtime.Serialization.Json;
using System.ServiceModel;
using System.ServiceModel.Web;

namespace BehaviorsAndInspectors
{
    public class ErrorHandler : IErrorHandler
    {

        public bool HandleError(Exception ex)
        {
            // Log exceptions here

            return true;

        } // end

        public void ProvideFault(Exception ex, MessageVersion version, ref Message fault)
        {
            // Get the outgoing response portion of the current context
            var response = WebOperationContext.Current.OutgoingResponse;

            // Set the default http status code 
            response.StatusCode = HttpStatusCode.InternalServerError;

            // Add ContentType header that specifies we are using JSON
            response.ContentType = new MediaTypeHeaderValue("application/json").ToString();

            // Create the fault message that is returned (note the ref parameter) with BaseDataResponseContract                
            fault = Message.CreateMessage(
                version,
                string.Empty,
                new CustomReturnType { ErrorMessage = "An unhandled exception occurred!" },
                new DataContractJsonSerializer(typeof(BaseDataResponseContract), new List<Type> { typeof(BaseDataResponseContract) }));

            if (ex.GetType() == typeof(VariousExceptionTypes))
            {
                 // You might want to catch different types of exceptions here and process them differently
            }

            // Tell WCF to use JSON encoding rather than default XML
            var webBodyFormatMessageProperty = new WebBodyFormatMessageProperty(WebContentFormat.Json);
            fault.Properties.Add(WebBodyFormatMessageProperty.Name, webBodyFormatMessageProperty);

        } // end

    } // end class

} // end namespace

```

In this example we attach the handler to the service behavior. You could also attach this to IEndpointBehavior, IContractBehavior, or IOperationBehavior in a similar way.

Attach to Service Behaviors:

```cs
using System;
using System.Collections.ObjectModel;
using System.ServiceModel;
using System.ServiceModel.Channels;
using System.ServiceModel.Configuration;
using System.ServiceModel.Description;
using System.ServiceModel.Dispatcher;

namespace BehaviorsAndInspectors
{
    public class ErrorHandlerExtension : BehaviorExtensionElement, IServiceBehavior
    {
        public override Type BehaviorType
        {
            get { return GetType(); }
        }

        protected override object CreateBehavior()
        {
            return this;
        }

        private IErrorHandler GetInstance()
        {
            return new ErrorHandler();
        }

        void IServiceBehavior.AddBindingParameters(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase, Collection<ServiceEndpoint> endpoints, BindingParameterCollection bindingParameters) { } // end

        void IServiceBehavior.ApplyDispatchBehavior(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase)
        {
            var errorHandlerInstance = GetInstance();

            foreach (ChannelDispatcher dispatcher in serviceHostBase.ChannelDispatchers)
            {
                dispatcher.ErrorHandlers.Add(errorHandlerInstance);
            }
        }

        void IServiceBehavior.Validate(ServiceDescription serviceDescription, ServiceHostBase serviceHostBase) { } // end
      
    } // end class

} // end namespace

```

Configs in Web.config:

```cs
...
<system.serviceModel>

    <services>      
      <service name="WebServices.MyService">
        <endpoint binding="webHttpBinding" contract="WebServices.IMyService" />
      </service>
    </services>

    <extensions>      
      <behaviorExtensions>        
        <!-- This extension if for the WCF Error Handling-->
        <add name="ErrorHandlerBehavior" type="WebServices.BehaviorsAndInspectors.ErrorHandlerExtensionBehavior, WebServices, Version=1.0.0.0, Culture=neutral, PublicKeyToken=null" />      
      </behaviorExtensions>    
    </extensions>

    <behaviors>          
      <serviceBehaviors>        
        <behavior>
          <serviceMetadata httpGetEnabled="true"/>
          <serviceDebug includeExceptionDetailInFaults="true"/>
          <ErrorHandlerBehavior />
        </behavior>     
      </serviceBehaviors>    
    </behaviors>

    ....
</system.serviceModel>
...

```

Here are a few links that may be helpful on this topic:

[https://msdn.microsoft.com/en-us/library/system.servicemodel.dispatcher.ierrorhandler(v=vs.100).aspx](https://msdn.microsoft.com/en-us/library/system.servicemodel.dispatcher.ierrorhandler(v=vs.100).aspx)

[http://www.brainthud.com/cards/5218/25441/which-four-behavior-interfaces-exist-for-interacting-with-a-service-or-client-description-what-methods-do-they-implement-and](http://www.brainthud.com/cards/5218/25441/which-four-behavior-interfaces-exist-for-interacting-with-a-service-or-client-description-what-methods-do-they-implement-and)

Other Examples:

[IErrorHandler returning wrong message body when HTTP status code is 401 Unauthorized](http://stackoverflow.com/questions/38231970/ierrorhandler-returning-wrong-message-body-when-http-status-code-is-401-unauthor)

[IErrorHandler doesn&#39;t seem to be handling my errors in WCF .. any ideas?](http://stackoverflow.com/questions/3036692/ierrorhandler-doesnt-seem-to-be-handling-my-errors-in-wcf-any-ideas)

[How to make custom WCF error handler return JSON response with non-OK http code?](http://stackoverflow.com/questions/1149037/how-to-make-custom-wcf-error-handler-return-json-response-with-non-ok-http-code)

[How do you set the Content-Type header for an HttpClient request?](http://stackoverflow.com/questions/10679214/how-do-you-set-the-content-type-header-for-an-httpclient-request?rq=1)



## Nesting of Exceptions & try catch blocks.


One is able to nest one exception / `try` `catch` block inside the other.

This way one can manage small blocks of code which are capable of working without disrupting your whole mechanism.

```cs
try 
{
//some code here
    try 
    {
        //some thing which throws an exception. For Eg : divide by 0
    }
    catch (DivideByZeroException dzEx)
    {
        //handle here only this exception
        //throw from here will be passed on to the parent catch block
    }
    finally
    {
        //any thing to do after it is done.
    }
 //resume from here & proceed as normal; 
}
catch(Exception e)
{
    //handle here
}

```

**Note:** Avoid [Swallowing Exceptions](https://stackoverflow.com/documentation/c%23/40/exception-handling/6940/exception-anti-patterns#t=201707281310293021372)  when throwing to the parent catch block

