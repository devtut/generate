---
metaTitle: "C# | Using Statement"
description: "Gotcha: returning the resource which you are disposing, Using Statement Basics, Multiple using statements with one block, Gotcha: Exception in Dispose method masking other errors in Using blocks, Using statements are null-safe, Using Dispose Syntax to define custom scope, Using Statements and Database Connections, Executing code in constraint context"
---

# Using Statement


Provides a convenient syntax that ensures the correct use of [IDisposable](https://docs.microsoft.com/en-us/dotnet/api/system.idisposable?view=netframework-4.7) objects.



## Gotcha: returning the resource which you are disposing


The following is a bad idea because it would dispose the `db` variable before returning it.

```cs
public IDBContext GetDBContext()
{
    using (var db = new DBContext())
    {
        return db;
    }
}

```

This can also create more subtle mistakes:

```cs
public IEnumerable<Person> GetPeople(int age)
{
    using (var db = new DBContext())
    {
        return db.Persons.Where(p => p.Age == age);
    }
}

```

This looks ok, but the catch is that the LINQ expression evaluation is lazy, and will possibly only be executed later when the underlying `DBContext` has already been disposed.

So in short the expression isn't evaluated before leaving the `using`. One possible solution to this problem, which still makes use of `using`, is to cause the expression to evaluate immediately by calling a method that will enumerate the result. For example `ToList()`, `ToArray()`, etc. If you are using the newest version of Entity Framework you could use the `async` counterparts like `ToListAsync()` or `ToArrayAsync()`.

Below you find the example in action:

```cs
public IEnumerable<Person> GetPeople(int age)
{
    using (var db = new DBContext())
    {
        return db.Persons.Where(p => p.Age == age).ToList();
    }
}

```

It is important to note, though, that by calling `ToList()` or `ToArray()`, the expression will be eagerly evaluated, meaning that all the persons with the specified age will be loaded to memory even if you do not iterate on them.



## Using Statement Basics


`using` is syntactic sugar that allows you to guarantee that a resource is cleaned up without needing an explicit `try-finally` block. This means your code will be much cleaner, and you won't leak non-managed resources.

Standard `Dispose` cleanup pattern, for objects that implement the `IDisposable` interface (which the `FileStream`'s base class `Stream` does in .NET):

```cs
int Foo()
{
    var fileName = "file.txt";

    {
        FileStream disposable = null;

        try
        {
            disposable = File.Open(fileName, FileMode.Open);

            return disposable.ReadByte();
        }
        finally
        {
            // finally blocks are always run
            if (disposable != null) disposable.Dispose();
        }
    }
}

```

`using` simplifies your syntax by hiding the explicit `try-finally`:

```cs
int Foo()
{
    var fileName = "file.txt";

    using (var disposable = File.Open(fileName, FileMode.Open))
    {
        return disposable.ReadByte();
    }
    // disposable.Dispose is called even if we return earlier
}

```

Just like `finally` blocks always execute regardless of errors or returns, `using` always calls `Dispose()`, even in the event of an error:

```cs
int Foo()
{
    var fileName = "file.txt";

    using (var disposable = File.Open(fileName, FileMode.Open))
    {
        throw new InvalidOperationException();
    }
    // disposable.Dispose is called even if we throw an exception earlier
}

```

**Note:**
Since `Dispose` is guaranteed to be called irrespective of the code flow, it's a good idea to make sure that `Dispose` never throws an exception when you implement `IDisposable`. Otherwise an actual exception would get overridden by the new exception resulting in a debugging nightmare.

### Returning from using block

```cs
using ( var disposable = new DisposableItem() )
{
    return disposable.SomeProperty;
}

```

Because of the semantics of `try..finally` to which the `using` block translates, the `return` statement works as expected - the return value is evaluated before `finally` block is executed and the value disposed. The order of evaluation is as follows:

1. Evaluate the `try` body
1. Evaluate and cache the returned value
1. Execute finally block
1. Return the cached return value

However, you may not return the variable `disposable` itself, as it would contain invalid, disposed reference - see [related example](http://stackoverflow.com/documentation/c%23/38/using-statement/327/gotcha-returning-the-resource-which-you-are-disposing#t=201608220847304515557).



## Multiple using statements with one block


It is possible to use multiple nested `using` statements without added multiple levels of nested braces. For example:

```cs
using (var input = File.OpenRead("input.txt"))
{
    using (var output = File.OpenWrite("output.txt"))
    {
        input.CopyTo(output);
    } // output is disposed here
} // input is disposed here

```

An alternative is to write:

```cs
using (var input = File.OpenRead("input.txt"))
using (var output = File.OpenWrite("output.txt"))
{
    input.CopyTo(output);
} // output and then input are disposed here

```

Which is exactly equivalent to the first example.

**Note:** Nested `using` statements might trigger Microsoft Code Analysis rule [CS2002](https://msdn.microsoft.com/en-us/library/ms182334.aspx) (see [this answer](http://stackoverflow.com/a/22323027/501011) for clarification) and generate a warning. As explained in the linked answer, it is generally safe to nest `using` statements.

When the types within the `using` statement are of the same type you can comma-delimit them and specify the type only once (though this is uncommon):

```cs
using (FileStream file = File.Open("MyFile.txt"), file2 = File.Open("MyFile2.txt"))
{
}

```

This can also be used when the types have a shared hierarchy:

```cs
using (Stream file = File.Open("MyFile.txt"), data = new MemoryStream())
{
}

```

The `var` keyword **cannot** be used in the above example. A compilation error would occur. Even the comma separated declaration won't work when the declared variables have types from different hierarchies.



## Gotcha: Exception in Dispose method masking other errors in Using blocks


Consider the following block of code.

```cs
try
{
    using (var disposable = new MyDisposable())
    {
        throw new Exception("Couldn't perform operation.");
    }
}
catch (Exception ex)
{
    Console.WriteLine(ex.Message);
}

class MyDisposable : IDisposable
{
    public void Dispose()
    {
        throw new Exception("Couldn't dispose successfully.");
    }
}

```

You may expect to see "Couldn't perform operation" printed to the Console but you would actually see "Couldn't dispose successfully." as the Dispose method is still called even after the first exception is thrown.

It is worth being aware of this subtlety as it may be masking the real error that prevented the object from being disposed and make it harder to debug.



## Using statements are null-safe


You don't have to check the `IDisposable` object for `null`. `using` will not throw an exception and `Dispose()` will not be called:

```cs
DisposableObject TryOpenFile()
{
    return null;
}

// disposable is null here, but this does not throw an exception 
using (var disposable = TryOpenFile())
{
    // this will throw a NullReferenceException because disposable is null
    disposable.DoSomething(); 

    if(disposable != null)
    {
        // here we are safe because disposable has been checked for null
        disposable.DoSomething();
    }
}

```



## Using Dispose Syntax to define custom scope


For some use cases, you can use the `using` syntax to help define a custom scope. For example, you can define the following class to execute code in a specific culture.

```cs
public class CultureContext : IDisposable
{
    private readonly CultureInfo originalCulture;

    public CultureContext(string culture)
    {
        originalCulture = CultureInfo.CurrentCulture;
        Thread.CurrentThread.CurrentCulture = new CultureInfo(culture);
    }

    public void Dispose()
    {
        Thread.CurrentThread.CurrentCulture = originalCulture;
    }
}

```

You can then use use this class to define blocks of code that execute in a specific culture.

```cs
Thread.CurrentThread.CurrentCulture = new CultureInfo("en-US");

using (new CultureContext("nl-NL"))
{
    // Code in this block uses the "nl-NL" culture
    Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 25-12-2016 00:00:00
}

using (new CultureContext("es-ES"))
{        
    // Code in this block uses the "es-ES" culture
    Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 25/12/2016 0:00:00
}

// Reverted back to the original culture
Console.WriteLine(new DateTime(2016, 12, 25)); // Output: 12/25/2016 12:00:00 AM

```

Note: as we don't use the `CultureContext` instance we create, we don't assign a variable for it.

This technique is used by the `BeginForm` [helper](https://msdn.microsoft.com/en-us/library/dd410596%28v=vs.100%29.aspx) in ASP.NET MVC.



## Using Statements and Database Connections


The `using` keyword ensures that the resource defined within the statement only exists within the scope of the statement itself. Any resources defined within the statement must implement the `IDisposable` interface.

These are incredibly important when dealing with any connections that implement the `IDisposable` interface as it can ensure the connections are not only properly closed but that their resources are freed after the `using` statement is out of scope.

### **Common `IDisposable` Data Classes**

Many of the following are data-related classes that implement the `IDisposable` interface and are perfect candidates for a `using` statement :

- `SqlConnection`,`SqlCommand`,`SqlDataReader`, etc.
- `OleDbConnection`,`OleDbCommand`,`OleDbDataReader`, etc.
- `MySqlConnection`, `MySqlCommand`, `MySqlDbDataReader`, etc.
- `DbContext`

All of these are commonly used to access data through C# and will be commonly encountered throughout building data-centric applications. Many other classes that are not mentioned that implement the same `FooConnection`,`FooCommand`,`FooDataReader` classes can be expected to behave the same way.

### **Common Access Pattern for ADO.NET Connections**

A common pattern that can be used when accessing your data through an ADO.NET connection might look as follows :

```cs
// This scopes the connection (your specific class may vary)
using(var connection = new SqlConnection("{your-connection-string}")
{
    // Build your query
    var query = "SELECT * FROM YourTable WHERE Property = @property");
    // Scope your command to execute
    using(var command = new SqlCommand(query, connection))
    {
         // Open your connection
         connection.Open();

         // Add your parameters here if necessary

         // Execute your query as a reader (again scoped with a using statement)
         using(var reader = command.ExecuteReader())
         {
               // Iterate through your results here
         }
    }
}

```

Or if you were just performing a simple update and didn't require a reader, the same basic concept would apply :

```cs
using(var connection = new SqlConnection("{your-connection-string}"))
{
     var query = "UPDATE YourTable SET Property = Value WHERE Foo = @foo";
     using(var command = new SqlCommand(query,connection))
     {
          connection.Open();
          
          // Add parameters here
          
          // Perform your update
          command.ExecuteNonQuery();
     }
}

```

### **Using Statements with DataContexts**

Many ORMs such as Entity Framework expose abstraction classes that are used to interact with underlying databases in the form of classes like `DbContext`. These contexts generally implement the `IDisposable` interface as well and should take advantage of this through `using` statements when possible :

```cs
using(var context = new YourDbContext())
{
      // Access your context and perform your query
      var data = context.Widgets.ToList();
}

```



## Executing code in constraint context


If you have code (a **routine**) you want to execute under a specific (constraint) context, you can use dependency injection.

The following example shows the constraint of executing under an open SSL connection. This first part would be in your library or framework, which you won't expose to the client code.

```cs
public static class SSLContext
{
    // define the delegate to inject
    public delegate void TunnelRoutine(BinaryReader sslReader, BinaryWriter sslWriter);

    // this allows the routine to be executed under SSL
    public static void ClientTunnel(TcpClient tcpClient, TunnelRoutine routine)
    {
        using (SslStream sslStream = new SslStream(tcpClient.GetStream(), true, _validate))
        {
            sslStream.AuthenticateAsClient(HOSTNAME, null, SslProtocols.Tls, false);

            if (!sslStream.IsAuthenticated)
            {
                throw new SecurityException("SSL tunnel not authenticated");
            }

            if (!sslStream.IsEncrypted)
            {
                throw new SecurityException("SSL tunnel not encrypted");
            }

            using (BinaryReader sslReader = new BinaryReader(sslStream))
            using (BinaryWriter sslWriter = new BinaryWriter(sslStream))
            {
                routine(sslReader, sslWriter);
            }
        }
    }
}

```

Now the client code which wants to do something under SSL but does not want to handle all the SSL details. You can now do whatever you want inside the SSL tunnel, for example exchange a symmetric key:

```cs
public void ExchangeSymmetricKey(BinaryReader sslReader, BinaryWriter sslWriter)
{
    byte[] bytes = new byte[8];
    (new RNGCryptoServiceProvider()).GetNonZeroBytes(bytes);
    sslWriter.Write(BitConverter.ToUInt64(bytes, 0));
}

```

You execute this routine as follows:

```cs
SSLContext.ClientTunnel(tcpClient, this.ExchangeSymmetricKey);

```

To do this, you need the `using()` clause because it is the only way (apart from a `try..finally` block) you can guarantee the client code (`ExchangeSymmetricKey`) never exits without properly disposing of the disposable resources. Without `using()` clause, you would never know if a routine could break the context's constraint to dispose of those resources.



#### Syntax


- using (disposable) { }
- using (IDisposable disposable = new MyDisposable()) { }



#### Remarks


The object in the `using` statement must implement the `IDisposable` interface.

```cs
using(var obj = new MyObject())
{
}

class MyObject : IDisposable
{
    public void Dispose()
    {
        // Cleanup
    }
}

```

More complete examples for `IDisposable` implementation can be found at the [MSDN docs](https://msdn.microsoft.com/en-us/library/fs2xkftw(v=vs.110).aspx).

