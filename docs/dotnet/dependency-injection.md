---
metaTitle: "Dependency Injection"
description: "Dependency Injection - Simple example, How Dependency Injection Makes Unit Testing Easier, Why We Use Dependency Injection Containers (IoC Containers)"
---

# Dependency Injection



## Dependency Injection - Simple example


This class is called `Greeter`. Its responsibility is to output a greeting. It has two **dependencies**. It needs something that will give it the greeting to output, and then it needs a way to output that greeting. Those dependencies are both described as interfaces, `IGreetingProvider` and `IGreetingWriter`. In this example, those two dependencies are "injected" into `Greeter`. (Further explanation following the example.)

```dotnet
public class Greeter
{
    private readonly IGreetingProvider _greetingProvider;
    private readonly IGreetingWriter _greetingWriter;

    public Greeter(IGreetingProvider greetingProvider, IGreetingWriter greetingWriter)
    {
        _greetingProvider = greetingProvider;
        _greetingWriter = greetingWriter;
    }

    public void Greet()
    {
        var greeting = _greetingProvider.GetGreeting();
        _greetingWriter.WriteGreeting(greeting);
    }
}

public interface IGreetingProvider
{
    string GetGreeting();
}

public interface IGreetingWriter
{
    void WriteGreeting(string greeting);
}

```

The `Greeting` class depends on both `IGreetingProvider` and `IGreetingWriter`, but it is not responsible for creating instances of either. Instead it requires them in its constructor. Whatever creates an instance of `Greeting` must provide those two dependencies. We can call that "injecting" the dependencies.

Because dependencies are provided to the class in its constructor, this is also called "constructor injection."

A few common conventions:

- The constructor saves the dependencies as `private` fields. As soon as the class is instantiated, those dependencies are available to all other non-static methods of the class.
- The `private` fields are `readonly`. Once they are set in the constructor they cannot be changed. This indicates that those fields should not (and cannot) be modified outside of the constructor. That further ensures that those dependencies will be available for the lifetime of the class.
- The dependencies are interfaces. This is not strictly necessary, but is common because it makes it easier to substitute one implementation of the dependency with another. It also allows providing a mocked version of the interface for unit testing purposes.



## How Dependency Injection Makes Unit Testing Easier


This builds on the previous example of the `Greeter` class which has two dependencies, `IGreetingProvider` and `IGreetingWriter`.

The actual implementation of `IGreetingProvider` might retrieve a string from an API call or a database. The implementation of `IGreetingWriter` might display the greeting in the console. But because `Greeter` has its dependencies injected into its constructor, it's easy to write a unit test that injects mocked versions of those interfaces. In real life we might use a framework like [Moq](http://www.moqthis.com/), but in this case I'll write those mocked implementations.

```dotnet
public class TestGreetingProvider : IGreetingProvider
{
    public const string TestGreeting = "Hello!";

    public string GetGreeting()
    {
        return TestGreeting;
    }
}

public class TestGreetingWriter : List<string>, IGreetingWriter
{
    public void WriteGreeting(string greeting)
    {
        Add(greeting);
    }
}

[TestClass]
public class GreeterTests
{
    [TestMethod]
    public void Greeter_WritesGreeting()
    {
        var greetingProvider = new TestGreetingProvider();
        var greetingWriter = new TestGreetingWriter();
        var greeter = new Greeter(greetingProvider, greetingWriter);
        greeter.Greet();
        Assert.AreEqual(greetingWriter[0], TestGreetingProvider.TestGreeting);
    }
}

```

The behavior of `IGreetingProvider` and `IGreetingWriter` are not relevant to this test. We want to test that `Greeter` gets a greeting and writes it. The design of `Greeter` (using dependency injection) allows us to inject mocked dependencies without any complicated moving parts. All we're testing is that `Greeter` interacts with those dependencies as we expect it to.



## Why We Use Dependency Injection Containers (IoC Containers)


Dependency injection means writing classes so that they do not control their dependencies - instead, their dependencies are provided to them ("injected.")

This is not the same thing as using a dependency injection framework (often called a "DI container", "IoC container", or just "container") like Castle Windsor, Autofac, SimpleInjector, Ninject, Unity, or others.

A container just makes dependency injection easier. For example, suppose you write a number of classes that rely on dependency injection. One class depends on several interfaces, the classes that implement those interfaces depend on other interfaces, and so on. Some depend on specific values. And just for fun, some of those classes implement `IDisposable` and need to be disposed.

Each individual class is well-written and easy to test. But now there's a different problem: Creating an instance of a class has become much more complicated. Suppose we're creating an instance of a `CustomerService` class. It has dependencies and its dependencies have dependencies. Constructing an instance might look something like this:

```dotnet
public CustomerData GetCustomerData(string customerNumber)
{
    var customerApiEndpoint = ConfigurationManager.AppSettings["customerApi:customerApiEndpoint"];
    var logFilePath = ConfigurationManager.AppSettings["logwriter:logFilePath"];
    var authConnectionString = ConfigurationManager.ConnectionStrings["authorization"].ConnectionString;
    using(var logWriter = new LogWriter(logFilePath ))
    {
        using(var customerApiClient = new CustomerApiClient(customerApiEndpoint))
        {
            var customerService = new CustomerService(
                new SqlAuthorizationRepository(authorizationConnectionString, logWriter),
                new CustomerDataRepository(customerApiClient, logWriter),
                logWriter
            );   
            
            // All this just to create an instance of CustomerService!         
            return customerService.GetCustomerData(string customerNumber);
        }
    }
}

```

You might wonder, why not put the whole giant construction in a separate function that just returns `CustomerService`? One reason is that because the dependencies for each class are injected into it, a class isn't responsible for knowing whether those dependencies are `IDisposable` or disposing them. It just uses them. So if a we had a `GetCustomerService()` function that returned a fully-constructed `CustomerService`, that class might contain a number of disposable resources and no way to access or dispose them.

And aside from disposing `IDisposable`, who wants to call a series of nested constructors like that, ever? That's a short example. It could get much, much worse. Again, that doesn't mean that we wrote the classes the wrong way. The classes might be individually perfect. The challenge is composing them together.

A dependency injection container simplifies that. It allows us to specify which class or value should be used to fulfill each dependency. This slightly oversimplified example uses Castle Windsor:

```dotnet
var container = new WindsorContainer()
container.Register(
    Component.For<CustomerService>(),
    Component.For<ILogWriter, LogWriter>()
        .DependsOn(Dependency.OnAppSettingsValue("logFilePath", "logWriter:logFilePath")),
    Component.For<IAuthorizationRepository, SqlAuthorizationRepository>()
        .DependsOn(Dependency.OnValue(connectionString, ConfigurationManager.ConnectionStrings["authorization"].ConnectionString)),
    Component.For<ICustomerDataProvider, CustomerApiClient>()
         .DependsOn(Dependency.OnAppSettingsValue("apiEndpoint", "customerApi:customerApiEndpoint"))   
);

```

We call this "registering dependencies" or "configuring the container." Translated, this tells our `WindsorContainer`:

- If a class requires `ILogWriter`, create an instance of `LogWriter`. `LogWriter` requires a file path. Use this value from `AppSettings`.
- If a class requires `IAuthorizationRepository`, create an instance of `SqlAuthorizationRepository`. It requires a connection string. Use this value from the `ConnectionStrings` section.
- If a class requires `ICustomerDataProvider`, create a `CustomerApiClient` and provide the string it needs from `AppSettings`.

When we request a dependency from the container we call that "resolving" a dependency. It's bad practice to do that directly using the container, but that's a different story. For demonstration purposes, we could now do this:

```dotnet
var customerService = container.Resolve<CustomerService>();
var data = customerService.GetCustomerData(customerNumber);
container.Release(customerService);

```

The container knows that `CustomerService` depends on `IAuthorizationRepository` and `ICustomerDataProvider`. It knows what classes it needs to create to fulfill those requirements. Those classes, in turn, have more dependencies, and the container knows how to fulfill those. It will create every class it needs to until it can return an instance of `CustomerService`.

If it gets to a point where a class requires a dependency that we haven't registered, like `IDoesSomethingElse`, then when we try to resolve `CustomerService` it will throw a clear exception telling us that we haven't registered anything to fulfill that requirement.

Each DI framework behaves a little differently, but typically they give us some control over how certain classes are instantiated. For example, do we want it to create one instance of `LogWriter` and provide it to every class that depends on `ILogWriter`, or do we want it to create a new one every time? Most containers have a way to specify that.

What about classes that implement `IDisposable`? That's why we call `container.Release(customerService);` at the end. Most containers (including Windsor) will step back through all of the dependencies created and `Dispose` the ones that need disposing. If `CustomerService` is `IDisposable` it will dispose that too.

Registering dependencies as seen above might just look like more code to write. But when we have lots of classes with lots of dependencies then it really pays off. And if we had to write those same classes **without** using dependency injection then that same application with lots of classes would become difficult to maintain and test.

This scratches the surface of **why** we use dependency injection containers. **How** we configure our application to use one (and use it correctly) is not just one topic - it's a number of topics, as the instructions and examples vary from one container to the next.



#### Remarks


**Problems Solved By Dependency Injection**

If we didn't use dependency injection, the `Greeter` class might look more like this:

```dotnet
public class ControlFreakGreeter
{
    public void Greet()
    {
        var greetingProvider = new SqlGreetingProvider(
            ConfigurationManager.ConnectionStrings["myConnectionString"].ConnectionString);
        var greeting = greetingProvider.GetGreeting();
        Console.WriteLine(greeting);
    }
}

```

It's a "control freak" because it controls creating the class that provides the greeting, it controls where the SQL connection string comes from, and it controls the output.

Using dependency injection, the `Greeter` class relinquishes those responsibilities in favor of a single responsibility, writing a greeting provided to it.

The [Dependency Inversion Principle](https://en.wikipedia.org/wiki/Dependency_inversion_principle) suggests that classes should depend on abstractions (like interfaces) rather than on other concrete classes. Direct dependencies (coupling) between classes can make maintenance progressively difficult. Depending on abstractions can reduce that coupling.

Dependency injection helps us to achieve that dependency inversion because it leads to writing classes that depend on abstractions. The `Greeter` class "knows" nothing at all of the implementation details of `IGreetingProvider` and `IGreetingWriter`. It only knows that the injected dependencies implement those interfaces. That means that changes to the concrete classes that implement `IGreetingProvider` and `IGreetingWriter` will not affect `Greeter`. Neither will replacing them with entirely different implementations. Only changes to the interfaces will. `Greeter` is decoupled.

`ControlFreakGreeter` is impossible to properly unit test. We want to test one small unit of code, but instead our test would include connecting to SQL and executing a stored procedure. It would also include testing the console output. Because ControlFreakGreeter does so much it's impossible to test in isolation from other classes.

`Greeter` is easy to unit test because we can inject mocked implementations of its dependencies that are easier to execute and verify than calling a stored procedure or reading the output of the console. It doesn't require a connection string in app.config.

The concrete implementations of `IGreetingProvider` and `IGreetingWriter` might become more complex. They, in turn might have their own dependencies which are injected into them. (For example, we'd inject the SQL connection string into `SqlGreetingProvider`.) But that complexity is "hidden" from other classes which only depend on the interfaces. That makes it easier to modify one class without a "ripple effect" that requires us to make corresponding changes to other classes.

