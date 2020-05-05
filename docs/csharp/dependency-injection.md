---
metaTitle: "C# | Dependency Injection"
description: "Dependency Injection C# and ASP.NET with Unity, Dependency injection using MEF"
---

# Dependency Injection



## Dependency Injection C# and ASP.NET with Unity


First why we should use depedency injection in our code ? We want to decouple other components from other classes in our program. For example we have class AnimalController which have code like this :

```cs
public class AnimalController()
{
    private SantaAndHisReindeer _SantaAndHisReindeer = new SantaAndHisReindeer();

    public AnimalController(){
            Console.WriteLine("");
    }
}

```

We look at this code and we think everything is ok but now our AnimalController is reliant on object _SantaAndHisReindeer. Automatically my Controller is bad to testing and reusability of my code will be very hard.

Very good explanation why we should use Depedency Injection and interfaces
[here](http://stackoverflow.com/questions/14301389/why-does-one-use-dependency-injection).

If we want  Unity to handle DI, the road to achieve this is very simple :)
With NuGet( package manager) we can easily import unity to our code.

> 
in Visual Studio        Tools -> NuGet Package Manager -> Manage Packages for Solution -> in search input write unity -> choose our project-> click install


Now two  files with nice comments will be created.

> 
in App-Data folder UnityConfig.cs and UnityMvcActivator.cs


> 
UnityConfig -  in RegisterTypes method, we can see type that will be injection in our constructors.


```cs
namespace Vegan.WebUi.App_Start
{

public class UnityConfig
{
    #region Unity Container
    private static Lazy<IUnityContainer> container = new Lazy<IUnityContainer>(() =>
    {
        var container = new UnityContainer();
        RegisterTypes(container);
        return container;
    });

    /// <summary>
    /// Gets the configured Unity container.
    /// </summary>
    public static IUnityContainer GetConfiguredContainer()
    {
        return container.Value;
    }
    #endregion

    /// <summary>Registers the type mappings with the Unity container.</summary>
    /// <param name="container">The unity container to configure.</param>
    /// <remarks>There is no need to register concrete types such as controllers or API controllers (unless you want to 
    /// change the defaults), as Unity allows resolving a concrete type even if it was not previously registered.</remarks>
    public static void RegisterTypes(IUnityContainer container)
    {
        // NOTE: To load from web.config uncomment the line below. Make sure to add a Microsoft.Practices.Unity.Configuration to the using statements.
        // container.LoadConfiguration();

        // TODO: Register your types here
        // container.RegisterType<IProductRepository, ProductRepository>();

        container.RegisterType<ISanta, SantaAndHisReindeer>();
        
     }
 }
}

```

> 
UnityMvcActivator - > also with nice comments which say that this class integrates Unity with ASP.NET MVC


```cs
using System.Linq;
using System.Web.Mvc;
using Microsoft.Practices.Unity.Mvc;

[assembly: WebActivatorEx.PreApplicationStartMethod(typeof(Vegan.WebUi.App_Start.UnityWebActivator), "Start")]
[assembly: WebActivatorEx.ApplicationShutdownMethod(typeof(Vegan.WebUi.App_Start.UnityWebActivator), "Shutdown")]
    
namespace Vegan.WebUi.App_Start
{
/// <summary>Provides the bootstrapping for integrating Unity with ASP.NET MVC.</summary>
public static class UnityWebActivator
{
    /// <summary>Integrates Unity when the application starts.</summary>
    public static void Start() 
    {
        var container = UnityConfig.GetConfiguredContainer();

        FilterProviders.Providers.Remove(FilterProviders.Providers.OfType<FilterAttributeFilterProvider>().First());
        FilterProviders.Providers.Add(new UnityFilterAttributeFilterProvider(container));

        DependencyResolver.SetResolver(new UnityDependencyResolver(container));

        // TODO: Uncomment if you want to use PerRequestLifetimeManager
        // Microsoft.Web.Infrastructure.DynamicModuleHelper.DynamicModuleUtility.RegisterModule(typeof(UnityPerRequestHttpModule));
    }

    /// <summary>Disposes the Unity container when the application is shut down.</summary>
    public static void Shutdown()
    {
        var container = UnityConfig.GetConfiguredContainer();
        container.Dispose();
    }
}
}

```

Now we can decouple our Controller from class SantAndHisReindeer :)

```cs

public class AnimalController()
    {
        private readonly SantaAndHisReindeer _SantaAndHisReindeer;

        public AnimalController(SantaAndHisReindeer SantaAndHisReindeer){

                _SantAndHisReindeer = SantaAndHisReindeer;
        }
    }

```

> 
There is one final thing we must do before running our application.


In Global.asax.cs we must add new line: UnityWebActivator.Start() which will start, configure Unity and register our types.

```cs
using System;
using System.Collections.Generic;
using System.Linq;
using System.Web;
using System.Web.Mvc;
using System.Web.Optimization;
using System.Web.Routing;
using Vegan.WebUi.App_Start;

namespace Vegan.WebUi
{
    public class MvcApplication : System.Web.HttpApplication
    {
        protected void Application_Start()
        {
            AreaRegistration.RegisterAllAreas();
            FilterConfig.RegisterGlobalFilters(GlobalFilters.Filters);
            RouteConfig.RegisterRoutes(RouteTable.Routes);
            BundleConfig.RegisterBundles(BundleTable.Bundles);
            UnityWebActivator.Start();
        }
    }
}

```



## Dependency injection using MEF


```cs
public interface ILogger
{
    void Log(string message);
}

[Export(typeof(ILogger))]
[ExportMetadata("Name", "Console")]  
public class ConsoleLogger:ILogger
{
    public void Log(string message)
    {
        Console.WriteLine(message);
    }
}

[Export(typeof(ILogger))]
[ExportMetadata("Name", "File")]  
public class FileLogger:ILogger
{
    public void Log(string message)
    {
        //Write the message to file
    }
}

public class User
{  
    private readonly ILogger logger;
    public User(ILogger logger)   
    {
        this.logger = logger;
    }
    public void LogUser(string message)
    {
        logger.Log(message)  ;
    }
}

public interface ILoggerMetaData
{
    string Name { get; }
}

internal class Program
{
    private CompositionContainer _container;
    
    [ImportMany]
    private IEnumerable<Lazy<ILogger, ILoggerMetaData>> _loggers;
    
    private static void Main()
    {            
        ComposeLoggers();
        Lazy<ILogger, ILoggerMetaData> loggerNameAndLoggerMapping = _ loggers.First((n) => ((n.Metadata.Name.ToUpper() =="Console"));
        ILogger logger= loggerNameAndLoggerMapping.Value
        var user = new User(logger);
        user.LogUser("user name");
    }
    
    private void ComposeLoggers()
    {
        //An aggregate catalog that combines multiple catalogs
        var catalog = new AggregateCatalog();
        string loggersDllDirectory =Path.Combine(Utilities.GetApplicationDirectory(), "Loggers");
        if (!Directory.Exists(loggersDllDirectory ))
        {
            Directory.CreateDirectory(loggersDllDirectory );
        }
        //Adds all the parts found in the same assembly as the PluginManager class
        catalog.Catalogs.Add(new AssemblyCatalog(typeof(Program).Assembly));
        catalog.Catalogs.Add(new DirectoryCatalog(loggersDllDirectory ));
        
        //Create the CompositionContainer with the parts in the catalog
        _container = new CompositionContainer(catalog);
        
        //Fill the imports of this object
        try
        {
            this._container.ComposeParts(this);
        }
        catch (CompositionException compositionException)
        {
            throw new CompositionException(compositionException.Message);
        }
    } 
}

```



#### Remarks


Wikipedia definition of dependency injection is:

In software engineering, dependency injection is a software design pattern that implements inversion of control for resolving dependencies. A dependency is an object that can be used (a service). An injection is the passing of a dependency to a dependent object (a client) that would use it.

**<strong>This site features an answer to the question How to explain Dependency
Injection to a 5-year old. The most highly rated answer, provided
by John Munsch   provides a surprisingly accurate
analogy targeted at the (imaginary) five-year-old inquisitor:
When you go and get things out of the refrigerator for yourself,
you can cause problems. You might leave the door open, you
might get something Mommy or Daddy doesn’t want you to
have. You might even be looking for something we don’t even
have or which has expired.
What you should be doing is stating a need, “I need something
to drink with lunch,” and then we will make sure you have
something when you sit down to eat.
What this means in terms of object-oriented software development
is this: collaborating classes (the five-year-olds)
should rely on the infrastructure (the parents) to provide</strong>

** This code uses MEF to dynamically load the dll and resolve the
dependencies. ILogger dependency is resolved by MEF and injectd into
the user class. User class never receives Concrete implementation of
ILogger and it has no idea of what or which type of logger its using.**

