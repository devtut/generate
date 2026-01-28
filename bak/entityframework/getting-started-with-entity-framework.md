---
metaTitle: "Entity Framework - Getting started with Entity Framework"
description: "Installing The Entity Framework NuGet Package, Using Entity Framework from C# (Code First), What is Entity Framework ?"
---

# Getting started with Entity Framework



## Installing The Entity Framework NuGet Package


In your Visual Studio open the **Solution Explorer** window then <kbd>right click</kbd> on your project then choose **Manage NuGet Packages** from the menu:

[<img src="http://i.stack.imgur.com/Wx3Hk.png" alt="Manage nuget packages" />](http://i.stack.imgur.com/Wx3Hk.png)

In the window that opens type `EntityFramework` in the search box in the top right.

[<img src="http://i.stack.imgur.com/NgmOs.png" alt="enter image description here" />](http://i.stack.imgur.com/NgmOs.png)

Or if you are using Visual Studio 2015 you'll see something like this:

[<img src="http://i.stack.imgur.com/ln0Z9.png" alt="enter image description here" />](http://i.stack.imgur.com/ln0Z9.png)

Then click Install.

We can also install entity framework using the package manager console. To do you have first to open it using the **Tools menu -> NuGet Package Manager -> Package Manager Console** then enter this:

```cs
Install-Package EntityFramework

```

[<img src="http://i.stack.imgur.com/6iSJR.png" alt="enter image description here" />](http://i.stack.imgur.com/6iSJR.png)

This will install Entity Framework and automatically add a reference to the assembly in your project.



## Using Entity Framework from C# (Code First)


Code first allows you to create your entities (classes) without using a GUI designer or a .edmx file. It is named **Code first**, because you can create your models **first** and **Entity framework** will create database according to mappings for you automatically. Or you can also use this approach with existing database, which is called **code first with existing database**  For example, if you want a table to hold a list of planets:

```cs
public class Planet
{
    public string Name { get; set; }
    public decimal AverageDistanceFromSun { get; set; }
}

```

Now create your context which is the bridge between your entity classes and the database. Give it one or more `DbSet<>` properties:

```cs
using System.Data.Entity;

public class PlanetContext : DbContext
{
    public DbSet<Planet> Planets { get; set; }
}

```

We can use this by doing the following:

```cs
using(var context = new PlanetContext())
{
    var jupiter = new Planet 
    {
        Name = "Jupiter", 
        AverageDistanceFromSun = 778.5
    };

    context.Planets.Add(jupiter);
    context.SaveChanges();
}

```

In this example we create a new `Planet` with the `Name` property with the value of `"Jupiter"` and the `AverageDistanceFromSun` property with the value of `778.5`

We can then add this `Planet` to the context by using the `DbSet`'s `Add()` method and commit our changes to the database by using the `SaveChanges()` method.

Or we can retrieve rows from the database:

```cs
using(var context = new PlanetContext())
{
    var jupiter = context.Planets.Single(p => p.Name == "Jupiter");
    Console.WriteLine($"Jupiter is {jupiter.AverageDistanceFromSun} million km from the sun.");
}

```



## What is Entity Framework ?


Writing and managing ADO.Net code for data access is a tedious and monotonous job. Microsoft has provided an **O/RM framework called "Entity Framework"** to automate database related activities for your application.

Entity framework is an Object/Relational Mapping (O/RM) framework. It is an enhancement to ADO.NET that gives developers an automated mechanism for accessing & storing the data in the database.

**What is O/RM?**

ORM is a tool for storing data from domain objects to the relational database like MS SQL Server, in an automated way, without much programming.
O/RM includes three main parts:

1. Domain class objects
1. Relational database objects
1. Mapping information on how domain objects map to relational database objects(**e.x** tables, views & stored procedures)

ORM allows us to keep our database design separate from our domain class design. This makes the application maintainable and extendable. It also automates standard CRUD operation (Create, Read, Update & Delete) so that the developer doesn't need to write it manually.



#### Remarks


Entity Framework (EF) is an object-relational mapper (ORM) that enables .NET developers to work with relational data using domain-specific objects. It eliminates the need for most of the data-access code that developers usually need to write.

Entity Framework allows you to create a model by writing code or using boxes and lines in the EF Designer. Both of these approaches can be used to target an existing database or create a new database.

Entity Framework is the main ORM that Microsoft provides for the .NET Framework and Microsoft’s recommended data access technology.

