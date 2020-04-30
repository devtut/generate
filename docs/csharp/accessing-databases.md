---
metaTitle: "Accessing Databases"
description: "Connection Strings, Entity Framework Connections, ADO.NET Connections"
---

# Accessing Databases



## Connection Strings


A Connection String is a string that specifies information about a particular data source and how to go about connecting to it by storing credentials, locations, and other information.

```cs
Server=myServerAddress;Database=myDataBase;User Id=myUsername;Password=myPassword;

```

### **Storing Your Connection String**

Typically, a connection string will be stored within a configuration file (such as an `app.config` or `web.config` within ASP.NET applications). The following is an example of what a local connection might look like within one of these files :

```cs
<connectionStrings> 
   <add name="WidgetsContext" providerName="System.Data.SqlClient"  connectionString="Server=.\SQLEXPRESS;Database=Widgets;Integrated Security=True;"/> 
</connectionStrings>

<connectionStrings> 
   <add name="WidgetsContext" providerName="System.Data.SqlClient"  connectionString="Server=.\SQLEXPRESS;Database=Widgets;Integrated Security=SSPI;"/> 
</connectionStrings>

```

This will allow your application to access the connection string programatically through `WidgetsContext`. Although both `Integrated Security=SSPI` and `Integrated Security=True` perform the same function;`Integrated Security=SSPI` is preferred since works with both SQLClient & OleDB provider where as `Integrated Security=true` throws an exception when used with the OleDb provider.

### **Different Connections for Different Providers**

Each data provider (SQL Server, MySQL, Azure, etc.) all feature their own flavor of syntax for their connection strings and expose different available properties. [ConnectionStrings.com](https://www.connectionstrings.com/) is an incredibly useful resource if you are unsure about what yours should look like.



## Entity Framework Connections


Entity Framework exposes abstraction classes that are used to interact with underlying databases in the form of classes like `DbContext`. These contexts generally consist of `DbSet<T>` properties that expose the available collections that can be queried :

```cs
public class ExampleContext: DbContext 
{ 
    public virtual DbSet<Widgets> Widgets { get; set; } 
}

```

The `DbContext` itself will handle making the connections with the databases and will generally read the appropriate Connection String data from a configuration to determine how to establish the connections :

```cs
public class ExampleContext: DbContext 
{ 
    // The parameter being passed in to the base constructor indicates the name of the 
    // connection string
    public ExampleContext() : base("ExampleContextEntities")
    {
    }

    public virtual DbSet<Widgets> Widgets { get; set; } 
}

```

### **Executing Entity Framework Queries**

Actually executing an Entity Framework query can be quite easy and simply requires you to create an instance of the context and then use the available properties on it to pull or access your data

```cs
using(var context = new ExampleContext())
{
      // Retrieve all of the Widgets in your database
      var data = context.Widgets.ToList();
}

```

Entity Framework also provides an extensive change-tracking system that can be used to handle updating entries within your database by simply calling the `SaveChanges()` method to push changes to the database :

```cs
using(var context = new ExampleContext())
{
      // Grab the widget you wish to update
      var widget = context.Widgets.Find(w => w.Id == id);
      // If it exists, update it
      if(widget != null)
      {
           // Update your widget and save your changes
           widget.Updated = DateTime.UtcNow;
           context.SaveChanges();
      }
}

```



## ADO.NET Connections


ADO.NET Connections are one of the simplest ways to connect to a database from a C# application. They rely on the use of a provider and a connection string that points to your database to perform queries against.

### **Common Data Provider Classes**

Many of the following are classes that are commonly used to query databases and their related namespaces :

- `SqlConnection`,`SqlCommand`,`SqlDataReader` from `System.Data.SqlClient`
- `OleDbConnection`,`OleDbCommand`,`OleDbDataReader` from `System.Data.OleDb`
- `MySqlConnection`, `MySqlCommand`, `MySqlDbDataReader` from [`MySql.Data`](http://dev.mysql.com/downloads/file/?id=13427)

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

You can even program against a set of common interfaces and not have to worry about the provider specific classes. The core interfaces provided by ADO.NET are:

- IDbConnection - for managing database connections
- IDbCommand - for running SQL commands
- IDbTransaction - for managing transactions
- IDataReader - for reading data returned by a command
- IDataAdapter - for channeling data to and from datasets

```cs
var connectionString = "{your-connection-string}";
var providerName = "{System.Data.SqlClient}"; //for Oracle use "Oracle.ManagedDataAccess.Client"
//most likely you will get the above two from ConnectionStringSettings object

var factory = DbProviderFactories.GetFactory(providerName);

using(var connection = new factory.CreateConnection()) {
    connection.ConnectionString = connectionString;
    connection.Open();

    using(var command = new connection.CreateCommand()) {
        command.CommandText = "{sql-query}";    //this needs to be tailored for each database system

        using(var reader = command.ExecuteReader()) {
            while(reader.Read()) {
                ...
            }
        }
    }
}

```

