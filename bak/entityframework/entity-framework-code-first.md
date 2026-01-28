---
metaTitle: "Entity Framework - Entity Framework Code First"
description: "Connect to an existing database"
---

# Entity Framework Code First



## Connect to an existing database


To achieve the simplest task in Entity Framework - to connect to an existing database `ExampleDatabase` on your local instance of MSSQL you have to implement two classes only.

First is the entity class, that will be mapped to our database table `dbo.People`.

```

   class Person
    {
        public int PersonId { get; set; }
        public string FirstName { get; set; }
    }

```

The class will use Entity Framework's conventions and map to table `dbo.People` which is expected to have primary key `PersonId` and a varchar(max) property `FirstName`.

Second is the context class which derives from `System.Data.Entity.DbContext` and which will manage the entity objects during runtime, pupulate them from database, handle concurrency and save them back to the database.

```

   class Context : DbContext
    {
        public Context(string connectionString) : base(connectionString)
        {
            Database.SetInitializer<Context>(null);
        }

        public DbSet<Person> People { get; set; }
    }

```

Please mind, that in the constructor of our context we need to set database initializer to null - we don't want Entity Framework to create the database, we just want to access it.

Now you are able manipulate data from that table, e.g. change the `FirstName` of first person in the database from a console application like this:

```

   class Program
    {
        static void Main(string[] args)
        {
            using (var ctx = new Context("DbConnectionString"))
            {
                var firstPerson = ctx.People.FirstOrDefault();
                if (firstPerson != null) {
                    firstPerson.FirstName = "John";
                    ctx.SaveChanges();
                }
            }
        }
    }

```

In the code above we created instance of Context with an argument "DbConnectionString". This has to be specified in our `app.config` file like this:

```

 <connectionStrings>
    <add  name="DbConnectionString" 
    connectionString="Data Source=.;Initial Catalog=ExampleDatabase;Integrated Security=True" 
    providerName="System.Data.SqlClient"/>
  </connectionStrings>

```

