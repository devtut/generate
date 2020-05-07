---
metaTitle: "Entity Framework - Database Initialisers"
description: "CreateDatabaseIfNotExists, DropCreateDatabaseIfModelChanges, DropCreateDatabaseAlways, Custom database initializer, MigrateDatabaseToLatestVersion"
---

# Database Initialisers



## CreateDatabaseIfNotExists


Implementation of `IDatabaseInitializer` that is used in EntityFramework by default. As the name implies, it creates the database if none exists.
However when you change the model, it throws an exception.

Usage:

```cs
public class MyContext : DbContext {
    public MyContext() {
        Database.SetInitializer(new CreateDatabaseIfNotExists<MyContext>());
    }
}

```



## DropCreateDatabaseIfModelChanges


This implementation of `IDatabaseInitializer` drops and recreates the database if the model changes automatically.

Usage:

```cs
public class MyContext : DbContext {
    public MyContext() {
        Database.SetInitializer(new DropCreateDatabaseIfModelChanges<MyContext>());
    }
}

```



## DropCreateDatabaseAlways


This implementation of `IDatabaseInitializer` drops and recreates the database everytime your context is used in applications app domain. Beware of the data loss due to the fact, that the database is recreated.

Usage:

```cs
public class MyContext : DbContext {
    public MyContext() {
        Database.SetInitializer(new DropCreateDatabaseAlways<MyContext>());
    }
}

```



## Custom database initializer


You can create your own implementation of `IDatabaseInitializer`.

Example implementation of an initializer, that will migrate the database to 0 and then migrate all the way to the newest migration (usefull e.g. when running integration tests). In order to do that you would need a `DbMigrationsConfiguration` type too.

```cs
public class RecreateFromScratch<TContext, TMigrationsConfiguration> : 
IDatabaseInitializer<TContext>
where TContext : DbContext
where TMigrationsConfiguration : DbMigrationsConfiguration<TContext>, new()
{
    private readonly DbMigrationsConfiguration<TContext> _configuration;

    public RecreateFromScratch()
    {
        _configuration = new TMigrationsConfiguration();
    }

    public void InitializeDatabase(TContext context)
    {
        var migrator = new DbMigrator(_configuration);
        migrator.Update("0");
        migrator.Update();
    }
}


```



## MigrateDatabaseToLatestVersion


An implementation of `IDatabaseInitializer` that will use Code First Migrations to update the database to the latest version. To use this initializer you have to use `DbMigrationsConfiguration` type too.

Usage:

```cs
public class MyContext : DbContext {
    public MyContext() {
        Database.SetInitializer(
            new MigrateDatabaseToLatestVersion<MyContext, Configuration>());
    }
}

```

