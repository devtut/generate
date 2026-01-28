---
metaTitle: "Entity Framework - Entity Framework with SQLite"
description: "Setting up a project to use Entity Framework with an SQLite provider"
---

# Entity Framework with SQLite


[SQLite](https://www.sqlite.org/about.html) is a self-contained, serverless, transactional SQL database. It can be used within a .NET application by utilizing both a freely available .NET SQLite library and Entity Framework SQLite provider. This topic will go into setup and usage of the Entity Framework SQLite provider.



## Setting up a project to use Entity Framework with an SQLite provider


The Entity Framework library comes only with an SQL Server provider. To use SQLite will require additional dependencies and configuration. All required dependencies are available on NuGet.

### Install SQLite Managed Libraries

All of the mananged depedencies can be installed using the NuGet Package Manager Console. Run the command `Install-Package System.Data.SQLite`.

[<img src="https://i.stack.imgur.com/gQayv.png" alt="Installing System.Data.SQLite" />](https://i.stack.imgur.com/gQayv.png)

As shown above, when installing `System.Data.SQLite`, all related managed libraries are installed with it. This includes `System.Data.SQLite.EF6`, the EF provider for SQLite. The project also now references the assemblies required to use the SQLite provider.

[<img src="https://i.stack.imgur.com/tgCyG.png" alt="Project references" />](https://i.stack.imgur.com/tgCyG.png)

### Including Unmanaged Library

The SQLite managed libraries are dependent on an unmanaged assembly named `SQLite.Interop.dll`. It is included with the package assemblies downloaded with the SQLite package, and they are automatically copied into your build directory when you build the project. However, because it's unmanaged, it will not be included in your reference list. But make note, this assembly most be distributed with the application for the SQLite assemblies to work.

**Note: This assembly is bit-dependent, meaning you will need to include a specific assembly for each bitness you plan to support (x86/x64).**

### Editing the project's App.config

The `app.config` file will require some modifications before SQLite can be used as an Entity Framework provider.

### Required Fixes

When installing the package, the `app.config` file is automatically updated to include the necessary entries for SQLite and SQLite EF. Unfortunately these entries contain some errors. They need to be modified before it will work correctly.

First, locate the `DbProviderFactories`element in the config file. It is within the `system.data` element and will contain the following

```cs
<DbProviderFactories>
  <remove invariant="System.Data.SQLite.EF6" />
  <add name="SQLite Data Provider (Entity Framework 6)" invariant="System.Data.SQLite.EF6" description=".NET Framework Data Provider for SQLite (Entity Framework 6)" type="System.Data.SQLite.EF6.SQLiteProviderFactory, System.Data.SQLite.EF6" />
  <remove invariant="System.Data.SQLite" /><add name="SQLite Data Provider" invariant="System.Data.SQLite" description=".NET Framework Data Provider for SQLite" type="System.Data.SQLite.SQLiteFactory, System.Data.SQLite" />
</DbProviderFactories>

```

This can be simplified to contain a single entry

```cs
<DbProviderFactories>
    <add name="SQLite Data Provider" invariant="System.Data.SQLite.EF6" description=".NET Framework Data Provider for SQLite" type="System.Data.SQLite.SQLiteFactory, System.Data.SQLite" />
</DbProviderFactories>

```

With this, we have specified the EF6 SQLite providers should use the SQLite factory.

### Add SQLite connection string

Connection strings can be added to the configuration file within the root element. Add a connection string for accessing an SQLite database.

```cs
<connectionStrings>
    <add name="TestContext" connectionString="data source=testdb.sqlite;initial catalog=Test;App=EntityFramework;" providerName="System.Data.SQLite.EF6"/>
</connectionStrings>

```

The important thing to note here is the `provider`. It has been set to `System.Data.SQLite.EF6`. This tells EF that when we use this connection string, we want to use SQLite. The `data source` specified is just an example and will be dependent on the location and name of your SQLite database.

### Your first SQLite DbContext

With all the installation and configuration complete, you can now start using a `DbContext` that will work on your SQLite database.

```cs
public class TestContext : DbContext
{
    public TestContext()
        : base("name=TestContext") { }
}

```

By specifying `name=TestContext`, I have indicating that the TestContext connection string located in the `app.config`file should be used to create the context. That connection string was configured to use SQLite, so this context will use an SQLite database.

