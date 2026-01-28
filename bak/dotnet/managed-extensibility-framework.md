---
metaTitle: ".NET Framework - Managed Extensibility Framework"
description: "Connecting (Basic), Exporting a Type (Basic), Importing (Basic)"
---

# Managed Extensibility Framework



## Connecting (Basic)


See the other (Basic) examples above.

```dotnet
using System.ComponentModel.Composition;
using System.ComponentModel.Composition.Hosting;

namespace Demo
{
    public static class Program
    {
        public static void Main()
        {
            using (var catalog = new ApplicationCatalog())
            using (var exportProvider = new CatalogExportProvider(catalog))
            using (var container = new CompositionContainer(exportProvider))
            {
                exportProvider.SourceProvider = container;

                UserWriter writer = new UserWriter();

                // at this point, writer's userProvider field is null
                container.ComposeParts(writer);

                // now, it should be non-null (or an exception will be thrown).
                writer.PrintAllUsers();
            }
        }
    }
}

```

As long as something in the application's assembly search path has `[Export(typeof(IUserProvider))]`, `UserWriter`'s corresponding import will be satisfied and the users will be printed.

Other types of catalogs (e.g., `DirectoryCatalog`) can be used instead of (or in addition to) `ApplicationCatalog`, to look in other places for exports that satisfy the imports.



## Exporting a Type (Basic)


```dotnet
using System.Collections.Generic;
using System.Collections.ObjectModel;
using System.ComponentModel.Composition;

namespace Demo
{
    [Export(typeof(IUserProvider))]
    public sealed class UserProvider : IUserProvider
    {
        public ReadOnlyCollection<User> GetAllUsers()
        {
            return new List<User>
            {
                new User(0, "admin"),
                new User(1, "Dennis"),
                new User(2, "Samantha"),
            }.AsReadOnly();
        }
    }
}

```

This could be defined virtually anywhere; all that matters is that the application knows where to look for it (via the ComposablePartCatalogs it creates).



## Importing (Basic)


```dotnet
using System;
using System.ComponentModel.Composition;

namespace Demo
{
    public sealed class UserWriter
    {
        [Import(typeof(IUserProvider))]
        private IUserProvider userProvider;

        public void PrintAllUsers()
        {
            foreach (User user in this.userProvider.GetAllUsers())
            {
                Console.WriteLine(user);
            }
        }
    }
}

```

This is a type that has a dependency on an `IUserProvider`, which could be defined anywhere.  Like the previous example, all that matters is that the application knows where to look for the matching export (via the ComposablePartCatalogs it creates).



#### Remarks


One of MEF's big advantages over other technologies that support the inversion-of-control pattern is that it supports resolving dependencies that are not known at design-time, without needing much (if any) configuration.

All examples require a reference to the System.ComponentModel.Composition assembly.

Also, all the (Basic) examples use these as their sample business objects:

```dotnet
using System.Collections.ObjectModel;

namespace Demo
{
    public sealed class User
    {
        public User(int id, string name)
        {
            this.Id = id;
            this.Name = name;
        }

        public int Id { get; }
        public string Name { get; }
        public override string ToString() => $"User[Id: {this.Id}, Name={this.Name}]";
    }

    public interface IUserProvider
    {
        ReadOnlyCollection<User> GetAllUsers();
    }
}

```

