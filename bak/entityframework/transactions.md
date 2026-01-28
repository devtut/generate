---
metaTitle: "Entity Framework - Transactions"
description: "Database.BeginTransaction()"
---

# Transactions



## Database.BeginTransaction()


Multiple operations can be executed against a single transaction so that changes can be rolled back if any of the operations fail.

```cs
using (var context = new PlanetContext())
{
    using (var transaction = context.Database.BeginTransaction())
    {
        try
        {
            //Lets assume this works
            var jupiter = new Planet { Name = "Jupiter" };
            context.Planets.Add(jupiter);
            context.SaveChanges();

            //And then this will throw an exception
            var neptune = new Planet { Name = "Neptune" };
            context.Planets.Add(neptune);
            context.SaveChanges();

            //Without this line, no changes will get applied to the database
            transaction.Commit();
        }
        catch (Exception ex)
        {
            //There is no need to call transaction.Rollback() here as the transaction object
            //will go out of scope and disposing will roll back automatically
        }
    }
}

```

Note that it may be a developers' convention to call `transaction.Rollback()` explicitly, because it makes the code more self-explanatory. Also, there **may** be (less well-known) query providers for Entity Framework out there that don't implement `Dipsose` correctly, which would also require an explicit `transaction.Rollback()` call.

