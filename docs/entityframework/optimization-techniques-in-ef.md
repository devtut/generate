---
metaTitle: "Entity Framework - Optimization Techniques in EF"
description: "Using AsNoTracking, Loading Only Required Data, Execute queries in the database when possible, not in memory., Execute multiple queries async and in parallel, Working with stub entities, Disable change tracking and proxy generation"
---

# Optimization Techniques in EF




## Using AsNoTracking


**Bad Example:**

```cs
var location =  dbContext.Location
                     .Where(l => l.Location.ID == location_ID)
                     .SingleOrDefault();

return location;

```

Since the above code is simply returning an entity without modifying or adding it, we can avoid tracking cost.

**Good Example:**

```cs
var location =  dbContext.Location.AsNoTracking()
                     .Where(l => l.Location.ID == location_ID)
                     .SingleOrDefault();

return location;

```

When we use function `AsNoTracking()` we are explicitly telling Entity Framework that the entities are not tracked by the context. This can be especially useful when retrieving large amounts of data from your data store. If you want to make changes to un-tracked entities however, you must remember to attach them before calling `SaveChanges`.



## Loading Only Required Data


One problem often seen in code is loading all the data. This will  greatly increase the load on the server.

Let's say I have a model called "location" that has 10 fields in it, but not all the fields are required at the same time. Let's say I only want the 'LocationName' parameter of that model.

**Bad Example**

```cs
var location =  dbContext.Location.AsNoTracking()
                         .Where(l => l.Location.ID == location_ID)
                         .SingleOrDefault();

return location.Name;

```

**Good Example**

```cs
var location =  dbContext.Location
                          .Where(l => l.Location.ID == location_ID)
                          .Select(l => l.LocationName);
                          .SingleOrDefault();

return location;

```

The code in the "good example" will only fetch 'LocationName' and nothing else.

Note that since no entity is materialized in this example, `AsNoTracking()` isn't necessary. There's nothing to be tracked anyway.

**Fetching more fields with Anonymous Types**

```cs
var location = dbContext.Location
                    .Where(l => l.Location.ID == location_ID)
                    .Select(l => new { Name = l.LocationName, Area = l.LocationArea })
                    .SingleOrDefault();

return location.Name + " has an area of " + location.Area;

```

Same as the example before, only the fields 'LocationName' and 'LocationArea' will be retrieved from the database, the Anonymous Type can hold as many values you want.



## Execute queries in the database when possible, not in memory.


Suppose we want to count how many counties are there in Texas:

```cs
var counties = dbContext.States.Single(s => s.Code == "tx").Counties.Count();

```

The query is correct, but inefficient. `States.Single(…)` loads a state from the database. Next, `Counties` loads all 254 counties with all of their fields in a second query. `.Count()` is then performed **in memory** on the loaded `Counties` collection.<br />
We've loaded a lot of data we don't need, and we can do better:

```cs
var counties = dbContext.Counties.Count(c => c.State.Code == "tx");

```

Here we only do one query, which in SQL translates to a count and a join. We return only the count from the database - we've saved returning rows, fields, and creation of objects.

It is easy to see where the query is made by looking at the collection type: `IQueryable<T>` vs. `IEnumerable<T>`.



## Execute multiple queries async and in parallel


When using async queries, you can execute multiple queries at the same time, but not on the same context. If the execution time of one query is 10s, the time for the bad example will be 20s, while the time for the good example will be 10s.

### Bad Example

```cs
IEnumerable<TResult1> result1;
IEnumerable<TResult2> result2;

using(var context = new Context())
{
    result1 = await context.Set<TResult1>().ToListAsync().ConfigureAwait(false);
    result2 = await context.Set<TResult1>().ToListAsync().ConfigureAwait(false);
}

```

### Good Example

```cs
public async Task<IEnumerable<TResult>> GetResult<TResult>()
{
    using(var context = new Context())
    {
        return await context.Set<TResult1>().ToListAsync().ConfigureAwait(false);
    }
}



IEnumerable<TResult1> result1;
IEnumerable<TResult2> result2;

var result1Task = GetResult<TResult1>();
var result2Task = GetResult<TResult2>();

await Task.WhenAll(result1Task, result2Task).ConfigureAwait(false);

var result1 = result1Task.Result;
var result2 = result2Task.Result;

```



## Working with stub entities


Say we have `Product`s and `Category`s in a many-to-many relationship:

```cs
public class Product
{
    public Product()
    {
        Categories = new HashSet<Category>();
    }
    public int ProductId { get; set; }
    public string ProductName { get; set; }
    public virtual ICollection<Category> Categories { get; private set; }
}

public class Category
{
    public Category()
    {
        Products = new HashSet<Product>();
    }
    public int CategoryId { get; set; }
    public string CategoryName { get; set; }
    public virtual ICollection<Product> Products { get; set; }
}

```

If we want to add a `Category` to a `Product`, we have to load the product and add the category to its `Categories`, for example:

**Bad Example:**

```cs
var product = db.Products.Find(1);
var category = db.Categories.Find(2);
product.Categories.Add(category);
db.SaveChanges();

```

(where `db` is a `DbContext` subclass).

This creates one record in the junction table between `Product` and `Category`. However, this table only contains two `Id` values. It's a waste of resources to load two full entities in order to create one tiny record.

A more efficient way is to use **stub entities**, i.e. entity objects, created in memory, containing only the bare minimum of data, usually only an `Id` value. This is what it looks like:

**Good example:**

```cs
// Create two stub entities
var product = new Product { ProductId = 1 };
var category = new Category { CategoryId = 2 };

// Attach the stub entities to the context
db.Entry(product).State = System.Data.Entity.EntityState.Unchanged;
db.Entry(category).State = System.Data.Entity.EntityState.Unchanged;

product.Categories.Add(category);
db.SaveChanges();

```

The end result is the same, but it avoids two roundtrips to the database.

**Prevent duplicates**

It you want to check if the association already exists, a cheap query suffices. For example:

```cs
var exists = db.Categories.Any(c => c.Id == 1 && c.Products.Any(p => p.Id == 14));

```

Again, this won't load full entities into memory. It effectively queries the junction table and only returns a boolean.



## Disable change tracking and proxy generation


If you just want to get data, but not modify anything, you can turn off change tracking and proxy creation. This will improve your performance and also prevent lazy loading.

**Bad Example:**

```cs
using(var context = new Context())
{
    return await context.Set<MyEntity>().ToListAsync().ConfigureAwait(false);
}

```

**Good Example:**

```cs
using(var context = new Context())
{
    context.Configuration.AutoDetectChangesEnabled = false;
    context.Configuration.ProxyCreationEnabled = false;

    return await context.Set<MyEntity>().ToListAsync().ConfigureAwait(false);
}

```

It is particularly common to turn these off from within the constructor of your context, especially if you wish these to be set across your solution:

```cs
public class MyContext : DbContext
{
    public MyContext()
        : base("MyContext")
    {
        Configuration.AutoDetectChangesEnabled = false;
        Configuration.ProxyCreationEnabled = false;
    }

    //snip
}

```

