---
metaTitle: "Entity Framework - Loading related entities"
description: "Eager loading, Explicit loading, Lazy loading, Projection Queries"
---

# Loading related entities



## Eager loading


**Eager loading** lets you load all your needed entities at once. If you prefer to get all your entities to work on in one database call, then **Eager loading** is the way to go. It also lets you load multiple levels.

You have **two options** to load related entities, you can choose either **strongly typed** or **string** overloads of the **Include** method.

### Strongly typed.

```cs
// Load one company with founder and address details
int companyId = ...;
Company company = context.Companies
    .Include(m => m.Founder)
    .Include(m => m.Addresses)
    .SingleOrDefault(m => m.Id == companyId);

// Load 5 companies with address details, also retrieve country and city
// information of addresses
List<Company> companies = context.Companies
    .Include(m => m.Addresses.Select(a => a.Country));
    .Include(m => m.Addresses.Select(a => a.City))
    .Take(5).ToList();

```

This method is available since Entity Framework 4.1. Make sure you have the reference `using System.Data.Entity;` set.

### String overload.

```cs
// Load one company with founder and address details
int companyId = ...;
Company company = context.Companies
    .Include("Founder")
    .Include("Addresses")
    .SingleOrDefault(m => m.Id == companyId);

// Load 5 companies with address details, also retrieve country and city
// information of addresses
List<Company> companies = context.Companies
    .Include("Addresses.Country");
    .Include("Addresses.City"))
    .Take(5).ToList();

```



## Explicit loading


After turning **Lazy loading** off you can lazily load entities by explicitly calling **Load** method for entries. **Reference** is used to load single navigation properties, whereas **Collection** is used to get collections.

```cs
Company company = context.Companies.FirstOrDefault();
// Load founder
context.Entry(company).Reference(m => m.Founder).Load();
// Load addresses
context.Entry(company).Collection(m => m.Addresses).Load();

```

As it is on **Eager loading** you can use overloads of above methods to load entiteis by their names:

```cs
Company company = context.Companies.FirstOrDefault();
// Load founder
context.Entry(company).Reference("Founder").Load();
// Load addresses
context.Entry(company).Collection("Addresses").Load();

```

### Filter related entities.

Using **Query** method we can filter loaded related entities:

```cs
Company company = context.Companies.FirstOrDefault();
// Load addresses which are in Baku
context.Entry(company)
    .Collection(m => m.Addresses)
    .Query()
    .Where(a => a.City.Name == "Baku")
    .Load();

```



## Lazy loading


**Lazy loading** is enabled by default. Lazy loading is achieved by creating derived proxy classes and overriding virtual navigation proeprties. Lazy loading occurs when property is accessed for the first time.

```cs
int companyId = ...;
Company company = context.Companies
    .First(m => m.Id == companyId);
Person founder = company.Founder; // Founder is loaded
foreach (Address address in company.Addresses)
{
    // Address details are loaded one by one.        
}

```

To turn Lazy loading off for specific navigation properties just remove virtual keyword from property declaration:

```cs
public Person Founder { get; set; } // "virtual" keyword has been removed

```

If you want to completely turn off Lazy loading, then you have to change Configuration, for example, at **Context constructor**:

```cs
public class MyContext : DbContext        
{
    public MyContext(): base("Name=ConnectionString")
    {
        this.Configuration.LazyLoadingEnabled = false;
    }
}

```

**Note:** Please remember to **turn off** Lazy loading if your are using serialization. Because serializers access every property you are going to load all of them from database. Additionally, you can run into loop between navigation properties.



## Projection Queries


If one needs related data in a denormalized type, or e.g. only a subset of columns one can use projection queries. If there is no reason for using an extra type, there is the possibility to join the values into an [anonymous type](https://docs.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/anonymous-types).

```cs
var dbContext = new MyDbContext();
var denormalizedType = from company in dbContext.Company
                       where company.Name == "MyFavoriteCompany"
                       join founder in dbContext.Founder
                       on company.FounderId equals founder.Id
                       select new 
                       {
                           CompanyName = company.Name,
                           CompanyId = company.Id,
                           FounderName = founder.Name,
                           FounderId = founder.Id
                       };

```

Or with query-syntax:

```cs
var dbContext = new MyDbContext();
var denormalizedType = dbContext.Company
                       .Join(dbContext.Founder, 
                                    c => c.FounderId,
                                    f => f.Id ,
                                    (c, f) => new 
                                    {
                                        CompanyName = c.Name,
                                        CompanyId = c.Id,
                                        FounderName = f.Name,
                                        FounderId = f.Id
                                    })
                       .Select(cf => cf);

```



#### Remarks


If models are correctly related you can easily load related data using EntityFramework. You have three options to chose from: **lazy loading**, **eager loading** and **explicit loading**.

Models used in examples:

```cs
public class Company
{
    public int Id { get; set; }
    public string FullName { get; set; }
    public string ShortName { get; set; }

    // Navigation properties
    public virtual Person Founder { get; set; }
    public virtual ICollection<Address> Addresses { get; set; }
}

public class Address 
{        
    public int Id { get; set; }
    public int CompanyId { get; set; }
    public int CountryId { get; set; }
    public int CityId { get; set; }
    public string Street { get; set; }

    // Navigation properties
    public virtual Company Company { get; set; }
    public virtual Country Country { get; set; }
    public virtual City City { get; set; }
}

```

