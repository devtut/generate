---
metaTitle: "Entity Framework - Advanced mapping scenarios: entity splitting, table splitting"
description: "Entity splitting, Table splitting"
---

# Advanced mapping scenarios: entity splitting, table splitting


How to configure your EF model to support entity splitting or table splitting.



## Entity splitting


So let's say you have an entity class like this:

```cs
public class Person  
{
   public int PersonId { get; set; }
   public string Name { get; set; }
   public string ZipCode { get; set; }
   public string City { get; set; }
   public string AddressLine { get; set; }
}

```

And then let's say that you want to map this Person entity into two tables — one with the PersonId and the Name, and another one with the address details. Of course you would need the PersonId here as well in order to identify which person the address belongs to. So basically what you want is to split the entity into two (or even more) parts. Hence the name, entity splitting.
You can do this by mapping each of the properties to a different table:

```cs
public class MyDemoContext : DbContext  
{       
  public DbSet<Person> Products { get; set; }

  protected override void OnModelCreating(DbModelBuilder modelBuilder)
  {
      modelBuilder.Entity<Person>().Map(m =>
      {
         m.Properties(t => new { t.PersonId, t.Name });
         m.ToTable("People");
      }).Map(m => 
      {   
        m.Properties(t => new { t.PersonId, t.AddressLine, t.City, t.ZipCode });
        m.ToTable("PersonDetails");
      });
  }
}

```

This will create two tables: People and PersonDetails. Person has two fields, PersonId and Name, PersonDetails has four columns, PersonId, AddressLine, City and ZipCode. In People, PersonId is the primary key. In PersonDetails the primary key is also PersonId, but it is also a foreign key referencing PersonId in the Person table.

If you query the People DbSet, EF will do a join on the PersonIds to get the data from both tables to populate the entities.

You can also change the name of the columns:

```cs
protected override void OnModelCreating(DbModelBuilder modelBuilder)  
{
  modelBuilder.Entity<Person>().Map(m =>
  {
    m.Properties(t => new { t.PersonId });
    m.Property(t => t.Name).HasColumnName("PersonName");
    m.ToTable("People");
  }).Map(m =>
  {
    m.Property(t => t.PersonId).HasColumnName("ProprietorId");
    m.Properties(t => new { t.AddressLine, t.City, t.ZipCode });
    m.ToTable("PersonDetails");
  });
}

```

This will create the same table structure, but in the People table there will be a PersonName column instead of the Name column, and in the PersonDetails table there will be a ProprietorId instead of the PersonId column.



## Table splitting


And now let's say you want to do the opposite of entity splitting: instead of mapping one entity into two tables, you would like to map one table into two entities. This is called table splitting.
Let's say you have one table with five columns: PersonId, Name, AddressLine, City, ZipCode, where PersonId is the primary key. And then you would like to create an EF model like this:

```cs
public class Person  
{
  public int PersonId { get; set; }
  public string Name { get; set; }
  public Address Address { get; set; }
}

public class Address  
{        
  public string ZipCode { get; set; }
  public string City { get; set; }
  public string AddressLine { get; set; }
  public int PersonId { get; set; }
  public Person Person { get; set; }
}

```

One thing jumps right out: there is no AddressId in Address. That's because the two entities are mapped to the same table, so they must have the same primary key as well. If you do table splitting, this is something you just have to deal with. So besides table splitting, you also have to configure the Address entity and specify the primary key. And here's how:

```cs
public class MyDemoContext : DbContext  
{
  public DbSet<Person> Products { get; set; }
  public DbSet<Address> Addresses { get; set; }

  protected override void OnModelCreating(DbModelBuilder modelBuilder)
  {
     modelBuilder.Entity<Address>().HasKey(t => t.PersonId);
     modelBuilder.Entity<Person>().HasRequired(t => t.Address)
                                  .WithRequiredPrincipal(t => t.Person);

     modelBuilder.Entity<Person>().Map(m => m.ToTable("People"));
     modelBuilder.Entity<Address>().Map(m => m.ToTable("People"));
  }
}

```

