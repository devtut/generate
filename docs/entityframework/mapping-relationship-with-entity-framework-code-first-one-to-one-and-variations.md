---
metaTitle: "Entity Framework - Mapping relationship with Entity Framework Code First: One-to-one and variations"
description: "Mapping one-to-zero or one, Mapping one-to-one, Mapping one or zero-to-one or zero"
---

# Mapping relationship with Entity Framework Code First: One-to-one and variations


This topic discusses how to map one-to-one type relationships using Entity Framework.



## Mapping one-to-zero or one


So let's say again that you have the following model:

```cs
public class Person
{
  public int PersonId { get; set; }
  public string Name { get; set; }
}

public class Car
{
  public int CarId { get; set; }
  public string LicensePlate { get; set; }
}

public class MyDemoContext : DbContext
{
  public DbSet<Person> People { get; set; }
  public DbSet<Car> Cars { get; set; }
}

```

And now you want to set it up so that you can express the following specification: one person can have one or zero car, and every car belongs to one person exactly (relationships are bidirectional, so if CarA belongs to PersonA, then PersonA 'owns' CarA).

So let's modify the model a bit: add the navigation properties and the foreign key properties:

```cs
public class Person
{
  public int PersonId { get; set; }
  public string Name { get; set; }
  public int CarId { get; set; }
  public virtual Car Car { get; set; }
}

public class Car
{
  public int CarId { get; set; }
  public string LicensePlate { get; set; }
  public int PersonId { get; set; }
  public virtual Person Person { get; set; }
}

```

And the configuration:

```cs
public class CarEntityTypeConfiguration : EntityTypeConfiguration<Car>
{
  public CarEntityTypeConfiguration()
  {
     this.HasRequired(c => c.Person).WithOptional(p => p.Car);                        
  }
}    

```

By this time this should be self-explanatory. The car has a required person ([**HasRequired()**](https://msdn.microsoft.com/en-us/library/gg671317(v=vs.113).aspx)), with the person having an optional car ([**WithOptional()**](https://msdn.microsoft.com/en-us/library/gg696231(v=vs.113).aspx)). Again, it doesn't matter which side you configure this relationship from, just be careful when you use the right combination of Has/With and Required/Optional. From the `Person` side, it would look like this:

```cs
public class PersonEntityTypeConfiguration : EntityTypeConfiguration<Person>
{
  public PersonEntityTypeConfiguration()
  {
     this.HasOptional(p => p.Car).WithOptional(c => c.Person);                        
  }
}    

```

Now let's check out the db schema:

<img src="https://dotnetfalconcontent.blob.core.windows.net/entity-framework-code-first-relationship-mapping/v08.png" alt="" />

Look closely: you can see that there is no FK in `People` to refer to `Car`. Also, the FK in `Car` is not the `PersonId`, but the `CarId`. Here's the actual script for the FK:

```cs
ALTER TABLE [dbo].[Cars]  WITH CHECK ADD  CONSTRAINT [FK_dbo.Cars_dbo.People_CarId] FOREIGN KEY([CarId])
REFERENCES [dbo].[People] ([PersonId])

```

So this means that the `CarId` and `PersonId` foregn key properties we have in the model are basically ignored. They are in the database, but they are not foreign keys, as it might be expected. That's because one-to-one mappings does not support adding the FK into your EF model. And that's because one-to-one mappings are quite problematic in a relational database.

The idea is that every person can have exactly one car, and that car can only belong to that person. Or there might be person records, which do not have cars associated with them.

So how could this be represented with foreign keys? Obviously, there could be a `PersonId` in `Car`, and a `CarId` in `People`. To enforce that every person can have only one car, `PersonId` would have to be unique in `Car`. But if `PersonId` is unique in `People`, then how can you add two or more records where `PersonId` is `NULL`(more than one car that don't have owners)? Answer: you can't (well actually, you can create a filtered unique index in SQL Server 2008 and newer, but let's forget about this technicality for a moment; not to mention other RDBMS). Not to mention the case where you specify both ends of the relationship...

The only real way to enforce this rule if the `People` and the `Car` tables have the 'same' primary key (same values in the connected records). And to do this, `CarId` in `Car` must be both a PK and an FK to the PK of People. And this makes the whole schema a mess. When I use this I rather name the PK/FK in `Car` `PersonId`, and configure it accordingly:

```cs
public class Person
{
  public int PersonId { get; set; }
  public string Name { get; set; }        
  public virtual Car Car { get; set; }
}

public class Car
{        
  public string LicensePlate { get; set; }
  public int PersonId { get; set; }
  public virtual Person Person { get; set; }
}

public class CarEntityTypeConfiguration : EntityTypeConfiguration<Car>
{
  public CarEntityTypeConfiguration()
  {
     this.HasRequired(c => c.Person).WithOptional(p => p.Car);
     this.HasKey(c => c.PersonId);
  }
}

```

Not ideal, but maybe a bit better. Still, you have to be alert when using this solution, because it goes against the usual naming conventions, which might lead you astray. Here's the schema generated from this model:

<img src="https://dotnetfalconcontent.blob.core.windows.net/entity-framework-code-first-relationship-mapping/v09.png" alt="" />

So this relationship is not enforced by the database schema, but by Entity Framework itself. That's why you have to be very careful when you use this, not to let anybody temper directly with the database.



## Mapping one-to-one


Mapping one-to-one (when both sides are required) is also a tricky thing.

Let's imagine how this could be represented with foreign keys. Again, a `CarId` in `People` that refers to `CarId` in `Car`, and a `PersonId` in Car that refers to the `PersonId` in `People`.

Now what happens if you want to insert a car record? In order for this to succeed, there must be a `PersonId` specified in this car record, because it is required. And for this `PersonId` to be valid, the corresponding record in `People` must exist. OK, so let's go ahead and insert the person record. But for this to succeed, a valid `CarId` must be in the person record — but that car is not inserted yet! It cannot be, because we have to insert the referred person record first. But we cannot insert the referred person record, because it refers back to the car record, so that must be inserted first (foreign key-ception :) ).

So this cannot be represented the 'logical' way either. Again, you have to drop one of the foreign keys. Which one you drop is up to you. The side that is left with a foreign key is called the 'dependent', the side that is left without a foreign key is called the 'principal'. And again, to ensure the uniqueness in the dependent, the PK has to be the FK, so adding an FK column and importing that to your model is not supported.

So here's the configuration:

```cs
public class CarEntityTypeConfiguration : EntityTypeConfiguration<Car>
{
  public CarEntityTypeConfiguration()
  {
    this.HasRequired(c => c.Person).WithRequiredDependent(p => p.Car);
    this.HasKey(c => c.PersonId);
  }
}

```

By now you really should have gotten the logic of it :) Just remember that you can choose the other side as well, just be careful to use the Dependent/Principal versions of WithRequired (and you still have to configure the PK in Car).

```cs
public class PersonEntityTypeConfiguration : EntityTypeConfiguration<Person>
{
  public PersonEntityTypeConfiguration()
  {
    this.HasRequired(p => p.Car).WithRequiredPrincipal(c => c.Person);
  }
}

```

If you check the DB schema, you'll find that it's exactly the same as it was in the case of the one-to-one or zero solution. That's because again, this is not enforced by the schema, but by EF itself. So again, be careful :)



## Mapping one or zero-to-one or zero


And to finish off, let's briefly look at the case when both sides are optional.

By now you should be really bored with these examples :), so I'm not going into the details and play with the idea of having two FK-s and the potential problems and warn you about the dangers of not enforcing these rules in the schema but in just EF itself.

Here's the config you need to apply:

```cs
public class CarEntityTypeConfiguration : EntityTypeConfiguration<Car>
{
  public CarEntityTypeConfiguration()
  {
    this.HasOptional(c => c.Person).WithOptionalPrincipal(p => p.Car);
    this.HasKey(c => c.PersonId);
  }
}

```

Again, you can configure from the other side as well, just be careful to use the right methods :)

