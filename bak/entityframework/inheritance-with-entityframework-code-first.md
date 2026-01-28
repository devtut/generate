---
metaTitle: "Entity Framework - Inheritance with EntityFramework (Code First)"
description: "Table per hierarchy, Table per type"
---

# Inheritance with EntityFramework (Code First)



## Table per hierarchy


This approach will generate one table on the database to represent all the inheritance structure.

**Example:**

```cs
public abstract class Person
{
    public int Id { get; set; }
    public string Name { get; set; }
    public DateTime BirthDate { get; set; }
}

public class Employee : Person
{
    public DateTime AdmissionDate { get; set; }
    public string JobDescription { get; set; }
}

public class Customer : Person
{
    public DateTime LastPurchaseDate { get; set; }
    public int TotalVisits { get; set; }
}

// On DbContext
public DbSet<Person> People { get; set; }
public DbSet<Employee> Employees { get; set; }
public DbSet<Customer> Customers { get; set; }

```

**The table generated will be:**

Table: People
Fields:
Id
Name
BirthDate
Discrimitator
AdmissionDate
JobDescription
LastPurchaseDate
TotalVisits

Where 'Discriminator' will hold the name of the subclass on the inheritance and 'AdmissionDate', 'JobDescription', 'LastPurchaseDate', 'TotalVisits' are nullable.

**Advantages**

- Better performance since no joins are required although for to many columns the database might require many paging operations.
- Simple to use and create
- Easy to add more subclasses and fields

**Disadvantages**

- Violates the 3rd Normal Form [Wikipedia: Third normal form](https://en.wikipedia.org/wiki/Third_normal_form)
- Creates lots of nullable fields



## Table per type


This approach will generate (n+1) tables on the database to represent all the inheritance structure where n is the number of subclasses.

**How to:**

```cs
public abstract class Person
{
    public int Id { get; set; }
    public string Name { get; set; }
    public DateTime BirthDate { get; set; }
}

[Table("Employees")]
public class Employee : Person
{
    public DateTime AdmissionDate { get; set; }
    public string JobDescription { get; set; }
}

[Table("Customers")]
public class Customer : Person
{
    public DateTime LastPurchaseDate { get; set; }
    public int TotalVisits { get; set; }
}

// On DbContext
public DbSet<Person> People { get; set; }
public DbSet<Employee> Employees { get; set; }
public DbSet<Customer> Customers { get; set; }

```

**The table generated will be:**

Table: People
Fields:
Id
Name
BirthDate

Table: Employees
Fields:
PersonId
AdmissionDate
JobDescription

Table: Customers:
Fields:
PersonId
LastPurchaseDate
TotalVisits

Where 'PersonId' on all tables will be a primary key and a constraint to People.Id

**Advantages**

- Normalized tables
- Easy to add columns and subclasses
- No nullable columns

**Disadvantages**

- Join is required to retrieve the data
- Subclass inference is more expensive

