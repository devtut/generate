---
metaTitle: "Entity Framework - Code First - Fluent API"
description: "Mapping models, Composite Primary Key, Maximum Length, Primary Key, Required properties (NOT NULL), Explict Foreign Key naming"
---

# Code First - Fluent API




## Mapping models


EntityFramewok Fluent API is a powerful and elegant way of mapping your **code-first** domain models to underlying database. This also can be used with **code-first with existing database**. You have two options when using **Fluent API**: you can directly map your models on **OnModelCreating** method or you can create mapper classes which inherits from **EntityTypeConfiguration** and then add that models to **modelBuilder** on **OnModelCreating** method. **Second** option is which I prefer and am going to show example of it.

### Step one: Create model.

```cs
public class Employee
{
    public int Id { get; set; }
    public string Surname { get; set; }    
    public string FirstName { get; set; }    
    public string LastName { get; set; }        
    public short Age { get; set; }    
    public decimal MonthlySalary { get; set; }
        
    public string FullName
    {
        get
        {
            return $"{Surname} {FirstName} {LastName}";
        }
    }
}

```

### Step two: Create mapper class

```cs
public class EmployeeMap
    : EntityTypeConfiguration<Employee>
{
    public EmployeeMap()
    {
        // Primary key
        this.HasKey(m => m.Id);
        
        this.Property(m => m.Id)
            .HasColumnType("int")
            .HasDatabaseGeneratedOption(DatabaseGeneratedOption.Identity);
            
        // Properties
        this.Property(m => m.Surname)
            .HasMaxLength(50);
            
        this.Property(m => m.FirstName)
            .IsRequired()
            .HasMaxLength(50);
            
        this.Property(m => m.LastName)
            .HasMaxLength(50);
            
        this.Property(m => m.Age)
            .HasColumnType("smallint");
            
        this.Property(m => m.MonthlySalary)
            .HasColumnType("number")
            .HasPrecision(14, 5);
            
        this.Ignore(m => m.FullName);
        
        // Table & column mappings
        this.ToTable("TABLE_NAME", "SCHEMA_NAME");
        this.Property(m => m.Id).HasColumnName("ID");
        this.Property(m => m.Surname).HasColumnName("SURNAME");
        this.Property(m => m.FirstName).HasColumnName("FIRST_NAME");
        this.Property(m => m.LastName).HasColumnName("LAST_NAME");
        this.Property(m => m.Age).HasColumnName("AGE");
        this.Property(m => m.MonthlySalary).HasColumnName("MONTHLY_SALARY");
    }
}

```

Let us explain mappings:

<li>**HasKey** - defines the primary key. **Composite primary keys** can also be
used. For example: **this.HasKey(m => new { m.DepartmentId, m.PositionId })**.</li>
- **Property** - lets us to configure model properties.
<li>**HasColumnType** - specify database level column type. Please note that,
it can be different for different databases like Oracle and MS SQL.</li>
<li>**HasDatabaseGeneratedOption** - specifies if property is calculated at
database level. Numeric PKs are **DatabaseGeneratedOption.Identity** by
default, you should specify **DatabaseGeneratedOption.None** if you do
not want them to be so.</li>
- **HasMaxLength** - limits the length of string.
- **IsRequired** - marks the property as requiered.
- **HasPrecision** - lets us to specify precision for decimals.
<li>**Ignore** - Ignores property completely and does not map it to database.
We ignored FullName, because we do not want this column at our table.</li>
- **ToTable** - specify table name and schema name (optional) for model.
<li>**HasColumnName** - relate property with column name. This is not needed
when property names and column names are identical.</li>

### Step three: Add mapping class to configurations.

We need to tell EntityFramework to use our mapper class. To do so, we have to add it to **modelBuilder.Configurations** on **OnModelCreating** method:

```cs
public class DbContext()
    : base("Name=DbContext")
{
    protected override void OnModelCreating(DbModelBuilder modelBuilder)
    {
        modelBuilder.Configurations.Add(new EmployeeMap());
    }
}

```

And that is it. We are all set to go.



## Composite Primary Key


By using the .HasKey() method, a set of properties can be explicitly configured as the composite primary key of the entity.

```cs
using System.Data.Entity;    
// ..

public class PersonContext : DbContext
{
    // ..

    protected override void OnModelCreating(DbModelBuilder modelBuilder)
    {
        // ..

        modelBuilder.Entity<Person>().HasKey(p => new { p.FirstName, p.LastName });
    }
}

```



## Maximum Length


By using the .HasMaxLength() method, the maximum character count can be configured for a property.

```cs
using System.Data.Entity;    
// ..

public class PersonContext : DbContext
{
    // ..

    protected override void OnModelCreating(DbModelBuilder modelBuilder)
    {
        // ..

        modelBuilder.Entity<Person>()
                    .Property(t => t.Name)
                    .HasMaxLength(100);
    }
}

```

The resulting column with the specified column length:

[<img src="http://i.stack.imgur.com/KEYue.png" alt="enter image description here" />](http://i.stack.imgur.com/KEYue.png)



## Primary Key


By using the .HasKey() method, a property can be explicitly configured as primary key of the entity.

```cs
using System.Data.Entity;    
// ..

public class PersonContext : DbContext
{
    // ..

    protected override void OnModelCreating(DbModelBuilder modelBuilder)
    {
        // ..

        modelBuilder.Entity<Person>().HasKey(p => p.PersonKey);
    }
}

```



## Required properties (NOT NULL)


By using the .IsRequired() method, properties can be specified as mandatory, which means that the column will have a NOT NULL constraint.

```cs
using System.Data.Entity;    
// ..

public class PersonContext : DbContext
{
    // ..

    protected override void OnModelCreating(DbModelBuilder modelBuilder)
    {
        // ..

        modelBuilder.Entity<Person>()
                    .Property(t => t.Name)
                    .IsRequired();
    }
}

```

The resulting column with the NOT NULL constraint:

[<img src="http://i.stack.imgur.com/VJm33.png" alt="enter image description here" />](http://i.stack.imgur.com/VJm33.png)



## Explict Foreign Key naming


When a navigation property exist on a model, Entity Framework will automatically create a Foreign Key column. If a specific Foreign Key name is desired but is not contained as a property in the model, it can be set explicitly using the Fluent API. By utilizing the `Map` method while establishing the Foreign Key relationship, any unique name can be used for Foreign Keys.

```cs
public class Company
{
    public int Id { get; set; }
}

public class Employee
{
    property int Id { get; set; }
    property Company Employer { get; set; }
}

public class EmployeeContext : DbContext
{
    protected override void OnModelCreating(DbModelBuilder modelBuilder)
    {
        modelBuilder.Entity<Employee>()
                    .HasRequired(x => x.Employer)
                    .WithRequiredDependent()
                    .Map(m => m.MapKey("EmployerId"));
    }
}

```

After specifying the relationship, the `Map` method allows the Foreign Key name to be explicitly set by executing `MapKey`. In this example, what would have resulted in a column name of Employer_Id is now EmployerId.



#### Remarks


There are two general ways of specifying HOW Entity Framework will map POCO classes to database tables, columns, etc.: **Data Annotations** and **Fluent API**.

While Data Annotations are a simple to read and understand, they lack of certain features such as specifying the "Cascade on Delete" behavior for an entity. The Fluent API on the other hand is a bit more complex to use, but provides a far more advanced set of features.

