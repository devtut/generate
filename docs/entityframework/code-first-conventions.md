---
metaTitle: "Entity Framework - Code First Conventions"
description: "Removing Conventions, Primary Key Convention, Type Discovery, DecimalPropertyConvention, Relationship Convention, Foreign Key Convention"
---

# Code First Conventions



## Removing Conventions


You can remove any of the conventions defined in the System.Data.Entity.ModelConfiguration.Conventions namespace, by overriding `OnModelCreating` method.

The following example removes PluralizingTableNameConvention.

```cs
public class EshopContext : DbContext 
{ 
    public DbSet<Product> Products { set; get; }
    . . . 
 
    protected override void OnModelCreating(DbModelBuilder modelBuilder) 
    { 
        modelBuilder.Conventions.Remove<PluralizingTableNameConvention>(); 
    } 
}

```

By default EF will create DB table with entity class name suffixed by 's'. In this example,  Code First is configured to ignore PluralizingTableName convention so, instead of `dbo.Products` table `dbo.Product` table will be created.



## Primary Key Convention


By default a property is a primary key if a property on a class is named “ID” (not case sensitive), or the class name followed by "ID". If the type of the primary key property is numeric or GUID it will be configured as an identity column. Simple Example:

```cs
public class Room
{ 
    // Primary key 
    public int RoomId{ get; set; } 
    ... 
}

```



## Type Discovery


By default Code First includes in model

1. Types defined as a DbSet property in context class.
<li>Reference types included in entity types even if they are defined in
different assembly.</li>
<li>Derived classes even if only the base class is defined as DbSet
property</li>

Here is an example, that we are only adding `Company` as `DbSet<Company>` in our context class:

```cs
public class Company
{
    public int Id { set; get; }
    public string Name { set; get; }
    public virtual ICollection<Department> Departments { set; get; }
}

public class Department
{
    public int Id { set; get; }
    public string Name { set; get; }
    public virtual ICollection<Person> Staff { set; get; }
}

[Table("Staff")]
public class Person
{
    public int Id { set; get; }
    public string Name { set; get; }
    public decimal Salary { set; get; }
}

public class ProjectManager : Person
{
   public string ProjectManagerProperty { set; get; }
}

public class Developer : Person
{
    public string DeveloperProperty { set; get; }
}

public class Tester : Person
{
    public string TesterProperty { set; get; }
}    

public class ApplicationDbContext : DbContext
{
    public DbSet<Company> Companies { set; get; }
}

```

We can see that all the classes are included in model

[<img src="http://i.stack.imgur.com/cIW5p.png" alt="enter image description here" />](http://i.stack.imgur.com/cIW5p.png)



## DecimalPropertyConvention


By default Entity Framework maps decimal properties to decimal(18,2) columns in database tables.

```cs
public class Box
{
    public int Id { set; get; }
    public decimal Length { set; get; }
    public decimal Width { set; get; }
    public decimal Height { set; get; }
}

```

[<img src="http://i.stack.imgur.com/AO6np.png" alt="enter image description here" />](http://i.stack.imgur.com/AO6np.png)

We can change the precision of decimal properties:

1.Use Fluent API:

```cs
protected override void OnModelCreating(DbModelBuilder modelBuilder)
{
    modelBuilder.Entity<Box>().Property(b => b.Width).HasPrecision(20, 4);
}

```

[<img src="http://i.stack.imgur.com/ykeMc.png" alt="enter image description here" />](http://i.stack.imgur.com/ykeMc.png)

Only "Width" Property is mapped to  decimal(20, 4).

2.Replace the convention:

```cs
protected override void OnModelCreating(DbModelBuilder modelBuilder)
{
    modelBuilder.Conventions.Remove<DecimalPropertyConvention>();
    modelBuilder.Conventions.Add(new DecimalPropertyConvention(10, 4));
}

```

[<img src="http://i.stack.imgur.com/NksFE.png" alt="enter image description here" />](http://i.stack.imgur.com/NksFE.png)

Every decimal property is mapped to decimal(10,4) columns.



## Relationship Convention


Code First infer the relationship between the two entities using navigation property. This navigation property can be a simple reference type or collection type. For example, we defined Standard navigation property in Student class and ICollection navigation property in Standard class. So, Code First automatically created one-to-many relationship between Standards and Students DB table by inserting Standard_StandardId foreign key column in the Students table.

```cs
public class Student
{
    
    public int StudentID { get; set; }
    public string StudentName { get; set; }
    public DateTime DateOfBirth { get; set; }      
        
    //Navigation property
    public Standard Standard { get; set; }
}

public class Standard
{
   
    public int StandardId { get; set; }
    public string StandardName { get; set; }
    
    //Collection navigation property
    public IList<Student> Students { get; set; }
   
}

```

The above entities created the following relationship using Standard_StandardId foreign key.

[<img src="http://i.stack.imgur.com/cSV0p.png" alt="enter image description here" />](http://i.stack.imgur.com/cSV0p.png)



## Foreign Key Convention


If class A is in relationship with class B and class B has property with the same name and type as the primary key of A, then EF automatically assumes that property is foreign key.

```cs
public class Department
{
    public int DepartmentId { set; get; }
    public string Name { set; get; }
    public virtual ICollection<Person> Staff { set; get; }
}

public class Person
{
    public int Id { set; get; }
    public string Name { set; get; }
    public decimal Salary { set; get; }
    public int DepartmentId { set; get; }
    public virtual Department Department { set; get; }
}

```

In this case DepartmentId is foreign key without explicit specification.



#### Remarks


Convention is a set of default rules to automatically configure a conceptual model based on domain class definitions when working with Code-First. Code-First conventions are defined in **System.Data.Entity.ModelConfiguration.Conventions** namespace ([EF 5](http://msdn.microsoft.com/en-us/library/system.data.entity.modelconfiguration.conventions(v=vs.103).aspx) & [EF 6](https://msdn.microsoft.com/en-us/library/system.data.entity.modelconfiguration.conventions(v=vs.113).aspx)).

