---
metaTitle: "Properties"
description: "Auto-implemented properties, Public Get, Public Set, Accessing Properties, Default Values for Properties, Various Properties in Context, Read-only properties"
---

# Properties



## Auto-implemented properties


[Auto-implemented properties](https://msdn.microsoft.com/en-us/library/bb384054.aspx) were introduced in C# 3.<br />
An auto-implemented property is declared with an empty getter and setter (accessors):

```cs
public bool IsValid { get; set; }

```

When an auto-implemented property is written in your code, the compiler creates a private anonymous field that can only be accessed through the property's accessors.

The above auto-implemented property statement is equivalent to writing this lengthy code:

```cs
private bool _isValid;
public bool IsValid
{
    get { return _isValid; }
    set { _isValid = value; }
}

```

Auto-implemented properties cannot have any logic in their accessors, for example:

```cs
public bool IsValid { get; set { PropertyChanged("IsValid"); } } // Invalid code

```

An auto-implemented property **can** however have different access modifiers for its accessors:

```cs
public bool IsValid { get; private set; }    

```

C# 6 allows auto-implemented properties to have no setter at all (making it immutable, since its value can be set only inside the constructor or hard coded):

```cs
public bool IsValid { get; }    
public bool IsValid { get; } = true;

```

For more information on initializing auto-implemented properties, read the [Auto-property initializers](http://stackoverflow.com/documentation/c%23/24/c-sharp-6-0-features/47/auto-property-initializers#t=201607211254385249419) documentation.



## Public Get


Getters are used to expose values from classes.

```cs
string name;
public string Name
{
    get { return this.name; }
}

```



## Public Set


Setters are used to assign values to properties.

```cs
string name;
public string Name 
{
    set { this.name = value; }
}

```



## Accessing Properties


```cs
class Program 
{
    public static void Main(string[] args)
    {
        Person aPerson = new Person("Ann Xena Sample", new DateTime(1984, 10, 22));
        //example of accessing properties (Id, Name & DOB)
        Console.WriteLine("Id is:  \t{0}\nName is:\t'{1}'.\nDOB is: \t{2:yyyy-MM-dd}.\nAge is: \t{3}", aPerson.Id, aPerson.Name, aPerson.DOB, aPerson.GetAgeInYears());
        //example of setting properties

        aPerson.Name = "   Hans Trimmer  ";
        aPerson.DOB = new DateTime(1961, 11, 11);
        //aPerson.Id = 5; //this won't compile as Id's SET method is private; so only accessible within the Person class.
        //aPerson.DOB = DateTime.UtcNow.AddYears(1); //this would throw a runtime error as there's validation to ensure the DOB is in past. 

        //see how our changes above take effect; note that the Name has been trimmed
        Console.WriteLine("Id is:  \t{0}\nName is:\t'{1}'.\nDOB is: \t{2:yyyy-MM-dd}.\nAge is: \t{3}", aPerson.Id, aPerson.Name, aPerson.DOB, aPerson.GetAgeInYears());

        Console.WriteLine("Press any key to continue");
        Console.Read();
    }
}

public class Person
{
    private static int nextId = 0;
    private string name;
    private DateTime dob; //dates are held in UTC; i.e. we disregard timezones
    public Person(string name, DateTime dob)
    {
        this.Id = ++Person.nextId;
        this.Name = name;
        this.DOB = dob;
    }
    public int Id
    {
        get;
        private set;
    }
    public string Name
    {
        get { return this.name; }
        set
        {
            if (string.IsNullOrWhiteSpace(value)) throw new InvalidNameException(value);
            this.name = value.Trim();
        }
    }
    public DateTime DOB
    {
        get { return this.dob; }
        set 
        {
            if (value < DateTime.UtcNow.AddYears(-200) || value > DateTime.UtcNow) throw new InvalidDobException(value);
            this.dob = value; 
        }
    }
    public int GetAgeInYears()
    {
        DateTime today = DateTime.UtcNow;
        int offset = HasHadBirthdayThisYear() ? 0 : -1;
        return today.Year - this.dob.Year + offset;
    }
    private bool HasHadBirthdayThisYear()
    {
        bool hasHadBirthdayThisYear = true;
        DateTime today = DateTime.UtcNow;
        if (today.Month > this.dob.Month)
        {
            hasHadBirthdayThisYear = true;
        }
        else
        {
            if (today.Month == this.dob.Month)
            {
                hasHadBirthdayThisYear = today.Day > this.dob.Day;
            }
            else
            {
                hasHadBirthdayThisYear = false;
            }
        }
        return hasHadBirthdayThisYear;
    }
}

public class InvalidNameException : ApplicationException
{
    const string InvalidNameExceptionMessage = "'{0}' is an invalid name.";
    public InvalidNameException(string value): base(string.Format(InvalidNameExceptionMessage,value)){}
}
public class InvalidDobException : ApplicationException
{ 
    const string InvalidDobExceptionMessage = "'{0:yyyy-MM-dd}' is an invalid DOB.  The date must not be in the future, or over 200 years in the past.";
    public InvalidDobException(DateTime value): base(string.Format(InvalidDobExceptionMessage,value)){}
}

```



## Default Values for Properties


Setting a default value can be done by using Initializers (C#6)

```cs
public class Name 
{
    public string First { get; set; } = "James";
    public string Last { get; set; } = "Smith";
}

```

If it is read only you can return values like this:

```

 public class Name 
  {
      public string First => "James";
      public string Last => "Smith";
  }

```



## Various Properties in Context


```cs
public class Person 
{
    //Id property can be read by other classes, but only set by the Person class
    public int Id {get; private set;}
    //Name property can be retrieved or assigned 
    public string Name {get; set;}
    
    private DateTime dob;
    //Date of Birth property is stored in a private variable, but retrieved or assigned through the public property.
    public DateTime DOB
    {
        get { return this.dob; }
        set { this.dob = value; }
    }
    //Age property can only be retrieved; it's value is derived from the date of birth 
    public int Age 
    {
        get 
        {
            int offset = HasHadBirthdayThisYear() ? 0 : -1;
            return DateTime.UtcNow.Year - this.dob.Year + offset;
        }
    }

    //this is not a property but a method; though it could be rewritten as a property if desired.
    private bool HasHadBirthdayThisYear() 
    {
        bool hasHadBirthdayThisYear = true;
        DateTime today = DateTime.UtcNow;
        if (today.Month > this.dob.Month)
        {
            hasHadBirthdayThisYear = true;
        }
        else 
        {
            if (today.Month == this.dob.Month)
            {
                hasHadBirthdayThisYear = today.Day > this.dob.Day;
            }
            else
            {
                hasHadBirthdayThisYear = false;
            }
        }
        return hasHadBirthdayThisYear;
    }
}

```



## Read-only properties


### Declaration

A common misunderstanding, especially beginners, have is read-only property is the one marked with `readonly` keyword. That's not correct and in fact **following is a compile time error**:

```cs
public readonly string SomeProp { get; set; }

```

A property is read-only when it only has a getter.

```cs
public string SomeProp { get; }

```

### Using read-only properties to create immutable classes

```cs
public Address
{
    public string ZipCode { get; }
    public string City { get; }
    public string StreetAddress { get; }

    public Address(
        string zipCode,
        string city,
        string streetAddress)
    {
        if (zipCode == null)
            throw new ArgumentNullException(nameof(zipCode));
        if (city == null)
            throw new ArgumentNullException(nameof(city));
        if (streetAddress == null)
            throw new ArgumentNullException(nameof(streetAddress));

        ZipCode = zipCode;
        City = city;
        StreetAddress = streetAddress;
    }
}

```



#### Remarks


Properties combine the class data storage of fields with the accessibility of methods.  Sometimes it may be hard to decide whether to use a property, a property referencing a field, or a method referencing a field.  As a rule of thumb:

<li>
Properties should be used without an internal field if they only get and/or set values; with no other logic occurring.  In such cases, adding an internal field would be adding code for no benefit.
</li>
<li>
Properties should be used with internal fields when you need to manipulate or validate the data.  An example may be removing leading and trailing spaces from strings or ensuring that a date is not in the past.
</li>

With regards to Methods vs Properties, where you can both retrieve (`get`) and update (`set`) a value, a property is the better choice.
Also, .Net provides a lot of functionality that makes use of a class's structure; e.g. adding a grid to a form, .Net will by default list all properties of the class on that form; thus to make best use of such conventions plan to use properties when this behaviour would be typically desirable, and methods where you'd prefer for the types to not be automatically added.

