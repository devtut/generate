---
metaTitle: "Equals and GetHashCode"
description: "Writing a good GetHashCode override, Default Equals behavior., Override Equals and GetHashCode on custom types, Equals and GetHashCode in IEqualityComparator"
---

# Equals and GetHashCode



## Writing a good GetHashCode override


`GetHashCode` has major performance effects on Dictionary<> and HashTable.

Good `GetHashCode` Methods

<li>should have an even distribution
<ul>
- every integer should have a roughly equal chance of returning for a random instance
- if your method returns the same integer (e.g. the constant '999') for each instance, you'll have bad performance

- These are NOT cryptographic hashes, where slowness is a feature
- the slower your hash function, the slower your dictionary

- if they do not (e.g. because `GetHashCode` returns a random number), items may not be found in a `List`, `Dictionary`, or similar.

A good method to implement `GetHashCode` is to use one prime number as a starting value, and add the hashcodes of the fields of the type multiplied by other prime numbers to that:

```cs
public override int GetHashCode()
{
    unchecked // Overflow is fine, just wrap
    {
        int hash = 3049; // Start value (prime number).

        // Suitable nullity checks etc, of course :)
        hash = hash * 5039 + field1.GetHashCode();
        hash = hash * 883 + field2.GetHashCode();
        hash = hash * 9719 + field3.GetHashCode();
        return hash;
    }
}

```

Only the fields which are used in the `Equals`-method should be used for the hash function.

If you have a need to treat the same type in different ways for Dictionary/HashTables, you can use IEqualityComparer.



## Default Equals behavior.


`Equals` is declared in the `Object` class itself.

```cs
public virtual bool Equals(Object obj);

```

By default, `Equals` has the following behavior:

<li>
If the instance is a reference type, then `Equals` will return true only if the references are the same.
</li>
<li>
If the instance is a value type, then `Equals` will return true only if the type and value are the same.
</li>
<li>
`string` is a special case. It behaves like a value type.
</li>

```cs
namespace ConsoleApplication
{
    public class Program
    {
        public static void Main(string[] args)
        {
            //areFooClassEqual: False
            Foo fooClass1 = new Foo("42");
            Foo fooClass2 = new Foo("42");
            bool areFooClassEqual = fooClass1.Equals(fooClass2);
            Console.WriteLine("fooClass1 and fooClass2 are equal: {0}", areFooClassEqual);
            //False

            //areFooIntEqual: True
            int fooInt1 = 42;
            int fooInt2 = 42;
            bool areFooIntEqual = fooInt1.Equals(fooInt2);
            Console.WriteLine("fooInt1 and fooInt2 are equal: {0}", areFooIntEqual);

            //areFooStringEqual: True
            string fooString1 = "42";
            string fooString2 = "42";
            bool areFooStringEqual = fooString1.Equals(fooString2);
            Console.WriteLine("fooString1 and fooString2 are equal: {0}", areFooStringEqual);
        }
    }

    public class Foo
    {
        public string Bar { get; }

        public Foo(string bar)
        {
            Bar = bar;
        }
    }
}

```



## Override Equals and GetHashCode on custom types


For a class `Person` like:

```cs
public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string Clothes { get; set; }
}

var person1 = new Person { Name = "Jon", Age = 20, Clothes = "some clothes" };
var person2 = new Person { Name = "Jon", Age = 20, Clothes = "some other clothes" };

bool result = person1.Equals(person2); //false because it's reference Equals

```

But defining `Equals` and `GetHashCode` as follows:

```cs
public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string Clothes { get; set; }

    public override bool Equals(object obj)
    {
        var person = obj as Person;
        if(person == null) return false;
        return Name == person.Name && Age == person.Age; //the clothes are not important when comparing two persons
    }

    public override int GetHashCode()
    {
        return Name.GetHashCode()*Age;
    }
}

var person1 = new Person { Name = "Jon", Age = 20, Clothes = "some clothes" };
var person2 = new Person { Name = "Jon", Age = 20, Clothes = "some other clothes" };

bool result = person1.Equals(person2); // result is true

```

Also using LINQ to make different queries on persons will check both `Equals` and `GetHashCode`:

```cs
var persons = new List<Person>
{
     new Person{ Name = "Jon", Age = 20, Clothes = "some clothes"},
     new Person{ Name = "Dave", Age = 20, Clothes = "some other clothes"},
     new Person{ Name = "Jon", Age = 20, Clothes = ""}
};

var distinctPersons = persons.Distinct().ToList();//distinctPersons has Count = 2

```



## Equals and GetHashCode in IEqualityComparator


For given type `Person`:

```cs
public class Person
{
    public string Name { get; set; }
    public int Age { get; set; }
    public string Clothes { get; set; }
}

List<Person> persons = new List<Person>
{
    new Person{ Name = "Jon", Age = 20, Clothes = "some clothes"},
    new Person{ Name = "Dave", Age = 20, Clothes = "some other clothes"},
    new Person{ Name = "Jon", Age = 20, Clothes = ""}
};

var distinctPersons = persons.Distinct().ToList();// distinctPersons has Count = 3

```

But defining `Equals` and `GetHashCode` into an `IEqualityComparator` :

```cs
public class PersonComparator : IEqualityComparer<Person>
{
    public bool Equals(Person x, Person y)
    {
        return x.Name == y.Name && x.Age == y.Age; //the clothes are not important when comparing two persons;
    }

    public int GetHashCode(Person obj) { return obj.Name.GetHashCode() * obj.Age; }
}

var distinctPersons = persons.Distinct(new PersonComparator()).ToList();// distinctPersons has Count = 2

```

Note that for this query, two objects have been considered equal if both the `Equals` returned true and the `GetHashCode` have returned the same hash code for the two persons.



#### Remarks


Each implementation of `Equals` must fulfil the following requirements:

<li>
**Reflexive**: An object must equal itself.<br/>`x.Equals(x)` returns `true`.
</li>
<li>
**Symmetric**: There is no difference if I compare x to y or y to x - the result is the same. <br/>`x.Equals(y)` returns the same value as `y.Equals(x)`.
</li>
<li>
**Transitive**: If one object is equal to another object and this one is equal to a third one, the first has to be equal to the third.<br/>if `(x.Equals(y) && y.Equals(z))` returns `true`, then `x.Equals(z)` returns `true`.
</li>
<li>
**Consistent**: If you compare an object to another multiple times, the result is always the same.<br/>Successive invocations of `x.Equals(y)` return the same value as long as the objects referenced by x and y are not modified.
</li>
<li>
**Comparison to null**: No object is equal to `null`.<br/>`x.Equals(null)` returns `false`.
</li>

Implementations of `GetHashCode`:

<li>
**Compatible with `Equals`**: If two objects are equal (meaning that `Equals` returns true), then `GetHashCode` **must** return the same value for each of them.
</li>
<li>
**Large range**: If two objects are not equal (`Equals` says false), there should be a **high probability** their hash codes are distinct. **Perfect** hashing is often not possible as there is a limited number of values to choose from.
</li>
<li>
**Cheap**: It should be inexpensive to calculate the hash code in all cases.
</li>

See: [Guidelines for Overloading Equals() and Operator ==](https://msdn.microsoft.com/en-us/library/ms173147.aspx)

