---
metaTitle: "C# | Functional Programming"
description: "Func and Action, Avoid Null References, Higher-Order Functions, Immutability, Immutable collections"
---

# Functional Programming



## Func and Action


**Func** provides a holder for parameterised anonymous functions. The leading types are the inputs and the last type is always the return value.

```cs
// square a number.
Func<double, double> square = (x) => { return x * x; };

// get the square root.
// note how the signature matches the built in method.
Func<double, double> squareroot = Math.Sqrt;

// provide your workings.
Func<double, double, string> workings = (x, y) => 
    string.Format("The square of {0} is {1}.", x, square(y))

```

**Action** objects are like void methods so they only have an input type. No result is placed on the evaluation stack.

```cs
// right-angled triangle.
class Triangle
{
    public double a;
    public double b;
    public double h;
}

// Pythagorean theorem.
Action<Triangle> pythagoras = (x) => 
    x.h = squareroot(square(x.a) + square(x.b));

Triangle t = new Triangle { a = 3, b = 4 };
pythagoras(t);
Console.WriteLine(t.h); // 5.

```



## Avoid Null References


C# developers get a lot of null reference exceptions to deal with. F# developers don't because they have the Option type. An Option<> type (some prefer Maybe<> as a name) provides a Some and a None return type. It makes it explicit that a method may be about to return a null record.

For instance, you can't read the following and know if you will have to deal with a null value.

```cs
var user = _repository.GetUser(id);

```

If you do know about the possible null you can introduce some boilerplate code to deal with it.

```cs
var username = user != null ? user.Name : string.Empty;

```

What if we have an Option<> returned instead?

```cs
Option<User> maybeUser = _repository.GetUser(id);

```

The code now makes it explicit that we may have a None record returned and the boilerplate code to check for Some or None is required:

```cs
var username = maybeUser.HasValue ? maybeUser.Value.Name : string.Empty;

```

The following method shows how to return an Option<>

```cs
public Option<User> GetUser(int id)
{
    var users = new List<User>
    {
        new User { Id = 1, Name = "Joe Bloggs" },
        new User { Id = 2, Name = "John Smith" }
    };

    var user = users.FirstOrDefault(user => user.Id == id);

    return user != null ? new Option<User>(user) : new Option<User>();
}

```

Here is a minimal implementation of Option<>.

```cs
public struct Option<T>
{
    private readonly T _value;

    public T Value
    {
        get
        {
            if (!HasValue)
                throw new InvalidOperationException();

            return _value;
        }
    }

    public bool HasValue
    {
        get { return _value != null; }
    }

    public Option(T value)
    {
        _value = value;
    }

    public static implicit operator Option<T>(T value)
    {
        return new Option<T>(value);
    }
}

```

To demonstrate the above [avoidNull.csx](https://gist.github.com/Boggin/d53660f32aeaa35e0b028919ddc465e3) can be run with the C# REPL.

As stated, this is a minimal implementation. A search for ["Maybe" NuGet packages](https://www.nuget.org/packages?q=maybe) will turn up a number of good libraries.



## Higher-Order Functions


A higher-order function is one that takes another function as an argument or returns a function (or both).

This is commonly done with lambdas, for example when passing a predicate to a LINQ Where clause:

```cs
var results = data.Where(p => p.Items == 0);

```

The Where() clause could receive many different predicates which gives it considerable flexibility.

Passing a method into another method is also seen when implementing the Strategy design pattern. For example, various sorting methods could be chosen from and passed into a Sort method on an object depending on the requirements at run-time.



## Immutability


Immutability is common in functional programming and rare in object oriented programming.

Create, for example, an address type with mutable state:

```cs
public class Address () 
{
    public string Line1 { get; set; }
    public string Line2 { get; set; }
    public string City  { get; set; }
}

```

Any piece of code could alter any property in the above object.

Now create the immutable address type:

```cs
public class Address () 
{
    public readonly string Line1;
    public readonly string Line2;
    public readonly string City;

    public Address(string line1, string line2, string city) 
    {
        Line1 = line1;
        Line2 = line2;
        City  = city;
    }
}

```

Bear in mind that having read-only collections does not respect immutability. For example,

```cs
public class Classroom
{
    public readonly List<Student> Students;
    
    public Classroom(List<Student> students)
    {
        Students = students;
    }
}

```

is not immutable, as the user of the object can alter the collection (add or remove elements from it). In order to make it immutable, one has either to use an interface like IEnumerable, which does not expose methods to add, or to make it a ReadOnlyCollection.

```cs
public class Classroom
{
    public readonly ReadOnlyCollection<Student> Students;

    public Classroom(ReadOnlyCollection<Student> students)
    {
        Students = students;
    }
}

List<Students> list = new List<Student>();
// add students
Classroom c = new Classroom(list.AsReadOnly());   

```

With the immutable object we have the following benefits:

- It will be in a known state (other code can't change it).
- It is thread safe.
- The constructor offers a single place for validation.
- Knowing that the object cannot be altered makes the code easier to understand.



## Immutable collections


The [`System.Collections.Immutable`](https://www.nuget.org/packages/System.Collections.Immutable/) NuGet package provides immutable collection classes.

### Creating and adding items

```cs
var stack = ImmutableStack.Create<int>();
var stack2 = stack.Push(1); // stack is still empty, stack2 contains 1
var stack3 = stack.Push(2); // stack2 still contains only one, stack3 has 2, 1

```

### Creating using the builder

Certain immutable collections have a `Builder` inner class that can be used to cheaply build large immutable instances:

```cs
var builder = ImmutableList.CreateBuilder<int>(); // returns ImmutableList.Builder
builder.Add(1);
builder.Add(2);
var list = builder.ToImmutable();

```

### Creating from an existing IEnumerable

```cs
var numbers = Enumerable.Range(1, 5);
var list = ImmutableList.CreateRange<int>(numbers);

```

List of all immutable collection types:

- [`System.Collections.Immutable.ImmutableArray<T>`](https://msdn.microsoft.com/en-us/library/dn638264(v=vs.111).aspx)
- [`System.Collections.Immutable.ImmutableDictionary<TKey,TValue>`](https://msdn.microsoft.com/en-us/library/dn467181(v=vs.111).aspx)
- [`System.Collections.Immutable.ImmutableHashSet<T>`](https://msdn.microsoft.com/en-us/library/dn467171(v=vs.111).aspx)
- [`System.Collections.Immutable.ImmutableList<T>`](https://msdn.microsoft.com/en-us/library/dn456077.aspx)
- [`System.Collections.Immutable.ImmutableQueue<T>`](https://msdn.microsoft.com/en-us/library/dn467186(v=vs.111).aspx)
- [`System.Collections.Immutable.ImmutableSortedDictionary<TKey,TValue>`](https://msdn.microsoft.com/en-us/library/dn467194(v=vs.111).aspx)
- [`System.Collections.Immutable.ImmutableSortedSet<T>`](https://msdn.microsoft.com/en-us/library/dn467193(v=vs.111).aspx)
- [`System.Collections.Immutable.ImmutableStack<T>`](https://msdn.microsoft.com/en-us/library/dn467197(v=vs.111).aspx)

