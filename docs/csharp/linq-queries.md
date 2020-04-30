---
metaTitle: "LINQ Queries"
description: "Chaining methods, First, FirstOrDefault, Last, LastOrDefault, Single, and SingleOrDefault, Except, SelectMany, Any, JOINS, Skip and Take, Defining a variable inside a Linq query (let keyword), Zip, Range and Repeat, All, Basics, Aggregate, SelectMany: Flattening a sequence of sequences, Distinct, Query collection by type / cast elements to type, GroupBy, Enumerating the Enumerable, Where, Using Range with various Linq methods, Using SelectMany instead of nested loops, Contains, GroupBy one or multiple fields, Query Ordering - OrderBy() ThenBy() OrderByDescending() ThenByDescending(), ToDictionary, SkipWhile, DefaultIfEmpty, SequenceEqual, ElementAt and ElementAtOrDefault, Joining multiple sequences, Joining on multiple keys, Sum, ToLookup, Any and First(OrDefault) - best practice, GroupBy Sum and Count, OrderBy, Select - Transforming elements, Union, Count and LongCount, Incrementally building a query, GroupJoin with outer range variable, Linq  Quantifiers, TakeWhile, Build your own Linq operators for IEnumerable<T>, Reverse, OrderByDescending, Concat, Select with Func<TSource, int, TResult> selector - Use to get ranking of elements"
---

# LINQ Queries


LINQ is an acronym which stands for **L**anguage **IN**tegrated **Q**uery. It is a concept which integrates a query language by offering a consistent model for working with data across various kinds of data sources and formats; you use the same basic coding patterns to query and transform data in XML documents, SQL databases, ADO.NET Datasets, .NET collections, and any other format for which a LINQ provider is available.



## Chaining methods


[Many LINQ functions](https://msdn.microsoft.com/en-us/library/system.linq.enumerable(v=vs.110).aspx) both operate on an `IEnumerable<TSource>` and also return an `IEnumerable<TResult>`. The type parameters `TSource` and `TResult` may or may not refer to the same type, depending on the method in question and any functions passed to it.

A few examples of this are

```cs
public static IEnumerable<TResult> Select<TSource, TResult>(
    this IEnumerable<TSource> source,
    Func<TSource, TResult> selector
)

public static IEnumerable<TSource> Where<TSource>(
    this IEnumerable<TSource> source,
    Func<TSource, int, bool> predicate
)

public static IOrderedEnumerable<TSource> OrderBy<TSource, TKey>(
    this IEnumerable<TSource> source,
    Func<TSource, TKey> keySelector
)

```

While some method chaining may require an entire set to be worked prior to moving on, LINQ takes advantage of [deferred execution](http://stackoverflow.com/documentation/c%23/68/linq-queries/8001/deferred-execution) by using [yield return <sup>**MSDN**</sup>](https://blogs.msdn.microsoft.com/oldnewthing/20080812-00/?p=21273/) which creates an Enumerable and an Enumerator behind the scenes. The process of chaining in LINQ is essentially building an enumerable (iterator) for the original set -- which is deferred -- until materialized by [enumerating the enumerable](http://stackoverflow.com/documentation/c%23/68/linq-queries/17356/enumerating-the-enumerable).

This allows these functions to be [fluently chained <sup>**wiki**</sup>](https://en.wikipedia.org/wiki/Fluent_interface), where one function can act directly on the result of another. This style of code can be used to perform many sequence based operations in a single statement.

For example, it's possible to combine `Select`, `Where` and `OrderBy` to transform, filter and sort a sequence in a single statement.

```cs
var someNumbers = { 4, 3, 2, 1 };

var processed = someNumbers
        .Select(n => n * 2)   // Multiply each number by 2
        .Where(n => n != 6)   // Keep all the results, except for 6
        .OrderBy(n => n);     // Sort in ascending order

```

**Output:**

> 
<p>2<br />
4<br />
8</p>


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/3Gta8X)

Any functions that both extend and return the generic `IEnumerable<T>` type can be used as chained clauses in a single statement. This style of fluent programming is powerful, and should be considered when creating your own [extension methods](http://stackoverflow.com/documentation/c%23/20/extension-methods#t=201607220826369208865).



## First, FirstOrDefault, Last, LastOrDefault, Single, and SingleOrDefault


All six methods return a single value of the sequence type, and can be called with or without a predicate.

Depending on the number of elements that match the `predicate` or, if no `predicate` is supplied, the number of elements in the source sequence, they behave as follows:

### First()

- Returns the first element of a sequence, or the first element matching the provided `predicate`.
- If the sequence contains no elements, an `InvalidOperationException` is thrown with the message: "Sequence contains no elements".
- If the sequence contains no elements matching the provided `predicate`, an `InvalidOperationException` is thrown with the message "Sequence contains no matching element".

**Example**

```cs
// Returns "a":
new[] { "a" }.First();

// Returns "a":
new[] { "a", "b" }.First();

// Returns "b":
new[] { "a", "b" }.First(x => x.Equals("b"));

// Returns "ba":
new[] { "ba", "be" }.First(x => x.Contains("b"));

// Throws InvalidOperationException:
new[] { "ca", "ce" }.First(x => x.Contains("b"));

// Throws InvalidOperationException:
new string[0].First();

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/ESYLcU)

### FirstOrDefault()

- Returns the first element of a sequence, or the first element matching the provided `predicate`.
- If the sequence contains no elements, or no elements matching the provided `predicate`, returns the default value of the sequence type using [`default(T)`](http://stackoverflow.com/documentation/c%23/26/keywords/109/default#t=201702071640321629621).

**Example**

```cs
// Returns "a":
new[] { "a" }.FirstOrDefault();

// Returns "a":
new[] { "a", "b" }.FirstOrDefault();

// Returns "b":
new[] { "a", "b" }.FirstOrDefault(x => x.Equals("b"));

// Returns "ba":
new[] { "ba", "be" }.FirstOrDefault(x => x.Contains("b"));

// Returns null:
new[] { "ca", "ce" }.FirstOrDefault(x => x.Contains("b"));

// Returns null:
new string[0].FirstOrDefault();

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/XJ93lr)

### Last()

- Returns the last element of a sequence, or the last element matching the provided `predicate`.
- If the sequence contains no elements, an `InvalidOperationException` is thrown with the message "Sequence contains no elements."
- If the sequence contains no elements matching the provided `predicate`, an `InvalidOperationException` is thrown with the message "Sequence contains no matching element".

**Example**

```cs
// Returns "a":
new[] { "a" }.Last();

// Returns "b":
new[] { "a", "b" }.Last();

// Returns "a":
new[] { "a", "b" }.Last(x => x.Equals("a"));

// Returns "be":
new[] { "ba", "be" }.Last(x => x.Contains("b"));

// Throws InvalidOperationException:
new[] { "ca", "ce" }.Last(x => x.Contains("b"));

// Throws InvalidOperationException:
new string[0].Last(); 

```

### LastOrDefault()

- Returns the last element of a sequence, or the last element matching the provided `predicate`.
- If the sequence contains no elements, or no elements matching the provided `predicate`, returns the default value of the sequence type using `default(T)`.

**Example**

```cs
// Returns "a":
new[] { "a" }.LastOrDefault();

// Returns "b":
new[] { "a", "b" }.LastOrDefault();

// Returns "a":
new[] { "a", "b" }.LastOrDefault(x => x.Equals("a"));

 // Returns "be":
new[] { "ba", "be" }.LastOrDefault(x => x.Contains("b"));

// Returns null:
new[] { "ca", "ce" }.LastOrDefault(x => x.Contains("b")); 

// Returns null:
new string[0].LastOrDefault();

```

### Single()

- If the sequence contains exactly one element, or exactly one element matching the provided `predicate`, that element is returned.
- If the sequence contains no elements, or no elements matching the provided `predicate`, an `InvalidOperationException` is thrown with the message "Sequence contains no elements".
- If the sequence contains more than one element, or more than one element matching the provided `predicate`, an `InvalidOperationException` is thrown with the message "Sequence contains more than one element".
- **Note:** in order to evaluate whether the sequence contains exactly one element, at most two elements has to be enumerated.

**Example**

```cs
// Returns "a":
new[] { "a" }.Single();

// Throws InvalidOperationException because sequence contains more than one element:
new[] { "a", "b" }.Single();

// Returns "b":
new[] { "a", "b" }.Single(x => x.Equals("b"));

// Throws InvalidOperationException:
new[] { "a", "b" }.Single(x => x.Equals("c"));

// Throws InvalidOperationException:
new string[0].Single(); 

// Throws InvalidOperationException because sequence contains more than one element:
new[] { "a", "a" }.Single();

```

### SingleOrDefault()

- If the sequence contains exactly one element, or exactly one element matching the provided `predicate`, that element is returned.
- If the sequence contains no elements, or no elements matching the provided `predicate`, `default(T)` is returned.
- If the sequence contains more than one element, or more than one element matching the provided `predicate`, an `InvalidOperationException` is thrown with the message "Sequence contains more than one element".
- If the sequence contains no elements matching the provided `predicate`, returns the default value of the sequence type using `default(T)`.
- **Note:** in order to evaluate whether the sequence contains exactly one element, at most two elements has to be enumerated.

**Example**

```cs
// Returns "a":
new[] { "a" }.SingleOrDefault();

// returns "a"
new[] { "a", "b" }.SingleOrDefault(x => x == "a"); 

// Returns null:
new[] { "a", "b" }.SingleOrDefault(x => x == "c");

// Throws InvalidOperationException:
new[] { "a", "a" }.SingleOrDefault(x => x == "a");

// Throws InvalidOperationException:
new[] { "a", "b" }.SingleOrDefault();

// Returns null:
new string[0].SingleOrDefault();

```

### Recommendations

<li>
Although you can use `FirstOrDefault`, `LastOrDefault` or `SingleOrDefault` to check whether a sequence contains any items, `Any` or `Count` are more reliable. This is because a return value of `default(T)` from one of these three methods doesn't prove that the sequence is empty, as the value of the first / last / single element of the sequence could equally be `default(T)`
</li>
<li>
Decide on which methods fits your code's purpose the most. For instance, use `Single` only if you must ensure that there is a single item in the collection matching your predicate — otherwise use `First`; as `Single` throw an exception if the sequence has more than one matching element. This of course applies to the "*OrDefault"-counterparts as well.
</li>
<li>
Regarding efficiency: Although it's often appropriate to ensure that there is only one item (`Single`) or, either only one or zero (`SingleOrDefault`) items, returned by a query, both of these methods require more, and often the entirety, of the collection to be examined to ensure there in no second match to the query. This is unlike the behavior of, for example, the `First` method, which can be satisfied after finding the first match.
</li>



## Except


The Except method returns the set of items which are contained in the first collection but are not contained in the second. The default [`IEqualityComparer`](https://msdn.microsoft.com/en-us/library/ms132151(v=vs.110).aspx) is used to compare the items within the two sets. There is an overload which accepts an [`IEqualityComparer`](https://msdn.microsoft.com/en-us/library/ms132151(v=vs.110).aspx) as an argument.

**Example:**

```cs
int[] first = { 1, 2, 3, 4 };
int[] second = { 0, 2, 3, 5 };

IEnumerable<int> inFirstButNotInSecond = first.Except(second);
// inFirstButNotInSecond = { 1, 4 }

```

**Output:**

> 
<p>1<br />
4</p>


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/m3EqTQ)

In this case `.Except(second)` excludes elements contained in the array `second`, namely 2 and 3 (0 and 5 are not contained in the `first` array and are skipped).

Note that `Except` implies `Distinct` (i.e., it removes repeated elements). For example:

```cs
int[] third = { 1, 1, 1, 2, 3, 4 };

IEnumerable<int> inThirdButNotInSecond = third.Except(second);
// inThirdButNotInSecond = { 1, 4 }

```

**Output:**

> 
<p>1<br />
4</p>


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/VlXBUp)

In this case, the elements 1 and 4 are returned only once.

Implementing [`IEquatable`](https://msdn.microsoft.com/en-us/library/ms131187(v=vs.110).aspx) or providing the function an [`IEqualityComparer`](https://msdn.microsoft.com/en-us/library/ms132151(v=vs.110).aspx) will allow using a different method to compare the elements.
Note that the [`GetHashCode`](https://msdn.microsoft.com/en-us/library/system.object.gethashcode(v=vs.110).aspx) method should also be overridden so that it will return an identical hash code for `object` that are identical according to the [`IEquatable`](https://msdn.microsoft.com/en-us/library/ms131187(v=vs.110).aspx) implementation.

****Example With IEquatable:****

```cs
class Holiday : IEquatable<Holiday>
{
    public string Name { get; set; }

    public bool Equals(Holiday other)
    {
        return Name == other.Name;
    }

    // GetHashCode must return true whenever Equals returns true.
    public override int GetHashCode()
    {
        //Get hash code for the Name field if it is not null.
        return Name?.GetHashCode() ?? 0;
    }
}

public class Program
{
    public static void Main()
    {
        List<Holiday> holidayDifference = new List<Holiday>();

        List<Holiday> remoteHolidays = new List<Holiday>
        {
            new Holiday { Name = "Xmas" },
            new Holiday { Name = "Hanukkah" },
            new Holiday { Name = "Ramadan" }
        };

        List<Holiday> localHolidays = new List<Holiday>
        {
            new Holiday { Name = "Xmas" },
            new Holiday { Name = "Ramadan" }
        };

        holidayDifference = remoteHolidays
            .Except(localHolidays)
            .ToList();

        holidayDifference.ForEach(x => Console.WriteLine(x.Name));
    }
}

```

Output:

> 
Hanukkah


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/9ilGqy)



## SelectMany


The SelectMany linq method 'flattens' an `IEnumerable<IEnumerable<T>>` into an `IEnumerable<T>`. All of the T elements within the `IEnumerable` instances contained in the source `IEnumerable` will be combined into a single `IEnumerable`.

```cs
var words = new [] { "a,b,c", "d,e", "f" };
var splitAndCombine = words.SelectMany(x => x.Split(','));
// returns { "a", "b", "c", "d", "e", "f" }

```

If you use a selector function which turns input elements into sequences, the result will be the elements of those sequences returned one by one.

Note that, unlike `Select()`, the number of elements in the output doesn't need to be the same as were in the input.

**More real-world example**

```cs
class School
{
    public Student[] Students { get; set; }
}

class Student 
{
    public string Name { get; set; }
}    
  
var schools = new [] {
    new School(){ Students = new [] { new Student { Name="Bob"}, new Student { Name="Jack"} }},
    new School(){ Students = new [] { new Student { Name="Jim"}, new Student { Name="John"} }}
};
               
var allStudents = schools.SelectMany(s=> s.Students);
             
foreach(var student in allStudents)
{
    Console.WriteLine(student.Name);
}

```

Output:

> 
<p>Bob<br />
Jack<br />
Jim<br />
John</p>


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/LNyymI)



## Any


`Any` is used to check if **any** element of a collection matches a condition or not.
<br/>**see also: [.All](https://stackoverflow.com/documentation/c%23/68/linq-queries/2773/all#t=201707041340119289445), [Any and FirstOrDefault: best practice](https://stackoverflow.com/documentation/c%23/68/linq-queries/16731/any-and-firstordefault-best-practice#t=201707041441456087738)**

### 1. Empty parameter

**Any**: Returns `true` if the collection has any elements and `false` if the collection is empty:

```cs
var numbers = new List<int>();
bool result = numbers.Any(); // false

var numbers = new List<int>(){ 1, 2, 3, 4, 5};
bool result = numbers.Any(); //true

```

### 2. Lambda expression as parameter

**Any**: Returns `true` if the collection has one or more elements that meet the condition in the lambda expression:

```cs
var arrayOfStrings = new string[] { "a", "b", "c" };
arrayOfStrings.Any(item => item == "a");    // true
arrayOfStrings.Any(item => item == "d");    // false

```

### 3. Empty collection

**Any**: Returns `false` if the collection is empty and a lambda expression is supplied:

```cs
var numbers = new List<int>();
bool result = numbers.Any(i => i >= 0); // false

```

**Note:**
`Any` will stop iteration of the collection as soon as it finds an element matching the condition. This means that the collection will not necessarily be fully enumerated; it will only be enumerated far enough to find the first item matching the condition.

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/IQ4wG4)



## JOINS


Joins are used to combine different lists or tables holding data via a common key.

Like in SQL, the following kinds of Joins are supported in LINQ: <br/>
**Inner, Left, Right, Cross** and **Full Outer** Joins.

The following two lists are used in the examples below:

```cs
var first = new List<string>(){ "a","b","c"}; // Left data
var second = new List<string>(){ "a", "c", "d"}; // Right data

```

### (Inner) Join

```cs
var result = from f in first
             join s in second on f equals s
             select new { f, s };

var result = first.Join(second, 
                        f => f, 
                        s => s,
                        (f, s) => new { f, s });

// Result: {"a","a"}
//         {"c","c"}

```

### Left outer join

```cs
var leftOuterJoin = from f in first
                    join s in second on f equals s into temp
                    from t in temp.DefaultIfEmpty()
                    select new { First = f, Second = t};

// Or can also do:
var leftOuterJoin = from f in first
                    from s in second.Where(x => x == f).DefaultIfEmpty()
                    select new { First = f, Second = s};

// Result: {"a","a"}
//         {"b", null}  
//         {"c","c"}  


// Left outer join method syntax
var leftOuterJoinFluentSyntax = first.GroupJoin(second,
                                      f => f,
                                      s => s,
                                      (f, s) => new { First = f, Second = s })
                                   .SelectMany(temp => temp.Second.DefaultIfEmpty(),
                                      (f, s) => new { First = f.First, Second = s });

```

### Right Outer Join

```cs
var rightOuterJoin = from s in second
                     join f in first on s equals f into temp
                     from t in temp.DefaultIfEmpty()
                     select new {First=t,Second=s};

// Result: {"a","a"}
//         {"c","c"}  
//         {null,"d"}  

```

### Cross Join

```cs
var CrossJoin = from f in first
                from s in second
                select new { f, s };

// Result: {"a","a"}
//         {"a","c"}  
//         {"a","d"}  
//         {"b","a"}
//         {"b","c"}  
//         {"b","d"}  
//         {"c","a"}
//         {"c","c"}  
//         {"c","d"}

```

### Full Outer Join

```cs
var fullOuterjoin = leftOuterJoin.Union(rightOuterJoin);

// Result: {"a","a"}
//         {"b", null}  
//         {"c","c"}  
//         {null,"d"}

```

### **Practical example**

The examples above have a simple data structure so you can focus on understanding the different LINQ joins technically, but in the real world you would have tables with columns you need to join.

In the following example, there is just one class `Region` used, in reality you would join two or more different tables which hold the same key (in this example `first` and `second` are joined via the common key `ID`).

**Example:** Consider the following data structure:

```cs
public class Region 
{
    public Int32 ID;
    public string RegionDescription;
    
    public Region(Int32 pRegionID, string pRegionDescription=null)
    {
        ID = pRegionID; RegionDescription = pRegionDescription;
    }
}

```

Now prepare the data (i.e. populate with data):

```cs
// Left data
var first = new List<Region>() 
                 { new Region(1), new Region(3), new Region(4) }; 
// Right data
var second = new List<Region>() 
                 { 
                    new Region(1, "Eastern"),  new Region(2, "Western"),
                    new Region(3, "Northern"), new Region(4, "Southern")
                 }; 

```

You can see that in this example `first` doesn't contain any region descriptions so you want to join them from `second`. Then the inner join would look like:

```cs
// do the inner join
var result = from f in first
             join s in second on f.ID equals s.ID
             select new { f.ID, s.RegionDescription };


 // Result: {1,"Eastern"}
 //         {3, Northern}  
 //         {4,"Southern"}  

```

This result has created anonymous objects on the fly, which is fine, but we have already created a proper class - so we can specify it: Instead of  `select new { f.ID, s.RegionDescription };` we can say `select new Region(f.ID, s.RegionDescription);`, which will return the same data but will create objects of type `Region` - that will maintain compatibility with the other objects.

[Live demo on .NET fiddle](https://dotnetfiddle.net/pP6enP)



## Skip and Take


The Skip method returns a collection excluding a number of items from the beginning of the source collection. The number of items excluded is the number given as an argument. If there are less items in the collection than specified in the argument then an empty collection is returned.

The Take method returns a collection containing a number of elements from the beginning of the source collection. The number of items included is the number given as an argument. If there are less items in the collection than specified in the argument then the collection returned will contain the same elements as the source collection.

```cs
var values = new [] { 5, 4, 3, 2, 1 };

var skipTwo        = values.Skip(2);         // { 3, 2, 1 }
var takeThree      = values.Take(3);         // { 5, 4, 3 }
var skipOneTakeTwo = values.Skip(1).Take(2); // { 4, 3 }
var takeZero       = values.Take(0);         // An IEnumerable<int> with 0 items

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/U2b76y)

**Skip and Take** are commonly used together to paginate results, for instance:

```cs
IEnumerable<T> GetPage<T>(IEnumerable<T> collection, int pageNumber, int resultsPerPage) {
    int startIndex = (pageNumber - 1) * resultsPerPage;
    return collection.Skip(startIndex).Take(resultsPerPage);
}

```

> 
**Warning:** LINQ to Entities only supports Skip on [ordered queries](http://stackoverflow.com/documentation/c%23/68/linq-queries/4389/query-ordering#t=201607261110520529272). If you try to use Skip without ordering you will get a **NotSupportedException** with the message "The method 'Skip' is only supported for sorted input in LINQ to Entities. The method 'OrderBy' must be called before the method 'Skip'."




## Defining a variable inside a Linq query (let keyword)


In order to define a variable inside a linq expression, you can use the **let** keyword. This is usually done in order to store the results of intermediate sub-queries, for example:

```

int[] numbers = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };

 var aboveAverages = from number in numbers
                     let average = numbers.Average()
                     let nSquared = Math.Pow(number,2)
                     where nSquared > average
                     select number;

 Console.WriteLine("The average of the numbers is {0}.", numbers.Average());

 foreach (int n in aboveAverages)
 {
   Console.WriteLine("Query result includes number {0} with square of {1}.", n, Math.Pow(n,2));
 }

```

**Output:**

> 
<p>The average of the numbers is 4.5.<br />
Query result includes number 3 with square of 9.<br />
Query result includes number 4 with square of 16.<br />
Query result includes number 5 with square of 25.<br />
Query result includes number 6 with square of 36.<br />
Query result includes number 7 with square of 49.<br />
Query result includes number 8 with square of 64.<br />
Query result includes number 9 with square of 81.</p>


[View Demo](https://dotnetfiddle.net/zbjrHZ)



## Zip


The `Zip` extension method acts upon two collections. It pairs each element in the two series together based on position. With a `Func` instance, we use `Zip` to handle elements from the two C# collections in pairs. If the series differ in size, the extra elements of the larger series will be ignored.

To take an example from the book "C# in a Nutshell",

```cs
int[] numbers = { 3, 5, 7 };
string[] words = { "three", "five", "seven", "ignored" };
IEnumerable<string> zip = numbers.Zip(words, (n, w) => n + "=" + w);

```

**Output:**

> 
<p>3=three<br />
5=five<br />
7=seven</p>


[View Demo](https://dotnetfiddle.net/nIA5E9)



## Range and Repeat


The `Range` and `Repeat` static methods on `Enumerable` can be used to generate simple sequences.

### **Range**

`Enumerable.Range()` generates a sequence of integers given a starting value and a count.

```cs
// Generate a collection containing the numbers 1-100 ([1, 2, 3, ..., 98, 99, 100])
var range = Enumerable.Range(1,100);

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/jA0VB1)

### **Repeat**

`Enumerable.Repeat()` generates a sequence of repeating elements given an element and the number of repetitions required.

```cs
// Generate a collection containing "a", three times (["a","a","a"])
var repeatedValues = Enumerable.Repeat("a", 3);

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/KpZfpt)



## All


`All` is used to check, if all elements of a collection match a condition or not.
<br/>**see also: [.Any](https://stackoverflow.com/documentation/c%23/68/linq-queries/5098/any#t=201707041342119744775)**

### 1. Empty parameter

**All**: is not allowed to be used with empty parameter.

### 2. Lambda expression as parameter

**All**: Returns `true` if all elements of collection satisfies the lambda expression and `false` otherwise:

```cs
var numbers = new List<int>(){ 1, 2, 3, 4, 5};
bool result = numbers.All(i => i < 10); // true
bool result = numbers.All(i => i >= 3); // false

```

### 3. Empty collection

**All**: Returns `true`  if the collection is empty and a lambda expression is supplied:

```cs
var numbers = new List<int>();
bool result = numbers.All(i => i >= 0); // true

```

**Note:**
`All` will stop iteration of the collection as soon as it finds an element **not** matching the condition. This means that the collection will not necessarily be fully enumerated; it will only be enumerated far enough to find the first item **not matching** the condition.



## Basics


LINQ is largely beneficial for querying collections (or arrays).

For example, given the following sample data:

```cs
var classroom = new Classroom
{
    new Student { Name = "Alice", Grade = 97, HasSnack = true  },
    new Student { Name = "Bob",   Grade = 82, HasSnack = false },
    new Student { Name = "Jimmy", Grade = 71, HasSnack = true  },
    new Student { Name = "Greg",  Grade = 90, HasSnack = false },
    new Student { Name = "Joe",   Grade = 59, HasSnack = false }
}

```

We can "query" on this data using LINQ syntax. For example, to retrieve all students who have a snack today:

```cs
var studentsWithSnacks = from s in classroom.Students
                         where s.HasSnack
                         select s;

```

Or, to retrieve students with a grade of 90 or above, and only return their names, not the full `Student` object:

```cs
var topStudentNames = from s in classroom.Students
                      where s.Grade >= 90
                      select s.Name;

```

The LINQ feature is comprised of two syntaxes that perform the same functions, have nearly identical performance, but are written very differently. The syntax in the example above is called **query syntax**. The following example, however, illustrates **method syntax**. The same data will be returned as in the example above, but the way the query is written is different.

```cs
var topStudentNames = classroom.Students
                               .Where(s => s.Grade >= 90)
                               .Select(s => s.Name);

```



## Aggregate


`Aggregate` Applies an accumulator function over a sequence.

```cs
int[] intList = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
int sum = intList.Aggregate((prevSum, current) => prevSum + current);
// sum = 55

```


- At the first step `prevSum = 1`
- At the second `prevSum = prevSum(at the first step) + 2`
- At the i-th step `prevSum = prevSum(at the (i-1) step) + i-th element of the array`

```cs
string[] stringList = { "Hello", "World", "!" };
string joinedString = stringList.Aggregate((prev, current) => prev + " " + current);
// joinedString = "Hello World !"

```

A second overload of `Aggregate` also receives an `seed` parameter which is the initial accumulator value. This can be used to calculate multiple conditions on a collection without iterating it more than once.

```cs
List<int> items = new List<int> { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12 };

```

For the collection of `items` we want to calculate

1. The total `.Count`
1. The amount of even numbers
1. Collect each forth item

Using `Aggregate` it can be done like this:

```cs
var result = items.Aggregate(new { Total = 0, Even = 0, FourthItems = new List<int>() },
                (accumelative,item) =>
                new {
                    Total = accumelative.Total + 1,
                    Even = accumelative.Even + (item % 2 == 0 ? 1 : 0),
                    FourthItems = (accumelative.Total + 1)%4 == 0 ? 
                        new List<int>(accumelative.FourthItems) { item } : 
                        accumelative.FourthItems 
                });
// Result:
// Total = 12
// Even = 6
// FourthItems = [4, 8, 12]

```

**Note that using an anonymous type as the seed one has to instantiate a new object each item because the properties are read only. Using a custom class one can simply assign the information and no `new` is needed (only when giving the initial `seed` parameter**



## SelectMany: Flattening a sequence of sequences


```cs
var sequenceOfSequences = new [] { new [] { 1, 2, 3 }, new [] { 4, 5 }, new [] { 6 } };
var sequence = sequenceOfSequences.SelectMany(x => x);
// returns { 1, 2, 3, 4, 5, 6 }

```

Use `SelectMany()` if you have, or you are creating a sequence of sequences, but you want the result as one long sequence.

In LINQ Query Syntax:

```cs
var sequence = from subSequence in sequenceOfSequences
               from item in subSequence
               select item;

```

If you have a collection of collections and would like to be able to work on data from parent and child collection at the same time, it is also possible with `SelectMany`.

Let's define simple classes

```cs
public class BlogPost
{
    public int Id { get; set; }
    public string Content { get; set; }
    public List<Comment> Comments { get; set; }
}

public class Comment
{
    public int Id { get; set; }
    public string Content { get; set; }
}

```

Let's assume we have following collection.

```cs
List<BlogPost> posts = new List<BlogPost>()
{
    new BlogPost()
    {
        Id = 1,
        Comments = new List<Comment>()
        {
            new Comment()
            {
                Id = 1,
                Content = "It's really great!",
            },
            new Comment()
            {
                Id = 2,
                Content = "Cool post!"
            }
        }
    },
    new BlogPost()
    {
        Id = 2,
        Comments = new List<Comment>()
        {
            new Comment()
            {
                Id = 3,
                Content = "I don't think you're right",
            },
            new Comment()
            {
                Id = 4,
                Content = "This post is a complete nonsense"
            }
        }
    }
};

```

Now we want to select comments `Content` along with `Id` of `BlogPost` associated with this comment. In order to do so, we can use appropriate `SelectMany` overload.

```cs
var commentsWithIds = posts.SelectMany(p => p.Comments, (post, comment) => new { PostId = post.Id, CommentContent = comment.Content });

```

Our `commentsWithIds` looks like this

```cs
{
    PostId = 1,
    CommentContent = "It's really great!"
},
{
    PostId = 1,
    CommentContent = "Cool post!"
},
{
    PostId = 2,
    CommentContent = "I don't think you're right"
},
{
    PostId = 2,
    CommentContent = "This post is a complete nonsense"
}

```



## Distinct


Returns unique values from an `IEnumerable`. Uniqueness is determined using the default equality comparer.

```cs
int[] array = { 1, 2, 3, 4, 2, 5, 3, 1, 2 };

var distinct = array.Distinct();
// distinct = { 1, 2, 3, 4, 5 }

```

To compare a custom data type, we need to implement the `IEquatable<T>` interface and provide `GetHashCode` and `Equals` methods for the type. Or the equality comparer may be overridden:

```cs
class SSNEqualityComparer : IEqualityComparer<Person> {
    public bool Equals(Person a, Person b) => return a.SSN == b.SSN;
    public int GetHashCode(Person p) => p.SSN;
}

List<Person> people;

distinct = people.Distinct(SSNEqualityComparer);

```



## Query collection by type / cast elements to type


```cs
interface IFoo { }
class Foo : IFoo { }
class Bar : IFoo { }

```

```cs
var item0 = new Foo();
var item1 = new Foo();
var item2 = new Bar();
var item3 = new Bar();
var collection = new IFoo[] { item0, item1, item2, item3 };

```

Using `OfType`

```cs
var foos = collection.OfType<Foo>(); // result: IEnumerable<Foo> with item0 and item1
var bars = collection.OfType<Bar>(); // result: IEnumerable<Bar> item item2 and item3
var foosAndBars = collection.OfType<IFoo>(); // result: IEnumerable<IFoo> with all four items

```

Using `Where`

```cs
var foos = collection.Where(item => item is Foo); // result: IEnumerable<IFoo> with item0 and item1
var bars = collection.Where(item => item is Bar); // result: IEnumerable<IFoo> with item2 and item3

```

Using `Cast`

```cs
var bars = collection.Cast<Bar>();                // throws InvalidCastException on the 1st item
var foos = collection.Cast<Foo>();                // throws InvalidCastException on the 3rd item
var foosAndBars = collection.Cast<IFoo>();        // OK 

```



## GroupBy


GroupBy is an easy way to sort a `IEnumerable<T>` collection of items into distinct groups.

### Simple Example

In this first example, we end up with two groups, odd and even items.

```cs
List<int> iList = new List<int>() { 1, 2, 3, 4, 5, 6, 7, 8, 9 };
var grouped = iList.GroupBy(x => x % 2 == 0);

//Groups iList into odd [13579] and even[2468] items 
       
foreach(var group in grouped)
{
    foreach (int item in group)
    {
        Console.Write(item); // 135792468  (first odd then even)
    }
}

```

### More Complex Example

Let's take grouping a list of people by age as an example.
First, we'll create a Person object which has two properties, Name and Age.

```cs
public class Person
{
    public int Age {get; set;}
    public string Name {get; set;}
}

```

Then we create our sample list of people with various names and ages.

```cs
List<Person> people = new List<Person>();
people.Add(new Person{Age = 20, Name = "Mouse"});
people.Add(new Person{Age = 30, Name = "Neo"});
people.Add(new Person{Age = 40, Name = "Morpheus"});
people.Add(new Person{Age = 30, Name = "Trinity"});
people.Add(new Person{Age = 40, Name = "Dozer"});
people.Add(new Person{Age = 40, Name = "Smith"});

```

Then we create a LINQ query to group our list of people by age.

```cs
var query = people.GroupBy(x => x.Age);

```

Doing so, we can see the Age for each group, and have a list of each person in the group.

```cs
foreach(var result in query)
{
    Console.WriteLine(result.Key);
                
    foreach(var person in result)
        Console.WriteLine(person.Name);
}

```

This results in the following output:

```cs
20
Mouse
30
Neo
Trinity
40
Morpheus
Dozer
Smith

```

You can play with the [live demo on .NET Fiddle](https://dotnetfiddle.net/VFOZ1x)



## Enumerating the Enumerable


The IEnumerable<T> interface is the base interface for all generic enumerators and is a quintessential part of understanding LINQ. At its core, it represents the sequence.

This underlying interface is inherited by all of the generic collections, such as [Collection<T>](https://msdn.microsoft.com/en-us/library/ms132397(v=vs.110).aspx), [Array](https://msdn.microsoft.com/en-us/library/system.array(v=vs.110).aspx), [List<T>](https://msdn.microsoft.com/en-us/library/6sh2ey19(v=vs.110).aspx), [Dictionary<TKey, TValue> Class](https://msdn.microsoft.com/en-us/library/xfhwa508(v=vs.110).aspx), and [HashSet<T>](https://msdn.microsoft.com/en-us/library/bb359438(v=vs.110).aspx).

In addition to representing the sequence, any class that inherits from IEnumerable<T> must provide an IEnumerator<T>. The enumerator exposes the iterator for the enumerable, and these two interconnected interfaces and ideas are the source of the saying "enumerate the enumerable".

"Enumerating the enumerable" is an important phrase. The enumerable is simply a structure for how to iterate, it does not hold any materialized objects. For example, when sorting, an enumerable may hold the criteria of the field to sort, but using `.OrderBy()` in itself will return an IEnumerable<T> which only knows **how** to sort. Using a call which will materialize the objects, as in iterate the set, is known as enumerating (for example `.ToList()`). The enumeration process will use the the enumerable definition of **how** in order to move through the series and return the relevant objects (in order, filtered, projected, etc.).

Only once the enumerable has been enumerated does it cause the materialization of the objects, which is when metrics like [time complexity](https://en.wikipedia.org/wiki/Time_complexity) (how long it should take related to series size) and spacial complexity (how much space it should use related to series size) can be measured.

Creating your own class that inherits from IEnumerable<T> can be a little complicated depending on the underlying series that needs to be enumerable. In general it is best to use one of the existing generic collections. That said, it is also possible to inherit from the IEnumerable<T> interface without having a defined array as the underlying structure.

For example, using the Fibonacci series as the underlying sequence. Note that the call to `Where` simply builds an `IEnumerable`, and it is not until a call to enumerate that enumerable is made that any of the values are materialized.

```cs
void Main()
{
    Fibonacci Fibo = new Fibonacci();
    IEnumerable<long> quadrillionplus = Fibo.Where(i => i > 1000000000000);
    Console.WriteLine("Enumerable built");
    Console.WriteLine(quadrillionplus.Take(2).Sum());
    Console.WriteLine(quadrillionplus.Skip(2).First());

    IEnumerable<long> fibMod612 = Fibo.OrderBy(i => i % 612);
    Console.WriteLine("Enumerable built");
    Console.WriteLine(fibMod612.First());//smallest divisible by 612
}

public class Fibonacci : IEnumerable<long>
{
    private int max = 90;

    //Enumerator called typically from foreach
    public IEnumerator GetEnumerator() {
        long n0 = 1;
        long n1 = 1;
        Console.WriteLine("Enumerating the Enumerable");
        for(int i = 0; i < max; i++){
            yield return n0+n1;
            n1 += n0;
            n0 = n1-n0;
        }
    }
    
    //Enumerable called typically from linq
    IEnumerator<long> IEnumerable<long>.GetEnumerator() {
        long n0 = 1;
        long n1 = 1;
        Console.WriteLine("Enumerating the Enumerable");
        for(int i = 0; i < max; i++){
            yield return n0+n1;
            n1 += n0;
            n0 = n1-n0;
        }
    }
}

```

Output

```cs
Enumerable built
Enumerating the Enumerable
4052739537881
Enumerating the Enumerable
4052739537881
Enumerable built
Enumerating the Enumerable
14930352

```

The strength in the second set (the fibMod612) is that even though we made the call to order our entire set of Fibonacci numbers, since only one value was taken using `.First()` the time complexity was O(n) as only 1 value needed to be compared during the ordering algorithm's execution. This is because our enumerator only asked for 1 value, and so the entire enumerable did not have to be materialized. Had we used `.Take(5)` instead of `.First()` the enumerator would have asked for 5 values, and at most 5 values would need to be materialized. Compared to needing to order an entire set **and then** take the first 5 values, the principle of saves a lot of execution time and space.



## Where


Returns a subset of items which the specified predicate is true for them.

```cs
List<string> trees = new List<string>{ "Oak", "Birch", "Beech", "Elm", "Hazel", "Maple" };

```

### Method syntax

```cs
// Select all trees with name of length 3
var shortTrees = trees.Where(tree => tree.Length == 3); // Oak, Elm

```

### Query syntax

```cs
var shortTrees = from tree in trees
                 where tree.Length == 3
                 select tree; // Oak, Elm

```



## Using Range with various Linq methods


You can use the Enumerable class alongside Linq queries to convert for loops into Linq one liners.

**Select Example**

Opposed to doing this:

```cs
var asciiCharacters = new List<char>();
for (var x = 0; x < 256; x++)
{
    asciiCharacters.Add((char)x);
}

```

You can do this:

```cs
var asciiCharacters = Enumerable.Range(0, 256).Select(a => (char) a);

```

**Where Example**

In this example, 100 numbers will be generated and even ones will be extracted

```cs
var evenNumbers = Enumerable.Range(1, 100).Where(a => a % 2 == 0);

```



## Using SelectMany instead of nested loops


Given 2 lists

```cs
var list1 = new List<string> { "a", "b", "c" };
var list2 = new List<string> { "1", "2", "3", "4" };

```

if you want to output all permutations you could use nested loops like

```cs
var result = new List<string>();
foreach (var s1 in list1)
    foreach (var s2 in list2)
        result.Add($"{s1}{s2}");

```

Using SelectMany you can do the same operation as

```cs
var result = list1.SelectMany(x => list2.Select(y => $"{x}{y}", x, y)).ToList();

```



## Contains


MSDN:

> 
<p>Determines whether a sequence contains a specified element by using a
specified `IEqualityComparer<T>`</p>


```cs
List<int> numbers = new List<int> { 1, 2, 3, 4, 5 };
var result1 = numbers.Contains(4); // true
var result2 = numbers.Contains(8); // false

List<int> secondNumberCollection = new List<int> { 4, 5, 6, 7 };
// Note that can use the Intersect method in this case
var result3 = secondNumberCollection.Where(item => numbers.Contains(item)); // will be true only for 4,5

```

Using a user defined object:

```cs
public class Person
{
   public string Name { get; set; }
}

List<Person> objects = new List<Person>
{
    new Person { Name = "Nikki"},
    new Person { Name = "Gilad"},
    new Person { Name = "Phil"},
    new Person { Name = "John"}
};

//Using the Person's Equals method - override Equals() and GetHashCode() - otherwise it
//will compare by reference and result will be false
var result4 = objects.Contains(new Person { Name = "Phil" }); // true

```

Using the `Enumerable.Contains(value, comparer)` overload:

```cs
public class Compare : IEqualityComparer<Person>
{
    public bool Equals(Person x, Person y)
    {
        return x.Name == y.Name;
    }
    public int GetHashCode(Person codeh)
    {
        return codeh.Name.GetHashCode();
    }
}

var result5 = objects.Contains(new Person { Name = "Phil" }, new Compare()); // true

```

**A smart usage of `Contains` would be to replace multiple `if` clauses to a `Contains` call.**

So instead of doing this:

```cs
if(status == 1 || status == 3 || status == 4)
{
    //Do some business operation
}
else
{
    //Do something else
}

```

Do this:

```cs
if(new int[] {1, 3, 4 }.Contains(status)
{
    //Do some business operaion
}
else 
{
    //Do something else
}

```



## GroupBy one or multiple fields


Lets assume we have some Film model:

```cs
public class Film {
    public string Title { get; set; }
    public string Category { get; set; }
    public int Year { get; set; }
}

```

Group by Category property:

```cs
foreach (var grp in films.GroupBy(f => f.Category)) {
    var groupCategory = grp.Key;
    var numberOfFilmsInCategory = grp.Count();
}

```

Group by Category and Year:

```cs
foreach (var grp in films.GroupBy(f => new { Category = f.Category, Year = f.Year })) {
    var groupCategory = grp.Key.Category;
    var groupYear = grp.Key.Year;
    var numberOfFilmsInCategory = grp.Count();
}

```



## Query Ordering - OrderBy() ThenBy() OrderByDescending() ThenByDescending()


```cs
string[] names= { "mark", "steve", "adam" };

```

**Ascending:**

**Query Syntax**

```cs
var sortedNames =
    from name in names
    orderby name
    select name;

```

**Method Syntax**

```cs
var sortedNames = names.OrderBy(name => name);

```

sortedNames contains the names in following order:
"adam","mark","steve"

**Descending:**

**Query Syntax**

```cs
var sortedNames =
    from name in names
    orderby name descending
    select name;

```

**Method Syntax**

```cs
var sortedNames = names.OrderByDescending(name => name);

```

sortedNames contains the names in following order:
"steve","mark","adam"

**Order by several fields**

```cs
Person[] people =
{
    new Person { FirstName = "Steve", LastName = "Collins", Age = 30},
    new Person { FirstName = "Phil" , LastName = "Collins", Age = 28},
    new Person { FirstName = "Adam" , LastName = "Ackerman", Age = 29},
    new Person { FirstName = "Adam" , LastName = "Ackerman", Age = 15}
};

```

**Query Syntax**

```cs
var sortedPeople = from person in people
                   orderby person.LastName, person.FirstName, person.Age descending
                   select person;

```

**Method Syntax**

```

sortedPeople = people.OrderBy(person => person.LastName)
                      .ThenBy(person => person.FirstName)
                      .ThenByDescending(person => person.Age);

```

**Result**

```cs
1. Adam Ackerman 29
2. Adam Ackerman 15
3. Phil Collins  28
4. Steve Collins 30

```



## ToDictionary


The `ToDictionary()` LINQ method can be used to generate a `Dictionary<TKey, TElement>` collection based on a given `IEnumerable<T>` source.

```cs
IEnumerable<User> users = GetUsers();
Dictionary<int, User> usersById = users.ToDictionary(x => x.Id);

```

In this example, the single argument passed to `ToDictionary` is of type `Func<TSource, TKey>`, which returns the key for each element.

This is a concise way to perform the following operation:

```cs
Dictionary<int, User> usersById = new Dictionary<int User>();
foreach (User u in users) 
{
  usersById.Add(u.Id, u);
}

```

You can also pass a second parameter to the `ToDictionary` method, which is of type `Func<TSource, TElement>` and returns the `Value` to be added for each entry.

```cs
IEnumerable<User> users = GetUsers();
Dictionary<int, string> userNamesById = users.ToDictionary(x => x.Id, x => x.Name);

```

It is also possible to specify the `IComparer` that is used to compare key values. This can be useful when the key is a string and you want it to match case-insensitive.

```cs
IEnumerable<User> users = GetUsers();
Dictionary<string, User> usersByCaseInsenstiveName = users.ToDictionary(x => x.Name, StringComparer.InvariantCultureIgnoreCase);

var user1 = usersByCaseInsenstiveName["john"];
var user2 = usersByCaseInsenstiveName["JOHN"];
user1 == user2; // Returns true

```

Note: the `ToDictionary` method requires all keys to be unique, there must be no duplicate keys. If there are, then an exception is thrown: `ArgumentException: An item with the same key has already been added.` If you have a scenario where you know that you will have multiple elements with the same key, then you are better off using [`ToLookup`](http://stackoverflow.com/documentation/c%23/68/linq-queries/14871/tolookup) instead.



## SkipWhile


`SkipWhile()` is used to exclude elements until first non-match (this might be counter intuitive to most)

```cs
int[] list = { 42, 42, 6, 6, 6, 42 };
var result = list.SkipWhile(i => i == 42); 
// Result: 6, 6, 6, 42

```



## DefaultIfEmpty


DefaultIfEmpty is used to return a Default Element if the Sequence contains no elements. This Element can be the Default of the Type or a user defined instance of that Type. Example:

```cs
var chars = new List<string>() { "a", "b", "c", "d" };

chars.DefaultIfEmpty("N/A").FirstOrDefault(); // returns "a";

chars.Where(str => str.Length > 1)
     .DefaultIfEmpty("N/A").FirstOrDefault(); // return "N/A"

chars.Where(str => str.Length > 1)
        .DefaultIfEmpty().First(); // returns null;

```

### **Usage in Left Joins**:

With `DefaultIfEmpty` the traditional Linq Join can return a default object if no match was found. Thus acting as a SQL's Left Join. Example:

```cs
var leftSequence = new List<int>() { 99, 100, 5, 20, 102, 105 };
var rightSequence = new List<char>() { 'a', 'b', 'c', 'i', 'd' };

var numbersAsChars = from l in leftSequence
                     join r in rightSequence
                     on l equals (int)r into leftJoin
                     from result in leftJoin.DefaultIfEmpty('?')
                     select new
                     {
                         Number = l,
                         Character = result
                     };

foreach(var item in numbersAsChars)
{
    Console.WriteLine("Num = {0} ** Char = {1}", item.Number, item.Character);
}

ouput: 

Num = 99         Char = c
Num = 100        Char = d
Num = 5          Char = ?
Num = 20         Char = ?
Num = 102        Char = ?
Num = 105        Char = i

```

In the case where a `DefaultIfEmpty` is used (without specifying a default value) and that will result will no matching items on the right sequence one must make sure that the object is not `null` before accessing its properties. Otherwise it will result in a `NullReferenceException`. Example:

```cs
var leftSequence = new List<int> { 1, 2, 5 };
var rightSequence = new List<dynamic>()
    {
        new { Value = 1 },
        new { Value = 2 },
        new { Value = 3 },
        new { Value = 4 },
    };

var numbersAsChars = (from l in leftSequence
                        join r in rightSequence
                        on l equals r.Value into leftJoin
                        from result in leftJoin.DefaultIfEmpty()
                        select new
                        {
                            Left = l,
                            // 5 will not have a matching object in the right so result 
                            // will be equal to null. 
                            // To avoid an error use:
                            //    -  C# 6.0 or above - ?. 
                            //    -  Under           - result == null ? 0 : result.Value
                            Right = result?.Value
                        }).ToList();

```



## SequenceEqual


`SequenceEqual` is used to compare two `IEnumerable<T>` sequences with each other.

```cs
int[] a = new int[] {1, 2, 3};
int[] b = new int[] {1, 2, 3};
int[] c = new int[] {1, 3, 2};

bool returnsTrue = a.SequenceEqual(b);
bool returnsFalse = a.SequenceEqual(c);

```



## ElementAt and ElementAtOrDefault


`ElementAt` will return the item at index `n`. If `n` is not within the range of the enumerable, throws an `ArgumentOutOfRangeException`.

```cs
int[] numbers  = { 1, 2, 3, 4, 5 };
numbers.ElementAt(2);  // 3
numbers.ElementAt(10); // throws ArgumentOutOfRangeException

```

`ElementAtOrDefault` will return the item at index `n`. If `n` is not within the range of the enumerable, returns a `default(T)`.

```cs
int[] numbers  = { 1, 2, 3, 4, 5 };
numbers.ElementAtOrDefault(2);  // 3
numbers.ElementAtOrDefault(10); // 0 = default(int)

```

Both `ElementAt` and `ElementAtOrDefault` are optimized for when the source is an `IList<T>` and normal indexing will be used in those cases.

Note that for `ElementAt`, if the provided index is greater than the size of the `IList<T>`, the list should (but is technically not guaranteed to) throw an `ArgumentOutOfRangeException`.



## Joining multiple sequences


Consider entities `Customer`, `Purchase` and `PurchaseItem` as follows:

```cs
public class Customer
{
   public string Id { get; set } // A unique Id that identifies customer    
   public string Name  {get; set; }
}

public class Purchase
{
   public string Id { get; set }
   public string CustomerId {get; set; }
   public string Description { get; set; }
}

public class PurchaseItem
{
   public string Id { get; set }
   public string PurchaseId {get; set; }
   public string Detail { get; set; }
}

```

Consider following sample data for above entities:

```cs
var customers = new List<Customer>()             
 {
    new Customer() {
        Id = Guid.NewGuid().ToString(),
        Name = "Customer1"            
    },
            
    new Customer() {
        Id = Guid.NewGuid().ToString(),
        Name = "Customer2"            
    }
 };        
    
 var purchases = new List<Purchase>() 
 {
     new Purchase() {                
         Id = Guid.NewGuid().ToString(),
         CustomerId = customers[0].Id,
         Description = "Customer1-Purchase1"            
     },

     new Purchase() {
         Id = Guid.NewGuid().ToString(),
         CustomerId = customers[0].Id,
         Description = "Customer1-Purchase2"            
     },
     
     new Purchase() {
         Id = Guid.NewGuid().ToString(),
         CustomerId = customers[1].Id,
         Description = "Customer2-Purchase1"            
     },

     new Purchase() {
         Id = Guid.NewGuid().ToString(),
         CustomerId = customers[1].Id,
         Description = "Customer2-Purchase2"            
     }
  };
    
 var purchaseItems = new List<PurchaseItem>() 
 {
     new PurchaseItem() {                
         Id = Guid.NewGuid().ToString(),
         PurchaseId= purchases[0].Id,
         Detail = "Purchase1-PurchaseItem1"            
     },

     new PurchaseItem() {                
         Id = Guid.NewGuid().ToString(),
         PurchaseId= purchases[1].Id,
         Detail = "Purchase2-PurchaseItem1"            
     },
     
     new PurchaseItem() {                
         Id = Guid.NewGuid().ToString(),
         PurchaseId= purchases[1].Id,
         Detail = "Purchase2-PurchaseItem2"            
     },

     new PurchaseItem() {                
         Id = Guid.NewGuid().ToString(),
         PurchaseId= purchases[3].Id,
         Detail = "Purchase3-PurchaseItem1"
     }
 };

```

Now, consider below linq query:

```cs
var result = from c in customers
            join p in purchases on c.Id equals p.CustomerId           // first join
            join pi in purchaseItems on p.Id equals pi.PurchaseId     // second join
            select new
            {
               c.Name, p.Description, pi.Detail
            };

```

To output the result of above query:

```cs
foreach(var resultItem in result)
{
    Console.WriteLine($"{resultItem.Name}, {resultItem.Description}, {resultItem.Detail}");
}

```

The output of the query would be:

> 
Customer1, Customer1-Purchase1, Purchase1-PurchaseItem1
Customer1, Customer1-Purchase2, Purchase2-PurchaseItem1
Customer1, Customer1-Purchase2, Purchase2-PurchaseItem2
Customer2, Customer2-Purchase2, Purchase3-PurchaseItem1


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/Db8uqp)



## Joining on multiple keys


```cs

 PropertyInfo[] stringProps = typeof (string).GetProperties();//string properties
  PropertyInfo[] builderProps = typeof(StringBuilder).GetProperties();//stringbuilder properties
    
    var query =
        from s in stringProps
        join b in builderProps
            on new { s.Name, s.PropertyType } equals new { b.Name, b.PropertyType }
        select new
        {
            s.Name,
            s.PropertyType,
            StringToken = s.MetadataToken,
            StringBuilderToken = b.MetadataToken
        };

```

Note that anonymous types in above `join` must contain same properties since objects are considered equal only if all their properties are equal. Otherwise query won't compile.



## Sum


The `Enumerable.Sum` extension method calculates the sum of numeric values.

In case the collection's elements are themselves numbers, you can calculate the sum directly.

```cs
int[] numbers = new int[] { 1, 4, 6 };
Console.WriteLine( numbers.Sum() ); //outputs 11

```

In case the type of the elements is a complex type, you can use a lambda expression to specify the value that should be calculated:

```cs
var totalMonthlySalary = employees.Sum( employee => employee.MonthlySalary );

```

Sum extension method can calculate with the following types:

- Int32
- Int64
- Single
- Double
- Decimal

In case your collection contains nullable types, you can use the null-coalescing operator to set a default value for null elements:

```cs
int?[] numbers = new int?[] { 1, null, 6 };
Console.WriteLine( numbers.Sum( number => number ?? 0 ) ); //outputs 7

```



## ToLookup


> 
<p>ToLookup returns a data structure that allows indexing. It is an
extension method. It produces an ILookup instance that can be indexed
or enumerated using a foreach-loop. The entries are combined into
groupings at each key. - dotnetperls</p>


```cs
string[] array = { "one", "two", "three" };
//create lookup using string length as key
var lookup = array.ToLookup(item => item.Length);

//join the values whose lengths are 3
Console.WriteLine(string.Join(",",lookup[3]));
//output: one,two

```

Another Example:

```cs
int[] array = { 1,2,3,4,5,6,7,8 };
//generate lookup for odd even numbers (keys will be 0 and 1)
var lookup = array.ToLookup(item => item % 2);

//print even numbers after joining
Console.WriteLine(string.Join(",",lookup[0]));
//output: 2,4,6,8

//print odd numbers after joining
Console.WriteLine(string.Join(",",lookup[1]));
//output: 1,3,5,7

```



## Any and First(OrDefault) - best practice


I won't explain what `Any` and `FirstOrDefault` does because there are already two good example about them. See [Any](https://stackoverflow.com/documentation/c%23/68/linq-queries/5098/any#t=201707200324548979636) and [First, FirstOrDefault, Last, LastOrDefault, Single, and SingleOrDefault](https://stackoverflow.com/documentation/c%23/68/linq-queries/329/first-firstordefault-last-lastordefault-single-and-singleordefault#t=201707200328069088515) for more information.

A pattern I often see in code which **should be avoided** is

```cs
if (myEnumerable.Any(t=>t.Foo == "Bob"))
{
    var myFoo = myEnumerable.First(t=>t.Foo == "Bob");
    //Do stuff
}

```

It could be written more efficiently like this

```cs
var myFoo = myEnumerable.FirstOrDefault(t=>t.Foo == "Bob");
if (myFoo != null)
{
    //Do stuff
}

```

By using the second example, the collection is searched only once and give the same result as the first one. The same idea can be applied to `Single`.



## GroupBy Sum and Count


Let's take a sample class:

```cs
public class Transaction
{
    public string Category { get; set; }
    public DateTime Date { get; set; }
    public decimal Amount { get; set; }
}

```

Now, let us consider a list of transactions:

```cs
var transactions = new List<Transaction>
{
   new Transaction { Category = "Saving Account", Amount = 56, Date = DateTime.Today.AddDays(1) },
   new Transaction { Category = "Saving Account", Amount = 10, Date = DateTime.Today.AddDays(-10) },
   new Transaction { Category = "Credit Card", Amount = 15, Date = DateTime.Today.AddDays(1) },
   new Transaction { Category = "Credit Card", Amount = 56, Date = DateTime.Today },
   new Transaction { Category = "Current Account", Amount = 100, Date = DateTime.Today.AddDays(5) },
};

```

If you want to calculate category wise sum of amount and count, you can use GroupBy as follows:

```cs
var summaryApproach1 = transactions.GroupBy(t => t.Category)
                           .Select(t => new
                           {
                               Category = t.Key,
                               Count = t.Count(),
                               Amount = t.Sum(ta => ta.Amount),
                           }).ToList();

Console.WriteLine("-- Summary: Approach 1 --");
summaryApproach1.ForEach(
            row => Console.WriteLine($"Category: {row.Category}, Amount: {row.Amount}, Count: {row.Count}"));

```

Alternatively, you can do this in one step:

```cs
var summaryApproach2 = transactions.GroupBy(t => t.Category, (key, t) =>
{
        var transactionArray = t as Transaction[] ?? t.ToArray();
        return new
        {
            Category = key,
            Count = transactionArray.Length,
            Amount = transactionArray.Sum(ta => ta.Amount),
        };
}).ToList();

Console.WriteLine("-- Summary: Approach 2 --");
summaryApproach2.ForEach(
row => Console.WriteLine($"Category: {row.Category}, Amount: {row.Amount}, Count: {row.Count}"));

```

Output for both the above queries would be same:

> 
Category: Saving Account, Amount: 66, Count: 2
Category: Credit Card, Amount: 71, Count: 2
Category: Current Account, Amount: 100, Count: 1


[Live Demo in .NET Fiddle](https://dotnetfiddle.net/1PfLGq#)



## OrderBy


> 
Orders a collection by a specified value.


When the value is an **integer**, **double** or **float** it starts with the **minimum value**, which means that you get first the negative values, than zero and afterwords the positive values (see Example 1).

When you order by a **char** the method compares the **ascii values** of the chars to sort the collection (see Example 2).

When you sort **strings** the OrderBy method compares them by taking a look at their [CultureInfo](https://msdn.microsoft.com/en-us/library/xk2wykcz(VS.71).aspx) but normaly starting with the **first letter** in the alphabet (a,b,c...).

This kind of order is called ascending, if you want it the other way round you need descending (see OrderByDescending).

**Example 1:**

```cs
int[] numbers = {2, 1, 0, -1, -2};
IEnumerable<int> ascending = numbers.OrderBy(x => x);
// returns {-2, -1, 0, 1, 2}

```

**Example 2:**

```cs

char[] letters = {' ', '!', '?', '[', '{', '+', '1', '9', 'a', 'A', 'b', 'B', 'y', 'Y', 'z', 'Z'};
 IEnumerable<char> ascending = letters.OrderBy(x => x);
 // returns { ' ', '!', '+', '1', '9', '?', 'A', 'B', 'Y', 'Z', '[', 'a', 'b', 'y', 'z', '{' }

```

**Example:**

```cs
class Person
{
   public string Name { get; set; }
   public int Age { get; set; }
}

var people = new[]
{
    new Person {Name = "Alice", Age = 25},
    new Person {Name = "Bob", Age = 21},
    new Person {Name = "Carol", Age = 43}
};
var youngestPerson = people.OrderBy(x => x.Age).First();
var name = youngestPerson.Name; // Bob

```



## Select - Transforming elements


Select allows you to apply a transformation to every element in any data structure implementing IEnumerable.

Getting the first character of each string in the following list:

```cs
List<String> trees = new List<String>{ "Oak", "Birch", "Beech", "Elm", "Hazel", "Maple" };

```

Using regular (lambda) syntax

```cs
//The below select stament transforms each element in tree into its first character.
IEnumerable<String> initials = trees.Select(tree => tree.Substring(0, 1));
foreach (String initial in initials) {
    System.Console.WriteLine(initial);
}

```

**Output:**

> 
<p>O<br />
B<br />
B<br />
E<br />
H<br />
M</p>


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/yYLT0K)

Using LINQ Query Syntax

```cs
initials = from tree in trees
           select tree.Substring(0, 1);

```



## Union


Merges two collections to create a distinct collection using the  default equality comparer

```cs
int[] numbers1 = { 1, 2, 3 };
int[] numbers2 = { 2, 3, 4, 5 };

var allElement = numbers1.Union(numbers2);   // AllElement now contains 1,2,3,4,5

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/oet2Uq)



## Count and LongCount


`Count` returns the number of elements in an `IEnumerable<T>`. `Count` also exposes an optional predicate parameter that allows you to filter the elements you want to count.

```cs
int[] array = { 1, 2, 3, 4, 2, 5, 3, 1, 2 };

int n = array.Count(); // returns the number of elements in the array
int x = array.Count(i => i > 2); // returns the number of elements in the array greater than 2

```

`LongCount` works the same way as `Count` but has a return type of `long` and is used for counting `IEnumerable<T>` sequences that are longer than `int.MaxValue`

```cs
int[] array = GetLargeArray();

long n = array.LongCount(); // returns the number of elements in the array
long x = array.LongCount(i => i > 100); // returns the number of elements in the array greater than 100

```



## Incrementally building a query


Because LINQ uses **deferred execution**, we can have a query object that doesn't actually contain the values, but will return the values when evaluated. We can thus dynamically build the query based on our control flow, and evaluate it once we are finished:

```cs
IEnumerable<VehicleModel> BuildQuery(int vehicleType, SearchModel search, int start = 1, int count = -1) {
    IEnumerable<VehicleModel> query = _entities.Vehicles
        .Where(x => x.Active && x.Type == vehicleType)
        .Select(x => new VehicleModel {
            Id = v.Id,
            Year = v.Year,
            Class = v.Class,
            Make = v.Make,
            Model = v.Model,
            Cylinders = v.Cylinders ?? 0
        });

```

We can conditionally apply filters:

```cs

   if (!search.Years.Contains("all", StringComparer.OrdinalIgnoreCase))
        query = query.Where(v => search.Years.Contains(v.Year));

    if (!search.Makes.Contains("all", StringComparer.OrdinalIgnoreCase)) {
        query = query.Where(v => search.Makes.Contains(v.Make));
    }

    if (!search.Models.Contains("all", StringComparer.OrdinalIgnoreCase)) {
        query = query.Where(v => search.Models.Contains(v.Model));
    }

    if (!search.Cylinders.Equals("all", StringComparer.OrdinalIgnoreCase)) {
        decimal minCylinders = 0;
        decimal maxCylinders = 0;
        switch (search.Cylinders) {
            case "2-4":
                maxCylinders = 4;
                break;
            case "5-6":
                minCylinders = 5;
                maxCylinders = 6;
                break;
            case "8":
                minCylinders = 8;
                maxCylinders = 8;
                break;
            case "10+":
                minCylinders = 10;
                break;
        }
        if (minCylinders > 0) {
            query = query.Where(v => v.Cylinders >= minCylinders);
        }
        if (maxCylinders > 0) {
            query = query.Where(v => v.Cylinders <= maxCylinders);
        }
    }

```

We can add a sort order to the query based on a condition:

```cs

   switch (search.SortingColumn.ToLower()) {
        case "make_model":
            query = query.OrderBy(v => v.Make).ThenBy(v => v.Model);
            break;
        case "year":
            query = query.OrderBy(v => v.Year);
            break;
        case "engine_size":
            query = query.OrderBy(v => v.EngineSize).ThenBy(v => v.Cylinders);
            break;
        default:
            query = query.OrderBy(v => v.Year); //The default sorting.
    }

```

Our query can be defined to start from a given point:

```cs

   query = query.Skip(start - 1);

```

and defined to return a specific number of records:

```cs

   if (count > -1) {
        query = query.Take(count);
    }
    return query;
}

```

Once we have the query object, we can evaluate the results with a `foreach` loop, or one of the LINQ methods that returns a set of values, such as `ToList` or `ToArray`:

```cs
SearchModel sm;

// populate the search model here
// ...

List<VehicleModel> list = BuildQuery(5, sm).ToList();

```



## GroupJoin with outer range variable


```cs
Customer[] customers = Customers.ToArray();
Purchase[] purchases = Purchases.ToArray();

var groupJoinQuery =
    from c in customers
    join p in purchases on c.ID equals p.CustomerID
    into custPurchases
    select new
    {
        CustName = c.Name,
        custPurchases
    };

```



## Linq  Quantifiers


Quantifier operations return a Boolean value if some or all of the elements in a sequence satisfy a condition. In this article, we will see some common LINQ to Objects scenarios where we can use these operators.
There are 3 Quantifiers operations that can be used in LINQ:

`All` – used to determine whether all the elements in a sequence satisfy a condition.
Eg:

```cs
int[] array = { 10, 20, 30 }; 
   
// Are all elements >= 10? YES
array.All(element => element >= 10); 
   
// Are all elements >= 20? NO
array.All(element => element >= 20);
    
// Are all elements < 40? YES
array.All(element => element < 40);

```

`Any` - used to determine whether any elements in a sequence satisfy a condition.
Eg:

```cs
int[] query=new int[] { 2, 3, 4 }
query.Any (n => n == 3);

```

`Contains` - used to determine whether a sequence contains a specified element.
Eg:

```cs
//for int array
int[] query =new int[] { 1,2,3 };
query.Contains(1);

//for string array
string[] query={"Tom","grey"};
query.Contains("Tom");

//for a string
var stringValue="hello";
stringValue.Contains("h");

```



## TakeWhile


`TakeWhile` returns elements from a sequence as long as the condition is true

```cs
int[] list = { 1, 10, 40, 50, 44, 70, 4 };
var result = list.TakeWhile(item => item < 50).ToList();
// result = { 1, 10, 40 }

```



## Build your own Linq operators for IEnumerable<T>


One of the great things about Linq is that it is so easy to extend.  You just need to create an [extension method](http://stackoverflow.com/documentation/c%23/20/extension-methods/33/using-an-extension-method#t=201607280952261411896) whose argument is `IEnumerable<T>`.

```cs
public namespace MyNamespace
{
    public static class LinqExtensions
    {
        public static IEnumerable<List<T>> Batch<T>(this IEnumerable<T> source, int batchSize)
        {
            var batch = new List<T>();
            foreach (T item in source)
            {
                batch.Add(item);
                if (batch.Count == batchSize)
                {
                    yield return batch;
                    batch = new List<T>();
                }
            }
            if (batch.Count > 0)
                yield return batch;
        }
    }
}

```

This example splits the items in an `IEnumerable<T>` into lists of a fixed size, the last list containing the remainder of the items. Notice how the object to which the extension method is applied is passed in (argument `source`) as the initial argument using the `this` keyword. Then the `yield` keyword is used to output the next item in the output `IEnumerable<T>` before continuing with execution from that point (see [yield keyword](http://stackoverflow.com/documentation/c%23/61/yield-keyword#t=201607281004094832778)).

This example would be used in your code like this:

```cs
//using MyNamespace;
var items = new List<int> { 2, 3, 4, 5, 6 };
foreach (List<int> sublist in items.Batch(3))
{
    // do something
}

```

On the first loop, sublist would be `{2, 3, 4}` and on the second `{5, 6}`.

Custom LinQ methods can be combined with standard LinQ methods too. e.g.:

```cs
//using MyNamespace;
var result = Enumerable.Range(0, 13)         // generate a list
                       .Where(x => x%2 == 0) // filter the list or do something other
                       .Batch(3)             // call our extension method
                       .ToList()             // call other standard methods

```

This query will return even numbers grouped in batches with a size of 3: `{0, 2, 4}, {6, 8, 10}, {12}`

Remember you need a `using MyNamespace;` line in order to be able to access the extension method.



## Reverse


- Inverts the order of the elements in a sequence.
- If there is no items throws a `ArgumentNullException: source is null.`

****Example:****

```cs
// Create an array.
int[] array = { 1, 2, 3, 4 };                         //Output:
// Call reverse extension method on the array.        //4
var reverse = array.Reverse();                        //3
// Write contents of array to screen.                 //2
foreach (int value in reverse)                        //1
    Console.WriteLine(value);

```

[Live code example](https://dotnetfiddle.net/ckrWUo)

Remeber that `Reverse()` may work diffrent depending on the chain order of your LINQ statements.

```cs

       //Create List of chars
        List<int> integerlist = new List<int>() { 1, 2, 3, 4, 5, 6 };

        //Reversing the list then taking the two first elements
        IEnumerable<int> reverseFirst = integerlist.Reverse<int>().Take(2);
        
        //Taking 2 elements and then reversing only thos two
        IEnumerable<int> reverseLast = integerlist.Take(2).Reverse();
        
        //reverseFirst output: 6, 5
        //reverseLast output:  2, 1

```

[Live code example](https://dotnetfiddle.net/ckrWUo)

**Reverse()** works by buffering everything then walk through it backwards, whitch is not very efficient, but neither is OrderBy from that perspective.

In LINQ-to-Objects, there are buffering operations (Reverse, OrderBy, GroupBy, etc) and non-buffering operations (Where, Take, Skip, etc).

****Example: Non-buffering Reverse extention****

```cs
public static IEnumerable<T> Reverse<T>(this IList<T> list) {
    for (int i = list.Count - 1; i >= 0; i--) 
        yield return list[i];
}

```

[Live code example](https://dotnetfiddle.net/ckrWUo)

This method can encounter problems if u mutate the list while iterating.



## OrderByDescending


> 
Orders a collection by a specified value.


When the value is an **integer**, **double** or **float** it starts with the **maximal value**, which means that you get first the positive values, than zero and afterwords the negative values (see Example 1).

When you order by a **char** the method compares the **ascii values** of the chars to sort the collection (see Example 2).

When you sort **strings** the OrderBy method compares them by taking a look at their [CultureInfo](https://msdn.microsoft.com/en-us/library/xk2wykcz(VS.71).aspx) but normaly starting with the **last letter** in the alphabet (z,y,x,...).

This kind of order is called descending, if you want it the other way round you need ascending (see OrderBy).

**Example 1:**

```cs
int[] numbers = {-2, -1, 0, 1, 2};
IEnumerable<int> descending = numbers.OrderByDescending(x => x);
// returns {2, 1, 0, -1, -2}

```

**Example 2:**

```cs
char[] letters = {' ', '!', '?', '[', '{', '+', '1', '9', 'a', 'A', 'b', 'B', 'y', 'Y', 'z', 'Z'};
IEnumerable<char> descending = letters.OrderByDescending(x => x);
// returns { '{', 'z', 'y', 'b', 'a', '[', 'Z', 'Y', 'B', 'A', '?', '9', '1', '+', '!', ' ' }

```

**Example 3:**

```cs
class Person
{
   public  string Name { get; set; }
   public  int Age { get; set; }
}

var people = new[]
{
    new Person {Name = "Alice", Age = 25},
    new Person {Name = "Bob", Age = 21},
    new Person {Name = "Carol", Age = 43}
};
var oldestPerson = people.OrderByDescending(x => x.Age).First();
var name = oldestPerson.Name; // Carol

```



## Concat


Merges two collections (without removing duplicates)

```cs
List<int> foo = new List<int> { 1, 2, 3 };
List<int> bar = new List<int> { 3, 4, 5 };

// Through Enumerable static class
var result = Enumerable.Concat(foo, bar).ToList(); // 1,2,3,3,4,5

// Through extension method
var result = foo.Concat(bar).ToList(); // 1,2,3,3,4,5

```



## Select with Func<TSource, int, TResult> selector - Use to get ranking of elements


On of the overloads of the `Select` extension methods also passes the `index` of the current item in the collection being `select`ed. These are a few uses of it.

**Get the "row number" of the items**

```cs
var rowNumbers = collection.OrderBy(item => item.Property1)
                           .ThenBy(item => item.Property2)
                           .ThenByDescending(item => item.Property3)
                           .Select((item, index) => new { Item = item, RowNumber = index })
                           .ToList();

```

**Get the rank of an item **within** its group**

```cs
var rankInGroup = collection.GroupBy(item => item.Property1)
                            .OrderBy(group => group.Key)
                            .SelectMany(group => group.OrderBy(item => item.Property2)
                                                   .ThenByDescending(item => item.Property3)
                                                   .Select((item, index) => new 
                                                   { 
                                                       Item = item, 
                                                       RankInGroup = index 
                                                   })).ToList();

```

**Get the ranking of groups (also known in Oracle as dense_rank)**

```cs
var rankOfBelongingGroup = collection.GroupBy(item => item.Property1)
                            .OrderBy(group => group.Key)
                            .Select((group, index) => new
                            {
                                Items = group,
                                Rank = index
                            })
                            .SelectMany(v => v.Items, (s, i) => new
                            {
                                Item = i,
                                DenseRank = s.Rank
                            }).ToList();

```

For testing this you can use:

```cs
public class SomeObject
{
    public int Property1 { get; set; }
    public int Property2 { get; set; }
    public int Property3 { get; set; }

    public override string ToString()
    {
        return string.Join(", ", Property1, Property2, Property3);
    }
}

```

And data:

```cs
List<SomeObject> collection = new List<SomeObject>
{
    new SomeObject { Property1 = 1, Property2 = 1, Property3 = 1},
    new SomeObject { Property1 = 1, Property2 = 2, Property3 = 1},
    new SomeObject { Property1 = 1, Property2 = 2, Property3 = 2},
    new SomeObject { Property1 = 2, Property2 = 1, Property3 = 1},
    new SomeObject { Property1 = 2, Property2 = 2, Property3 = 1},
    new SomeObject { Property1 = 2, Property2 = 2, Property3 = 1},
    new SomeObject { Property1 = 2, Property2 = 3, Property3 = 1}
};

```



#### Syntax


<li>
Query syntax :
<ul>
- from <range variable> in <collection>
- [from <range variable> in <collection>, ...]
- <filter, joining, grouping, aggregate operators, ...> <lambda expression>
- <select or groupBy operator> <formulate the result>

Method syntax :

- Enumerable.Aggregate(func)
- Enumerable.Aggregate(seed, func)
- Enumerable.Aggregate(seed, func, resultSelector)
- Enumerable.All(predicate)
- Enumerable.Any()
- Enumerable.Any(predicate)
- Enumerable.AsEnumerable()
- Enumerable.Average()
- Enumerable.Average(selector)
- Enumerable.Cast<Result>()
- Enumerable.Concat(second)
- Enumerable.Contains(value)
- Enumerable.Contains(value, comparer)
- Enumerable.Count()
- Enumerable.Count(predicate)
- Enumerable.DefaultIfEmpty()
- Enumerable.DefaultIfEmpty(defaultValue)
- Enumerable.Distinct()
- Enumerable.Distinct(comparer)
- Enumerable.ElementAt(index)
- Enumerable.ElementAtOrDefault(index)
- Enumerable.Empty()
- Enumerable.Except(second)
- Enumerable.Except(second, comparer)
- Enumerable.First()
- Enumerable.First(predicate)
- Enumerable.FirstOrDefault()
- Enumerable.FirstOrDefault(predicate)
- Enumerable.GroupBy(keySelector)
- Enumerable.GroupBy(keySelector, resultSelector)
- Enumerable.GroupBy(keySelector, elementSelector)
- Enumerable.GroupBy(keySelector, comparer)
- Enumerable.GroupBy(keySelector, resultSelector, comparer)
- Enumerable.GroupBy(keySelector, elementSelector, resultSelector)
- Enumerable.GroupBy(keySelector, elementSelector, comparer)
- Enumerable.GroupBy(keySelector, elementSelector, resultSelector, comparer)
- Enumerable.Intersect(second)
- Enumerable.Intersect(second, comparer)
- Enumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector)
- Enumerable.Join(inner, outerKeySelector, innerKeySelector, resultSelector, comparer)
- Enumerable.Last()
- Enumerable.Last(predicate)
- Enumerable.LastOrDefault()
- Enumerable.LastOrDefault(predicate)
- Enumerable.LongCount()
- Enumerable.LongCount(predicate)
- Enumerable.Max()
- Enumerable.Max(selector)
- Enumerable.Min()
- Enumerable.Min(selector)
- Enumerable.OfType<TResult>()
- Enumerable.OrderBy(keySelector)
- Enumerable.OrderBy(keySelector, comparer)
- Enumerable.OrderByDescending(keySelector)
- Enumerable.OrderByDescending(keySelector, comparer)
- Enumerable.Range(start, count)
- Enumerable.Repeat(element, count)
- Enumerable.Reverse()
- Enumerable.Select(selector)
- Enumerable.SelectMany(selector)
- Enumerable.SelectMany(collectionSelector, resultSelector)
- Enumerable.SequenceEqual(second)
- Enumerable.SequenceEqual(second, comparer)
- Enumerable.Single()
- Enumerable.Single(predicate)
- Enumerable.SingleOrDefault()
- Enumerable.SingleOrDefault(predicate)
- Enumerable.Skip(count)
- Enumerable.SkipWhile(predicate)
- Enumerable.Sum()
- Enumerable.Sum(selector)
- Enumerable.Take(count)
- Enumerable.TakeWhile(predicate)
- orderedEnumerable.ThenBy(keySelector)
- orderedEnumerable.ThenBy(keySelector, comparer)
- orderedEnumerable.ThenByDescending(keySelector)
- orderedEnumerable.ThenByDescending(keySelector, comparer)
- Enumerable.ToArray()
- Enumerable.ToDictionary(keySelector)
- Enumerable.ToDictionary(keySelector, elementSelector)
- Enumerable.ToDictionary(keySelector, comparer)
- Enumerable.ToDictionary(keySelector, elementSelector, comparer)
- Enumerable.ToList()
- Enumerable.ToLookup(keySelector)
- Enumerable.ToLookup(keySelector, elementSelector)
- Enumerable.ToLookup(keySelector, comparer)
- Enumerable.ToLookup(keySelector, elementSelector, comparer)
- Enumerable.Union(second)
- Enumerable.Union(second, comparer)
- Enumerable.Where(predicate)
- Enumerable.Zip(second, resultSelector)



#### Remarks


To use LINQ queries you need to import `System.Linq`.

The Method Syntax is more powerful and flexible, but the Query Syntax may be simpler and more familiar. All queries written in Query syntax are translated into the functional syntax by the compiler, so performance is the same.

Query objects are not evaluated until they are used, so they can be changed or added to without a performance penalty.

