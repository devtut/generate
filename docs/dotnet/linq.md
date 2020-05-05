---
metaTitle: ".NET Framework - LINQ"
description: "SelectMany (flat map), Where (filter), Any, GroupJoin, Except, Zip, Aggregate (fold), ToLookup, Intersect, Concat, All, Sum, SequenceEqual, Min, Distinct, Count, Cast, Select (map), OrderBy, OrderByDescending, Contains, First (find), Single, Last, LastOrDefault, SingleOrDefault, FirstOrDefault, Skip, Take, Reverse, OfType, Max, Average, GroupBy, ToDictionary, Union, ToArray, ToList, ElementAt, ElementAtOrDefault, SkipWhile, TakeWhile, DefaultIfEmpty, Join, Empty, ThenBy, Range, Left Outer Join, Repeat"
---

# LINQ


LINQ (Language Integrated Query) is an expression that retrieves data from a data source. LINQ simplifies this situation by offering a consistent model for working with data across various kinds of data sources and formats. In a LINQ query, you are always working with objects. You use the same basic coding patterns to query and transform data in XML documents, SQL databases, ADO.NET Datasets, .NET collections, and any other format for which a provider is available. LINQ can be used in C# and VB.



## SelectMany (flat map)


[`Enumerable.Select`](https://msdn.microsoft.com/en-us/library/bb548891(v=vs.100).aspx) returns an output element for every input element.
Whereas  [`Enumerable.SelectMany`](https://msdn.microsoft.com/en-us/library/bb534336(v=vs.100).aspx) produces a variable number of output elements for each input element. This means that the output sequence may contain more or fewer elements than were in the input sequence.

[`Lambda expressions`](http://stackoverflow.com/documentation/c%23/46/lambda-expressions) passed to `Enumerable.Select` must return a single item. Lambda expressions passed to `Enumerable.SelectMany` must produce a child sequence. This child sequence may contain a varying number of elements for each element in the input sequence.

**Example**

```dotnet
class Invoice
{
    public int Id { get; set; }
}

class Customer
{
    public Invoice[] Invoices {get;set;}
}

var customers = new[] {
    new Customer {
        Invoices = new[] {
            new Invoice {Id=1},
            new Invoice {Id=2},
        }
    },
    new Customer {
        Invoices = new[] {
            new Invoice {Id=3},
            new Invoice {Id=4},
        }
    },
    new Customer {
        Invoices = new[] {
            new Invoice {Id=5},
            new Invoice {Id=6},
        }
    }
};

var allInvoicesFromAllCustomers = customers.SelectMany(c => c.Invoices);

Console.WriteLine(
    string.Join(",", allInvoicesFromAllCustomers.Select(i => i.Id).ToArray()));

```

**Output:**

> 
1,2,3,4,5,6


[View Demo](https://dotnetfiddle.net/XKGtBr)

`Enumerable.SelectMany` can also be achieved with a syntax-based query using two consecutive `from` clauses:

```dotnet
var allInvoicesFromAllCustomers
    = from customer in customers
      from invoice in customer.Invoices
      select invoice;

```



## Where (filter)


This method returns an IEnumerable with all the elements that meets the lambda expression

**Example**

```dotnet
var personNames = new[] 
{
    "Foo", "Bar", "Fizz", "Buzz"
};

var namesStartingWithF = personNames.Where(p => p.StartsWith("F"));
Console.WriteLine(string.Join(",", namesStartingWithF));

```

**Output:**

> 
Foo,Fizz


[View Demo](https://dotnetfiddle.net/nTbZI0)



## Any


Returns `true` if the collection has any elements that meets the condition in the lambda expression:

```dotnet
var numbers = new[] {1,2,3,4,5};

var isNotEmpty = numbers.Any();
Console.WriteLine(isNotEmpty); //True

var anyNumberIsOne = numbers.Any(n => n == 1);
Console.WriteLine(anyNumberIsOne); //True

var anyNumberIsSix = numbers.Any(n => n == 6);
Console.WriteLine(anyNumberIsSix); //False    

var anyNumberIsOdd = numbers.Any(n => (n & 1) == 1);
Console.WriteLine(anyNumberIsOdd); //True

var anyNumberIsNegative = numbers.Any(n => n < 0);
Console.WriteLine(anyNumberIsNegative); //False

```



## GroupJoin


```dotnet
class Developer
{
    public int Id { get; set; }
    public string Name { get; set; }
}

class Project
{
    public int DeveloperId { get; set; }
    public string Name { get; set; }
}

var developers = new[] {
    new Developer {
        Id = 1,
        Name = "Foobuzz"
    },
    new Developer {
        Id = 2,
        Name = "Barfizz"
    }
};

var projects = new[] {
    new Project {
        DeveloperId = 1,
        Name = "Hello World 3D"
    },
    new Project {
        DeveloperId = 1,
        Name = "Super Fizzbuzz Maker"
    },
    new Project {
        DeveloperId = 2,
        Name = "Citizen Kane - The action game"
    },
    new Project {
        DeveloperId = 2,
        Name = "Pro Pong 2016"
    }
};

var grouped = developers.GroupJoin(
    inner: projects,
    outerKeySelector: dev => dev.Id,
    innerKeySelector: proj => proj.DeveloperId,
    resultSelector: 
        (dev, projs) => new {
            DeveloperName = dev.Name, 
            ProjectNames = projs.Select(p => p.Name).ToArray()});
    
foreach(var item in grouped)
{
    Console.WriteLine(
        "{0}'s projects: {1}", 
        item.DeveloperName,
        string.Join(", ", item.ProjectNames));
}

//Foobuzz's projects: Hello World 3D, Super Fizzbuzz Maker
//Barfizz's projects: Citizen Kane - The action game, Pro Pong 2016

```



## Except


```dotnet
var numbers = new[] { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
var evenNumbersBetweenSixAndFourteen = new[] { 6, 8, 10, 12 };

var result = numbers.Except(evenNumbersBetweenSixAndFourteen);

Console.WriteLine(string.Join(",", result));

//1, 2, 3, 4, 5, 7, 9

```



## Zip


```dotnet
var tens = new[] {10,20,30,40,50};
var units = new[] {1,2,3,4,5};

var sums = tens.Zip(units, (first, second) => first + second);

Console.WriteLine(string.Join(",", sums));

//11,22,33,44,55

```



## Aggregate (fold)


Generating a new object in each step:

```dotnet
var elements = new[] {1,2,3,4,5};

var commaSeparatedElements = elements.Aggregate(
    seed: "",
    func: (aggregate, element) => $"{aggregate}{element},");
    
Console.WriteLine(commaSeparatedElements);  //1,2,3,4,5,

```

Using the same object in all steps:

```dotnet
var commaSeparatedElements2 = elements.Aggregate(
    seed: new StringBuilder(),
    func: (seed, element) => seed.Append($"{element},"));
    
Console.WriteLine(commaSeparatedElements2.ToString());  //1,2,3,4,5,

```

Using a result selector:

```dotnet
var commaSeparatedElements3 = elements.Aggregate(
    seed: new StringBuilder(),
    func: (seed, element) => seed.Append($"{element},"),
    resultSelector: (seed) => seed.ToString());
Console.WriteLine(commaSeparatedElements3);  //1,2,3,4,5,

```

If a seed is omitted, the first element becomes the seed:

```dotnet
var seedAndElements = elements.Select(n=>n.ToString());
var commaSeparatedElements4 = seedAndElements.Aggregate(
    func: (aggregate, element) => $"{aggregate}{element},");

Console.WriteLine(commaSeparatedElements4);  //12,3,4,5,

```



## ToLookup


```dotnet
var persons = new[] {
    new { Name="Fizz", Job="Developer"},
    new { Name="Buzz", Job="Developer"},
    new { Name="Foo", Job="Astronaut"},
    new { Name="Bar", Job="Astronaut"},
};

var groupedByJob = persons.ToLookup(p => p.Job);

foreach(var theGroup in groupedByJob)
{
    Console.WriteLine(
        "{0} are {1}s", 
        string.Join(",", theGroup.Select(g => g.Name).ToArray()),
        theGroup.Key);
}

//Fizz,Buzz are Developers
//Foo,Bar are Astronauts

```



## Intersect


```dotnet
var numbers1to10 = new[] {1,2,3,4,5,6,7,8,9,10};
var numbers5to15 = new[] {5,6,7,8,9,10,11,12,13,14,15};

var numbers5to10 = numbers1to10.Intersect(numbers5to15);

Console.WriteLine(string.Join(",", numbers5to10));

//5,6,7,8,9,10

```



## Concat


```dotnet
var numbers1to5 = new[] {1, 2, 3, 4, 5};
var numbers4to8 = new[] {4, 5, 6, 7, 8};

var numbers1to8 = numbers1to5.Concat(numbers4to8);

Console.WriteLine(string.Join(",", numbers1to8));

//1,2,3,4,5,4,5,6,7,8

```

Note that duplicates are kept in the result. If this is undesirable, use `Union` instead.



## All


```dotnet
var numbers = new[] {1,2,3,4,5};

var allNumbersAreOdd = numbers.All(n => (n & 1) == 1);
Console.WriteLine(allNumbersAreOdd); //False

var allNumbersArePositive = numbers.All(n => n > 0);
Console.WriteLine(allNumbersArePositive); //True

```

Note that the `All` method functions by checking for the first element to evaluate as `false` according to the predicate. Therefore, the method will return `true` for **any** predicate in the case that the set is empty:

```dotnet
var numbers = new int[0];
var allNumbersArePositive = numbers.All(n => n > 0);
Console.WriteLine(allNumbersArePositive); //True

```



## Sum


```dotnet
var numbers = new[] {1,2,3,4};

var sumOfAllNumbers = numbers.Sum();
Console.WriteLine(sumOfAllNumbers); //10

var cities = new[] {
    new {Population = 1000},
    new {Population = 2500},
    new {Population = 4000}
};

var totalPopulation = cities.Sum(c => c.Population);
Console.WriteLine(totalPopulation); //7500

```



## SequenceEqual


```dotnet
var numbers = new[] {1,2,3,4,5};
var sameNumbers = new[] {1,2,3,4,5};
var sameNumbersInDifferentOrder = new[] {5,1,4,2,3};

var equalIfSameOrder = numbers.SequenceEqual(sameNumbers);
Console.WriteLine(equalIfSameOrder); //True

var equalIfDifferentOrder = numbers.SequenceEqual(sameNumbersInDifferentOrder);
Console.WriteLine(equalIfDifferentOrder); //False

```



## Min


```dotnet
var numbers = new[] {1,2,3,4};

var minNumber = numbers.Min();
Console.WriteLine(minNumber); //1

var cities = new[] {
    new {Population = 1000},
    new {Population = 2500},
    new {Population = 4000}
};

var minPopulation = cities.Min(c => c.Population);
Console.WriteLine(minPopulation); //1000

```



## Distinct


```dotnet
var numbers = new[] {1, 1, 2, 2, 3, 3, 4, 4, 5, 5};
var distinctNumbers = numbers.Distinct();

Console.WriteLine(string.Join(",", distinctNumbers));

//1,2,3,4,5

```



## Count


```dotnet
IEnumerable<int> numbers = new[] {1,2,3,4,5,6,7,8,9,10};

var numbersCount = numbers.Count();
Console.WriteLine(numbersCount); //10

var evenNumbersCount = numbers.Count(n => (n & 1) == 0);
Console.WriteLine(evenNumbersCount); //5

```



## Cast


`Cast` is different from the other methods of `Enumerable` in that it is an extension method for `IEnumerable`, not for `IEnumerable<T>`. Thus it can be used to convert instances of the former into instances of the later.

This does not compile since `ArrayList` does not implement `IEnumerable<T>`:

```dotnet
var numbers = new ArrayList() {1,2,3,4,5};
Console.WriteLine(numbers.First());

```

This works as expected:

```dotnet
var numbers = new ArrayList() {1,2,3,4,5};
Console.WriteLine(numbers.Cast<int>().First()); //1

```

`Cast` does **not** perform conversion casts. The following compiles but throws `InvalidCastException` at runtime:

```dotnet
var numbers = new int[] {1,2,3,4,5};
decimal[] numbersAsDecimal = numbers.Cast<decimal>().ToArray();

```

The proper way to perform a converting cast to a collection is as follows:

```dotnet
var numbers= new int[] {1,2,3,4,5};
decimal[] numbersAsDecimal = numbers.Select(n => (decimal)n).ToArray();

```



## Select (map)


```dotnet
var persons = new[] 
{
    new {Id = 1, Name = "Foo"},
    new {Id = 2, Name = "Bar"},
    new {Id = 3, Name = "Fizz"},
    new {Id = 4, Name = "Buzz"}
};

var names = persons.Select(p => p.Name);
Console.WriteLine(string.Join(",", names.ToArray()));

//Foo,Bar,Fizz,Buzz

```

This type of function is usually called `map` in functional programming languages.



## OrderBy


```dotnet
var persons = new[] 
{
    new {Id = 1, Name = "Foo"},
    new {Id = 2, Name = "Bar"},
    new {Id = 3, Name = "Fizz"},
    new {Id = 4, Name = "Buzz"}
};

var personsSortedByName = persons.OrderBy(p => p.Name);

Console.WriteLine(string.Join(",", personsSortedByName.Select(p => p.Id).ToArray()));

//2,4,3,1

```



## OrderByDescending


```dotnet
var persons = new[] 
{
    new {Id = 1, Name = "Foo"},
    new {Id = 2, Name = "Bar"},
    new {Id = 3, Name = "Fizz"},
    new {Id = 4, Name = "Buzz"}
};

var personsSortedByNameDescending = persons.OrderByDescending(p => p.Name);

Console.WriteLine(string.Join(",", personsSortedByNameDescending.Select(p => p.Id).ToArray()));

//1,3,4,2

```



## Contains


```dotnet
var numbers = new[] {1,2,3,4,5};
Console.WriteLine(numbers.Contains(3)); //True
Console.WriteLine(numbers.Contains(34)); //False

```



## First (find)


```dotnet
var numbers = new[] {1,2,3,4,5};

var firstNumber = numbers.First();
Console.WriteLine(firstNumber); //1

var firstEvenNumber = numbers.First(n => (n & 1) == 0);
Console.WriteLine(firstEvenNumber); //2

```

The following throws `InvalidOperationException` with message "Sequence contains no matching element":

```dotnet
var firstNegativeNumber = numbers.First(n => n < 0);

```



## Single


```dotnet
var oneNumber = new[] {5};
var theOnlyNumber = oneNumber.Single();
Console.WriteLine(theOnlyNumber);  //5

var numbers = new[] {1,2,3,4,5};

var theOnlyNumberSmallerThanTwo = numbers.Single(n => n < 2);
Console.WriteLine(theOnlyNumberSmallerThanTwo);  //1

```

The following throws `InvalidOperationException` since there is more than one element in the sequence:

```dotnet
var theOnlyNumberInNumbers = numbers.Single();
var theOnlyNegativeNumber = numbers.Single(n => n < 0);

```



## Last


```dotnet
var numbers = new[] {1,2,3,4,5};

var lastNumber = numbers.Last();
Console.WriteLine(lastNumber); //5

var lastEvenNumber = numbers.Last(n => (n & 1) == 0);
Console.WriteLine(lastEvenNumber); //4

```

The following throws `InvalidOperationException`:

```dotnet
var lastNegativeNumber = numbers.Last(n => n < 0);

```



## LastOrDefault


```dotnet
var numbers = new[] {1,2,3,4,5};

var lastNumber = numbers.LastOrDefault();
Console.WriteLine(lastNumber); //5

var lastEvenNumber = numbers.LastOrDefault(n => (n & 1) == 0);
Console.WriteLine(lastEvenNumber); //4

var lastNegativeNumber = numbers.LastOrDefault(n => n < 0);
Console.WriteLine(lastNegativeNumber); //0

var words = new[] { "one", "two", "three", "four", "five" };

var lastWord = words.LastOrDefault();
Console.WriteLine(lastWord); // five

var lastLongWord = words.LastOrDefault(w => w.Length > 4);
Console.WriteLine(lastLongWord); // three

var lastMissingWord = words.LastOrDefault(w => w.Length > 5);
Console.WriteLine(lastMissingWord); // null

```



## SingleOrDefault


```dotnet
var oneNumber = new[] {5};
var theOnlyNumber = oneNumber.SingleOrDefault();
Console.WriteLine(theOnlyNumber);  //5

var numbers = new[] {1,2,3,4,5};

var theOnlyNumberSmallerThanTwo = numbers.SingleOrDefault(n => n < 2);
Console.WriteLine(theOnlyNumberSmallerThanTwo);  //1

var theOnlyNegativeNumber = numbers.SingleOrDefault(n => n < 0);
Console.WriteLine(theOnlyNegativeNumber);  //0

```

The following throws `InvalidOperationException`:

```dotnet
var theOnlyNumberInNumbers = numbers.SingleOrDefault();

```



## FirstOrDefault


```dotnet
var numbers = new[] {1,2,3,4,5};

var firstNumber = numbers.FirstOrDefault();
Console.WriteLine(firstNumber); //1

var firstEvenNumber = numbers.FirstOrDefault(n => (n & 1) == 0);
Console.WriteLine(firstEvenNumber); //2

var firstNegativeNumber = numbers.FirstOrDefault(n => n < 0);
Console.WriteLine(firstNegativeNumber); //0

var words = new[] { "one", "two", "three", "four", "five" };

var firstWord = words.FirstOrDefault();
Console.WriteLine(firstWord); // one

var firstLongWord = words.FirstOrDefault(w => w.Length > 3);
Console.WriteLine(firstLongWord); // three

var firstMissingWord = words.FirstOrDefault(w => w.Length > 5);
Console.WriteLine(firstMissingWord); // null

```



## Skip


Skip will enumerate the first N items without returning them.
Once item number N+1 is reached, Skip starts returning every enumerated item:

```dotnet
var numbers = new[] {1,2,3,4,5};

var allNumbersExceptFirstTwo = numbers.Skip(2);
Console.WriteLine(string.Join(",", allNumbersExceptFirstTwo.ToArray()));

//3,4,5

```



## Take


This method takes the first `n` elements from an enumerable.

```dotnet
var numbers = new[] {1,2,3,4,5};

var threeFirstNumbers = numbers.Take(3);
Console.WriteLine(string.Join(",", threeFirstNumbers.ToArray()));

//1,2,3

```



## Reverse


```dotnet
var numbers = new[] {1,2,3,4,5};
var reversed = numbers.Reverse();

Console.WriteLine(string.Join(",", reversed.ToArray()));

//5,4,3,2,1

```



## OfType


```dotnet
var mixed = new object[] {1,"Foo",2,"Bar",3,"Fizz",4,"Buzz"};
var numbers = mixed.OfType<int>();

Console.WriteLine(string.Join(",", numbers.ToArray()));

//1,2,3,4

```



## Max


```dotnet
var numbers = new[] {1,2,3,4};

var maxNumber = numbers.Max();
Console.WriteLine(maxNumber); //4

var cities = new[] {
    new {Population = 1000},
    new {Population = 2500},
    new {Population = 4000}
};

var maxPopulation = cities.Max(c => c.Population);
Console.WriteLine(maxPopulation); //4000

```



## Average


```dotnet
var numbers = new[] {1,2,3,4};

var averageNumber = numbers.Average();
Console.WriteLine(averageNumber); 
// 2,5

```

This method calculates the average of enumerable of numbers.

```dotnet
var cities = new[] {
    new {Population = 1000},
    new {Population = 2000},
    new {Population = 4000}
};

var averagePopulation = cities.Average(c => c.Population);
Console.WriteLine(averagePopulation);
// 2333,33

```

This method calculates the average of enumerable using delegated function.



## GroupBy


```dotnet
var persons = new[] {
    new { Name="Fizz", Job="Developer"},
    new { Name="Buzz", Job="Developer"},
    new { Name="Foo", Job="Astronaut"},
    new { Name="Bar", Job="Astronaut"},
};

var groupedByJob = persons.GroupBy(p => p.Job);

foreach(var theGroup in groupedByJob)
{
    Console.WriteLine(
        "{0} are {1}s", 
        string.Join(",", theGroup.Select(g => g.Name).ToArray()),
        theGroup.Key);
}

//Fizz,Buzz are Developers
//Foo,Bar are Astronauts

```

Group invoices by country, generating a new object with the number of record, total paid, and average paid

```dotnet
var a = db.Invoices.GroupBy(i => i.Country)
          .Select(g => new { Country = g.Key,
                             Count = g.Count(),
                             Total = g.Sum(i => i.Paid),
                             Average = g.Average(i => i.Paid) });

```

If we want only the totals, no group

```dotnet
var a = db.Invoices.GroupBy(i => 1)
          .Select(g => new { Count = g.Count(),
                             Total = g.Sum(i => i.Paid),
                             Average = g.Average(i => i.Paid) });

```

If we need several counts

```dotnet
var a = db.Invoices.GroupBy(g => 1)
          .Select(g => new { High = g.Count(i => i.Paid >= 1000),
                             Low = g.Count(i => i.Paid < 1000),
                             Sum = g.Sum(i => i.Paid) });

```



## ToDictionary


Returns a new dictionary from the source `IEnumerable` using the provided keySelector function to determine keys. Will throw an `ArgumentException` if keySelector is not injective(returns a unique value for each member of the source collection.) There are overloads which allow one to specify the value to be stored as well as the key.

```dotnet
var persons = new[] {
    new { Name="Fizz", Id=1},
    new { Name="Buzz", Id=2},
    new { Name="Foo", Id=3},
    new { Name="Bar", Id=4},
};

```

Specifying just a key selector function will create a `Dictionary<TKey,TVal>` with `TKey` the return Type of the key selector, `TVal` the original object Type, and the original object as the stored value.

```dotnet
var personsById = persons.ToDictionary(p => p.Id);
// personsById is a Dictionary<int,object>

Console.WriteLine(personsById[1].Name); //Fizz
Console.WriteLine(personsById[2].Name); //Buzz

```

Specifying a value selector function as well will create a `Dictionary<TKey,TVal>` with `TKey` still the return type of the key selector, but `TVal` now the return type of the value selector function, and the returned value as the stored value.

```dotnet
var namesById = persons.ToDictionary(p => p.Id, p => p.Name);
//namesById is a Dictionary<int,string>

Console.WriteLine(namesById[3]); //Foo
Console.WriteLine(namesById[4]); //Bar

```

As stated above, the keys returned by the key selector must be unique. The following will throw an exception.

```dotnet
var persons = new[] {
    new { Name="Fizz", Id=1},
    new { Name="Buzz", Id=2},
    new { Name="Foo", Id=3},
    new { Name="Bar", Id=4},
    new { Name="Oops", Id=4}
};

var willThrowException = persons.ToDictionary(p => p.Id)

```

If a unique key can not be given for the source collection, consider using ToLookup instead. On the surface, ToLookup behaves similarly to ToDictionary, however, in the resulting Lookup each key is paired with a collection of values with matching keys.



## Union


```dotnet
var numbers1to5 = new[] {1,2,3,4,5};
var numbers4to8 = new[] {4,5,6,7,8};

var numbers1to8 = numbers1to5.Union(numbers4to8);

Console.WriteLine(string.Join(",", numbers1to8));

//1,2,3,4,5,6,7,8

```

Note that duplicates are removed from the result. If this is undesirable, use `Concat` instead.



## ToArray


```dotnet
var numbers = new[] {1,2,3,4,5,6,7,8,9,10};
var someNumbers = numbers.Where(n => n < 6);

Console.WriteLine(someNumbers.GetType().Name);
//WhereArrayIterator`1

var someNumbersArray = someNumbers.ToArray();

Console.WriteLine(someNumbersArray.GetType().Name);
//Int32[]

```



## ToList


```dotnet
var numbers = new[] {1,2,3,4,5,6,7,8,9,10};
var someNumbers = numbers.Where(n => n < 6);

Console.WriteLine(someNumbers.GetType().Name);
//WhereArrayIterator`1

var someNumbersList = someNumbers.ToList();

Console.WriteLine(
    someNumbersList.GetType().Name + " - " +
    someNumbersList.GetType().GetGenericArguments()[0].Name);
//List`1 - Int32

```



## ElementAt


```dotnet
var names = new[] {"Foo","Bar","Fizz","Buzz"};

var thirdName = names.ElementAt(2);
Console.WriteLine(thirdName); //Fizz

//The following throws ArgumentOutOfRangeException

var minusOnethName = names.ElementAt(-1);
var fifthName = names.ElementAt(4);

```



## ElementAtOrDefault


```dotnet
var names = new[] {"Foo","Bar","Fizz","Buzz"};

var thirdName = names.ElementAtOrDefault(2);
Console.WriteLine(thirdName); //Fizz

var minusOnethName = names.ElementAtOrDefault(-1);
Console.WriteLine(minusOnethName); //null

var fifthName = names.ElementAtOrDefault(4);
Console.WriteLine(fifthName); //null

```



## SkipWhile


```dotnet
var numbers = new[] {2,4,6,8,1,3,5,7};

var oddNumbers = numbers.SkipWhile(n => (n & 1) == 0);

Console.WriteLine(string.Join(",", oddNumbers.ToArray()));

//1,3,5,7

```



## TakeWhile


```dotnet
var numbers = new[] {2,4,6,1,3,5,7,8};

var evenNumbers = numbers.TakeWhile(n => (n & 1) == 0);

Console.WriteLine(string.Join(",", evenNumbers.ToArray()));

//2,4,6

```



## DefaultIfEmpty


```dotnet
var numbers = new[] {2,4,6,8,1,3,5,7};

var numbersOrDefault = numbers.DefaultIfEmpty();
Console.WriteLine(numbers.SequenceEqual(numbersOrDefault)); //True

var noNumbers = new int[0];

var noNumbersOrDefault = noNumbers.DefaultIfEmpty();
Console.WriteLine(noNumbersOrDefault.Count()); //1
Console.WriteLine(noNumbersOrDefault.Single()); //0

var noNumbersOrExplicitDefault = noNumbers.DefaultIfEmpty(34);
Console.WriteLine(noNumbersOrExplicitDefault.Count()); //1
Console.WriteLine(noNumbersOrExplicitDefault.Single()); //34

```



## Join


```dotnet
class Developer
{
    public int Id { get; set; }
    public string Name { get; set; }
}

class Project
{
    public int DeveloperId { get; set; }
    public string Name { get; set; }
}

var developers = new[] {
    new Developer {
        Id = 1,
        Name = "Foobuzz"
    },
    new Developer {
        Id = 2,
        Name = "Barfizz"
    }
};

var projects = new[] {
    new Project {
        DeveloperId = 1,
        Name = "Hello World 3D"
    },
    new Project {
        DeveloperId = 1,
        Name = "Super Fizzbuzz Maker"
    },
    new Project {
        DeveloperId = 2,
        Name = "Citizen Kane - The action game"
    },
    new Project {
        DeveloperId = 2,
        Name = "Pro Pong 2016"
    }
};

var denormalized = developers.Join(
    inner: projects,
    outerKeySelector: dev => dev.Id,
    innerKeySelector: proj => proj.DeveloperId,
    resultSelector: 
        (dev, proj) => new {
            ProjectName = proj.Name,
            DeveloperName = dev.Name});
    
foreach(var item in denormalized)
{
    Console.WriteLine("{0} by {1}", item.ProjectName, item.DeveloperName);
}

//Hello World 3D by Foobuzz
//Super Fizzbuzz Maker by Foobuzz
//Citizen Kane - The action game by Barfizz
//Pro Pong 2016 by Barfizz

```



## Empty


To create an empty IEnumerable of int:

```dotnet
IEnumerable<int> emptyList = Enumerable.Empty<int>(); 

```

This empty IEnumerable is cached for each Type T, so that:

```dotnet
Enumerable.Empty<decimal>() == Enumerable.Empty<decimal>(); // This is True
Enumerable.Empty<int>() == Enumerable.Empty<decimal>();     // This is False

```



## ThenBy


`ThenBy` can only be used after a `OrderBy` clause allowing to order using multiple criteria

```dotnet
var persons = new[] 
{
    new {Id = 1, Name = "Foo", Order = 1},
    new {Id = 1, Name = "FooTwo", Order = 2},
    new {Id = 2, Name = "Bar", Order = 2},
    new {Id = 2, Name = "BarTwo", Order = 1},
    new {Id = 3, Name = "Fizz", Order = 2},
    new {Id = 3, Name = "FizzTwo", Order = 1},  
};

var personsSortedByName = persons.OrderBy(p => p.Id).ThenBy(p => p.Order);

Console.WriteLine(string.Join(",", personsSortedByName.Select(p => p.Name)));
//This will display : 
//Foo,FooTwo,BarTwo,Bar,FizzTwo,Fizz

```



## Range


The two parameters to `Range` are the **first** number and the **count** of elements to produce (not the last number).

```dotnet
// prints 1,2,3,4,5,6,7,8,9,10
Console.WriteLine(string.Join(",", Enumerable.Range(1, 10)));

// prints 10,11,12,13,14
Console.WriteLine(string.Join(",", Enumerable.Range(10, 5)));

```



## Left Outer Join


```dotnet
class Person
{
    public string FirstName { get; set; }
    public string LastName { get; set; }
}

class Pet
{
    public string Name { get; set; }
    public Person Owner { get; set; }
}

public static void Main(string[] args)
{
    var magnus = new Person { FirstName = "Magnus", LastName = "Hedlund" };
    var terry = new Person { FirstName = "Terry", LastName = "Adams" };

    var barley = new Pet { Name = "Barley", Owner = terry };

    var people = new[] { magnus, terry };
    var pets = new[] { barley };

    var query =
        from person in people
        join pet in pets on person equals pet.Owner into gj
        from subpet in gj.DefaultIfEmpty()
        select new
        {
            person.FirstName,
            PetName = subpet?.Name ?? "-" // Use - if he has no pet
        };

    foreach (var p in query)
        Console.WriteLine($"{p.FirstName}: {p.PetName}");
}

```



## Repeat


`Enumerable.Repeat` generates a sequence of a repeated value. In this example it generates "Hello" 4 times.

```dotnet
var repeats = Enumerable.Repeat("Hello", 4);
   
foreach (var item in repeats)
{
    Console.WriteLine(item);
}

/* output:
    Hello
    Hello
    Hello
    Hello
*/

```



#### Syntax


- public static TSource Aggregate<TSource>(this IEnumerable<TSource> source, Func<TSource, TSource, TSource> func)
- public static TAccumulate Aggregate<TSource, TAccumulate>(this IEnumerable<TSource> source, TAccumulate seed, Func<TAccumulate, TSource, TAccumulate> func)
- public static TResult Aggregate<TSource, TAccumulate, TResult>(this IEnumerable<TSource> source, TAccumulate seed, Func<TAccumulate, TSource, TAccumulate> func, Func<TAccumulate, TResult> resultSelector)
- public static Boolean All<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static Boolean Any<TSource>(this IEnumerable<TSource> source)
- public static Boolean Any<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static IEnumerable<TSource> AsEnumerable<TSource>(this IEnumerable<TSource> source)
- public static Decimal Average(this IEnumerable<Decimal> source)
- public static Double Average(this IEnumerable<Double> source)
- public static Double Average(this IEnumerable<Int32> source)
- public static Double Average(this IEnumerable<Int64> source)
- public static Nullable<Decimal> Average(this IEnumerable<Nullable<Decimal>> source)
- public static Nullable<Double> Average(this IEnumerable<Nullable<Double>> source)
- public static Nullable<Double> Average(this IEnumerable<Nullable<Int32>> source)
- public static Nullable<Double> Average(this IEnumerable<Nullable<Int64>> source)
- public static Nullable<Single> Average(this IEnumerable<Nullable<Single>> source)
- public static Single Average(this IEnumerable<Single> source)
- public static Decimal Average<TSource>(this IEnumerable<TSource> source, Func<TSource, Decimal> selector)
- public static Double Average<TSource>(this IEnumerable<TSource> source, Func<TSource, Double> selector)
- public static Double Average<TSource>(this IEnumerable<TSource> source, Func<TSource, Int32> selector)
- public static Double Average<TSource>(this IEnumerable<TSource> source, Func<TSource, Int64> selector)
- public static Nullable<Decimal> Average<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Decimal>> selector)
- public static Nullable<Double> Average<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Double>> selector)
- public static Nullable<Double> Average<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Int32>> selector)
- public static Nullable<Double> Average<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Int64>> selector)
- public static Nullable<Single> Average<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Single>> selector)
- public static Single Average<TSource>(this IEnumerable<TSource> source, Func<TSource, Single> selector)
- public static IEnumerable<TResult> Cast<TResult>(this IEnumerable source)
- public static IEnumerable<TSource> Concat<TSource>(this IEnumerable<TSource> first, IEnumerable<TSource> second)
- public static Boolean Contains<TSource>(this IEnumerable<TSource> source, TSource value)
- public static Boolean Contains<TSource>(this IEnumerable<TSource> source, TSource value, IEqualityComparer<TSource> comparer)
- public static Int32 Count<TSource>(this IEnumerable<TSource> source)
- public static Int32 Count<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static IEnumerable<TSource> DefaultIfEmpty<TSource>(this IEnumerable<TSource> source)
- public static IEnumerable<TSource> DefaultIfEmpty<TSource>(this IEnumerable<TSource> source, TSource defaultValue)
- public static IEnumerable<TSource> Distinct<TSource>(this IEnumerable<TSource> source)
- public static IEnumerable<TSource> Distinct<TSource>(this IEnumerable<TSource> source, IEqualityComparer<TSource> comparer)
- public static TSource ElementAt<TSource>(this IEnumerable<TSource> source, Int32 index)
- public static TSource ElementAtOrDefault<TSource>(this IEnumerable<TSource> source, Int32 index)
- public static IEnumerable<TResult> Empty<TResult>()
- public static IEnumerable<TSource> Except<TSource>(this IEnumerable<TSource> first, IEnumerable<TSource> second)
- public static IEnumerable<TSource> Except<TSource>(this IEnumerable<TSource> first, IEnumerable<TSource> second, IEqualityComparer<TSource> comparer)
- public static TSource First<TSource>(this IEnumerable<TSource> source)
- public static TSource First<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static TSource FirstOrDefault<TSource>(this IEnumerable<TSource> source)
- public static TSource FirstOrDefault<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static IEnumerable<IGrouping<TKey, TSource>> GroupBy<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector)
- public static IEnumerable<IGrouping<TKey, TSource>> GroupBy<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, IEqualityComparer<TKey> comparer)
- public static IEnumerable<IGrouping<TKey, TElement>> GroupBy<TSource, TKey, TElement>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, Func<TSource, TElement> elementSelector)
- public static IEnumerable<IGrouping<TKey, TElement>> GroupBy<TSource, TKey, TElement>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, Func<TSource, TElement> elementSelector, IEqualityComparer<TKey> comparer)
- public static IEnumerable<TResult> GroupBy<TSource, TKey, TResult>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, Func<TKey, IEnumerable<TSource>, TResult> resultSelector)
- public static IEnumerable<TResult> GroupBy<TSource, TKey, TResult>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, Func<TKey, IEnumerable<TSource>, TResult> resultSelector, IEqualityComparer<TKey> comparer)
- public static IEnumerable<TResult> GroupBy<TSource, TKey, TElement, TResult>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, Func<TSource, TElement> elementSelector, Func<TKey, IEnumerable<TElement>, TResult> resultSelector)
- public static IEnumerable<TResult> GroupBy<TSource, TKey, TElement, TResult>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, Func<TSource, TElement> elementSelector, Func<TKey, IEnumerable<TElement>, TResult> resultSelector, IEqualityComparer<TKey> comparer)
- public static IEnumerable<TResult> GroupJoin<TOuter, TInner, TKey, TResult>(this IEnumerable<TOuter> outer,IEnumerable<TInner> inner, Func<TOuter, TKey> outerKeySelector, Func<TInner, TKey> innerKeySelector, Func<TOuter, IEnumerable<TInner>, TResult> resultSelector)
- public static IEnumerable<TResult> GroupJoin<TOuter, TInner, TKey, TResult>(this IEnumerable<TOuter> outer, IEnumerable<TInner> inner, Func<TOuter, TKey> outerKeySelector, Func<TInner, TKey> innerKeySelector, Func<TOuter, IEnumerable<TInner>, TResult> resultSelector, IEqualityComparer<TKey> comparer)
- public static IEnumerable<TSource> Intersect<TSource>(this IEnumerable<TSource> first, IEnumerable<TSource> second)
- public static IEnumerable<TSource> Intersect<TSource>(this IEnumerable<TSource> first, IEnumerable<TSource> second, IEqualityComparer<TSource> comparer)
- public static IEnumerable<TResult> Join<TOuter, TInner, TKey, TResult>(this IEnumerable<TOuter> outer, IEnumerable<TInner> inner, Func<TOuter, TKey> outerKeySelector, Func<TInner, TKey> innerKeySelector, Func<TOuter, TInner, TResult> resultSelector)
- public static IEnumerable<TResult> Join<TOuter, TInner, TKey, TResult>(this IEnumerable<TOuter> outer, IEnumerable<TInner> inner, Func<TOuter, TKey> outerKeySelector, Func<TInner, TKey> innerKeySelector, Func<TOuter, TInner, TResult> resultSelector, IEqualityComparer<TKey> comparer)
- public static TSource Last<TSource>(this IEnumerable<TSource> source)
- public static TSource Last<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static TSource LastOrDefault<TSource>(this IEnumerable<TSource> source)
- public static TSource LastOrDefault<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static Int64 LongCount<TSource>(this IEnumerable<TSource> source)
- public static Int64 LongCount<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static Decimal Max(this IEnumerable<Decimal> source)
- public static Double Max(this IEnumerable<Double> source)
- public static Int32 Max(this IEnumerable<Int32> source)
- public static Int64 Max(this IEnumerable<Int64> source)
- public static Nullable<Decimal> Max(this IEnumerable<Nullable<Decimal>> source)
- public static Nullable<Double> Max(this IEnumerable<Nullable<Double>> source)
- public static Nullable<Int32> Max(this IEnumerable<Nullable<Int32>> source)
- public static Nullable<Int64> Max(this IEnumerable<Nullable<Int64>> source)
- public static Nullable<Single> Max(this IEnumerable<Nullable<Single>> source)
- public static Single Max(this IEnumerable<Single> source)
- public static TSource Max<TSource>(this IEnumerable<TSource> source)
- public static Decimal Max<TSource>(this IEnumerable<TSource> source, Func<TSource, Decimal> selector)
- public static Double Max<TSource>(this IEnumerable<TSource> source, Func<TSource, Double> selector)
- public static Int32 Max<TSource>(this IEnumerable<TSource> source, Func<TSource, Int32> selector)
- public static Int64 Max<TSource>(this IEnumerable<TSource> source, Func<TSource, Int64> selector)
- public static Nullable<Decimal> Max<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Decimal>> selector)
- public static Nullable<Double> Max<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Double>> selector)
- public static Nullable<Int32> Max<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Int32>> selector)
- public static Nullable<Int64> Max<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Int64>> selector)
- public static Nullable<Single> Max<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Single>> selector)
- public static Single Max<TSource>(this IEnumerable<TSource> source, Func<TSource, Single> selector)
- public static TResult Max<TSource, TResult>(this IEnumerable<TSource> source, Func<TSource, TResult> selector)
- public static Decimal Min(this IEnumerable<Decimal> source)
- public static Double Min(this IEnumerable<Double> source)
- public static Int32 Min(this IEnumerable<Int32> source)
- public static Int64 Min(this IEnumerable<Int64> source)
- public static Nullable<Decimal> Min(this IEnumerable<Nullable<Decimal>> source)
- public static Nullable<Double> Min(this IEnumerable<Nullable<Double>> source)
- public static Nullable<Int32> Min(this IEnumerable<Nullable<Int32>> source)
- public static Nullable<Int64> Min(this IEnumerable<Nullable<Int64>> source)
- public static Nullable<Single> Min(this IEnumerable<Nullable<Single>> source)
- public static Single Min(this IEnumerable<Single> source)
- public static TSource Min<TSource>(this IEnumerable<TSource> source)
- public static Decimal Min<TSource>(this IEnumerable<TSource> source, Func<TSource, Decimal> selector)
- public static Double Min<TSource>(this IEnumerable<TSource> source, Func<TSource, Double> selector)
- public static Int32 Min<TSource>(this IEnumerable<TSource> source, Func<TSource, Int32> selector)
- public static Int64 Min<TSource>(this IEnumerable<TSource> source, Func<TSource, Int64> selector)
- public static Nullable<Decimal> Min<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Decimal>> selector)
- public static Nullable<Double> Min<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Double>> selector)
- public static Nullable<Int32> Min<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Int32>> selector)
- public static Nullable<Int64> Min<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Int64>> selector)
- public static Nullable<Single> Min<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Single>> selector)
- public static Single Min<TSource>(this IEnumerable<TSource> source, Func<TSource, Single> selector)
- public static TResult Min<TSource, TResult>(this IEnumerable<TSource> source, Func<TSource, TResult> selector)
- public static IEnumerable<TResult> OfType<TResult>(this IEnumerable source)
- public static IOrderedEnumerable<TSource> OrderBy<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector)
- public static IOrderedEnumerable<TSource> OrderBy<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, IComparer<TKey> comparer)
- public static IOrderedEnumerable<TSource> OrderByDescending<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector)
- public static IOrderedEnumerable<TSource> OrderByDescending<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, IComparer<TKey> comparer)
- public static IEnumerable<Int32> Range(Int32 start, Int32 count)
- public static IEnumerable<TResult> Repeat<TResult>(TResult element, Int32 count)
- public static IEnumerable<TSource> Reverse<TSource>(this IEnumerable<TSource> source)
- public static IEnumerable<TResult> Select<TSource, TResult>(this IEnumerable<TSource> source, Func<TSource, TResult> selector)
- public static IEnumerable<TResult> Select<TSource, TResult>(this IEnumerable<TSource> source, Func<TSource, Int32, TResult> selector)
- public static IEnumerable<TResult> SelectMany<TSource, TResult>(this IEnumerable<TSource> source, Func<TSource, IEnumerable<TResult>> selector)
- public static IEnumerable<TResult> SelectMany<TSource, TResult>(this IEnumerable<TSource> source, Func<TSource, Int32, IEnumerable<TResult>> selector)
- public static IEnumerable<TResult> SelectMany<TSource, TCollection, TResult>(this IEnumerable<TSource> source, Func<TSource, IEnumerable<TCollection>> collectionSelector, Func<TSource, TCollection, TResult> resultSelector)
- public static IEnumerable<TResult> SelectMany<TSource, TCollection, TResult>(this IEnumerable<TSource> source, Func<TSource, Int32, IEnumerable<TCollection>> collectionSelector, Func<TSource, TCollection, TResult> resultSelector)
- public static Boolean SequenceEqual<TSource>(this IEnumerable<TSource> first, IEnumerable<TSource> second)
- public static Boolean SequenceEqual<TSource>(this IEnumerable<TSource> first, IEnumerable<TSource> second, IEqualityComparer<TSource> comparer)
- public static TSource Single<TSource>(this IEnumerable<TSource> source)
- public static TSource Single<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static TSource SingleOrDefault<TSource>(this IEnumerable<TSource> source)
- public static TSource SingleOrDefault<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static IEnumerable<TSource> Skip<TSource>(this IEnumerable<TSource> source, Int32 count)
- public static IEnumerable<TSource> SkipWhile<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static IEnumerable<TSource> SkipWhile<TSource>(this IEnumerable<TSource> source, Func<TSource, Int32, Boolean> predicate)
- public static Decimal Sum(this IEnumerable<Decimal> source)
- public static Double Sum(this IEnumerable<Double> source)
- public static Int32 Sum(this IEnumerable<Int32> source)
- public static Int64 Sum(this IEnumerable<Int64> source)
- public static Nullable<Decimal> Sum(this IEnumerable<Nullable<Decimal>> source)
- public static Nullable<Double> Sum(this IEnumerable<Nullable<Double>> source)
- public static Nullable<Int32> Sum(this IEnumerable<Nullable<Int32>> source)
- public static Nullable<Int64> Sum(this IEnumerable<Nullable<Int64>> source)
- public static Nullable<Single> Sum(this IEnumerable<Nullable<Single>> source)
- public static Single Sum(this IEnumerable<Single> source)
- public static Decimal Sum<TSource>(this IEnumerable<TSource> source, Func<TSource, Decimal> selector)
- public static Double Sum<TSource>(this IEnumerable<TSource> source, Func<TSource, Double> selector)
- public static Int32 Sum<TSource>(this IEnumerable<TSource> source, Func<TSource, Int32> selector)
- public static Int64 Sum<TSource>(this IEnumerable<TSource> source, Func<TSource, Int64> selector)
- public static Nullable<Decimal> Sum<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Decimal>> selector)
- public static Nullable<Double> Sum<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Double>> selector)
- public static Nullable<Int32> Sum<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Int32>> selector)
- public static Nullable<Int64> Sum<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Int64>> selector)
- public static Nullable<Single> Sum<TSource>(this IEnumerable<TSource> source, Func<TSource, Nullable<Single>> selector)
- public static Single Sum<TSource>(this IEnumerable<TSource> source, Func<TSource, Single> selector)
- public static IEnumerable<TSource> Take<TSource>(this IEnumerable<TSource> source, Int32 count)
- public static IEnumerable<TSource> TakeWhile<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static IEnumerable<TSource> TakeWhile<TSource>(this IEnumerable<TSource> source, Func<TSource, Int32, Boolean> predicate)
- public static IOrderedEnumerable<TSource> ThenBy<TSource, TKey>(this IOrderedEnumerable<TSource> source, Func<TSource, TKey> keySelector)
- public static IOrderedEnumerable<TSource> ThenBy<TSource, TKey>(this IOrderedEnumerable<TSource> source, Func<TSource, TKey> keySelector, IComparer<TKey> comparer)
- public static IOrderedEnumerable<TSource> ThenByDescending<TSource, TKey>(this IOrderedEnumerable<TSource> source, Func<TSource, TKey> keySelector)
- public static IOrderedEnumerable<TSource> ThenByDescending<TSource, TKey>(this IOrderedEnumerable<TSource> source, Func<TSource, TKey> keySelector, IComparer<TKey> comparer)
- public static TSource[] ToArray<TSource>(this IEnumerable<TSource> source)
- public static Dictionary<TKey, TSource> ToDictionary<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector)
- public static Dictionary<TKey, TSource> ToDictionary<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, IEqualityComparer<TKey> comparer)
- public static Dictionary<TKey, TElement> ToDictionary<TSource, TKey, TElement>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, Func<TSource, TElement> elementSelector)
- public static Dictionary<TKey, TElement> ToDictionary<TSource, TKey, TElement>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, Func<TSource, TElement> elementSelector, IEqualityComparer<TKey> comparer)
- public static List<TSource> ToList<TSource>(this IEnumerable<TSource> source)
- public static ILookup<TKey, TSource> ToLookup<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector)
- public static ILookup<TKey, TSource> ToLookup<TSource, TKey>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, IEqualityComparer<TKey> comparer)
- public static ILookup<TKey, TElement> ToLookup<TSource, TKey, TElement>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, Func<TSource, TElement> elementSelector)
- public static ILookup<TKey, TElement> ToLookup<TSource, TKey, TElement>(this IEnumerable<TSource> source, Func<TSource, TKey> keySelector, Func<TSource, TElement> elementSelector, IEqualityComparer<TKey> comparer)
- public static IEnumerable<TSource> Union<TSource>(this IEnumerable<TSource> first, IEnumerable<TSource> second)
- public static IEnumerable<TSource> Union<TSource>(this IEnumerable<TSource> first, IEnumerable<TSource> second, IEqualityComparer<TSource> comparer)
- public static IEnumerable<TSource> Where<TSource>(this IEnumerable<TSource> source, Func<TSource, Boolean> predicate)
- public static IEnumerable<TSource> Where<TSource>(this IEnumerable<TSource> source, Func<TSource, Int32, Boolean> predicate)
- public static IEnumerable<TResult> Zip<TFirst, TSecond, TResult>(this IEnumerable<TFirst> first, IEnumerable<TSecond> second, Func<TFirst, TSecond, TResult> resultSelector)



#### Remarks


- See also [LINQ](http://stackoverflow.com/documentation/linq/842/introduction-to-linq#t=201607230635481225221).

The LINQ built-in methods are extension methods for the `IEnumerable<T>` interface that live in the `System.Linq.Enumerable` class in the `System.Core` assembly. They are available in .NET Framework 3.5 and later.

LINQ allows for simple modification, transformation, and combination of various `IEnumerable`s using a query-like or functional syntax.

While the standard LINQ methods can work on any `IEnumerable<T>`, including the simple arrays and `List<T>`s, they can also be used on database objects, where the set of LINQ expressions can be transformed in many cases to SQL if the data object supports it. See [LINQ to SQL](https://msdn.microsoft.com/en-us/library/bb425822.aspx).

For the methods that compare objects (such as `Contains` and `Except`), `IEquatable<T>.Equals` is used if the type T of the collection implements that interface. Otherwise, the standard `Equals` and `GetHashCode` of the type (possibly overriden from the default `Object` implementations) are used. There are also overloads for these methods that allow to specify a custom `IEqualityComparer<T>`.

For the `...OrDefault` methods, `default(T)` is used to generate default values.

Official reference: [Enumerable class](https://msdn.microsoft.com/en-us/library/system.linq.enumerable(v=vs.110).aspx)

### Lazy Evaluation

Virtually every query that returns an `IEnumerable<T>` is not evaluated immediately; instead, the logic is delayed until the query is iterated over.  One implication is that each time someone iterates over an `IEnumerable<T>` created from one of these queries, e.g., `.Where()`, the full query logic is repeated.  If the predicate is long-running, this can be a cause for performance issues.

One simple solution (when you know or can control the approximate size of the resulting sequence) is to fully buffer the results using `.ToArray()` or `.ToList()`.  `.ToDictionary()` or `.ToLookup()` can fulfill the same role.  One can also, of course, iterate over the entire sequence and buffer the elements according to other custom logic.

### `ToArray()` or `ToList()`?

Both `.ToArray()` and `.ToList()` loop through all elements of an `IEnumerable<T>` sequence and save the results in a collection stored in-memory.  Use the following guidelines to determine which to choose:

- Some APIs may require a `T[]` or a `List<T>`.
- `.ToList()` typically runs faster and generates less garbage than `.ToArray()`, because the latter must copy all the elements into a new fixed-size collection one more time than the former, in almost every case.
- Elements can be added to or removed from the `List<T>` returned by `.ToList()`, whereas the `T[]` returned from `.ToArray()` remains a fixed size throughout its lifetime. In other words, `List<T>` is mutable, and `T[]` is immutable.
- The `T[]` returned from`.ToArray()` uses less memory than the `List<T>` returned from `.ToList()`, so if the result is going to be stored for a long time, prefer `.ToArray()`.  Calling `List<T>.TrimExcess()` would make the memory difference strictly academic, at the cost of eliminating the relative speed advantage of `.ToList()`.

