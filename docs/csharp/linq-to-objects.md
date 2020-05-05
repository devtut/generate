---
metaTitle: "C# | Linq to Objects"
description: "Using LINQ to Objects in C#, How LINQ to Object executes queries"
---

# Linq to Objects


LINQ to Objects refers to the use of LINQ queries with any IEnumerable collection.



## Using LINQ to Objects in C#


**A simple SELECT query in Linq**

```cs
static void Main(string[] args)
{
    string[] cars = { "VW Golf", 
                        "Opel Astra", 
                        "Audi A4", 
                        "Ford Focus", 
                        "Seat Leon", 
                        "VW Passat", 
                        "VW Polo", 
                        "Mercedes C-Class" };

    var list = from car in cars
               select car;

    StringBuilder sb = new StringBuilder();

    foreach (string entry in list)
    {
        sb.Append(entry + "\n");
    }

    Console.WriteLine(sb.ToString());
    Console.ReadLine();
}

```

In the example above, an array of strings (cars) is used as a collection of objects to be queried using LINQ. In a LINQ query, the from clause comes first in order to introduce the data source (cars) and the range variable (car). When the query is executed, the range variable will serve as a reference to each successive element in cars. Because the compiler can infer the type of car, you do not have to specify it explicitly

When the above code is compiled and executed, it produces the following result:
[<img src="https://i.stack.imgur.com/lG65Q.png" alt="enter image description here" />](https://i.stack.imgur.com/lG65Q.png)

**SELECT with a WHERE Clause**

```cs
var list = from car in cars
           where car.Contains("VW")
           select car;

```

The WHERE clause is used to query the string array (cars) to find and return a subset of array which satisfies the WHERE clause.

When the above code is compiled and executed, it produces the following result:

[<img src="https://i.stack.imgur.com/llGXx.png" alt="enter image description here" />](https://i.stack.imgur.com/llGXx.png)

**Generating an Ordered List**

```cs
var list = from car in cars
           orderby car ascending 
           select car;

```

Sometimes it is useful to sort the returned data. The orderby clause will cause the elements to be sorted according to the default comparer for the type being sorted.

When the above code is compiled and executed, it produces the following result:

[<img src="https://i.stack.imgur.com/ODH55.png" alt="enter image description here" />](https://i.stack.imgur.com/ODH55.png)

**Working with a custom type**

In this example, a typed list is created, populated, and then queried

```cs
public class Car
{
    public String Name { get; private set; }
    public int UnitsSold { get; private set; }

    public Car(string name, int unitsSold)
    {
        Name = name;
        UnitsSold = unitsSold;
    }
}

class Program
{
    static void Main(string[] args)
    {

        var car1 = new Car("VW Golf", 270952);
        var car2 = new Car("Opel Astra", 56079);
        var car3 = new Car("Audi A4", 52493);
        var car4 = new Car("Ford Focus", 51677);
        var car5 = new Car("Seat Leon", 42125);
        var car6 = new Car("VW Passat", 97586);
        var car7 = new Car("VW Polo", 69867);
        var car8 = new Car("Mercedes C-Class", 67549);

        var cars = new List<Car> { 
            car1, car2, car3, car4, car5, car6, car7, car8 };
        var list = from car in cars
                   select car.Name;

        foreach (var entry in list)
        {
            Console.WriteLine(entry);
        }
        Console.ReadLine();
    }
}

```

When the above code is compiled and executed, it produces the following result:

[<img src="https://i.stack.imgur.com/0jUOC.png" alt="enter image description here" />](https://i.stack.imgur.com/0jUOC.png)

Until now the examples don't seem amazing as one can just iterate through the array to do basically the same. However, with the few examples below you can see how to create more complex queries with LINQ to Objects and achieve more with a lot less of code.

In the example below we can select cars that have been sold over 60000 units and sort them over the number of units sold:

```cs
var list = from car in cars
           where car.UnitsSold > 60000 
           orderby car.UnitsSold descending 
           select car;

StringBuilder sb = new StringBuilder();

foreach (var entry in list)
{
    sb.AppendLine($"{entry.Name} - {entry.UnitsSold}");
}
Console.WriteLine(sb.ToString());

```

When the above code is compiled and executed, it produces the following result:
[<img src="https://i.stack.imgur.com/ZDeTt.png" alt="enter image description here" />](https://i.stack.imgur.com/ZDeTt.png)

In the example below we can select cars that have sold an odd number of units and order them alphabetically over its name:

```cs
var list = from car in cars
           where car.UnitsSold % 2 != 0 
           orderby car.Name ascending 
           select car;

```

When the above code is compiled and executed, it produces the following result:
[<img src="https://i.stack.imgur.com/fJnTp.png" alt="enter image description here" />](https://i.stack.imgur.com/fJnTp.png)



## How LINQ to Object executes queries


LINQ queries do not execute immediately. When you are building the query you are simply storing the query for future execution. Only when you actually request to iterate the query is the query executed (e.g. in a for loop, when calling ToList, Count, Max, Average, First, etc.)

This is considered **deferred execution**. This allows you to build up the query in multiple steps, potentially modifying it based on conditional statements, and then execute it later only once you require the result.

Given the code:

```cs
var query = from n in numbers 
            where n % 2 != 0
            select n;

```

The example above only stores the query into `query` variable. It does not execute the query itself.

The `foreach` statement forces the query execution:

```cs
foreach(var n in query) {
    Console.WriteLine($"Number selected {n}");
}

```

Some LINQ methods will also trigger the query execution, `Count`, `First`, `Max`, `Average`. They return single values. `ToList` and  `ToArray` collects result and turn them to a List or a Array respectively.

Be aware that it is possible for you to iterate across the query multiple times if you call multiple LINQ functions on the same query. This could give you different results at each call. If you only want to work with one data set, be sure to save it into a list or array.

