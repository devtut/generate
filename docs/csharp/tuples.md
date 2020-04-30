---
metaTitle: "Tuples"
description: "Accessing tuple elements, Creating tuples, Comparing and sorting Tuples, Return multiple values from a method"
---

# Tuples




## Accessing tuple elements


To access tuple elements use `Item1`-`Item8` properties. Only the properties with index number less or equal to tuple size are going to be available (i.e. one cannot access `Item3` property in `Tuple<T1,T2>`).

```cs
var tuple = new Tuple<string, int, bool, MyClass>("foo", 123, true, new MyClass());
var item1 = tuple.Item1; // "foo"
var item2 = tuple.Item2; // 123
var item3 = tuple.Item3; // true
var item4 = tuple.Item4; // new My Class()

```



## Creating tuples


Tuples are created using generic types `Tuple<T1>`-`Tuple<T1,T2,T3,T4,T5,T6,T7,T8>`. Each of the types represents a tuple containing 1 to 8 elements. Elements can be of different types.

```cs
// tuple with 4 elements
var tuple = new Tuple<string, int, bool, MyClass>("foo", 123, true, new MyClass());

```

Tuples can also be created using static `Tuple.Create` methods. In this case, the types of the elements are inferred by the C# Compiler.

```cs
// tuple with 4 elements
var tuple = Tuple.Create("foo", 123, true, new MyClass());

```

Since C# 7.0, Tuples can be easily created using [ValueTuple](https://stackoverflow.com/documentation/c%23/1936/c-sharp-7-0-features/6329/language-support-for-tuples#t=201705312047498619514).

```cs
var tuple = ("foo", 123, true, new MyClass());

```

Elements can be named for easier decomposition.

```cs
(int number, bool flag, MyClass instance) tuple = (123, true, new MyClass());

```



## Comparing and sorting Tuples


Tuples can be compared based on their elements.

As an example, an enumerable whose elements are of type `Tuple` can be sorted based on comparisons operators defined on a specified element:

```cs
List<Tuple<int, string>> list = new List<Tuple<int, string>>();
list.Add(new Tuple<int, string>(2, "foo"));
list.Add(new Tuple<int, string>(1, "bar"));
list.Add(new Tuple<int, string>(3, "qux"));

list.Sort((a, b) => a.Item2.CompareTo(b.Item2)); //sort based on the string element

foreach (var element in list) {
    Console.WriteLine(element);
}

// Output:
// (1, bar)
// (2, foo)
// (3, qux)

```

Or to reverse the sort use:

```cs
list.Sort((a, b) => b.Item2.CompareTo(a.Item2));

```



## Return multiple values from a method


Tuples can be used to return multiple values from a method without using out parameters. In the following example `AddMultiply` is used to return two values (sum, product).

```cs
void Write()
{
    var result = AddMultiply(25, 28);
    Console.WriteLine(result.Item1);
    Console.WriteLine(result.Item2);
}

Tuple<int, int> AddMultiply(int a, int b)
{
    return new Tuple<int, int>(a + b, a * b);
}

```

Output:

> 
<p>53<br />
700</p>


Now C# 7.0 offers an alternative way to return multiple values from methods using value tuples [More info about `ValueTuple` struct](https://stackoverflow.com/documentation/c%23/1936/c-sharp-7-0-features/6329/language-support-for-tuples#t=201705312047498619514).

