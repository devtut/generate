---
metaTitle: "C# | Looping"
description: "Foreach Loop, For Loop, Do - While Loop, Looping styles, Nested loops, break, While loop, continue"
---

# Looping




## Foreach Loop


foreach will iterate over any object of a class that implements `IEnumerable` (take note that `IEnumerable<T>` inherits from it). Such objects include some built-in ones, but not limit to: `List<T>`, `T[]` (arrays of any type), `Dictionary<TKey, TSource>`, as well as interfaces like `IQueryable` and `ICollection`, etc.

**syntax**

```cs
foreach(ItemType itemVariable in enumerableObject)
    statement;

```

**remarks**

1. The type `ItemType` does not need to match the precise type of the items, it just needs to be assignable from the type of the items
1. Instead of `ItemType`, alternatively `var` can be used which will infer the items type from the enumerableObject by inspecting the generic argument of the `IEnumerable` implementation
1. The statement can be a block, a single statement or even an empty statement (`;`)
1. If `enumerableObject` is not implementing `IEnumerable`, the code will not compile
1. During each iteration the current item is cast to `ItemType` (even if this is not specified but compiler-inferred via `var`) and if the item cannot be cast an `InvalidCastException` will be thrown.

Consider this example:

```cs
var list = new List<string>();
list.Add("Ion");
list.Add("Andrei");
foreach(var name in list)
{
    Console.WriteLine("Hello " + name);
}

```

is equivalent to:

```cs
var list = new List<string>();
list.Add("Ion");
list.Add("Andrei");
IEnumerator enumerator;
try
{
    enumerator = list.GetEnumerator();
    while(enumerator.MoveNext())
    {
        string name = (string)enumerator.Current;
        Console.WriteLine("Hello " + name);
    }
}
finally
{
    if (enumerator != null)
        enumerator.Dispose();
}

```



## For Loop


A For Loop is great for doing things a certain amount of time. It's like a While Loop but the increment is included with the condition.

A For Loop is set up like this:

```cs
for (Initialization; Condition; Increment)
{
    // Code
}

```

> 
<p>Initialization - Makes a new local variable that can only be used in the loop.<br />
Condition - The loop only runs when the condition is true.<br />
Increment - How the variable changes every time the loop runs.</p>


An example:

```cs
for (int i = 0; i < 5; i++)
{
    Console.WriteLine(i);
}

```

Output:

> 
<p>0<br />
1<br />
2<br />
3<br />
4</p>


You can also leave out spaces in the For Loop, but you have to have all semicolons for it to function.

```cs
int input = Console.ReadLine();    

for ( ; input < 10; input + 2)
{
    Console.WriteLine(input);
}

```

Output for 3:

> 
<p>3<br />
5<br />
7<br />
9<br />
11</p>




## Do - While Loop


It is similar to a `while` loop, except that it tests the condition at the **end** of the loop body. The Do - While loop executes the loop once irrespective of whether the condition is true or not.

```cs
int[] numbers = new int[] { 6, 7, 8, 10 };
    
// Sum values from the array until we get a total that's greater than 10,
// or until we run out of values.
int sum = 0;
int i = 0;
do
{
    sum += numbers[i];
    i++;
} while (sum <= 10 && i < numbers.Length);
    
System.Console.WriteLine(sum); // 13

```



## Looping styles


**While**

The most trivial loop type. Only drawback is there is no intrinsic clue to know where you are in the loop.

```cs
/// loop while the condition satisfies
while(condition)
{
    /// do something
}

```

**Do**

Similar to `while`, but the condition is evaluated at the end of the loop instead of the beginning. This results in executing the loops at least once.

```cs
do
{
    /// do something
} while(condition) /// loop while the condition satisfies

```

**For**

Another trivial loop style. While looping an index (`i`) gets increased and you can use it. It is usually used for handling arrays.

```cs
for ( int i = 0; i < array.Count; i++ )
{
    var currentItem = array[i];
    /// do something with "currentItem"
}

```

**Foreach**

Modernized way of looping through `IEnumarable` objects. Good thing that you don't have to think about the index of the item or the item count of the list.

```cs
foreach ( var item in someList )
{
    /// do something with "item"
}

```

**Foreach Method**

While the other styles are used for selecting or updating the elements in collections, this style is usually used for **calling a method** straight away for all elements in a collection.

```cs
list.ForEach(item => item.DoSomething());

// or
list.ForEach(item => DoSomething(item));

// or using a method group
list.ForEach(Console.WriteLine);

// using an array
Array.ForEach(myArray, Console.WriteLine);

```

It is important to note that this method in only available on `List<T>` instances and as a static method on `Array` - it is **not** part of Linq.

**Linq Parallel Foreach**

Just like Linq Foreach, except this one does the job in a parallel manner. Meaning that all the items in the collection will run the given action at the same time, simultaneously.

```cs
collection.AsParallel().ForAll(item => item.DoSomething());

/// or
collection.AsParallel().ForAll(item => DoSomething(item));

```



## Nested loops


```cs
// Print the multiplication table up to 5s
for (int i = 1; i <= 5; i++)
{
    for (int j = 1; j <= 5; j++)
    {
        int product = i * j;
        Console.WriteLine("{0} times {1} is {2}", i, j, product);
    }
}

```



## break


Sometimes loop condition should be checked in the middle of the loop. The former is arguably more elegant than the latter:

```cs
for (;;)
{
    // precondition code that can change the value of should_end_loop expression

    if (should_end_loop)
        break;

    // do something
}

```

Alternative:

```cs
bool endLoop = false;
for (; !endLoop;)
{
    // precondition code that can set endLoop flag

    if (!endLoop)
    {
        // do something
    }
}

```

Note: In nested loops and/or `switch` must use more than just a simple `break`.



## While loop


```cs
int n = 0;
while (n < 5) 
{
    Console.WriteLine(n);
    n++;
}

```

Output:

> 
<p>0<br />
1<br />
2<br />
3<br />
4</p>


IEnumerators can be iterated with a while loop:

```cs
// Call a custom method that takes a count, and returns an IEnumerator for a list
// of strings with the names of theh largest city metro areas.
IEnumerator<string> largestMetroAreas = GetLargestMetroAreas(4);

while (largestMetroAreas.MoveNext())
{
    Console.WriteLine(largestMetroAreas.Current);
}

```

Sample output:

> 
<p>Tokyo/Yokohama<br />
New York Metro<br />
Sao Paulo<br />
Seoul/Incheon</p>




## continue


In addition to `break`, there is also the keyword `continue`. Instead of breaking completely the loop, it will simply skip the current iteration. It could be useful if you don't want some code to be executed if a particular value is set.

Here's a simple example:

```cs
for (int i = 1; i <= 10; i++)
{
    if (i < 9)
        continue;

    Console.WriteLine(i);
}

```

Will result in:

```cs
9
10

```

**Note:** `Continue` is often most useful in while or do-while loops. For-loops, with well-defined exit conditions, may not benefit as much.

