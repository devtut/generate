---
metaTitle: "Iterators"
description: "Creating Iterators Using Yield, Simple Numeric Iterator Example"
---

# Iterators



## Creating Iterators Using Yield


Iterators **produce** enumerators. In C#, enumerators are produced by defining methods, properties or indexers that contain `yield` statements.

Most methods will return control to their caller through normal `return` statements, which disposes all state local to that method. In contrast, methods that use `yield` statements allow them to return multiple values to the caller on request while **preserving** local state in-between returning those values. These returned values constitute a sequence. There are two types of `yield` statements used within iterators:

<li>
`yield return`, which returns control to the caller but preserves state. The callee will continue execution from this line when control is passed back to it.
</li>
<li>
`yield break`, which functions similarly to a normal `return` statement - this signifies the end of the sequence. Normal `return` statements themselves are illegal within an iterator block.
</li>

This example below demonstrates an iterator method that can be used to generate the [Fibonacci sequence](https://en.wikipedia.org/wiki/Fibonacci_number):

```cs
IEnumerable<int> Fibonacci(int count)
{
    int prev = 1;
    int curr = 1;
    
    for (int i = 0; i < count; i++)
    {
        yield return prev;
        int temp = prev + curr;
        prev = curr;
        curr = temp;
    }
}

```

This iterator can then be used to produce an enumerator of the Fibonacci sequence that can be consumed by a calling method. The code below demonstrates how the first ten terms within the Fibonacci sequence can be enumerated:

```cs
void Main()
{
    foreach (int term in Fibonacci(10))
    {
        Console.WriteLine(term);
    }
}

```

**Output**

```cs
1
1
2
3
5
8
13
21
34
55

```



## Simple Numeric Iterator Example


A common use-case for iterators is to perform some operation over a collection of numbers. The example below demonstrates how each element within an array of numbers can be individually printed out to the console.

This is possible because arrays implement the `IEnumerable` interface, allowing clients to obtain an iterator for the array using the `GetEnumerator()` method. This method returns an **enumerator**, which is a read-only, forward-only cursor over each number in the array.

```cs
int[] numbers = { 1, 2, 3, 4, 5 };

IEnumerator iterator = numbers.GetEnumerator();

while (iterator.MoveNext())
{
    Console.WriteLine(iterator.Current);
}

```

**Output**

```cs
1
2
3
4
5

```

It's also possible to achieve the same results using a `foreach` statement:

```cs
foreach (int number in numbers)
{
    Console.WriteLine(number);
}

```



#### Remarks


An iterator is a method, get accessor, or operator that performs a custom iteration over an array or collection class by using the yield keyword

