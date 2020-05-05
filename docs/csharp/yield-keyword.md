---
metaTitle: "C# | Yield Keyword"
description: "Simple Usage, Correctly checking arguments, Early Termination, More Pertinent Usage, Lazy Evaluation, Try...finally, Using yield to create an IEnumerator<T> when implementing IEnumerable<T>, Eager evaluation, Return another Enumerable within a method returning Enumerable, Lazy Evaluation Example: Fibonacci Numbers, The difference between break and yield break"
---

# Yield Keyword


When you use the yield keyword in a statement, you indicate that the method, operator, or get accessor in which it appears is an iterator. Using yield to define an iterator removes the need for an explicit extra class (the class that holds the state for an enumeration) when you implement the IEnumerable and IEnumerator pattern for a custom collection type.



## Simple Usage


The `yield` keyword is used to define a function which returns an `IEnumerable` or `IEnumerator` (as well as their derived generic variants) whose values are generated lazily as a caller iterates over the returned collection. Read more about the purpose in the [remarks section](http://stackoverflow.com/documentation/c%23/61/yield-keyword#remarks).

The following example has a yield return statement that's inside a `for` loop.

```cs
public static IEnumerable<int> Count(int start, int count)
{
    for (int i = 0; i <= count; i++)
    {
        yield return start + i;
    }
}

```

Then you can call it:

```cs
foreach (int value in Count(start: 4, count: 10))
{
    Console.WriteLine(value);
}

```

**Console Output**

```cs
4
5
6
...
14

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/qtKObr)

Each iteration of the `foreach` statement body creates a call to the `Count` iterator function. Each call to the iterator function proceeds to the next execution of the `yield return` statement, which occurs during the next iteration of the `for` loop.



## Correctly checking arguments


An iterator method is not executed until the return value is enumerated. It's therefore advantageous to assert preconditions outside of the iterator.

```cs
public static IEnumerable<int> Count(int start, int count)
{
    // The exception will throw when the method is called, not when the result is iterated
    if (count < 0)
        throw new ArgumentOutOfRangeException(nameof(count));

    return CountCore(start, count);
}

private static IEnumerable<int> CountCore(int start, int count)
{
    // If the exception was thrown here it would be raised during the first MoveNext()
    // call on the IEnumerator, potentially at a point in the code far away from where
    // an incorrect value was passed.
    for (int i = 0; i < count; i++)
    {
        yield return start + i;
    }
}

```

**Calling Side Code (Usage):**

```cs
// Get the count
var count = Count(1,10);
// Iterate the results
foreach(var x in count)
{
    Console.WriteLine(x);
}

```

**Output:**

> 
<p>1<br />
2<br />
3<br />
4<br />
5<br />
6<br />
7<br />
8<br />
9<br />
10</p>


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/yIYxo6)

When a method uses `yield` to generate an enumerable the compiler creates a state machine that when iterated over will run code up to a `yield`. It then returns the yielded item, and saves its state.

This means you won't find out about invalid arguments (passing `null` etc.) when you first call the method (because that creates the state machine), only when you try and access the first element (because only then does the code within the method get ran by the state machine). By wrapping it in a normal method that first checks arguments you can check them when the method is called. This is an example of failing fast.

When using C# 7+, the `CountCore` function can be conveniently hidden into the `Count` function as a **local function**. See example [here](http://stackoverflow.com/documentation/c%23/1936/c-sharp-7-0-features/6330/local-functions#t=201607251321358412005#t=201607251057101259341).



## Early Termination


You can extend the functionality of existing `yield` methods by passing in one or more values or elements that could define a terminating condition within the function by calling a `yield break` to stop the inner loop from executing.

```cs
public static IEnumerable<int> CountUntilAny(int start, HashSet<int> earlyTerminationSet)
{
    int curr = start;

    while (true)
    {
        if (earlyTerminationSet.Contains(curr))
        {
            // we've hit one of the ending values
            yield break;
        }

        yield return curr;

        if (curr == Int32.MaxValue)
        {
            // don't overflow if we get all the way to the end; just stop
            yield break;
        }

        curr++;
    }
}

```

The above method would iterate from a given `start` position until one of the values within the `earlyTerminationSet` was encountered.

```cs
// Iterate from a starting point until you encounter any elements defined as 
// terminating elements
var terminatingElements = new HashSet<int>{ 7, 9, 11 };
// This will iterate from 1 until one of the terminating elements is encountered (7)
foreach(var x in CountUntilAny(1,terminatingElements))
{
    // This will write out the results from 1 until 7 (which will trigger terminating)
    Console.WriteLine(x);
}

```

**Output:**

> 
<p>1<br />
2<br />
3<br />
4<br />
5<br />
6</p>


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/pctiOz)



## More Pertinent Usage


```cs
public IEnumerable<User> SelectUsers()
{
    // Execute an SQL query on a database.
    using (IDataReader reader = this.Database.ExecuteReader(CommandType.Text, "SELECT Id, Name FROM Users"))
    {
        while (reader.Read())
        {
            int id = reader.GetInt32(0);
            string name = reader.GetString(1);
            yield return new User(id, name);
        }
    }
}

```

There are other ways of getting an `IEnumerable<User>` from an SQL database, of course -- this just demonstrates that you can use `yield` to turn anything that has "sequence of elements" semantics into an `IEnumerable<T>` that someone can iterate over.



## Lazy Evaluation


Only when the `foreach` statement moves to the next item  does the iterator block evaluate up to the next `yield` statement.

Consider the following example:

```cs
private IEnumerable<int> Integers()
{
    var i = 0;
    while(true)
    {
        Console.WriteLine("Inside iterator: " + i);
        yield return i;
        i++;
    }
}

private void PrintNumbers()
{
    var numbers = Integers().Take(3);
    Console.WriteLine("Starting iteration");

    foreach(var number in numbers)
    {
        Console.WriteLine("Inside foreach: " + number);
    }
}

```

This will output:

> 
<p>Starting iteration<br />
Inside iterator: 0<br />
Inside foreach: 0<br />
Inside iterator: 1<br />
Inside foreach: 1<br />
Inside iterator: 2<br />
Inside foreach: 2</p>


[View Demo](https://dotnetfiddle.net/2qGV0B)

As a consequence:

- "Starting iteration" is printed first even though the iterator method was called before the line printing it because the line `Integers().Take(3);` does not actually starts iteration (no call to `IEnumerator.MoveNext()` was made)
- The lines printing to console alternate between the one inside the iterator method and the one inside the `foreach`, rather than all the ones inside the iterator method evaluating first
- This program terminates due to the `.Take()` method, even though the iterator method has a `while true` which it never breaks out of.



## Try...finally


If an iterator method has a yield inside a `try...finally`, then the returned `IEnumerator` will execute the `finally` statement when `Dispose` is called on it, as long as the current point of evaluation is inside the `try` block.

Given the function:

```cs
private IEnumerable<int> Numbers()
{
    yield return 1;
    try
    {
        yield return 2;
        yield return 3;
    }
    finally
    {
        Console.WriteLine("Finally executed");
    }
}

```

When calling:

```cs
private void DisposeOutsideTry()
{
    var enumerator = Numbers().GetEnumerator();

    enumerator.MoveNext();
    Console.WriteLine(enumerator.Current);
    enumerator.Dispose();
}

```

Then it prints:

> 
1


[View Demo](https://dotnetfiddle.net/MJt7dt)

When calling:

```cs
private void DisposeInsideTry()
{
    var enumerator = Numbers().GetEnumerator();

    enumerator.MoveNext();
    Console.WriteLine(enumerator.Current);
    enumerator.MoveNext();
    Console.WriteLine(enumerator.Current);
    enumerator.Dispose();
}

```

Then it prints:

> 
<p>1<br />
2<br />
Finally executed</p>


[View Demo](https://dotnetfiddle.net/HlMroV)



## Using yield to create an IEnumerator<T> when implementing IEnumerable<T>


The `IEnumerable<T>` interface has a single method, `GetEnumerator()`, which returns an `IEnumerator<T>`.

While the `yield` keyword can be used to directly create an `IEnumerable<T>`, it can **also** be used in exactly the same way to create an `IEnumerator<T>`. The only thing that changes is the return type of the method.

This can be useful if we want to create our own class which implements `IEnumerable<T>`:

```cs
public class PrintingEnumerable<T> : IEnumerable<T>
{
    private IEnumerable<T> _wrapped;

    public PrintingEnumerable(IEnumerable<T> wrapped)
    {
        _wrapped = wrapped;
    }

    // This method returns an IEnumerator<T>, rather than an IEnumerable<T>
    // But the yield syntax and usage is identical.
    public IEnumerator<T> GetEnumerator()
    {
        foreach(var item in _wrapped)
        {
            Console.WriteLine("Yielding: " + item);
            yield return item;
        }
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}

```

(Note that this particular example is just illustrative, and could be more cleanly implemented with a single iterator method returning an `IEnumerable<T>`.)



## Eager evaluation


The `yield` keyword allows lazy-evaluation of the collection.  Forcibly loading the whole collection into memory is called **eager evaluation**.

The following code shows this:

```cs
IEnumerable<int> myMethod()
{
    for(int i=0; i <= 8675309; i++)
    {
        yield return i;
    }
}
...
// define the iterator
var it = myMethod.Take(3);
// force its immediate evaluation
// list will contain 0, 1, 2
var list = it.ToList();

```

Calling `ToList`, `ToDictionary` or `ToArray` will force the immediate evaluation of the enumeration, retrieving all the elements into a collection.



## Return another Enumerable within a method returning Enumerable


```cs
public IEnumerable<int> F1()
{
    for (int i = 0; i < 3; i++)
        yield return i;

    //return F2(); // Compile Error!!
    foreach (var element in F2())
        yield return element;
}

public int[] F2()
{
    return new[] { 3, 4, 5 };
}

```



## Lazy Evaluation Example: Fibonacci Numbers


```cs
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics; // also add reference to System.Numberics

namespace ConsoleApplication33
{
    class Program
    {
        private static IEnumerable<BigInteger> Fibonacci()
        {
            BigInteger prev = 0;
            BigInteger current = 1;
            while (true)
            {
                yield return current;
                var next = prev + current;
                prev = current;
                current = next;
            }
        }

        static void Main()
        {
            // print Fibonacci numbers from 10001 to 10010
            var numbers = Fibonacci().Skip(10000).Take(10).ToArray();
            Console.WriteLine(string.Join(Environment.NewLine, numbers));
        }
    }
}

```

How it works under the hood (I recommend to decompile resulting .exe file in IL Disaambler tool):

1. C# compiler generates a class implementing `IEnumerable<BigInteger>` and `IEnumerator<BigInteger>` (`<Fibonacci>d__0` in ildasm).
1. This class implements a state machine. State consists of current position in method and values of local variables.
<li>The most interesting code are in `bool IEnumerator.MoveNext()` method. Basically, what `MoveNext()` do:
<ul>
1. Restores current state. Variables like `prev` and `current` become fields in our class (`<current>5__2` and `<prev>5__1` in ildasm). In our method we have two positions (`<>1__state`): first at the opening curly brace, second at `yield return`.
1. Executes code until next `yield return` or `yield break`/`}`.
1. For `yield return` resulting value is saved, so `Current` property can return it. `true` is returned. At this point current state is saved again for the next `MoveNext` invocation.
1. For `yield break`/`}` method just returns `false` meaning iteration is done.
</ul>
</li>

Also note, that 10001th number is 468 bytes long. State machine only saves `current` and `prev` variables as fields. While if we would like to save all numbers in the sequence from the first to the 10000th, the consumed memory size will be over 4 megabytes. So lazy evaluation, if properly used, can reduce memory footprint in some cases.



## The difference between break and yield break


Using `yield break` as opposed to `break` might not be as obvious as one may think. There are lot of bad examples on the Internet where the usage of the two is interchangeable and doesn't really demonstrate the difference.

The confusing part is that both of the keywords (or key phrases) make sense only within loops (`foreach`, `while`...) So when to choose one over the other?

It's important to realize that once you use the [`yield`](https://msdn.microsoft.com/en-us/library/9k7k7cf0.aspx) keyword in a method you effectively turn the method into an [iterator](https://msdn.microsoft.com/en-us/library/mt639331.aspx). The only purpose of the such method is then to iterate over a finite or infinite collection and yield (output) its elements. Once the purpose is fulfilled, there's no reason to continue method's execution. Sometimes, it happens naturally with the last closing bracket of the method `}`. But sometimes, you want to end the method prematurely. In a normal (non-iterating) method you would use the [`return`](https://msdn.microsoft.com/en-us/library/1h3swy84.aspx) keyword. But you can't use `return` in an iterator, you have to use `yield break`. In other words, `yield break` for an iterator is the same as `return` for a standard method. Whereas, the [`break`](https://msdn.microsoft.com/en-us/library/adbctzc4.aspx) statement just terminates the closest loop.

Let's see some examples:

```cs

   /// <summary>
    /// Yields numbers from 0 to 9
    /// </summary>
    /// <returns>{0,1,2,3,4,5,6,7,8,9}</returns>
    public static IEnumerable<int> YieldBreak()
    {
        for (int i = 0; ; i++)
        {
            if (i < 10)
            {
                // Yields a number
                yield return i;
            }
            else
            {
                // Indicates that the iteration has ended, everything 
                // from this line on will be ignored
                yield break;
            }
        }
        yield return 10; // This will never get executed
    }

```

```cs
    /// <summary>
    /// Yields numbers from 0 to 10
    /// </summary>
    /// <returns>{0,1,2,3,4,5,6,7,8,9,10}</returns>
    public static IEnumerable<int> Break()
    {
        for (int i = 0; ; i++)
        {
            if (i < 10)
            {
                // Yields a number
                yield return i;
            }
            else
            {
                // Terminates just the loop
                break;
            }
        }
        // Execution continues
        yield return 10;
    }

```



#### Syntax


- yield return [TYPE]
- yield break



#### Remarks


Putting the `yield` keyword in a method with the return type of `IEnumerable`, `IEnumerable<T>`, `IEnumerator`, or `IEnumerator<T>` tells the compiler to generate an implementation of the return type (`IEnumerable` or `IEnumerator`) that, when looped over, runs the method up to each "yield" to get each result.

The `yield` keyword is useful when you want to return "the next" element of a theoretically unlimited sequence, so calculating the entire sequence beforehand would be impossible, or when calculating the complete sequence of values before returning would lead to an undesirable pause for the user.

`yield break` can also be used to terminate the sequence at any time.

As the `yield` keyword requires an iterator interface type as the return type, such as `IEnumerable<T>`, you cannot use this in an async method as this returns a `Task<IEnumerable<T>>` object.

**Further reading**

- [https://msdn.microsoft.com/en-us/library/9k7k7cf0.aspx](https://msdn.microsoft.com/en-us/library/9k7k7cf0.aspx)

