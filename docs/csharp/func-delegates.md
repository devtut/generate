---
metaTitle: "Func delegates"
description: "Without parameters , With multiple variables, Lambda & anonymous methods, Covariant & Contravariant Type Parameters"
---

# Func delegates



## Without parameters 


This example shows how to create a delegate that encapsulates the method that returns the current time

```cs
static DateTime UTCNow()
{
    return DateTime.UtcNow;
}

static DateTime LocalNow()
{
    return DateTime.Now;
}

static void Main(string[] args)
{
    Func<DateTime> method = UTCNow;
    // method points to the UTCNow method
    // that retuns current UTC time  
    DateTime utcNow = method();

    method = LocalNow;
    // now method points to the LocalNow method
    // that returns local time

    DateTime localNow = method();
}

```



## With multiple variables


```cs
static int Sum(int a, int b)
{
    return a + b;
}

static int Multiplication(int a, int b)
{
    return a * b;
}

static void Main(string[] args)
{
    Func<int, int, int> method = Sum;
    // method points to the Sum method
    // that retuns 1 int variable and takes 2 int variables  
    int sum = method(1, 1);

    method = Multiplication;
    // now method points to the Multiplication method

    int multiplication = method(1, 1);
}

```



## Lambda & anonymous methods


An anonymous method can be assigned wherever a delegate is expected:

```cs
Func<int, int> square = delegate (int x) { return x * x; }

```

Lambda expressions can be used to express the same thing:

```cs
Func<int, int> square = x => x * x;

```

In either case, we can now invoke the method stored inside `square` like this:

```cs
var sq = square.Invoke(2);

```

Or as a shorthand:

```cs
var sq = square(2);

```

Notice that for the assignment to be type-safe, the parameter types and return type of the anonymous method must match those of the delegate type:

```cs
Func<int, int> sum = delegate (int x, int y) { return x + y; } // error
Func<int, int> sum = (x, y) => x + y; // error

```



## Covariant & Contravariant Type Parameters


`Func` also supports [Covariant & Contravariant](https://msdn.microsoft.com/en-us/library/dd799517(v=vs.110).aspx)

```cs
// Simple hierarchy of classes.
public class Person { }
public class Employee : Person { }

class Program
{
    static Employee FindByTitle(String title)
    {
        // This is a stub for a method that returns
        // an employee that has the specified title.
        return new Employee();
    }

    static void Test()
    {
        // Create an instance of the delegate without using variance.
        Func<String, Employee> findEmployee = FindByTitle;

        // The delegate expects a method to return Person,
        // but you can assign it a method that returns Employee.
        Func<String, Person> findPerson = FindByTitle;

        // You can also assign a delegate 
        // that returns a more derived type 
        // to a delegate that returns a less derived type.
        findPerson = findEmployee;

    }
}

```



#### Syntax


- `public delegate TResult Func<in T, out TResult>(T arg)`
- `public delegate TResult Func<in T1, in T2, out TResult>(T1 arg1, T2 arg2)`
- `public delegate TResult Func<in T1, in T2, in T3, out TResult>(T1 arg1, T2 arg2, T3 arg3)`
- `public delegate TResult Func<in T1, in T2, in T3, in T4, out TResult>(T1 arg1, T2 arg2, T3 arg3, T4 arg4)`



#### Parameters


|Parameter|Details
|------
|`arg` or `arg1`|the (first) parameter of the method
|`arg2`|the second parameter of the method
|`arg3`|the third parameter of the method
|`arg4`|the fourth parameter of the method
|`T` or `T1`|the type of the (first) parameter of the method
|`T2`|the type of the second parameter of the method
|`T3`|the type of the third parameter of the method
|`T4`|the type of the fourth parameter of the method
|`TResult`|the return type of the method

