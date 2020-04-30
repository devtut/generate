---
metaTitle: "Delegates"
description: "Declaring a delegate type, The Func<T, TResult>, Action<T> and Predicate<T> delegate types, Combine Delegates (Multicast Delegates), Safe invoke multicast delegate, Delegate Equality, Underlying references of named method delegates, Assigning a named method to a delegate, Assigning to a delegate by lambda, Passing delegates as parameters, Closure inside a delegate, Encapsulating transformations in funcs"
---

# Delegates



## Declaring a delegate type


The following syntax creates a `delegate` type with name `NumberInOutDelegate`, representing a method which takes an `int` and returns an `int`.

```cs
public delegate int NumberInOutDelegate(int input);

```

This can be used as follows:

```cs
public static class Program
{
    static void Main()
    {
        NumberInOutDelegate square = MathDelegates.Square;
        int answer1 = square(4); 
        Console.WriteLine(answer1); // Will output 16

        NumberInOutDelegate cube = MathDelegates.Cube;
        int answer2 = cube(4);
        Console.WriteLine(answer2); // Will output 64            
    }
}

public static class MathDelegates
{
    static int Square (int x)
    {
        return x*x;
    }

    static int Cube (int x)
    {
        return x*x*x;
    }
}

```

The `example` delegate instance is executed in the same way as the `Square` method. A delegate instance literally acts as a delegate for the caller: the caller invokes the
delegate, and then the delegate calls the target method. This indirection decouples
the caller from the target method.

You can declare a **generic** delegate type, and in that case you may specify that the type is covariant (`out`) or contravariant (`in`) in some of the type arguments. For example:

```cs
public delegate TTo Converter<in TFrom, out TTo>(TFrom input);

```

Like other generic types, generic delegate types can have constraints, such as `where TFrom : struct, IConvertible where TTo : new()`.

Avoid co- and contravariance for delegate types that are meant to be used for multicast delegates, such as event handler types. This is because concatenation (`+`) can fail if the run-time type is different from the compile-time type because of the variance. For example, avoid:

```cs
public delegate void EventHandler<in TEventArgs>(object sender, TEventArgs e);

```

Instead, use an invariant generic type:

```cs
public delegate void EventHandler<TEventArgs>(object sender, TEventArgs e);

```

Also supported are delegates where some parameters are modified by `ref` or `out`, as in:

```cs
public delegate bool TryParser<T>(string input, out T result);

```

(sample use `TryParser<decimal> example = decimal.TryParse;`), or delegates where the last parameter has the `params` modifier. Delegate types can have optional parameters (supply default values). Delegate types can use pointer types like `int*` or `char*` in their signatures or return types (use `unsafe` keyword). A delegate type and its parameters can carry custom attributes.



## The Func<T, TResult>, Action<T> and Predicate<T> delegate types


The System namespace contains `Func<..., TResult>` delegate types with between 0 and 15 generic parameters, returning type `TResult`.

```cs
private void UseFunc(Func<string> func)
{
    string output = func(); // Func with a single generic type parameter returns that type
    Console.WriteLine(output);
}

private void UseFunc(Func<int, int, string> func)
{
    string output = func(4, 2); // Func with multiple generic type parameters takes all but the first as parameters of that type
    Console.WriteLine(output);
}

```

The System namespace also contains `Action<...>` delegate types with different number of generic parameters (from 0 to 16). It is similar to `Func<T1, .., Tn>`, but it always returns `void`.

```cs
private void UseAction(Action action)
{
    action(); // The non-generic Action has no parameters
}

private void UseAction(Action<int, string> action)
{
    action(4, "two"); // The generic action is invoked with parameters matching its type arguments
}

```

`Predicate<T>` is also a form of `Func` but it will always return `bool`. A predicate is a way of specifying a custom criteria. Depending on the value of the input and the logic defined within the predicate, it will return either `true` or `false`. `Predicate<T>` therefore behaves in the same way as `Func<T, bool>` and both can be initialized and used in the same way.

```cs
Predicate<string> predicate = s => s.StartsWith("a");
Func<string, bool> func = s => s.StartsWith("a");

// Both of these return true
var predicateReturnsTrue = predicate("abc");
var funcReturnsTrue = func("abc");

// Both of these return false
var predicateReturnsFalse = predicate("xyz");
var funcReturnsFalse = func("xyz");

```

The choice of whether to use `Predicate<T>` or `Func<T, bool>` is really a matter of opinion. `Predicate<T>` is arguably more expressive of the author's intent, while `Func<T, bool>` is likely to be familiar to a greater proportion of C# developers.

In addition to that, there are some cases where only one of the options is available, especially when interacting with another API. For example `List<T>` and `Array<T>` generally take `Predicate<T>` for their methods, while most LINQ extensions only accept `Func<T, bool>`.



## Combine Delegates (Multicast Delegates)


Addition `+` and subtraction `-` operations can be used to combine delegate instances. The delegate contains a list of the assigned delegates.

```cs
using System;
using System.Reflection;
using System.Reflection.Emit;

namespace DelegatesExample {
    class MainClass {
        private delegate void MyDelegate(int a);

        private static void PrintInt(int a) {
            Console.WriteLine(a);
        }

        private static void PrintType<T>(T a) {
            Console.WriteLine(a.GetType());
        }

        public static void Main (string[] args)
        {
            MyDelegate d1 = PrintInt;
            MyDelegate d2 = PrintType;

            // Output:
            // 1
            d1(1);

            // Output:
            // System.Int32
            d2(1);

            MyDelegate d3 = d1 + d2;
            // Output:
            // 1
            // System.Int32
            d3(1);

            MyDelegate d4 = d3 - d2;
            // Output:
            // 1
            d4(1);

            // Output:
            // True
            Console.WriteLine(d1 == d4);
        }
    }
}

```

In this example `d3` is a combination of `d1` and `d2` delegates, so when called the program outputs both `1` and `System.Int32` strings.

Combining delegates with **non void** return types:

If a multicast delegate has a `nonvoid` return type, the caller receives the return value
from the last method to be invoked. The preceding methods are still called, but their
return values are discarded.

```cs

   class Program
    {
        public delegate int Transformer(int x);

        static void Main(string[] args)
        {
            Transformer t = Square;
            t += Cube;
            Console.WriteLine(t(2));  // O/P 8 
        }

        static int Square(int x) { return x * x; }

        static int Cube(int x) { return x*x*x; }
    }

```

`t(2)` will call first `Square` and then `Cube`. The return value of Square is discarded and return value of the last method i.e. `Cube` is retained.



## Safe invoke multicast delegate


Ever wanted to call a multicast delegate but you want the entire invokation list to be called even if an exception occurs in any in the chain. Then you are in luck, I have created an extension method that does just that, throwing an `AggregateException` only after execution of the entire list completes:

```cs
public static class DelegateExtensions
{
    public static void SafeInvoke(this Delegate del,params object[] args)
    {
        var exceptions = new List<Exception>();

        foreach (var handler in del.GetInvocationList())
        {
            try
            {
                handler.Method.Invoke(handler.Target, args);
            }
            catch (Exception ex)
            {
                exceptions.Add(ex);
            }
        }

        if(exceptions.Any())
        {
            throw new AggregateException(exceptions);
        }
    }
}

public class Test
{
    public delegate void SampleDelegate();

    public void Run()
    {
        SampleDelegate delegateInstance = this.Target2;
        delegateInstance += this.Target1;

        try
        {
            delegateInstance.SafeInvoke();
        } 
        catch(AggregateException ex)
        {
            // Do any exception handling here
        }
    }

    private void Target1()
    {
        Console.WriteLine("Target 1 executed");
    }

    private void Target2()
    {
        Console.WriteLine("Target 2 executed");
        throw new Exception();
    }
}

```

This outputs:

```cs
Target 2 executed
Target 1 executed

```

Invoking directly, without `SaveInvoke`, would only execute Target 2.



## Delegate Equality


Calling `.Equals()` on a delegate compares by reference equality:

```cs
Action action1 = () => Console.WriteLine("Hello delegates");
Action action2 = () => Console.WriteLine("Hello delegates");
Action action1Again = action1;

Console.WriteLine(action1.Equals(action1)) // True
Console.WriteLine(action1.Equals(action2)) // False
Console.WriteLine(action1Again.Equals(action1)) // True

```

These rules also apply when doing `+=` or `-=` on a multicast delegate, for example when subscribing and unsubscribing from events.



## Underlying references of named method delegates


When assigning named methods to delegates, they will refer to the same underlying object if:

<li>
They are the same instance method, on the same instance of a class
</li>
<li>
They are the same static method on a class

```cs
public class Greeter
{
    public void WriteInstance()
    {
        Console.WriteLine("Instance");
    }

    public static void WriteStatic()
    {
        Console.WriteLine("Static");
    }
}

// ...

Greeter greeter1 = new Greeter();
Greeter greeter2 = new Greeter();

Action instance1 = greeter1.WriteInstance;
Action instance2 = greeter2.WriteInstance;
Action instance1Again = greeter1.WriteInstance;

Console.WriteLine(instance1.Equals(instance2)); // False
Console.WriteLine(instance1.Equals(instance1Again)); // True

Action @static = Greeter.WriteStatic;
Action staticAgain = Greeter.WriteStatic;

Console.WriteLine(@static.Equals(staticAgain)); // True

```


</li>



## Assigning a named method to a delegate


Named methods can be assigned to delegates with matching signatures:

```cs
public static class Example
{
    public static int AddOne(int input)
    {
        return input + 1;
    }
}


Func<int,int> addOne = Example.AddOne

```

`Example.AddOne` takes an `int` and returns an `int`, its signature matches the delegate `Func<int,int>`. `Example.AddOne` can be directly assigned to `addOne` because they have matching signatures.



## Assigning to a delegate by lambda


Lambdas can be used to create anonymous methods to assign to a delegate:

```cs
Func<int,int> addOne = x => x+1;

```

Note that the explicit declaration of type is required when creating a variable this way:

```cs
var addOne = x => x+1; // Does not work

```



## Passing delegates as parameters


Delegates can be used as typed function pointers:

```cs
class FuncAsParameters
{
  public void Run()
  {
    DoSomething(ErrorHandler1);
    DoSomething(ErrorHandler2);
  }

  public bool ErrorHandler1(string message)
  {
    Console.WriteLine(message);
    var shouldWeContinue = ...  
    return shouldWeContinue;
  }

  public bool ErrorHandler2(string message)
  {
    // ...Write message to file...
    var shouldWeContinue = ...  
    return shouldWeContinue;
  }

  public void DoSomething(Func<string, bool> errorHandler)
  {
    // In here, we don't care what handler we got passed!
    ...
    if (...error...)
    {
      if (!errorHandler("Some error occurred!"))
      {
        // The handler decided we can't continue
        return;
      }
    }
  }
}

```



## Closure inside a delegate


Closures are inline anonymous methods that have the ability to use `Parent` method variables and other anonymous methods which are defined in the parent's scope.

> 
<p>In essence, a closure is a block of code which can be executed at a
later time, but which maintains the environment in which it was first
created - i.e. it can still use the local variables etc of the method
which created it, even after that method has finished executing.
**-- Jon Skeet**</p>


```cs
delegate int testDel();
static void Main(string[] args)
{
    int foo = 4;
    testDel myClosure = delegate()
    {
        return foo;
    };
    int bar = myClosure();

}

```

Example taken from [Closures in .NET](http://stackoverflow.com/a/428621/1016343).



## Encapsulating transformations in funcs


```cs
public class MyObject{
    public DateTime? TestDate { get; set; }

    public Func<MyObject, bool> DateIsValid = myObject => myObject.TestDate.HasValue && myObject.TestDate > DateTime.Now;

    public void DoSomething(){
        //We can do this:
        if(this.TestDate.HasValue && this.TestDate > DateTime.Now){
            CallAnotherMethod();
        }

        //or this:
        if(DateIsValid(this)){
            CallAnotherMethod();
        }
    }
}

```

In the spirit of clean coding, encapsulating checks and transformations like the one above as a Func can make your code easier to read and understand.  While the above example is very simple, what if there were multiple DateTime properties each with their own differing validation rules and we wanted to check different combinations?  Simple, one-line Funcs that each have established return logic can be both readable and reduce the apparent complexity of your code.  Consider the below Func calls and imagine how much more code would be cluttering up the method:

```cs
public void CheckForIntegrity(){
    if(ShipDateIsValid(this) && TestResultsHaveBeenIssued(this) && !TestResultsFail(this)){
        SendPassingTestNotification();
    }
}

```



#### Remarks


### Summary

A **delegate type** is a type representing a particular method signature. An instance of this type refers to a particular method with a matching signature. Method parameters may have delegate types, and so this one method to be passed a reference to another method, which may then be invoked

### In-built delegate types: `Action<...>`, `Predicate<T>`  and `Func<...,TResult>`

The `System` namespace contains `Action<...>`,`Predicate<T>` and `Func<...,TResult>` delegates, where the "..." represents between 0 and 16 generic type parameters (for 0 parameters, `Action` is non-generic).

`Func` represents methods with a return type matching `TResult`, and `Action` represents methods without a return value (void). In both cases, the additional generic type parameters match, in order, the method parameters.

`Predicate` represents method with boolean return type, T is input parameter.

### Custom delegate types

Named delegate types can be declared using the `delegate` keyword.

### Invoking delegates

Delegates can be invoked using the same syntax as methods: the name of the delegate instance, followed by parentheses containing any parameters.

### Assigning to delegates

Delegates can be assigned to in the following ways:

- Assigning a named method
- Assigning an anonymous method using a lambda
- Assigning a named method using the `delegate` keyword.

### Combining delegates

Multiple delegate objects can be assigned to one delegate instance by using the `+` operator. The `-` operator can be used to remove a component delegate from another delegate.

