---
metaTitle: "Casting"
description: "Cast an object to a base type, Checking compatibility without casting, Explicit Casting, Safe Explicit Casting (`as` operator), Implicit Casting, Explicit Numeric Conversions, Conversion Operators, LINQ Casting operations"
---

# Casting



## Cast an object to a base type


Given the following definitions :

```cs
public interface IMyInterface1
{
    string GetName();
}

public interface IMyInterface2
{
    string GetName();
}

public class MyClass : IMyInterface1, IMyInterface2
{
    string IMyInterface1.GetName()
    {
        return "IMyInterface1";
    }

    string IMyInterface2.GetName()
    {
        return "IMyInterface2";
    }
}

```

Casting an object to a base type example :

```

   MyClass obj = new MyClass();

    IMyInterface1 myClass1 = (IMyInterface1)obj;
    IMyInterface2 myClass2 = (IMyInterface2)obj;

    Console.WriteLine("I am : {0}", myClass1.GetName());
    Console.WriteLine("I am : {0}", myClass2.GetName());

    // Outputs :
    // I am : IMyInterface1
    // I am : IMyInterface2

```



## Checking compatibility without casting


If you need to know whether a value's type extends or implements a given type, but you don't want to actually cast it as that type, you can use the `is` operator.

```cs
if(value is int)
{
   Console.WriteLine(value + "is an int");
}

```



## Explicit Casting


If you know that a value is of a specific type, you can explicitly cast it to that type in order to use it in a context where that type is needed.

```cs
object value = -1;
int number = (int) value;
Console.WriteLine(Math.Abs(number));

```

If we tried passing `value` directly to `Math.Abs()`, we would get a compile-time exception because `Math.Abs()` doesn't have an overload that takes an `object` as a parameter.

If `value` could not be cast to an `int`, then the second line in this example would throw an `InvalidCastException`



## Safe Explicit Casting (`as` operator)


If you aren't sure whether a value is of the type you think it is, you can safely cast it using the `as` operator. If the value is not of that type, the resulting value will be `null`.

```cs
object value = "-1";
int? number = value as int?;
if(number != null)
{
    Console.WriteLine(Math.Abs(number.Value));
}

```

Note that `null` values have no type, so the `as` keyword will safely yield `null` when casting any `null` value.



## Implicit Casting


A value will automatically be cast to the appropriate type if the compiler knows that it can always be converted to that type.

```cs
int number = -1;
object value = number;
Console.WriteLine(value);

```

In this example, we didn't need to use the typical explicit casting syntax because the compiler knows all `int`s can be cast to `object`s. In fact, we could avoid creating variables and pass `-1` directly as the argument of `Console.WriteLine()` that expects an `object`.

```cs
Console.WriteLine(-1);

```



## Explicit Numeric Conversions


Explicit casting operators can be used to perform conversions of numeric types, even though they don't extend or implement one another.

```cs
double value = -1.1;
int number = (int) value;

```

Note that in cases where the destination type has less precision than the original type, precision will be lost. For example, `-1.1` as a double value in the above example becomes `-1` as an integer value.

Also, numeric conversions rely on compile-time types, so they won't work if the numeric types have been "boxed" into objects.

```cs
object value = -1.1;
int number = (int) value; // throws InvalidCastException

```



## Conversion Operators


In C#, types can define custom **Conversion Operators**, which allow values to be converted to and from other types using either explicit or implicit casts. For example, consider a class that is meant to represent a JavaScript expression:

```cs
public class JsExpression
{
    private readonly string expression;
    public JsExpression(string rawExpression)
    {
        this.expression = rawExpression;
    }
    public override string ToString()
    {
        return this.expression;
    }
    public JsExpression IsEqualTo(JsExpression other)
    {
        return new JsExpression("(" + this + " == " + other + ")");
    }
}

```

If we wanted to create a JsExpression representing a comparison of two JavaScript values, we could do something like this:

```cs
JsExpression intExpression = new JsExpression("-1");
JsExpression doubleExpression = new JsExpression("-1.0");
Console.WriteLine(intExpression.IsEqualTo(doubleExpression)); // (-1 == -1.0)

```

But we can add some **explicit conversion operators** to `JsExpression`, to allow a simple conversion when using explicit casting.

```cs
public static explicit operator JsExpression(int value)
{
    return new JsExpression(value.ToString());
}
public static explicit operator JsExpression(double value)
{
    return new JsExpression(value.ToString());
}

// Usage:
JsExpression intExpression = (JsExpression)(-1);
JsExpression doubleExpression = (JsExpression)(-1.0);
Console.WriteLine(intExpression.IsEqualTo(doubleExpression)); // (-1 == -1.0)

```

Or, we could change these operators to **implicit** to make the syntax much simpler.

```cs
public static implicit operator JsExpression(int value)
{
    return new JsExpression(value.ToString());
}
public static implicit operator JsExpression(double value)
{
    return new JsExpression(value.ToString());
}

// Usage:
JsExpression intExpression = -1;
Console.WriteLine(intExpression.IsEqualTo(-1.0)); // (-1 == -1.0)

```



## LINQ Casting operations


Suppose you have types like the following:

```cs
interface IThing {  }
class Thing : IThing {  }

```

LINQ allows you to create a projection that changes the compile-time generic type of an `IEnumerable<>` via the `Enumerable.Cast<>()` and `Enumerable.OfType<>()` extension methods.

```cs
IEnumerable<IThing> things = new IThing[] {new Thing()};
IEnumerable<Thing> things2 = things.Cast<Thing>();
IEnumerable<Thing> things3 = things.OfType<Thing>();

```

When `things2` is evaluated, the `Cast<>()` method will try to cast all of the values in `things` into `Thing`s. If it encounters a value that cannot be cast, an `InvalidCastException` will be thrown.

When `things3` is evaluated, the `OfType<>()` method will do the same, except that if it encounters a value that cannot be cast, it will simply omit that value rather than throw an exception.

Due to the generic type of these methods, they cannot invoke Conversion Operators or perform numeric conversions.

```cs
double[] doubles = new[]{1,2,3}.Cast<double>().ToArray(); // Throws InvalidCastException

```

You can simply perform a cast inside a `.Select()` as a workaround:

```cs
double[] doubles = new[]{1,2,3}.Select(i => (double)i).ToArray();

```



#### Remarks


**Casting** is not the same as **Converting**. It is possible to convert the string value `"-1"` to an integer value (`-1`), but this must be done through library methods like `Convert.ToInt32()` or `Int32.Parse()`. It cannot be done using casting syntax directly.

