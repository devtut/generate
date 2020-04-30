---
metaTitle: "Function with multiple return values"
description: "anonymous object + dynamic keyword solution , Tuple solution, Ref and Out Parameters"
---

# Function with multiple return values



## "anonymous object" + "dynamic keyword" solution 


You can return an anonymous object from your function

```cs
public static object FunctionWithUnknowReturnValues ()
{
    /// anonymous object
    return new { a = 1, b = 2 };
}

```

And assign the result to a dynamic object and read the values in it.

```cs
/// dynamic object
dynamic x = FunctionWithUnknowReturnValues();

Console.WriteLine(x.a);
Console.WriteLine(x.b);

```



## Tuple solution


You can return an instance of `Tuple` class from your function with two template parameters as `Tuple<string, MyClass>`:

```cs
public Tuple<string, MyClass> FunctionWith2ReturnValues ()
{
    return Tuple.Create("abc", new MyClass());
}

```

And read the values like below:

```cs
Console.WriteLine(x.Item1);
Console.WriteLine(x.Item2);

```



## Ref and Out Parameters


The `ref` keyword is used to pass an [Argument as Reference](http://stackoverflow.com/documentation/c%23/3014/value-type-vs-reference-type#t=201607261617231313768). `out` will do the same as `ref` but it does not require an assigned value by the caller prior to calling the function.

**Ref Parameter** :-If you want to pass a variable as ref parameter then you need to initialize it before you pass it as ref parameter to method.

**Out Parameter :-**
If you want to pass a variable as out parameter you don’t need to initialize it before you pass it as out parameter to method.

```cs
static void Main(string[] args)
{
    int a = 2;
    int b = 3;
    int add = 0;
    int mult= 0;
    AddOrMult(a, b, ref add, ref mult); //AddOrMult(a, b, out add, out mult);
    Console.WriteLine(add); //5
    Console.WriteLine(mult); //6
}

private static void AddOrMult(int a, int b, ref int add, ref int mult) //AddOrMult(int a, int b, out int add, out int mult)
{
    add = a + b;
    mult = a * b;
}

```



#### Remarks


There is no inherent answer in C# to this - so called - need. Nonetheless there are workarounds to satisfy this need.

The reason I qualify the need as "so called" is that we only need methods with 2 or more than 2 values to return when we violate good programming principals. Especially the [Single Responsibility Principle](https://en.wikipedia.org/wiki/Single_responsibility_principle).

Hence, it would be better to be alerted when we need functions returning 2 or more values, and improve our design.

