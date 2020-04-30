---
metaTitle: "Overload Resolution"
description: "Basic Overloading Example, params is not expanded, unless necessary., Passing null as one of the arguments"
---

# Overload Resolution



## Basic Overloading Example


This code contains an overloaded method named **Hello**:

```cs
class Example
{
    public static void Hello(int arg)
    {
        Console.WriteLine("int");
    }
 
    public static void Hello(double arg)
    {
        Console.WriteLine("double");
    }
 
    public static void Main(string[] args) 
    {
        Hello(0);
        Hello(0.0);
    }
}

```

When the **Main** method is called, it will print

```cs
int
double

```

At compile-time, when the compiler finds the method call `Hello(0)`, it finds all methods with the name `Hello`. In this case, it finds two of them. It then tries to determine which of the methods is **better**. The algorithm for determining which method is better is complex, but it usually boils down to "make as few implicit conversions as possible".

Thus, in the case of `Hello(0)`, no conversion is needed for the method `Hello(int)` but an implicit numeric conversion is needed for the method `Hello(double)`. Thus, the first method is chosen by the compiler.

In the case of `Hello(0.0)`, there is no way to convert `0.0` to an `int` implicitly, so the method `Hello(int)` is not even considered for overload resolution. Only method remains and so it is chosen by the compiler.



## "params" is not expanded, unless necessary.


The following program:

```cs
class Program
{
    static void Method(params Object[] objects)
    {
        System.Console.WriteLine(objects.Length);
    }   
    static void Method(Object a, Object b)
    {
        System.Console.WriteLine("two");
    }
    static void Main(string[] args)
    {
        object[] objectArray = new object[5];

        Method(objectArray);
        Method(objectArray, objectArray);
        Method(objectArray, objectArray, objectArray);
    }
}

```

will print:

```cs
5
two
3

```

The call expression `Method(objectArray)` could be interpreted in two ways: a single `Object` argument that happens to be an array (so the program would output `1` because that would be the number of arguments, or as an array of arguments, given in the normal form, as though the method `Method` did not have the keyword `params`. In these situations, the normal, non-expanded form always takes precedence. So, the program outputs `5`.

In the second expression, `Method(objectArray, objectArray)`, both the expanded form of the first method and the traditional second method are applicable. In this case also, non-expanded forms take precedence, so the program prints `two`.

In the third expression, `Method(objectArray, objectArray, objectArray)`, the only option is to use the expanded form of the first method, and so the program prints `3`.



## Passing null as one of the arguments


If you have

```cs
void F1(MyType1 x) {
    // do something
}

void F1(MyType2 x) {
    // do something else
}

```

and for some reason you need to call the first overload of `F1` but with `x = null`, then doing simply

```cs
F1(null);

```

will not compile as the call is ambiguous. To counter this you can do

```cs
F1(null as MyType1);

```



#### Remarks


The process of overload resolution is described in the [C# specification](https://www.microsoft.com/en-us/download/details.aspx?id=7029), section 7.5.3. Also relevant are the sections 7.5.2 (type inference) and 7.6.5 (invocation expressions).

How overload resolution works will probably be changed in C# 7. The design notes indicate that Microsoft will roll out a new system for determining which method is better (in complicated scenarios).

