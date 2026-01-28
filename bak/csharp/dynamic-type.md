---
metaTitle: "C# | Dynamic type"
description: "Creating a dynamic object with properties, Creating a dynamic variable, Returning dynamic, Handling Specific Types Unknown at Compile Time"
---

# Dynamic type



## Creating a dynamic object with properties


```cs
using System;
using System.Dynamic;

dynamic info = new ExpandoObject();
info.Id = 123;
info.Another = 456;

Console.WriteLine(info.Another);
// 456

Console.WriteLine(info.DoesntExist);
// Throws RuntimeBinderException

```



## Creating a dynamic variable


```cs
dynamic foo = 123;
Console.WriteLine(foo + 234);
// 357    Console.WriteLine(foo.ToUpper())
// RuntimeBinderException, since int doesn't have a ToUpper method

foo = "123";
Console.WriteLine(foo + 234);
// 123234
Console.WriteLine(foo.ToUpper()):
// NOW A STRING

```



## Returning dynamic


```cs
using System;

public static void Main()
{
    var value = GetValue();
    Console.WriteLine(value);
    // dynamics are useful!
}

private static dynamic GetValue()
{
    return "dynamics are useful!";
}

```



## Handling Specific Types Unknown at Compile Time


The following output equivalent results:

```cs
class IfElseExample
{
    public string DebugToString(object a)
    {
        if (a is StringBuilder)
        {
            return DebugToStringInternal(a as StringBuilder);
        }
        else if (a is List<string>)
        {
            return DebugToStringInternal(a as List<string>);
        }
        else
        {
            return a.ToString();
        }
    }

    private string DebugToStringInternal(object a)
    {
        // Fall Back
        return a.ToString();
    }

    private string DebugToStringInternal(StringBuilder sb)
    {
        return $"StringBuilder - Capacity: {sb.Capacity}, MaxCapacity: {sb.MaxCapacity}, Value: {sb.ToString()}";
    }

    private string DebugToStringInternal(List<string> list)
    {
        return $"List<string> - Count: {list.Count}, Value: {Environment.NewLine + "\t" + string.Join(Environment.NewLine + "\t", list.ToArray())}";
    }
}

class DynamicExample
{
    public string DebugToString(object a)
    {
        return DebugToStringInternal((dynamic)a);
    }

    private string DebugToStringInternal(object a)
    {
        // Fall Back
        return a.ToString();
    }

    private string DebugToStringInternal(StringBuilder sb)
    {
        return $"StringBuilder - Capacity: {sb.Capacity}, MaxCapacity: {sb.MaxCapacity}, Value: {sb.ToString()}";
    }

    private string DebugToStringInternal(List<string> list)
    {
        return $"List<string> - Count: {list.Count}, Value: {Environment.NewLine + "\t" + string.Join(Environment.NewLine + "\t", list.ToArray())}";
    }
}

```

The advantage to the dynamic, is adding a new Type to handle just requires adding an overload of DebugToStringInternal of the new type.  Also eliminates the need to manually cast it to the type as well.



#### Remarks


The `dynamic` keyword declares a variable whose type is not known at compile time. A `dynamic` variable can contain any value, and the type of the value can change during runtime.

As noted in the book "Metaprogramming in .NET", C# does not have a backing type for the `dynamic` keyword:

> 
<p>The functionality enabled by the `dynamic` keyword is a clever set of compiler actions that emit and use `CallSite` objects in the site container of the local execution scope. The compiler manages what programmers perceive as dynamic object
references through those `CallSite` instances.  The parameters, return types, fields, and properties that get dynamic treatment at compile time may be marked with some metadata to indicate that they were generated for dynamic use, but the underlying data type for them will always be `System.Object`.</p>


