---
metaTitle: "C# | Object Oriented Programming In C#"
description: "Classes:"
---

# Object Oriented Programming In C#


This topic try to tell us how we can write programs based on OOP approach.But we don't try to teach Object Oriented Programming paradigm.
We'll be covering following topics:
Classes,Properties,Inheritance,Polymorphism,Interfaces and so on.



## Classes:


Skeleton of declaring class is:

<>:Required

[]:Optional

```cs
[private/public/protected/internal] class <Desired Class Name> [:[Inherited class][,][[Interface Name 1],[Interface Name 2],...]
{
    //Your code
}

```

Don't worry if you can't understand whole syntax,We'll be get familiar with all part of that.for first example consider following class:

```cs
class MyClass
{
    int i = 100;
    public void getMyValue()
    {
        Console.WriteLine(this.i);//Will print number 100 in output
    }
}

```

in this class we create variable `i` with `int` type and with default private [Access Modifiers](https://msdn.microsoft.com/en-us/library/ms173121.aspx) and `getMyValue()` method with public access modifiers.

