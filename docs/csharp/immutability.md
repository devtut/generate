---
metaTitle: "Immutability"
description: "System.String class, Strings and immutability"
---

# Immutability



## System.String class


In C# (and .NET) a string is represented by class System.String. The `string` keyword is an alias for this class.

The System.String class is immutable, i.e once created its state cannot be altered.

So all the operations you perform on a string like Substring, Remove, Replace, concatenation using `+` operator etc will create a new string and return it.

See the following program for demonstration -

```cs
string str = "mystring";
string newString = str.Substring(3);
Console.WriteLine(newString);
Console.WriteLine(str);

```

This will print `string` and `mystring` respectively.



## Strings and immutability


Immutable types are types that when changed create a new version of the object in memory, rather than changing the existing object in memory. The simplest example of this is the built-in `string` type.

Taking the following code, that appends " world" onto the word "Hello"

```cs
string myString = "hello";
myString += " world";

```

What is happening in memory in this case is that a new object is created when you append to the `string` in the second line. If you do this as part of a  large loop, there is the potential for this to cause performance issues in your application.

The mutable equivalent for a `string` is a `StringBuilder`

Taking the following code

```cs
StringBuilder myStringBuilder = new StringBuilder("hello");
myStringBuilder.append(" world");

```

When you run this, you are modifying the `StringBuilder` object itself in memory.

