---
metaTitle: "String Concatenate"
description: "+ Operator, Concatenate strings using System.Text.StringBuilder, Concat string array elements using String.Join, Concatenation of two strings using $"
---

# String Concatenate



## + Operator


```cs
string s1 = "string1";
string s2 = "string2";

string s3 = s1 + s2; // "string1string2"

```



## Concatenate strings using System.Text.StringBuilder


Concatenating strings using a [StringBuilder](https://msdn.microsoft.com/en-us/library/system.text.stringbuilder(v=vs.110).aspx) can offer performance advantages over simple string concatenation using `+`.  This is due to the way memory is allocated.  Strings are reallocated with each concatenation, StringBuilders allocate memory in blocks only reallocating when the current block is exhausted. This can make a huge difference when doing a lot of small concatenations.

```cs
StringBuilder sb = new StringBuilder();
for (int i = 1; i <= 5; i++)
{
    sb.Append(i);
    sb.Append(" ");
}
Console.WriteLine(sb.ToString()); // "1 2 3 4 5 "

```

Calls to `Append()` can be daisy chained, because it returns a reference to the `StringBuilder`:

```cs
StringBuilder sb = new StringBuilder();
sb.Append("some string ")
  .Append("another string");

```



## Concat string array elements using String.Join


The `String.Join` method can be used to concatenate multiple elements from a string array.

```cs
string[] value = {"apple", "orange", "grape", "pear"};
string separator = ", ";

string result = String.Join(separator, value, 1, 2);
Console.WriteLine(result);

```

> 
Produces the following output: "orange, grape"


This example uses the `String.Join(String, String[], Int32, Int32)` overload, which specifies the start index and count on top of the separator and value.

If you do not wish to use the startIndex and count overloads, you can join all string given. Like this:

```cs
string[] value = {"apple", "orange", "grape", "pear"};
string separator = ", ";
string result = String.Join(separator, value);
Console.WriteLine(result);

```

which will produce;

> 
apple, orange, grape, pear




## Concatenation of two strings using $


$ provides an easy and a concise method to concatenate multiple strings.

```cs
var str1 = "text1";
var str2 = " ";
var str3 = "text3";
string result2 = $"{str1}{str2}{str3}"; //"text1 text3"

```



#### Remarks


If you are creating a dynamic string, It is a good practice to opt for `StringBuilder` class rather than joining strings using + or `Concat` method as each +/`Concat` creates a new string object everytime it is executed.

