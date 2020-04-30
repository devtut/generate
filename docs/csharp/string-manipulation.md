---
metaTitle: "String Manipulation"
description: "Replacing a string within a string, Changing the case of characters within a String, Finding a string within a string, Removing (Trimming) white-space from a string, Splitting a string using a delimiter, Concatenate an array of strings into a single string, String Concatenation"
---

# String Manipulation



## Replacing a string within a string


Using the [`System.String.Replace`](https://msdn.microsoft.com/en-us/library/fk49wtc1(v=vs.110).aspx) method, you can replace part of a string with another string.

```cs
string s = "Hello World";
s = s.Replace("World", "Universe"); // s = "Hello Universe"

```

All the occurrences of the search string are replaced:

```cs
string s = "Hello World";
s = s.Replace("l", "L"); // s = "HeLLo WorLD"

```

`String.Replace` can also be used to **remove** part of a string, by specifying an empty string as the replacement value:

```cs
string s = "Hello World";
s = s.Replace("ell", String.Empty); // s = "Ho World"

```



## Changing the case of characters within a String


The [`System.String`](https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx) class supports a number of methods to convert between uppercase and lowercase characters in a string.

- [`System.String.ToLowerInvariant`](https://msdn.microsoft.com/en-us/library/system.string.tolowerinvariant(v=vs.110).aspx) is used to return a String object converted to lowercase.

- [`System.String.ToUpperInvariant`](https://msdn.microsoft.com/en-us/library/system.string.toupperinvariant(v=vs.110).aspx) is used to return a String object converted to uppercase.

**Note:** The reason to use the **invariant** versions of these methods is to prevent producing unexpected culture-specific letters. This is explained [here in detail](http://stackoverflow.com/a/19778131/1379664).

Example:

```cs
string s = "My String";
s = s.ToLowerInvariant(); // "my string"
s = s.ToUpperInvariant(); // "MY STRING"

```

Note that you **can** choose to specify a specific **[Culture](https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo(v=vs.110).aspx)** when converting to lowercase and uppercase by using the [String.ToLower(CultureInfo)](https://msdn.microsoft.com/en-us/library/s8z5yt00(v=vs.110).aspx) and [String.ToUpper(CultureInfo)](https://msdn.microsoft.com/en-us/library/24kc78ka(v=vs.110).aspx) methods accordingly.



## Finding a string within a string


Using the
[`System.String.Contains`](https://msdn.microsoft.com/en-us/library/dy85x1sa(v=vs.110).aspx) you can find out if a particular string exists within a string. The method returns a boolean, true if the string exists else false.

```cs
string s = "Hello World";
bool stringExists = s.Contains("ello");  //stringExists =true as the string contains the substring 

```

Using the [`System.String.IndexOf`](https://msdn.microsoft.com/en-us/library/k8b1470s(v=vs.110).aspx) method, you can locate the starting position of a substring within an existing string.<br />
Note the returned position is zero-based, a value of -1 is returned if the substring is not found.

```cs
string s = "Hello World";
int location = s.IndexOf("ello"); // location = 1

```

To find the first location from the ****end**** of a string, use the [`System.String.LastIndexOf`](https://msdn.microsoft.com/en-us/library/system.string.lastindexof(v=vs.110).aspx) method:

```cs
string s = "Hello World";
int location = s.LastIndexOf("l"); // location = 9

```



## Removing (Trimming) white-space from a string


The [`System.String.Trim`](https://msdn.microsoft.com/en-us/library/t97s7bs3(v=vs.110).aspx) method can be used to remove all leading and trailing white-space characters from a string:

```cs
string s = "     String with spaces at both ends      ";
s = s.Trim(); // s = "String with spaces at both ends"

```

In addition:

<li>
To remove white-space only from the **beginning** of a string use: [`System.String.TrimStart`](https://msdn.microsoft.com/en-us/library/system.string.trimstart(v=vs.110).aspx)
</li>
<li>
To remove white-space only from the **end** of a string use: [`System.String.TrimEnd`](https://msdn.microsoft.com/en-us/library/system.string.trimend(v=vs.110).aspx)
</li>

**Substring to extract part of a string.**

The [`System.String.Substring`](https://msdn.microsoft.com/en-us/library/hxthx5h6(v=vs.110).aspx) method can be used to extract a portion of the string.

```cs
string s ="A portion of word that is retained";
s=str.Substring(26);  //s="retained"

s1 = s.Substring(0,5);  //s="A por"

```



## Splitting a string using a delimiter


Use the [`System.String.Split`](https://msdn.microsoft.com/en-us/library/system.string.split(v=vs.110).aspx) method to return a string array that contains substrings of the original string, split based on a specified delimiter:

```cs
string sentence = "One Two Three Four";
string[] stringArray = sentence.Split(' ');

foreach (string word in stringArray)
{
    Console.WriteLine(word);    
}

```

Output:

> 
<p>One<br />
Two<br />
Three<br />
Four</p>




## Concatenate an array of strings into a single string


The [`System.String.Join`](https://msdn.microsoft.com/en-us/library/57a79xd0(v=vs.110).aspx) method allows to concatenate all elements in a string array, using a specified separator between each element:

```cs
string[] words = {"One", "Two", "Three", "Four"};
string singleString = String.Join(",", words); // singleString = "One,Two,Three,Four"

```



## String Concatenation


String Concatenation can be done by using the [`System.String.Concat`](https://msdn.microsoft.com/en-us/library/system.string.concat(v=vs.110).aspx) method, or (much easier) using the `+` operator:

```cs
string first = "Hello ";
string second = "World";

string concat = first + second; // concat = "Hello World"
concat = String.Concat(first, second); // concat = "Hello World"

```

