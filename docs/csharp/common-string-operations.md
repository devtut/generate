---
metaTitle: "Common String Operations"
description: "Formatting a string, Padding a string to a fixed length, Correctly reversing a string, Getting x characters from the right side of a string, Checking for empty String using String.IsNullOrEmpty() and String.IsNullOrWhiteSpace(), Trimming Unwanted Characters Off the Start and/or End of Strings., Construct a string from Array, Formatting using ToString, Convert Decimal Number to Binary,Octal and Hexadecimal Format, Splitting a String by specific character, Getting Substrings of a given string, Determine whether a string begins with a given sequence, Joining an array of strings into a new one , Getting a char at specific index and enumerating the string, Splitting a String by another string,  Replacing a string within a string, Changing the case of characters within a String, Concatenate an array of strings into a single string, String Concatenation"
---

# Common String Operations




## Formatting a string


Use the `String.Format()` method to replace one or more items in the string with the string representation of a specified object:

```cs
String.Format("Hello {0} Foo {1}", "World", "Bar") //Hello World Foo Bar

```



## Padding a string to a fixed length


```cs
string s = "Foo";
string paddedLeft = s.PadLeft(5);        // paddedLeft = "  Foo" (pads with spaces by default)
string paddedRight = s.PadRight(6, '+'); // paddedRight = "Foo+++"
string noPadded = s.PadLeft(2);          // noPadded = "Foo" (original string is never shortened)

```



## Correctly reversing a string


Most times when people have to reverse a string, they do it more or less like this:

```cs
char[] a = s.ToCharArray();
System.Array.Reverse(a);
string r = new string(a);

```

However, what these people don't realize is that this is actually wrong. <br />
And I don't mean because of the missing NULL check.

It is actually wrong because a Glyph/GraphemeCluster can consist out of several codepoints (aka. characters).

To see why this is so, we first have to be aware of the fact what the term "character" actually means.

[Reference:](https://stackoverflow.com/questions/27331819/whats-the-difference-between-a-character-a-code-point-a-glyph-and-a-grapheme)

> 
Character is an overloaded term than can mean many things.
<p>A code point is the atomic unit of information. Text is a sequence of
code points. Each code point is a number which is given meaning by the
Unicode standard.</p>
<p>A grapheme is a sequence of one or more code points that are displayed
as a single, graphical unit that a reader recognizes as a single
element of the writing system. For example, both a and ä are
graphemes, but they may consist of multiple code points (e.g. ä may be
two code points, one for the base character a followed by one for the
diaresis; but there's also an alternative, legacy, single code point
representing this grapheme). Some code points are never part of any
grapheme (e.g. the zero-width non-joiner, or directional overrides).</p>
<p>A glyph is an image, usually stored in a font (which is a collection
of glyphs), used to represent graphemes or parts thereof. Fonts may
compose multiple glyphs into a single representation, for example, if
the above ä is a single code point, a font may chose to render that as
two separate, spatially overlaid glyphs. For OTF, the font's GSUB and
GPOS tables contain substitution and positioning information to make
this work. A font may contain multiple alternative glyphs for the same
grapheme, too.</p>


So in C#, a character is actually a CodePoint.

Which means, if you just reverse a valid string like `Les Misérables`, which can look like this

```cs
string s = "Les Mise\u0301rables";

```

as a sequence of characters, you will get:

> 
selbaŕesiM seL


As you can see, the accent is on the R character, instead of the e character. <br />
Although string.reverse.reverse will yield the original string if you both times reverse the char array, this kind of reversal is definitely NOT the reverse of the original string.

You'll need to reverse each GraphemeCluster only. <br />
So, if done correctly, you reverse a string like this:

```cs

   private static System.Collections.Generic.List<string> GraphemeClusters(string s)
    {
        System.Collections.Generic.List<string> ls = new System.Collections.Generic.List<string>();

        System.Globalization.TextElementEnumerator enumerator = System.Globalization.StringInfo.GetTextElementEnumerator(s);
        while (enumerator.MoveNext())
        {
            ls.Add((string)enumerator.Current);
        }

        return ls;
    }


    // this 
    private static string ReverseGraphemeClusters(string s)
    {
        if(string.IsNullOrEmpty(s) || s.Length == 1)
             return s;
        
        System.Collections.Generic.List<string> ls = GraphemeClusters(s);
        ls.Reverse();

        return string.Join("", ls.ToArray());
    }

    public static void TestMe()
    {
        string s = "Les Mise\u0301rables";
        // s = "noël";
        string r = ReverseGraphemeClusters(s);

        // This would be wrong:
        // char[] a = s.ToCharArray();
        // System.Array.Reverse(a);
        // string r = new string(a);

        System.Console.WriteLine(r);
    }

```

And - oh joy - you'll realize if you do it correctly like this, it will also work for Asian/South-Asian/East-Asian languages (and French/Swedish/Norwegian, etc.)...



## Getting x characters from the right side of a string


Visual Basic has Left, Right, and Mid functions that returns characters from the Left, Right, and Middle of a string. These methods does not exist in C#, but can be implemented with `Substring()`.  They can be implemented as an extension methods like the following:

```cs

  public static class StringExtensions
   {
      /// <summary>
      /// VB Left function
      /// </summary>
      /// <param name="stringparam"></param>
      /// <param name="numchars"></param>
      /// <returns>Left-most numchars characters</returns>
      public static string Left( this string stringparam, int numchars )
      {
         // Handle possible Null or numeric stringparam being passed
         stringparam += string.Empty;
    
         // Handle possible negative numchars being passed
         numchars = Math.Abs( numchars );
    
         // Validate numchars parameter        
         if (numchars > stringparam.Length)
            numchars = stringparam.Length;
    
         return stringparam.Substring( 0, numchars );
      }
    
      /// <summary>
      /// VB Right function
      /// </summary>
      /// <param name="stringparam"></param>
      /// <param name="numchars"></param>
      /// <returns>Right-most numchars characters</returns>
      public static string Right( this string stringparam, int numchars )
      {
         // Handle possible Null or numeric stringparam being passed
         stringparam += string.Empty;
    
         // Handle possible negative numchars being passed
         numchars = Math.Abs( numchars );
    
         // Validate numchars parameter        
         if (numchars > stringparam.Length)
            numchars = stringparam.Length;
    
         return stringparam.Substring( stringparam.Length - numchars );
      }
    
      /// <summary>
      /// VB Mid function - to end of string
      /// </summary>
      /// <param name="stringparam"></param>
      /// <param name="startIndex">VB-Style startindex, 1st char startindex = 1</param>
      /// <returns>Balance of string beginning at startindex character</returns>
      public static string Mid( this string stringparam, int startindex )
      {
         // Handle possible Null or numeric stringparam being passed
         stringparam += string.Empty;
    
         // Handle possible negative startindex being passed
         startindex = Math.Abs( startindex );
    
         // Validate numchars parameter        
         if (startindex > stringparam.Length)
            startindex = stringparam.Length;
         
         // C# strings are zero-based, convert passed startindex
         return stringparam.Substring( startindex - 1 );
      }
    
      /// <summary>
      /// VB Mid function - for number of characters
      /// </summary>
      /// <param name="stringparam"></param>
      /// <param name="startIndex">VB-Style startindex, 1st char startindex = 1</param>
      /// <param name="numchars">number of characters to return</param>
      /// <returns>Balance of string beginning at startindex character</returns>
      public static string Mid( this string stringparam, int startindex, int numchars)
      {
         // Handle possible Null or numeric stringparam being passed
         stringparam += string.Empty;
    
         // Handle possible negative startindex being passed
         startindex = Math.Abs( startindex );
    
         // Handle possible negative numchars being passed
         numchars = Math.Abs( numchars );
    
         // Validate numchars parameter        
         if (startindex > stringparam.Length)
            startindex = stringparam.Length;
    
         // C# strings are zero-based, convert passed startindex
         return stringparam.Substring( startindex - 1, numchars );

       }
    }

```

This extension method can be used as follows:

```cs
string myLongString = "Hello World!";
string myShortString = myLongString.Right(6);  // "World!"
string myLeftString = myLongString.Left(5);    // "Hello"
string myMidString1 = myLongString.Left(4);    // "lo World"
string myMidString2 = myLongString.Left(2,3);    // "ell"

```



## Checking for empty String using String.IsNullOrEmpty() and String.IsNullOrWhiteSpace()


```cs
string nullString = null;
string emptyString = "";
string whitespaceString = "    ";
string tabString = "\t";
string newlineString = "\n";
string nonEmptyString = "abc123";

bool result;

result = String.IsNullOrEmpty(nullString);            // true
result = String.IsNullOrEmpty(emptyString);           // true
result = String.IsNullOrEmpty(whitespaceString);      // false
result = String.IsNullOrEmpty(tabString);             // false
result = String.IsNullOrEmpty(newlineString);         // false
result = String.IsNullOrEmpty(nonEmptyString);        // false

result = String.IsNullOrWhiteSpace(nullString);       // true
result = String.IsNullOrWhiteSpace(emptyString);      // true
result = String.IsNullOrWhiteSpace(tabString);        // true
result = String.IsNullOrWhiteSpace(newlineString);    // true
result = String.IsNullOrWhiteSpace(whitespaceString); // true
result = String.IsNullOrWhiteSpace(nonEmptyString);   // false

```



## Trimming Unwanted Characters Off the Start and/or End of Strings.


### `String.Trim()`

```cs
string x = "   Hello World!    ";
string y = x.Trim(); // "Hello World!"

string q = "{(Hi!*";
string r = q.Trim( '(', '*', '{' ); // "Hi!"

```

### `String.TrimStart()` and `String.TrimEnd()`

```cs
string q = "{(Hi*";
string r = q.TrimStart( '{' ); // "(Hi*"
string s = q.TrimEnd( '*' );   // "{(Hi" 

```



## Construct a string from Array


The `String.Join` method will help us to construct a string From array/list of characters or string. This method accepts two parameters. The first one is the delimiter or the separator which will help you to separate each element in the array. And the second parameter is the Array itself.

**String from `char array`:**

```cs
string delimiter=",";
char[] charArray = new[] { 'a', 'b', 'c' };
string inputString = String.Join(delimiter, charArray);

```

**Output** : `a,b,c` if we change the `delimiter` as `""` then the output will become `abc`.

**String from `List of char`:**

```cs
string delimiter = "|";
List<char> charList = new List<char>() { 'a', 'b', 'c' };
string inputString = String.Join(delimiter, charList);

```

**Output** : `a|b|c`

**String from `List of Strings`:**

```cs
string delimiter = " ";
List<string> stringList = new List<string>() { "Ram", "is", "a","boy" };
string inputString = String.Join(delimiter, stringList);

```

**Output** : `Ram is a boy`

**String from `array of strings`:**

```cs
string delimiter = "_";
string[] stringArray = new [] { "Ram", "is", "a","boy" };
string inputString = String.Join(delimiter, stringArray);

```

**Output** : `Ram_is_a_boy`



## Formatting using ToString


Usually we are using `String.Format` method for formatting purpose, the`.ToString` is usually used for converting other types to string. We can specify the format along with the ToString method while conversion is taking place, So we can avoid an additional Formatting. Let Me Explain how it works with different types;

**Integer to formatted string:**

```cs
int intValue = 10;
string zeroPaddedInteger = intValue.ToString("000"); // Output will be "010"
string customFormat = intValue.ToString("Input value is 0"); // output will be   "Input value is 10" 

```

**double to formatted string:**

```cs
double doubleValue = 10.456;
string roundedDouble = doubleValue.ToString("0.00"); // output 10.46
string integerPart = doubleValue.ToString("00");    // output 10
string customFormat = doubleValue.ToString("Input value is 0.0");  // Input value is 10.5

```

**Formatting DateTime using ToString**

```cs
DateTime currentDate = DateTime.Now; //  {7/21/2016 7:23:15 PM}
string dateTimeString = currentDate.ToString("dd-MM-yyyy HH:mm:ss"); // "21-07-2016 19:23:15"
string dateOnlyString = currentDate.ToString("dd-MM-yyyy"); // "21-07-2016"
string dateWithMonthInWords = currentDate.ToString("dd-MMMM-yyyy HH:mm:ss"); // "21-July-2016 19:23:15"

```



## Convert Decimal Number to Binary,Octal and Hexadecimal Format


<li>
To convert decimal number to binary format use **base 2**

```cs
Int32 Number = 15;
Console.WriteLine(Convert.ToString(Number, 2));  //OUTPUT : 1111

```


</li>
<li>
To convert decimal number to octal format use **base 8**

```cs
int Number = 15;
Console.WriteLine(Convert.ToString(Number, 8));  //OUTPUT : 17

```


</li>
<li>
To convert decimal number to hexadecimal format use **base 16**

```cs
var Number = 15;
Console.WriteLine(Convert.ToString(Number, 16));  //OUTPUT : f

```


</li>



## Splitting a String by specific character


```cs
string helloWorld = "hello world, how is it going?";
string[] parts1 = helloWorld.Split(',');

//parts1: ["hello world", " how is it going?"]

string[] parts2 = helloWorld.Split(' ');

//parts2: ["hello", "world,", "how", "is", "it", "going?"]

```



## Getting Substrings of a given string


```cs
string helloWorld = "Hello World!";
string world = helloWorld.Substring(6); //world = "World!"
string hello = helloWorld.Substring(0,5); // hello = "Hello"

```

`Substring` returns the string up from a given index, or between two indexes (both inclusive).



## Determine whether a string begins with a given sequence


```cs
string HelloWorld = "Hello World";
HelloWorld.StartsWith("Hello"); // true
HelloWorld.StartsWith("Foo"); // false

```

**Finding a string within a string**

Using the
[`System.String.Contains`](https://msdn.microsoft.com/en-us/library/dy85x1sa(v=vs.110).aspx) you can find out if a particular string exists within a string. The method returns a boolean, true if the string exists else false.

```cs
string s = "Hello World";
bool stringExists = s.Contains("ello");  //stringExists =true as the string contains the substring 

```



## Joining an array of strings into a new one 


```cs
var parts = new[] { "Foo", "Bar", "Fizz", "Buzz"};
var joined = string.Join(", ", parts);

//joined = "Foo, Bar, Fizz, Buzz"

```



## Getting a char at specific index and enumerating the string


You can use the `Substring` method to get any number of characters from a string at any given location. However, if you only want a single character, you can use the string indexer to get a single character at any given index like you do with an array:

```cs
string s = "hello";
char c = s[1]; //Returns 'e'

```

Notice that the return type is `char`, unlike the `Substring` method which returns a `string` type.

You can also use the indexer to iterate through the characters of the string:

```cs
string s = "hello";
foreach (char c in s)
    Console.WriteLine(c);
/********* This will print each character on a new line:
h
e
l
l
o
**********/

```



## Splitting a String by another string


```cs
string str = "this--is--a--complete--sentence";
string[] tokens = str.Split(new[] { "--" }, StringSplitOptions.None);

```

Result:

> 
[ "this", "is", "a", "complete", "sentence" ]




##  Replacing a string within a string


Using the [`System.String.Replace`](https://msdn.microsoft.com/en-us/library/fk49wtc1(v=vs.110).aspx) method, you can replace part of a string with another string.

```cs
string s = "Hello World";
 s = s.Replace("World", "Universe"); // s = "Hello Universe"

```

All the occurrences of the search string are replaced.

This method can also be used to remove part of a string, using the [`String.Empty`](https://msdn.microsoft.com/en-us/library/system.string.empty(v=vs.110).aspx) field:

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

In C# 6 this can be done as follows:

```cs
string concat = $"{first},{second}";

```

