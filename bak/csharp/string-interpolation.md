---
metaTitle: "C# | String Interpolation"
description: "Format dates in strings, Padding the output, Expressions, Simple Usage, Formatting numbers in strings"
---

# String Interpolation



## Format dates in strings


```cs
var date = new DateTime(2015, 11, 11);
var str = $"It's {date:MMMM d, yyyy}, make a wish!";
System.Console.WriteLine(str);

```

You can also use the [`DateTime.ToString`](https://msdn.microsoft.com/en-us/library/zdtaw1bw(v=vs.110).aspx) method to format the `DateTime` object. This will produce the same output as the code above.

```cs
var date = new DateTime(2015, 11, 11);
var str = date.ToString("MMMM d, yyyy");
str = "It's " + str + ", make a wish!";
Console.WriteLine(str);

```

**Output:**

> 
It's November 11, 2015, make a wish!


[Live Demo on .NET Fiddle](https://dotnetfiddle.net/DpRwV5)

[Live Demo using DateTime.ToString](https://dotnetfiddle.net/YnV9J0)

> 
**Note:** `MM` stands for months and `mm` for minutes. Be very careful when using these as mistakes can introduce bugs that may be difficult to discover.




## Padding the output


String can be formatted to accept a padding parameter that will specify how many character positions the inserted string will use :

```cs
${value, padding}

```

> 
<p>**NOTE:** Positive padding values indicate left padding and negative
padding values indicate right padding.</p>


### **Left Padding**

A left padding of 5 (adds 3 spaces before the value of number, so it takes up a total of 5 character positions in the resulting string.)

```cs
var number = 42;
var str = $"The answer to life, the universe and everything is {number, 5}.";
//str is "The answer to life, the universe and everything is    42.";
//                                                           ^^^^^
System.Console.WriteLine(str);

```

**Output:**

```cs
The answer to life, the universe and everything is    42.

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/PpZXmk)

### **Right Padding**

Right padding, which uses a negative padding value, will add spaces to the end of the current value.

```cs
var number = 42;
var str = $"The answer to life, the universe and everything is ${number, -5}.";
//str is "The answer to life, the universe and everything is 42   .";
//                                                           ^^^^^
System.Console.WriteLine(str);

```

**Output:**

```cs
The answer to life, the universe and everything is 42   .

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/QtKjGF)

### **Padding with Format Specifiers**

You can also use existing formatting specifiers in conjunction with padding.

```cs
var number = 42;
var str = $"The answer to life, the universe and everything is ${number, 5:f1}";
//str is "The answer to life, the universe and everything is 42.1 ";
//                                                           ^^^^^

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/34ZxP0)



## Expressions


Full expressions can also be used in interpolated strings.

```cs
var StrWithMathExpression = $"1 + 2 = {1 + 2}"; // -> "1 + 2 = 3"

string world = "world";
var StrWithFunctionCall = $"Hello, {world.ToUpper()}!"; // -> "Hello, WORLD!"

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/u9lzeg)



## Simple Usage


```cs
var name = "World";
var str = $"Hello, {name}!";
//str now contains: "Hello, World!";

```

### Behind the scenes

Internally this

```cs
$"Hello, {name}!" 

```

Will be compiled to something like this:

```cs
string.Format("Hello, {0}!", name);

```



## Formatting numbers in strings


You can use a colon and the [standard numeric format syntax](https://msdn.microsoft.com/en-us/library/dwhawy9k.aspx) to control how numbers are formatted.

```cs
var decimalValue = 120.5;

var asCurrency = $"It costs {decimalValue:C}";
// String value is "It costs $120.50" (depending on your local currency settings)

var withThreeDecimalPlaces = $"Exactly {decimalValue:F3}";
// String value is "Exactly 120.500"

var integerValue = 57;

var prefixedIfNecessary = $"{integerValue:D5}";
// String value is "00057"

```

[Live Demo on .NET Fiddle](https://dotnetfiddle.net/z2XbG7)



#### Syntax


- $"content {expression} content"
- $"content {expression:format} content"
- $"content {expression} { {content in braces} } content}"
- $"content {expression:format} { { content in braces } } content}"



#### Remarks


String interpolation is a shorthand for the `string.Format()` method that makes it easier to build strings with variable and expression values inside of them.

```cs
var name = "World";
var oldWay = string.Format("Hello, {0}!", name);  // returns "Hello, World"
var newWay = $"Hello, {name}!";                   // returns "Hello, World"

```

