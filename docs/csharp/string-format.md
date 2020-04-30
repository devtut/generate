---
metaTitle: "String.Format"
description: "Since C# 6.0, Places where String.Format is 'embedded' in the framework, Create a custom format provider, Date Formatting, Currency Formatting, Using custom number format, Align left/ right, pad with spaces, Numeric formats, Escaping curly brackets inside a String.Format() expression, ToString(), Relationship with ToString()"
---

# String.Format


The `Format` methods are a set of [overloads](https://msdn.microsoft.com/en-us/library/system.string.format(v=vs.110).aspx) in the [`System.String`](https://msdn.microsoft.com/en-us/library/system.string(v=vs.110).aspx) class used to create strings that combine objects into specific string representations. This information can be applied to [`String.Format`](https://msdn.microsoft.com/en-us/library/system.string.format(v=vs.110).aspx), various `WriteLine` methods as well as other methods in the .NET framework.



## Since C# 6.0


Since C# 6.0 it is possible to use string interpolation in place of `String.Format`.

```cs
string name = "John";
string lastname = "Doe";
Console.WriteLine($"Hello {name} {lastname}!");

```

> 
Hello John Doe!


<sup>More examples for this under the topic C# 6.0 features: [String interpolation](http://stackoverflow.com/documentation/c%23/24/c-sharp-6-0-features/49/string-interpolation#t=201607220912379524818).</sup>



## Places where String.Format is 'embedded' in the framework


There are several places where you can use `String.Format` **indirectly**: The secret is to look for the overload with the signature `string format, params object[] args`, e.g.:

```cs
Console.WriteLine(String.Format("{0} - {1}", name, value));

```

Can be replaced with shorter version:

```cs
Console.WriteLine("{0} - {1}", name, value);

```

There are other methods which also use `String.Format`e.g.:

```cs
Debug.WriteLine(); // and Print()
StringBuilder.AppendFormat();

```



## Create a custom format provider


```cs
public class CustomFormat : IFormatProvider, ICustomFormatter
{
    public string Format(string format, object arg, IFormatProvider formatProvider)
    {
        if (!this.Equals(formatProvider))
        {
            return null;
        }

        if (format == "Reverse")
        {
            return String.Join("", arg.ToString().Reverse());
        }

        return arg.ToString();
    }

    public object GetFormat(Type formatType)
    {
        return formatType==typeof(ICustomFormatter) ? this:null;
    }
}

```

Usage:

```cs
String.Format(new CustomFormat(), "-> {0:Reverse} <-", "Hello World");

```

Output:

```cs
-> dlroW olleH <-

```



## Date Formatting


```cs
DateTime date = new DateTime(2016, 07, 06, 18, 30, 14);
// Format: year, month, day hours, minutes, seconds

Console.Write(String.Format("{0:dd}",date)); 

//Format by Culture info
String.Format(new System.Globalization.CultureInfo("mn-MN"),"{0:dddd}",date);

```

```cs
Console.Write($"{date:ddd}");

```

output :

```cs
06
Лхагва
06

```

|Specifier|Meaning|Sample|Result
|------
|d|Date|`{0:d}`|7/6/2016
|dd|Day, zero-padded|`{0:dd}`|06
|ddd|Short day name|`{0:ddd}`|Wed
|dddd|Full day name|`{0:dddd}`|Wednesday
|D|Long date|`{0:D}`|Wednesday, July 6, 2016
|f|Full date and time, short|`{0:f}`|Wednesday, July 6, 2016 6:30 PM
|ff|Second fractions, 2 digits|`{0:ff}`|20
|fff|Second fractions, 3 digits|`{0:fff}`|201
|ffff|Second fractions, 4 digits|`{0:ffff}`|2016
|F|Full date and time, long|`{0:F}`|Wednesday, July 6, 2016 6:30:14 PM
|g|Default date and time|`{0:g}`|7/6/2016 6:30 PM
|gg|Era|`{0:gg}`|A.D
|hh|Hour (2 digits, 12H)|`{0:hh}`|06
|HH|Hour (2 digits, 24H)|`{0:HH}`|18
|M|Month and day|`{0:M}`|July 6
|mm|Minutes, zero-padded|`{0:mm}`|30
|MM|Month, zero-padded|`{0:MM}`|07
|MMM|3-letter month name|`{0:MMM}`|Jul
|MMMM|Full month name|`{0:MMMM}`|July
|ss|Seconds|`{0:ss}`|14
|r|RFC1123 date|`{0:r}`|Wed, 06 Jul 2016 18:30:14 GMT
|s|Sortable date string|`{0:s}`|2016-07-06T18:30:14
|t|Short time|`{0:t}`|6:30 PM
|T|Long time|`{0:T}`|6:30:14 PM
|tt|AM/PM|`{0:tt}`|PM
|u|Universal sortable local time|`{0:u}`|2016-07-06 18:30:14Z
|U|Universal GMT|`{0:U}`|Wednesday, July 6, 2016 9:30:14 AM
|Y|Month and year|`{0:Y}`|July 2016
|yy|2 digit year|`{0:yy}`|16
|yyyy|4 digit year|`{0:yyyy}`|2016
|zz|2 digit timezone offset|`{0:zz}`|+09
|zzz|full time zone offset|`{0:zzz}`|+09:00



## Currency Formatting


The "c" (or currency) format specifier converts a number to a string that represents a currency amount.

```cs
string.Format("{0:c}", 112.236677) // $112.23 - defaults to system

```

### Precision

Default is 2. Use c1, c2, c3 and so on to control precision.

```cs
string.Format("{0:C1}", 112.236677) //$112.2
string.Format("{0:C3}", 112.236677) //$112.237
string.Format("{0:C4}", 112.236677) //$112.2367
string.Format("{0:C9}", 112.236677) //$112.236677000

```

### Currency Symbol

1. Pass `CultureInfo` instance to use custom culture symbol.

```cs
string.Format(new CultureInfo("en-US"), "{0:c}", 112.236677); //$112.24
string.Format(new CultureInfo("de-DE"), "{0:c}", 112.236677); //112,24 €
string.Format(new CultureInfo("hi-IN"), "{0:c}", 112.236677); //₹ 112.24

```


1. Use any string as currency symbol. Use `NumberFormatInfo` as to customize currency symbol.

```cs
NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;
nfi = (NumberFormatInfo) nfi.Clone();
nfi.CurrencySymbol = "?";
string.Format(nfi, "{0:C}", 112.236677); //?112.24
nfi.CurrencySymbol = "?%^&";
string.Format(nfi, "{0:C}", 112.236677); //?%^&112.24

```

### Position of Currency Symbol

Use [CurrencyPositivePattern](https://msdn.microsoft.com/en-us/library/system.globalization.numberformatinfo.currencypositivepattern(v=vs.110).aspx) for positive values and [CurrencyNegativePattern](https://msdn.microsoft.com/en-us/library/system.globalization.numberformatinfo.currencynegativepattern(v=vs.110).aspx) for negative values.

```cs
NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;        
nfi.CurrencyPositivePattern = 0;
string.Format(nfi, "{0:C}", 112.236677); //$112.24 - default
nfi.CurrencyPositivePattern = 1;
string.Format(nfi, "{0:C}", 112.236677); //112.24$
nfi.CurrencyPositivePattern = 2;
string.Format(nfi, "{0:C}", 112.236677); //$ 112.24
nfi.CurrencyPositivePattern = 3; 
string.Format(nfi, "{0:C}", 112.236677); //112.24 $

```

Negative pattern usage is the same as positive pattern. A lot more use cases please refer to original link.

### Custom Decimal Separator

```cs
NumberFormatInfo nfi = new CultureInfo( "en-US", false ).NumberFormat;        
nfi.CurrencyPositivePattern = 0;
nfi.CurrencyDecimalSeparator = "..";
string.Format(nfi, "{0:C}", 112.236677); //$112..24

```



## Using custom number format


`NumberFormatInfo` can be used for formatting both integer and float numbers.

```cs
// invariantResult is "1,234,567.89"
var invarianResult = string.Format(CultureInfo.InvariantCulture, "{0:#,###,##}", 1234567.89);

// NumberFormatInfo is one of classes that implement IFormatProvider
var customProvider = new NumberFormatInfo
{
    NumberDecimalSeparator = "_NS_", // will be used instead of ','
    NumberGroupSeparator = "_GS_", // will be used instead of '.'
};

// customResult is "1_GS_234_GS_567_NS_89"
var customResult = string.Format(customProvider, "{0:#,###.##}", 1234567.89);

```



## Align left/ right, pad with spaces


The second value in the curly braces dictates the length of the replacement string.
By adjusting the second value to be positive or negative, the alignment of the string can be changed.

```cs
string.Format("LEFT:  string: ->{0,-5}<- int: ->{1,-5}<-", "abc", 123);
string.Format("RIGHT: string: ->{0,5}<- int: ->{1,5}<-", "abc", 123);

```

Output:

```cs
LEFT:  string: ->abc  <- int: ->123  <-
RIGHT: string: ->  abc<- int: ->  123<-

```



## Numeric formats


```cs
// Integral types as hex
string.Format("Hexadecimal: byte2: {0:x2}; byte4: {0:X4}; char: {1:x2}", 123, (int)'A');

// Integers with thousand separators
string.Format("Integer, thousand sep.: {0:#,#}; fixed length: >{0,10:#,#}<", 1234567);

// Integer with leading zeroes
string.Format("Integer, leading zeroes: {0:00}; ", 1);

// Decimals
string.Format("Decimal, fixed precision: {0:0.000}; as percents: {0:0.00%}", 0.12);

```

Output:

```cs
Hexadecimal: byte2: 7b; byte4: 007B; char: 41
Integer, thousand sep.: 1,234,567; fixed length: > 1,234,567<
Integer, leading zeroes: 01; 
Decimal, fixed precision: 0.120; as percents: 12.00%

```



## Escaping curly brackets inside a String.Format() expression


```cs
string outsidetext = "I am outside of bracket";
string.Format("{{I am in brackets!}} {0}", outsidetext);

//Outputs "{I am in brackets!} I am outside of bracket"

```



## ToString()


The ToString() method is present on all reference object types. This is due to all reference types being derived from Object which has the ToString() method on it. The ToString() method on the object base class returns the type name. The fragment below will print out "User" to the console.

```cs
public class User
{
    public string Name { get; set; }
    public int Id { get; set; }
}

...

var user = new User {Name = "User1", Id = 5};
Console.WriteLine(user.ToString());

```

However, the class User can also override ToString() in order to alter the string it returns. The code fragment below prints out "Id: 5, Name: User1" to the console.

```cs
public class User
{
    public string Name { get; set; }
    public int Id { get; set; }
    public override ToString()
    {
        return string.Format("Id: {0}, Name: {1}", Id, Name);
    }
}

...

var user = new User {Name = "User1", Id = 5};
Console.WriteLine(user.ToString());

```



## Relationship with ToString()


While the `String.Format()` method is certainly useful in formatting data as strings, it may often be a bit overkill, especially when dealing with a single object as seen below :

```cs
String.Format("{0:C}", money);  // yields "$42.00"

```

An easier approach might be to simply use the `ToString()` method available on all objects within C#. It supports all of the same [standard and custom formatting strings](https://msdn.microsoft.com/en-us/library/dwhawy9k(v=vs.110).aspx), but doesn't require the necessary parameter mapping as there will only be a single argument :

```cs
money.ToString("C");  // yields "$42.00"

```

### **Caveats & Formatting Restrictions**

While this approach may be simpler in some scenarios, the `ToString()` approach is limited with regards to adding left or right padding like you might do within the `String.Format()` method :

```cs
String.Format("{0,10:C}", money);  // yields "    $42.00"

```

In order to accomplish this same behavior with the `ToString()` method, you would need to use another method like `PadLeft()` or `PadRight()` respectively :

```cs
money.ToString("C").PadLeft(10);  // yields "    $42.00"

```



#### Syntax


- string.Format(string format, params object[] args)
- string.Format(IFormatProvider provider, string format, params object[] args)
- $"string {text} blablabla" // Since C#6



#### Parameters


|Parameter|Details
|------
|format|A [composite format string](https://msdn.microsoft.com/en-us/library/txafckwd(v=vs.110).aspx), which defines the way **args** should be combined into a string.
|args|A sequence of objects to be combined into a string. Since this uses a [`params`](https://stackoverflow.com/documentation/c%23/26/keywords/2513/params#t=201607212143476676934) argument, you can either use a comma-separated list of arguments or an actual object array.
|provider|A collection of ways of formatting objects to strings. Typical values include [CultureInfo.InvariantCulture](https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.invariantculture(v=vs.110).aspx) and [CultureInfo.CurrentCulture](https://msdn.microsoft.com/en-us/library/system.globalization.cultureinfo.currentculture(v=vs.110).aspx).



#### Remarks


Notes:

- `String.Format()` handles `null` arguments without throwing an exception.
- There are overloads that replace the `args` parameter with one, two, or three object parameters.

