---
metaTitle: "C# | Enum"
description: "Enum as flags, Enum basics, Using << notation for flags, Test flags-style enum values with bitwise logic, Enum to string and back, Add and remove values from flagged enum, Default value for enum == ZERO, Adding additional description information to an enum value, Enums can have unexpected values, Get all the members values of an enum, Bitwise Manipulation using enums"
---

# Enum


An enum can derive from any of the following types: byte, sbyte, short, ushort, int, uint, long, ulong. The default is int, and can be changed by specifying the type in the enum definition:

public enum Weekday : byte { Monday = 1, Tuesday = 2, Wednesday = 3, Thursday = 4, Friday = 5 }

This is useful when P/Invoking to native code, mapping to data sources, and similar circumstances. In general, the default int should be used, because most developers expect an enum to be an int.



## Enum as flags


The `FlagsAttribute` can be applied to an enum changing the behaviour of the `ToString()` to match the nature of the enum:

```cs
[Flags]
enum MyEnum
{
    //None = 0, can be used but not combined in bitwise operations
    FlagA = 1,
    FlagB = 2,
    FlagC = 4,
    FlagD = 8  
    //you must use powers of two or combinations of powers of two 
    //for bitwise operations to work
}

var twoFlags = MyEnum.FlagA | MyEnum.FlagB;

// This will enumerate all the flags in the variable: "FlagA, FlagB".
Console.WriteLine(twoFlags);

```

Because `FlagsAttribute` relies on the enumeration constants to be powers of two (or their combinations) and enum values are ultimately numeric values, you are limited by the size of the underlying numeric type. The largest available numeric type that you can use is `UInt64`, which allows you to specify 64 distinct (non-combined) flag enum constants. The `enum` keyword defaults to the underlying type `int`, which is `Int32`. The compiler will allow the declaration of values wider than 32 bit. Those will wrap around without a warning and result in two or more enum members of the same value. Therefore, if an enum is meant to accomodate a bitset of more than 32 flags, you need to specify a bigger type explicitely:

```cs
public enum BigEnum : ulong
{
    BigValue = 1 << 63
}

```

Although flags are often only a single bit, they can be combined into named "sets" for easier use.

```cs
[Flags]
enum FlagsEnum
{
    None = 0,
    Option1 = 1,
    Option2 = 2,
    Option3 = 4,
       
    Default = Option1 | Option3,
    All = Option1 | Option2 | Option3,
}

```

To avoid spelling out the decimal values of powers of two, the [left-shift operator (<<)](https://msdn.microsoft.com/en-gb/library/a1sway8w.aspx) can also be used to declare the same enum

```cs
[Flags]
enum FlagsEnum
{
    None = 0,
    Option1 = 1 << 0,
    Option2 = 1 << 1,
    Option3 = 1 << 2,
       
    Default = Option1 | Option3,
    All = Option1 | Option2 | Option3,
}

```

Starting with C# 7.0, [binary literals](http://stackoverflow.com/documentation/c%23/1936/c-sharp-7-0-features/6327/binary-literals#t=201705181538117083427) can be used too.

To check if the value of enum variable has a certain flag set, the [`HasFlag`](https://msdn.microsoft.com/en-us/library/system.enum.hasflag(v=vs.110).aspx) method can be used. Let's say we have

```cs
[Flags]
enum MyEnum
{
    One = 1,
    Two = 2,
    Three = 4
}

```

And a `value`

```cs
var value = MyEnum.One | MyEnum.Two;

```

With `HasFlag` we can check if any of the flags is set

```cs
if(value.HasFlag(MyEnum.One))
    Console.WriteLine("Enum has One");

if(value.HasFlag(MyEnum.Two))
    Console.WriteLine("Enum has Two");

if(value.HasFlag(MyEnum.Three))
    Console.WriteLine("Enum has Three");

```

Also we can iterate through all values of enum to get all flags that are set

```cs
var type = typeof(MyEnum);
var names = Enum.GetNames(type);

foreach (var name in names)
{
    var item = (MyEnum)Enum.Parse(type, name);

    if (value.HasFlag(item))
        Console.WriteLine("Enum has " + name);
}

```

Or

```cs
foreach(MyEnum flagToCheck in Enum.GetValues(typeof(MyEnum)))
{
    if(value.HasFlag(flagToCheck))
    {
         Console.WriteLine("Enum has " + flagToCheck);
    }
}

```

All three examples will print:

```cs
Enum has One
Enum has Two

```



## Enum basics


From [MSDN](https://msdn.microsoft.com/en-us/library/cc138362.aspx):

> 
An enumeration type (also named an enumeration or an enum) provides an efficient way to define a set of named **integral constants** that may be **assigned to a variable**.


Essentially, an enum is a type that only allows a set of finite options, and each option corresponds to a number. By default, those numbers are increasing in the order the values are declared, starting from zero. For example, one could declare an enum for the days of the week:

```cs
public enum Day
{
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday,
    Sunday
}

```

That enum could be used like this:

```cs
// Define variables with values corresponding to specific days
Day myFavoriteDay = Day.Friday;
Day myLeastFavoriteDay = Day.Monday;

// Get the int that corresponds to myFavoriteDay
// Friday is number 4
int myFavoriteDayIndex = (int)myFavoriteDay;

// Get the day that represents number 5
Day dayFive = (Day)5;

```

By default the underlying type of each element in the `enum` is `int`, but `byte`, `sbyte`, `short`, `ushort`, `uint`, `long` and `ulong` can be used as well. If you use a type other than `int`, you must specify the type using a colon after the enum name:

```cs
public enum Day : byte 
{
    // same as before 
}

```

The numbers after the name are now bytes instead of integers. You could get the underlying type of the enum as follows:

```cs
Enum.GetUnderlyingType(typeof(Days)));

```

Output:

```cs
System.Byte

```

Demo: [.NET fiddle](https://dotnetfiddle.net/EGi301)



## Using << notation for flags


The left-shift operator (`<<`) can be used in flag enum declarations to ensure that each flag has exactly one `1` in binary representation, as flags should.

This also helps to improve readability of large enums with plenty of flags in them.

```cs
[Flags]
public enum MyEnum 
{
    None  = 0,
    Flag1 = 1 << 0,
    Flag2 = 1 << 1,
    Flag3 = 1 << 2,
    Flag4 = 1 << 3,
    Flag5 = 1 << 4,
    ...
    Flag31 = 1 << 30
}

```

It is obvious now that `MyEnum` contains proper flags only and not any messy stuff like `Flag30 = 1073741822` (or 111111111111111111111111111110 in binary) which is inappropriate.



## Test flags-style enum values with bitwise logic


A flags-style enum value needs to be tested with bitwise logic because it may not match any single value.

```cs
[Flags]
enum FlagsEnum
{
    Option1 = 1,
    Option2 = 2,
    Option3 = 4,
    Option2And3 = Option2 | Option3;

    Default = Option1 | Option3,
}

```

The `Default` value is actually a combination of two others **merged** with a bitwise OR. Therefore to test for the presence of a flag we need to use a bitwise AND.

```cs
var value = FlagsEnum.Default;

bool isOption2And3Set = (value & FlagsEnum.Option2And3) == FlagsEnum.Option2And3;

Assert.True(isOption2And3Set);

```



## Enum to string and back


```cs
public enum DayOfWeek
{
    Sunday,
    Monday,
    Tuesday,
    Wednesday,
    Thursday,
    Friday,
    Saturday
}

    
// Enum to string
string thursday = DayOfWeek.Thursday.ToString(); // "Thursday"

string seventhDay = Enum.GetName(typeof(DayOfWeek), 6); // "Saturday"

string monday = Enum.GetName(typeof(DayOfWeek), DayOfWeek.Monday); // "Monday"


// String to enum (.NET 4.0+ only - see below for alternative syntax for earlier .NET versions)
DayOfWeek tuesday;
Enum.TryParse("Tuesday", out tuesday); // DayOfWeek.Tuesday

DayOfWeek sunday;
bool matchFound1 = Enum.TryParse("SUNDAY", out sunday); // Returns false (case-sensitive match)

DayOfWeek wednesday;
bool matchFound2 = Enum.TryParse("WEDNESDAY", true, out wednesday); // Returns true; DayOfWeek.Wednesday (case-insensitive match)


// String to enum (all .NET versions)
DayOfWeek friday = (DayOfWeek)Enum.Parse(typeof(DayOfWeek), "Friday"); // DayOfWeek.Friday

DayOfWeek caturday = (DayOfWeek)Enum.Parse(typeof(DayOfWeek), "Caturady"); // Thows ArgumentException

// All names of an enum type as strings
string[] weekdays = Enum.GetNames(typeof(DayOfWeek));

```



## Add and remove values from flagged enum


This code is to  add and remove a value from a flagged enum-instance:

```cs
[Flags]
public enum MyEnum
{
    Flag1 = 1 << 0,
    Flag2 = 1 << 1,
    Flag3 = 1 << 2
}

var value = MyEnum.Flag1;

// set additional value
value |= MyEnum.Flag2;  //value is now Flag1, Flag2
value |= MyEnum.Flag3;  //value is now Flag1, Flag2, Flag3

// remove flag
value &= ~MyEnum.Flag2; //value is now Flag1, Flag3    

```



## Default value for enum == ZERO


**The default value for an enum is zero**.  If an enum does not define an item with a value of zero, its default value will be zero.

```cs
public class Program
{        
    enum EnumExample
    {
        one = 1,
        two = 2
    }
    
    public void Main()
    {              
        var e = default(EnumExample);
        
        if (e == EnumExample.one)
            Console.WriteLine("defaults to one");
        else
            Console.WriteLine("Unknown");    
    }    
}

```

Example:
[https://dotnetfiddle.net/l5Rwie](https://dotnetfiddle.net/l5Rwie)



## Adding additional description information to an enum value


In some cases you might want to add an additional description to an enum value, for instance when the enum value itself is less readable than what you might want to display to the user. In such cases you can use the [`System.ComponentModel.DescriptionAttribute`](https://msdn.microsoft.com/en-us/library/system.componentmodel.descriptionattribute(v=vs.110).aspx) class.

For example:

```cs
public enum PossibleResults
{
    [Description("Success")]
    OK = 1,
    [Description("File not found")]
    FileNotFound = 2,
    [Description("Access denied")]
    AccessDenied = 3
}

```

Now, if you would like to return the description of a specific enum value you can do the following:

```cs
public static string GetDescriptionAttribute(PossibleResults result)
{
        return ((DescriptionAttribute)Attribute.GetCustomAttribute((result.GetType().GetField(result.ToString())), typeof(DescriptionAttribute))).Description;
}

static void Main(string[] args)
{
    PossibleResults result = PossibleResults.FileNotFound;
    Console.WriteLine(result); // Prints "FileNotFound"
    Console.WriteLine(GetDescriptionAttribute(result)); // Prints "File not found"
}

```

This can also be easily transformed to an extension method for all enums:

```cs
static class EnumExtensions
{
    public static string GetDescription(this Enum enumValue)
    {
        return ((DescriptionAttribute)Attribute.GetCustomAttribute((enumValue.GetType().GetField(enumValue.ToString())), typeof(DescriptionAttribute))).Description;
    }
}

```

And then easily used like this:
`Console.WriteLine(result.GetDescription());`



## Enums can have unexpected values


Since an enum can be cast to and from its underlying integral type, the value may fall outside the range of values given in the definition of the enum type.

Although the below enum type `DaysOfWeek` only has 7 defined values, it can still hold any `int` value.

```cs
public enum DaysOfWeek
{
    Monday = 1,
    Tuesday = 2,
    Wednesday = 3,
    Thursday = 4,
    Friday = 5,
    Saturday = 6,
    Sunday = 7
}

DaysOfWeek d = (DaysOfWeek)31;
Console.WriteLine(d); // prints 31

DaysOFWeek s = DaysOfWeek.Sunday;
s++; // No error

```

There is currently no way to define an enum which does not have this behavior.

However, undefined enum values can be detected by using the method `Enum.IsDefined`. For example,

```cs
DaysOfWeek d = (DaysOfWeek)31;
Console.WriteLine(Enum.IsDefined(typeof(DaysOfWeek),d)); // prints False

```



## Get all the members values of an enum


```cs
enum MyEnum
{
    One,
    Two,
    Three
}

foreach(MyEnum e in Enum.GetValues(typeof(MyEnum)))
    Console.WriteLine(e);

```

This will print:

```cs
One
Two
Three

```



## Bitwise Manipulation using enums


The [FlagsAttribute](https://msdn.microsoft.com/en-us/library/system.flagsattribute(v=vs.110).aspx) should be used whenever the enumerable represents a collection of flags, rather than a single value.
The numeric value assigned to each enum value helps when manipulating enums using bitwise operators.

**Example 1 : With [Flags]**

```cs
[Flags]
enum Colors
{
    Red=1,
    Blue=2,
    Green=4,
    Yellow=8
}

var color = Colors.Red | Colors.Blue;
Console.WriteLine(color.ToString());

```

> 
prints Red,Blue


**<strong>Example 2 : Without [Flags]**</strong>

```cs
enum Colors
{
    Red=1,
    Blue=2,
    Green=4,
    Yellow=8
}
var color = Colors.Red | Colors.Blue;
Console.WriteLine(color.ToString());

```

> 
prints 3




#### Syntax


- enum Colors { Red, Green, Blue } // Enum declaration
- enum Colors : byte { Red, Green, Blue } // Declaration with specific type
- enum Colors { Red = 23, Green = 45, Blue = 12 } // Declaration with defined values
- Colors.Red // Access an element of an Enum
- int value = (int)Colors.Red // Get the int value of an enum element
- Colors color = (Colors)intValue // Get an enum element from int



#### Remarks


An Enum (short for "enumerated type") is a type consisting of a set of named constants, represented by a type-specific identifier.

Enums are most useful for representing concepts that have a (usually small) number of possible discrete values. For example, they can be used to represent a day of the week or a month of the year. They can be also be used as flags which can be combined or checked for, using bitwise operations.

