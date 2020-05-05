---
metaTitle: "C# | Naming Conventions"
description: "Capitalization conventions, Enums, Interfaces, Exceptions, Private fields, Namespaces"
---

# Naming Conventions


This topic outlines some basic naming conventions used when writing in the C# language. Like all conventions, they are not enforced by the compiler, but will ensure readability between developers.

For comprehensive .NET framework design guidelines, see [docs.microsoft.com/dotnet/standard/design-guidelines](https://docs.microsoft.com/dotnet/standard/design-guidelines/).



## Capitalization conventions


The following terms describe different ways to case identifiers.

### Pascal Casing

The first letter in the identifier and the first letter of each subsequent concatenated word are capitalized. You can use Pascal case for identifiers of three or more characters. For example: `BackColor`

### Camel Casing

The first letter of an identifier is lowercase and the first letter of each subsequent concatenated word is capitalized. For example: `backColor`

### Uppercase

All letters in the identifier are capitalized. For example: `IO`

### Rules

When an identifier consists of multiple words, do not use separators, such as underscores ("_") or hyphens ("-"), between words. Instead, use casing to indicate the beginning of each word.

The following table summarizes the capitalization rules for identifiers and provides examples for the different types of identifiers:

|Identifier|Case|Example
|---|---|---|---|---|---|---|---|---|---
|Local variable|Camel|carName
|Class|Pascal|AppDomain
|Enumeration type|Pascal|ErrorLevel
|Enumeration values|Pascal|FatalError
|Event|Pascal|ValueChanged
|Exception class|Pascal|WebException
|Read-only static field|Pascal|RedValue
|Interface|Pascal|IDisposable
|Method|Pascal|ToString
|Namespace|Pascal|System.Drawing
|Parameter|Camel|typeName
|Property|Pascal|BackColor

More information can be found on [MSDN](https://msdn.microsoft.com/library/ms229043(v=vs.110).aspx).



## Enums


### Use a singular name for most Enums

```cs
public enum Volume
{
   Low,
   Medium,
   High
}

```

### Use a plural name for Enum types that are bit fields

```cs
[Flags]
public enum MyColors
{
    Yellow = 1,
    Green = 2,
    Red = 4,
    Blue = 8
}

```

**Note: Always add the [`FlagsAttribute`](https://msdn.microsoft.com/en-us/library/system.flagsattribute(v=vs.110).aspx) to a bit field Enum type.**

### Do **not** add 'enum' as a suffix

```cs
public enum VolumeEnum // Incorrect

```

### Do **not** use the enum name in each entry

```cs
public enum Color
{
    ColorBlue, // Remove Color, unnecessary
    ColorGreen,
}

```



## Interfaces


Interfaces should be named with nouns or noun phrases, or adjectives that describe behaviour. For example `IComponent` uses a descriptive noun, `ICustomAttributeProvider` uses a noun phrase and `IPersistable` uses an adjective.

Interface names should be prefixed with the letter `I`, to indicate that the type is an interface, and Pascal case should be used.

Below are correctly named interfaces:

```cs
public interface IServiceProvider
public interface IFormatable

```



## Exceptions


### Add 'exception' as a suffix

Custom exception names should be suffixed with "-Exception".

Below are correctly named exceptions:

```cs
public class MyCustomException : Exception
public class FooException : Exception

```



## Private fields


There are two common conventions for private fields: `camelCase` and `_camelCaseWithLeadingUnderscore`.

### Camel case

```cs
public class Rational
{
    private readonly int numerator;
    private readonly int denominator;

    public Rational(int numerator, int denominator)
    {
        // "this" keyword is required to refer to the class-scope field
        this.numerator = numerator;
        this.denominator = denominator;
    }
}

```

### Camel case with underscore

```cs
public class Rational
{
    private readonly int _numerator;
    private readonly int _denominator;

    public Rational(int numerator, int denominator)
    {
        // Names are unique, so "this" keyword is not required
        _numerator = numerator;
        _denominator = denominator;
    }
}

```



## Namespaces


The general format for namespaces is:

```cs
<Company>.(<Product>|<Technology>)[.<Feature>][.<Subnamespace>].

```

Examples include:

```cs
Fabrikam.Math
Litware.Security

```

Prefixing namespace names with a company name prevents namespaces from different companies from having the same name.



#### Remarks


### Choose easily readable identifier names

For example, a property named HorizontalAlignment is more readable in English than AlignmentHorizontal.

### Favor readability over brevity

The property name `CanScrollHorizontally` is better than `ScrollableX` (an obscure reference to the X-axis).

Avoid using underscores, hyphens, or any other non-alphanumeric characters.

### Do **not** use Hungarian notation

Hungarian notation is the practice of including a prefix in identifiers to encode some metadata about the parameter, such as the data type of the identifier, e.g. `string strName`.

Also, avoid using identifiers that conflict with keywords already used within C#.

### Abbreviations and acronyms

In general, you should not use abbreviations or acronyms; these make your names less readable. Similarly, it is difficult to know when it is safe to assume that an acronym is widely recognized.

