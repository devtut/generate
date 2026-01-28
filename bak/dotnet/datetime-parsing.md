---
metaTitle: ".NET Framework - DateTime parsing"
description: "ParseExact, TryParse, TryParseExact"
---

# DateTime parsing



## ParseExact


```dotnet
var dateString = "2015-11-24";

var date = DateTime.ParseExact(dateString, "yyyy-MM-dd", null);
Console.WriteLine(date);

```

> 
11/24/2015 12:00:00 AM


Note that passing `CultureInfo.CurrentCulture` as the third parameter is identical to passing `null`. Or, you can pass a specific culture.

**Format Strings**

**Input string can be in any format that matches the format string**

```dotnet
var date = DateTime.ParseExact("24|201511", "dd|yyyyMM", null);
Console.WriteLine(date);

```

> 
11/24/2015 12:00:00 AM


**Any characters that are not format specifiers are treated as literals**

```dotnet
var date = DateTime.ParseExact("2015|11|24", "yyyy|MM|dd", null);
Console.WriteLine(date);

```

> 
11/24/2015 12:00:00 AM


**Case matters for format specifiers**

```dotnet
var date = DateTime.ParseExact("2015-01-24 11:11:30", "yyyy-mm-dd hh:MM:ss", null);
Console.WriteLine(date);

```

> 
11/24/2015 11:01:30 AM


Note that the month and minute values were parsed into the wrong destinations.

**Single-character format strings must be one of the standard formats**

```dotnet
var date = DateTime.ParseExact("11/24/2015", "d", new CultureInfo("en-US"));
var date = DateTime.ParseExact("2015-11-24T10:15:45", "s", null);
var date = DateTime.ParseExact("2015-11-24 10:15:45Z", "u", null);

```

**Exceptions**

**ArgumentNullException**

```dotnet
var date = DateTime.ParseExact(null, "yyyy-MM-dd", null);
var date = DateTime.ParseExact("2015-11-24", null, null);

```

**FormatException**

```dotnet
var date = DateTime.ParseExact("", "yyyy-MM-dd", null);
var date = DateTime.ParseExact("2015-11-24", "", null);
var date = DateTime.ParseExact("2015-0C-24", "yyyy-MM-dd", null);
var date = DateTime.ParseExact("2015-11-24", "yyyy-QQ-dd", null);

// Single-character format strings must be one of the standard formats
var date = DateTime.ParseExact("2015-11-24", "q", null);

// Format strings must match the input exactly* (see next section)
var date = DateTime.ParseExact("2015-11-24", "d", null); // Expects 11/24/2015 or 24/11/2015 for most cultures

```

**Handling multiple possible formats**

```dotnet
var date = DateTime.ParseExact("2015-11-24T10:15:45", 
  new [] { "s", "t", "u", "yyyy-MM-dd" }, // Will succeed as long as input matches one of these
  CultureInfo.CurrentCulture, DateTimeStyles.None);

```

**Handling culture differences**

```dotnet
var dateString = "10/11/2015";
var date = DateTime.ParseExact(dateString, "d", new CultureInfo("en-US"));
Console.WriteLine("Day: {0}; Month: {1}", date.Day, date.Month);

```

> 
Day: 11; Month: 10


```dotnet
date = DateTime.ParseExact(dateString, "d", new CultureInfo("en-GB"));
Console.WriteLine("Day: {0}; Month: {1}", date.Day, date.Month);

```

> 
Day: 10; Month: 11




## TryParse


This method accepts a string as input, attempts to parse it into a `DateTime`, and returns a Boolean result indicating success or failure. If the call succeeds, the variable passed as the `out` parameter is populated with the parsed result.

If the parse fails, the variable passed as the `out` parameter is set to the default value, `DateTime.MinValue`.

**TryParse(string, out DateTime)**

```dotnet
DateTime parsedValue;

if (DateTime.TryParse("monkey", out parsedValue))
{
   Console.WriteLine("Apparently, 'monkey' is a date/time value. Who knew?");
}

```

This method attempts to parse the input string based on the system regional settings and known formats such as ISO 8601 and other common formats.

```dotnet
DateTime.TryParse("11/24/2015 14:28:42", out parsedValue); // true
DateTime.TryParse("2015-11-24 14:28:42", out parsedValue); // true
DateTime.TryParse("2015-11-24T14:28:42", out parsedValue); // true
DateTime.TryParse("Sat, 24 Nov 2015 14:28:42", out parsedValue); // true

```

Since this method does not accept culture info, it uses the system locale. This can lead to unexpected results.

```dotnet
// System set to en-US culture
bool result = DateTime.TryParse("24/11/2015", out parsedValue);
Console.WriteLine(result);

```

> 
False


```dotnet
// System set to en-GB culture
bool result = DateTime.TryParse("11/24/2015", out parsedValue);
Console.WriteLine(result);

```

> 
False


```dotnet
// System set to en-GB culture
bool result = DateTime.TryParse("10/11/2015", out parsedValue);
Console.WriteLine(result);

```

> 
True


Note that if you are in the US, you might be surprised that the parsed result is November 10, not October 11.

**TryParse(string, IFormatProvider, DateTimeStyles, out DateTime)**

```dotnet
if (DateTime.TryParse(" monkey ", new CultureInfo("en-GB"),
    DateTimeStyles.AllowLeadingWhite | DateTimeStyles.AllowTrailingWhite, out parsedValue)
{
    Console.WriteLine("Apparently, ' monkey ' is a date/time value. Who knew?");
}

```

Unlike its sibling method, this overload allows a specific culture and style(s) to be specified. Passing `null` for the `IFormatProvider` parameter uses the system culture.

**Exceptions**

Note that it is possible for this method to throw an exception under certain conditions. These relate to the parameters introduced for this overload: `IFormatProvider` and `DateTimeStyles`.

- `NotSupportedException`: `IFormatProvider` specifies a neutral culture
- `ArgumentException`: `DateTimeStyles` is not a valid option, or contains incompatible flags such as `AssumeLocal` and `AssumeUniversal`.



## TryParseExact


This method behaves as a combination of `TryParse` and `ParseExact`: It allows custom format(s) to be specified, and returns a Boolean result indicating success or failure rather than throwing an exception if the parse fails.

**TryParseExact(string, string, IFormatProvider, DateTimeStyles, out DateTime)**

This overload attempts to parse the input string against a specific format. The input string must match that format in order to be parsed.

```dotnet
DateTime.TryParseExact("11242015", "MMddyyyy", null, DateTimeStyles.None, out parsedValue); // true

```

**TryParseExact(string, string[], IFormatProvider, DateTimeStyles, out DateTime)**

This overload attempts to parse the input string against an array of formats. The input string must match at least one format in order to be parsed.

```dotnet
DateTime.TryParseExact("11242015", new [] { "yyyy-MM-dd", "MMddyyyy" }, null, DateTimeStyles.None, out parsedValue); // true

```

