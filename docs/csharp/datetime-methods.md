---
metaTitle: "DateTime Methods"
description: "DateTime Formating, DateTime.AddDays(Double), DateTime.AddHours(Double), DateTime.Parse(String), DateTime.TryParse(String, DateTime), DateTime.AddMilliseconds(Double), DateTime.Compare(DateTime t1, 	DateTime t2 ), DateTime.DaysInMonth(Int32, Int32), DateTime.AddYears(Int32), Pure functions warning when dealing with DateTime, DateTime.TryParseExact(String, String, IFormatProvider, DateTimeStyles, DateTime), DateTime.Add(TimeSpan), Parse and TryParse with culture info, DateTime as initializer in for-loop, DateTime ToString, ToShortDateString, ToLongDateString and ToString formatted, Current Date, DateTime.ParseExact(String, String, IFormatProvider)"
---

# DateTime Methods



## DateTime Formating


**Standard DateTime Formatting**

DateTimeForma­tInfo specifies a set of specifiers for simple date and time formating. Every specifier correspond to a particular DateTimeFormatInfo format pattern.

```cs
//Create datetime
DateTime dt = new DateTime(2016,08,01,18,50,23,230);

var t = String.Format("{0:t}", dt); // "6:50 PM"                             ShortTime
var d = String.Format("{0:d}", dt); // "8/1/2016"                            ShortDate
var T = String.Format("{0:T}", dt); // "6:50:23 PM"                          LongTime
var D = String.Format("{0:D}", dt); // "Monday, August 1, 2016"              LongDate
var f = String.Format("{0:f}", dt); // "Monday, August 1, 2016 6:50 PM"      LongDate+ShortTime
var F = String.Format("{0:F}", dt); // "Monday, August 1, 2016 6:50:23 PM"   FullDateTime
var g = String.Format("{0:g}", dt); // "8/1/2016 6:50 PM"                    ShortDate+ShortTime
var G = String.Format("{0:G}", dt); // "8/1/2016 6:50:23 PM"                 ShortDate+LongTime
var m = String.Format("{0:m}", dt); // "August 1"                            MonthDay
var y = String.Format("{0:y}", dt); // "August 2016"                         YearMonth
var r = String.Format("{0:r}", dt); // "SMon, 01 Aug 2016 18:50:23 GMT"      RFC1123
var s = String.Format("{0:s}", dt); // "2016-08-01T18:50:23"                 SortableDateTime
var u = String.Format("{0:u}", dt); // "2016-08-01 18:50:23Z"                UniversalSortableDateTime

```

**Custom DateTime Formatting**

There are following custom format specifiers:

- `y` (year)
- `M` (month)
- `d` (day)
- `h` (hour 12)
- `H` (hour 24)
- `m` (minute)
- `s` (second)
- `f` (second fraction)
- `F` (second fraction, trailing zeroes are trimmed)
- `t` (P.M or A.M)
- `z` (time zone).

```cs
var year =       String.Format("{0:y yy yyy yyyy}", dt); // "16 16 2016 2016"  year
var month =      String.Format("{0:M MM MMM MMMM}", dt); // "8 08 Aug August"  month
var day =        String.Format("{0:d dd ddd dddd}", dt); // "1 01 Mon Monday"  day
var hour =       String.Format("{0:h hh H HH}",     dt); // "6 06 18 18"       hour 12/24
var minute =     String.Format("{0:m mm}",          dt); // "50 50"            minute
var secound =    String.Format("{0:s ss}",          dt); // "23 23"            second
var fraction =   String.Format("{0:f ff fff ffff}", dt); // "2 23 230 2300"    sec.fraction
var fraction2 =  String.Format("{0:F FF FFF FFFF}", dt); // "2 23 23 23"       without zeroes
var period =     String.Format("{0:t tt}",          dt); // "P PM"             A.M. or P.M.
var zone =       String.Format("{0:z zz zzz}",      dt); // "+0 +00 +00:00"    time zone

```

You can use also date separator `/` (slash) and time sepatator `:` (colon).

[For code example](https://dotnetfiddle.net/rcovMN)

For more information [MSDN](https://msdn.microsoft.com/en-us/library/system.globalization.datetimeformatinfo.aspx).



## DateTime.AddDays(Double)


Add days into a dateTime object.

```cs
DateTime today = DateTime.Now;
DateTime answer = today.AddDays(36);
Console.WriteLine("Today: {0:dddd}", today);
Console.WriteLine("36 days from today: {0:dddd}", answer);

```

You also can subtract days passing a negative value:

```cs
DateTime today = DateTime.Now;
DateTime answer = today.AddDays(-3);
Console.WriteLine("Today: {0:dddd}", today);
Console.WriteLine("-3 days from today: {0:dddd}", answer);

```



## DateTime.AddHours(Double)


```cs
double[] hours = {.08333, .16667, .25, .33333, .5, .66667, 1, 2, 
                        29, 30, 31, 90, 365};
DateTime dateValue = new DateTime(2009, 3, 1, 12, 0, 0);

foreach (double hour in hours)
  Console.WriteLine("{0} + {1} hour(s) = {2}", dateValue, hour, 
                           dateValue.AddHours(hour));

```



## DateTime.Parse(String)


```cs
// Converts the string representation of a date and time to its DateTime equivalent

var dateTime = DateTime.Parse("14:23 22 Jul 2016");

Console.WriteLine(dateTime.ToString());

```



## DateTime.TryParse(String, DateTime)


```cs
// Converts the specified string representation of a date and time to its DateTime equivalent and returns a value that indicates whether the conversion succeeded

string[] dateTimeStrings = new []{
    "14:23 22 Jul 2016",
    "99:23 2x Jul 2016",
    "22/7/2016 14:23:00"
};

foreach(var dateTimeString in dateTimeStrings){

    DateTime dateTime;
    
    bool wasParsed = DateTime.TryParse(dateTimeString, out dateTime);
    
    string result = dateTimeString +
        (wasParsed 
            ? $"was parsed to {dateTime}" 
            : "can't be parsed to DateTime");
            
    Console.WriteLine(result);
}

```



## DateTime.AddMilliseconds(Double)


```cs
string dateFormat = "MM/dd/yyyy hh:mm:ss.fffffff"; 
DateTime date1 = new DateTime(2010, 9, 8, 16, 0, 0);
Console.WriteLine("Original date: {0} ({1:N0} ticks)\n",
                  date1.ToString(dateFormat), date1.Ticks);

DateTime date2 = date1.AddMilliseconds(1);
Console.WriteLine("Second date:   {0} ({1:N0} ticks)",
                  date2.ToString(dateFormat), date2.Ticks);
Console.WriteLine("Difference between dates: {0} ({1:N0} ticks)\n",
                  date2 - date1, date2.Ticks - date1.Ticks);                        

DateTime date3 = date1.AddMilliseconds(1.5);
Console.WriteLine("Third date:    {0} ({1:N0} ticks)",
                  date3.ToString(dateFormat), date3.Ticks);
Console.WriteLine("Difference between dates: {0} ({1:N0} ticks)",
                  date3 - date1, date3.Ticks - date1.Ticks);   

```



## DateTime.Compare(DateTime t1, 	DateTime t2 )


```cs
DateTime date1 = new DateTime(2009, 8, 1, 0, 0, 0);
DateTime date2 = new DateTime(2009, 8, 1, 12, 0, 0);
int result = DateTime.Compare(date1, date2);
string relationship;

if (result < 0)
    relationship = "is earlier than";
else if (result == 0)
    relationship = "is the same time as";         
else relationship = "is later than";

Console.WriteLine("{0} {1} {2}", date1, relationship, date2);

```



## DateTime.DaysInMonth(Int32, Int32)


```cs
const int July = 7;
const int Feb = 2;

int daysInJuly = System.DateTime.DaysInMonth(2001, July);
Console.WriteLine(daysInJuly);

// daysInFeb gets 28 because the year 1998 was not a leap year.
int daysInFeb = System.DateTime.DaysInMonth(1998, Feb);
Console.WriteLine(daysInFeb);

// daysInFebLeap gets 29 because the year 1996 was a leap year.
int daysInFebLeap = System.DateTime.DaysInMonth(1996, Feb);
Console.WriteLine(daysInFebLeap);

```



## DateTime.AddYears(Int32)


Add years on the dateTime object:

```cs
DateTime baseDate = new DateTime(2000, 2, 29);
Console.WriteLine("Base Date: {0:d}\n", baseDate);

// Show dates of previous fifteen years.
for (int ctr = -1; ctr >= -15; ctr--)
   Console.WriteLine("{0,2} year(s) ago:{1:d}", 
                      Math.Abs(ctr), baseDate.AddYears(ctr));

Console.WriteLine();

// Show dates of next fifteen years.
for (int ctr = 1; ctr <= 15; ctr++)
   Console.WriteLine("{0,2} year(s) from now: {1:d}", 
                     ctr, baseDate.AddYears(ctr));

```



## Pure functions warning when dealing with DateTime


Wikipedia currently defines a pure function as follows:

1. The function always evaluates the same result value given the same argument value(s). The function result value cannot depend on any hidden information or state that may change while program execution proceeds or between different executions of the program, nor can it depend on any external input from I/O devices .
1. Evaluation of the result does not cause any semantically observable side effect or output, such as mutation of mutable objects or output to I/O devices

As a developer you need to be aware of pure methods and you will stumble upon these a lot in many areas. One I have seen that bites many junior developers is working with DateTime class methods. A lot of these are pure and if you are unaware of these you can be in for a suprise. An example:

```cs

       DateTime sample = new DateTime(2016, 12, 25);
        sample.AddDays(1);
        Console.WriteLine(sample.ToShortDateString());

```

Given the example above one may expect the result printed to console to be '26/12/2016' but in reality you end up with the same date. This is because AddDays is a pure method and does not affect the original date. To get the expected output you would have to modify the AddDays call to the following:

```cs

       sample = sample.AddDays(1);

```



## DateTime.TryParseExact(String, String, IFormatProvider, DateTimeStyles, DateTime)


Converts the specified string representation of a date and time to its DateTime equivalent using the specified format, culture-specific format information, and style. The format of the string representation must match the specified format exactly. The method returns a value that indicates whether the conversion succeeded.

For Example

```cs
CultureInfo enUS = new CultureInfo("en-US");
string dateString;
System.DateTime dateValue;

```

Parse date with no style flags.

```cs
dateString = " 5/01/2009 8:30 AM";
if (DateTime.TryParseExact(dateString, "g", enUS, DateTimeStyles.None, out dateValue))
{
   Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
}
else
{
   Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
}


// Allow a leading space in the date string.
if(DateTime.TryParseExact(dateString, "g", enUS, DateTimeStyles.AllowLeadingWhite, out dateValue))
{
   Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
else
{
   Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
}

```

Use custom formats with M and MM.

```cs
dateString = "5/01/2009 09:00";
if(DateTime.TryParseExact(dateString, "M/dd/yyyy hh:mm", enUS, DateTimeStyles.None, out dateValue))
{
    Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
}
else
{
   Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
}

// Allow a leading space in the date string.
if(DateTime.TryParseExact(dateString, "MM/dd/yyyy hh:mm", enUS, DateTimeStyles.None, out dateValue))
{
   Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
}
else
{
   Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
}

```

Parse a string with time zone information.

```cs
dateString = "05/01/2009 01:30:42 PM -05:00";
if (DateTime.TryParseExact(dateString, "MM/dd/yyyy hh:mm:ss tt zzz", enUS, DateTimeStyles.None, out dateValue))
{
    Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
}
else
{
   Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
}


// Allow a leading space in the date string.
if (DateTime.TryParseExact(dateString, "MM/dd/yyyy hh:mm:ss tt zzz", enUS, DateTimeStyles.AdjustToUniversal, out dateValue))
{
   Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
}
else
{
   Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
}

```

Parse a string represengting UTC.

```cs
dateString = "2008-06-11T16:11:20.0904778Z";
if(DateTime.TryParseExact(dateString, "o", CultureInfo.InvariantCulture, DateTimeStyles.None, out dateValue))
{
   Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
}
else
{
  Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
}

if (DateTime.TryParseExact(dateString, "o", CultureInfo.InvariantCulture, DateTimeStyles.RoundtripKind, out dateValue))
{
   Console.WriteLine("Converted '{0}' to {1} ({2}).", dateString, dateValue, dateValue.Kind);
}
else
{
   Console.WriteLine("'{0}' is not in an acceptable format.", dateString);
}

```

Outputs

```cs
' 5/01/2009 8:30 AM' is not in an acceptable format.
Converted ' 5/01/2009 8:30 AM' to 5/1/2009 8:30:00 AM (Unspecified).
Converted '5/01/2009 09:00' to 5/1/2009 9:00:00 AM (Unspecified).
'5/01/2009 09:00' is not in an acceptable format.
Converted '05/01/2009 01:30:42 PM -05:00' to 5/1/2009 11:30:42 AM (Local).
Converted '05/01/2009 01:30:42 PM -05:00' to 5/1/2009 6:30:42 PM (Utc).
Converted '2008-06-11T16:11:20.0904778Z' to 6/11/2008 9:11:20 AM (Local).
Converted '2008-06-11T16:11:20.0904778Z' to 6/11/2008 4:11:20 PM (Utc).

```



## DateTime.Add(TimeSpan)


```cs
// Calculate what day of the week is 36 days from this instant.
System.DateTime today = System.DateTime.Now;
System.TimeSpan duration = new System.TimeSpan(36, 0, 0, 0);
System.DateTime answer = today.Add(duration);
System.Console.WriteLine("{0:dddd}", answer);

```



## Parse and TryParse with culture info


You might want to use it when parsing DateTimes from [different cultures (languages)](https://msdn.microsoft.com/en-gb/library/ee825488(v=cs.20).aspx), following example parses Dutch date.

```cs
DateTime dateResult;
var dutchDateString = "31 oktober 1999 04:20";
var dutchCulture = CultureInfo.CreateSpecificCulture("nl-NL");
DateTime.TryParse(dutchDateString, dutchCulture, styles, out dateResult);
// output {31/10/1999 04:20:00}

```

Example of Parse:

```cs
DateTime.Parse(dutchDateString, dutchCulture)
// output {31/10/1999 04:20:00}

```



## DateTime as initializer in for-loop


```cs
// This iterates through a range between two DateTimes 
// with the given iterator (any of the Add methods) 

DateTime start = new DateTime(2016, 01, 01);
DateTime until = new DateTime(2016, 02, 01);

// NOTICE: As the add methods return a new DateTime you have
// to overwrite dt in the iterator like dt = dt.Add()
for (DateTime dt = start; dt < until; dt = dt.AddDays(1))
{
    Console.WriteLine("Added {0} days. Resulting DateTime: {1}", 
                      (dt - start).Days, dt.ToString());
}

```

**Iterating on a `TimeSpan` works the same way.**



## DateTime ToString, ToShortDateString, ToLongDateString and ToString formatted


```cs
using System;
                    
public class Program
{
    public static void Main()
    {
        var date = new DateTime(2016,12,31);
        
        Console.WriteLine(date.ToString());        //Outputs: 12/31/2016 12:00:00 AM
        Console.WriteLine(date.ToShortDateString()); //Outputs: 12/31/2016
        Console.WriteLine(date.ToLongDateString()); //Outputs: Saturday, December 31, 2016
        Console.WriteLine(date.ToString("dd/MM/yyyy"));    //Outputs: 31/12/2016
    }
}

```



## Current Date


To get the current date you use the `DateTime.Today` property. This returns a `DateTime` object with today's date. When this is then converted `.ToString()` it is done so in your system's locality by default.

For example:

```cs
Console.WriteLine(DateTime.Today);

```

Writes today's date, in your local format to the console.



## DateTime.ParseExact(String, String, IFormatProvider)


Converts the specified string representation of a date and time to its DateTime equivalent using the specified format and culture-specific format information. The format of the string representation must match the specified format exactly.

**Convert a specific format string to equivalent DateTime**

Let's say we have a culture-specific DateTime string `08-07-2016 11:30:12 PM` as `MM-dd-yyyy hh:mm:ss tt` format and we want it to convert to equivalent `DateTime` object

```cs
string str = "08-07-2016 11:30:12 PM";
DateTime date = DateTime.ParseExact(str, "MM-dd-yyyy hh:mm:ss tt", CultureInfo.CurrentCulture);

```

**Convert a date time string to equivalent `DateTime` object without any specific culture format**

Let's say we have a DateTime string in `dd-MM-yy hh:mm:ss tt` format and we want it to convert to equivalent `DateTime` object, without any specific culture information

```cs
string str = "17-06-16 11:30:12 PM";
DateTime date = DateTime.ParseExact(str, "dd-MM-yy hh:mm:ss tt", CultureInfo.InvariantCulture);

```

**Convert a date time string to equivalent DateTime object without any specific culture format with different format**

Let's say we have a Date string , example like '23-12-2016' or '12/23/2016' and we want it to convert to equivalent `DateTime` object, without any specific culture information

```

  string date =  '23-12-2016' or date = 12/23/2016';
   string[] formats = new string[] {"dd-MM-yyyy","MM/dd/yyyy"}; // even can add more possible formats.
   DateTime date = DateTime.ParseExact(date,formats, CultureInfo.InvariantCulture,DateTimeStyles.None);

```

**NOTE : `System.Globalization` needs to be added for CultureInfo Class**

