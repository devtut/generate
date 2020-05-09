---
metaTitle: "VBA - Date Time Manipulation"
description: "Calendar, Base functions, Extraction functions, Calculation functions, Conversion and Creation"
---

# Date Time Manipulation



## Calendar


VBA supports 2 calendars : [Gregorian](https://en.wikipedia.org/wiki/Gregorian_calendar) and [Hijri](https://en.wikipedia.org/wiki/Islamic_calendar)

The `Calendar` property is used to modify or display the current calendar.

The 2 values for the Calendar are:

|Value|Constant|Description
|---|---|---|---|---|---|---|---|---|---
|0|vbCalGreg|Gregorian calendar (default)
|1|vbCalHijri|Hijri calendar

### Example

```vb
Sub CalendarExample()
    'Cache the current setting.
    Dim Cached As Integer
    Cached = Calendar

    ' Dates in Gregorian Calendar
    Calendar = vbCalGreg
    Dim Sample As Date
    'Create sample date of 2016-07-28
    Sample = DateSerial(2016, 7, 28)

    Debug.Print "Current Calendar : " & Calendar
    Debug.Print "SampleDate = " & Format$(Sample, "yyyy-mm-dd")
    
    ' Date in Hijri Calendar
    Calendar = vbCalHijri
    Debug.Print "Current Calendar : " & Calendar
    Debug.Print "SampleDate = " & Format$(Sample, "yyyy-mm-dd")
    
    'Reset VBA to cached value.
    Cached = Calendar
End Sub

```

This Sub prints the following ;

```vb
Current Calendar : 0
SampleDate = 2016-07-28
Current Calendar : 1
SampleDate = 1437-10-23

```



## Base functions


### Retrieve System DateTime

VBA supports 3 built-in functions to retrieve the date and/or time from the system's clock.

|Function|Return Type|Return Value
|---|---|---|---|---|---|---|---|---|---
|Now|Date|Returns the current date and time
|Date|Date|Returns the date portion of the current date and time
|Time|Date|Returns the time portion of the current date and time

```vb
Sub DateTimeExample()

    ' -----------------------------------------------------
    ' Note : EU system with default date format DD/MM/YYYY
    ' -----------------------------------------------------
    
    Debug.Print Now   ' prints 28/07/2016 10:16:01 (output below assumes this date and time)
    Debug.Print Date  ' prints 28/07/2016
    Debug.Print Time  ' prints 10:16:01
    
    ' Apply a custom format to the current date or time
    Debug.Print Format$(Now, "dd mmmm yyyy hh:nn")  ' prints 28 July 2016 10:16
    Debug.Print Format$(Date, "yyyy-mm-dd")         ' prints 2016-07-28
    Debug.Print Format$(Time, "hh") & " hour " & _
                Format$(Time, "nn") & " min " & _
                Format$(Time, "ss") & " sec "       ' prints 10 hour 16 min 01 sec
    
End Sub

```

### Timer Function

The `Timer` function returns a Single representing the number of seconds elapsed since midnight. The precision is one hundredth of a second.

```vb
Sub TimerExample()

    Debug.Print Time    ' prints 10:36:31  (time at execution)
    Debug.Print Timer   ' prints 38191,13  (seconds since midnight)

End Sub

```

Because `Now` and `Time` functions are only precise to seconds, `Timer` offers a convenient way to increase accuracy of time measurement:

```vb
Sub GetBenchmark()
       
    Dim StartTime  As Single
    StartTime = Timer       'Store the current Time
    
    Dim i As Long
    Dim temp As String
    For i = 1 To 1000000    'See how long it takes Left$ to execute 1,000,000 times
        temp = Left$("Text", 2)
    Next i
    
    Dim Elapsed As Single
    Elapsed = Timer - StartTime
    Debug.Print "Code completed in " & CInt(Elapsed * 1000) & " ms"

End Sub

```

### IsDate()

IsDate() tests whether an expression is a valid date or not. Returns a `Boolean`.

```vb
Sub IsDateExamples()

    Dim anything As Variant
       
    anything = "September 11, 2001"

    Debug.Print IsDate(anything)    'Prints True
            
    anything = #9/11/2001#

    Debug.Print IsDate(anything)    'Prints True
   
    anything = "just a string"

    Debug.Print IsDate(anything)    'Prints False

    anything = vbNull
    
    Debug.Print IsDate(anything)    'Prints False
    
End Sub

```



## Extraction functions


These functions take a `Variant` that can be cast to a `Date` as a parameter and return an `Integer` representing a portion of a date or time. If the parameter can not be cast to a `Date`, it will result in a run-time error 13: Type mismatch.

|Function|Description|Returned value
|---|---|---|---|---|---|---|---|---|---
|Year()|Returns the year portion of the date argument.|Integer (100 to 9999)
|Month()|Returns the month portion of the date argument.|Integer (1 to 12)
|Day()|Returns the day portion of the date argument.|Integer (1 to 31)
|WeekDay()|Returns the day of the week of the date argument. Accepts an optional second argument definining the first day of the week|Integer (1 to 7)
|Hour()|Returns the hour portion of the date argument.|Integer (0 to 23)
|Minute()|Returns the minute portion of the date argument.|Integer (0 to 59)
|Second()|Returns the second portion of the date argument.|Integer (0 to 59)

**Examples:**

```vb
Sub ExtractionExamples()

    Dim MyDate As Date
    
    MyDate = DateSerial(2016, 7, 28) + TimeSerial(12, 34, 56)

    Debug.Print Format$(MyDate, "yyyy-mm-dd hh:nn:ss") ' prints 2016-07-28 12:34:56

    Debug.Print Year(MyDate)                           ' prints 2016
    Debug.Print Month(MyDate)                          ' prints 7
    Debug.Print Day(MyDate)                            ' prints 28
    Debug.Print Hour(MyDate)                           ' prints 12
    Debug.Print Minute(MyDate)                         ' prints 34
    Debug.Print Second(MyDate)                         ' prints 56
    
    Debug.Print Weekday(MyDate)                        ' prints 5
    'Varies by locale - i.e. will print 4 in the EU and 5 in the US
    Debug.Print Weekday(MyDate, vbUseSystemDayOfWeek)
    Debug.Print Weekday(MyDate, vbMonday)              ' prints 4
    Debug.Print Weekday(MyDate, vbSunday)              ' prints 5
    
End Sub

```

### DatePart() Function

`DatePart()` is also a function returning a portion of a date, but works differently and allow more possibilities than the functions above. It can for instance return the Quarter of the year or the Week of the year.

**Syntax:**

```vb
DatePart ( interval, date  [, firstdayofweek] [, firstweekofyear] )

```

**interval** argument can be :

|Interval|Description
|---|---|---|---|---|---|---|---|---|---
|"yyyy"|Year (100 to 9999)
|"y"|Day of the year (1 to 366)
|"m"|Month (1 to 12)
|"q"|Quarter (1 to 4)
|"ww"|Week (1 to 53)
|"w"|Day of the week (1 to 7)
|"d"|Day of the month (1 to 31)
|"h"|Hour (0 to 23)
|"n"|Minute (0 to 59)
|"s"|Second (0 to 59)

**firstdayofweek** is optional. it is a constant that specifies the first day of the week. If not specified, `vbSunday` is assumed.

**firstweekofyear** is optional. it is a constant that specifies the first week of the year. If not specified, the first week is assumed to be the week in which January 1 occurs.

**Examples:**

```vb
Sub DatePartExample()

    Dim MyDate As Date
    
    MyDate = DateSerial(2016, 7, 28) + TimeSerial(12, 34, 56)

    Debug.Print Format$(MyDate, "yyyy-mm-dd hh:nn:ss") ' prints 2016-07-28 12:34:56
    
    Debug.Print DatePart("yyyy", MyDate)              ' prints 2016
    Debug.Print DatePart("y", MyDate)                 ' prints 210
    Debug.Print DatePart("h", MyDate)                 ' prints 12
    Debug.Print DatePart("Q", MyDate)                 ' prints 3
    Debug.Print DatePart("w", MyDate)                 ' prints 5
    Debug.Print DatePart("ww", MyDate)                ' prints 31

End Sub

```



## Calculation functions


### DateDiff()

`DateDiff()` returns a `Long` representing the number of time intervals between two specified dates.

**Syntax**

```vb
DateDiff ( interval, date1, date2  [, firstdayofweek] [, firstweekofyear] )

```


- **interval** can be any of the intervals defined in the [`DatePart()`](http://stackoverflow.com/documentation/vba/4452/date-time-manipulation/15553/extraction-functions) function
- **date1** and **date2** are the two dates you want to use in the calculation
- **firstdayofweek** and **firstweekofyear** are optional. Refer to [`DatePart()`](http://stackoverflow.com/documentation/vba/4452/date-time-manipulation/15553/extraction-functions) function for explanations

**Examples**

```vb
Sub DateDiffExamples()

    ' Check to see if 2016 is a leap year.
    Dim NumberOfDays As Long
    NumberOfDays = DateDiff("d", #1/1/2016#, #1/1/2017#)
    
    If NumberOfDays = 366 Then
        Debug.Print "2016 is a leap year."              'This will output.
    End If
           
    ' Number of seconds in a day
    Dim StartTime As Date
    Dim EndTime As Date
    StartTime = TimeSerial(0, 0, 0)
    EndTime = TimeSerial(24, 0, 0)
    Debug.Print DateDiff("s", StartTime, EndTime)       'prints 86400

End Sub

```

### DateAdd()

`DateAdd()` returns a `Date` to which a specified date or time interval has been added.

**Syntax**

```vb
DateAdd ( interval, number, date  ) 

```


- **interval** can be any of the intervals defined in the [`DatePart()`](http://stackoverflow.com/documentation/vba/4452/date-time-manipulation/15553/extraction-functions) function
- **number** Numeric expression that is the number of intervals you want to add. It can be positive (to get dates in the future) or negative (to get dates in the past).
- **date** is a `Date` or literal representing date to which the interval is added

**Examples :**

```vb
Sub DateAddExamples()

    Dim Sample As Date
    'Create sample date and time of 2016-07-28 12:34:56
    Sample = DateSerial(2016, 7, 28) + TimeSerial(12, 34, 56)
    
    ' Date 5 months previously (prints 2016-02-28):
    Debug.Print Format$(DateAdd("m", -5, Sample), "yyyy-mm-dd")
    
    ' Date 10 months previously (prints 2015-09-28):
    Debug.Print Format$(DateAdd("m", -10, Sample), "yyyy-mm-dd")
    
    ' Date in 8 months (prints 2017-03-28):
    Debug.Print Format$(DateAdd("m", 8, Sample), "yyyy-mm-dd")

    ' Date/Time 18 hours previously (prints 2016-07-27 18:34:56):
    Debug.Print Format$(DateAdd("h", -18, Sample), "yyyy-mm-dd hh:nn:ss")
    
    ' Date/Time in 36 hours (prints 2016-07-30 00:34:56):
    Debug.Print Format$(DateAdd("h", 36, Sample), "yyyy-mm-dd hh:nn:ss")

End Sub

```



## Conversion and Creation


### CDate()

`CDate()` converts something from any datatype to a `Date` datatype

```vb
Sub CDateExamples()

    Dim sample As Date

    ' Converts a String representing a date and time to a Date
    sample = CDate("September 11, 2001 12:34")
    Debug.Print Format$(sample, "yyyy-mm-dd hh:nn:ss")      ' prints 2001-09-11 12:34:00
    
    ' Converts a String containing a date to a Date
    sample = CDate("September 11, 2001")
    Debug.Print Format$(sample, "yyyy-mm-dd hh:nn:ss")      ' prints 2001-09-11 00:00:00

    ' Converts a String containing a time to a Date
    sample = CDate("12:34:56")
    Debug.Print Hour(sample)                        ' prints 12
    Debug.Print Minute(sample)                      ' prints 34
    Debug.Print Second(sample)                      ' prints 56
    
    ' Find the 10000th day from the epoch date of 1899-12-31
    sample = CDate(10000)
    Debug.Print Format$(sample, "yyyy-mm-dd")       ' prints 1927-05-18
    
End Sub

```

Note that VBA also has a loosely typed `CVDate()` that functions in the same way as the `CDate()` function other than returning a date typed `Variant` instead of a strongly typed `Date`. The `CDate()` version should be preferred when passing to a `Date` parameter or assigning to a `Date` variable, and the `CVDate()` version should be preferred when when passing to a `Variant` parameter or assigning to a `Variant` variable. This avoids implicit type casting.

### DateSerial()

`DateSerial()` function is used to create a date. It returns a `Date` for a specified year, month, and day.

**Syntax:**

```vb
DateSerial ( year, month, day ) 

```

With year, month and day arguments being valid Integers (Year from 100 to 9999, Month from 1 to 12, Day from 1 to 31).

**Examples**

```vb
Sub DateSerialExamples()

    ' Build a specific date
    Dim sample As Date
    sample = DateSerial(2001, 9, 11)
    Debug.Print Format$(sample, "yyyy-mm-dd")                   ' prints 2001-09-11
    
    ' Find the first day of the month for a date.
    sample = DateSerial(Year(sample), Month(sample), 1)
    Debug.Print Format$(sample, "yyyy-mm-dd")                   ' prints 2001-09-11
    
    ' Find the last day of the previous month.
    sample = DateSerial(Year(sample), Month(sample), 1) - 1
    Debug.Print Format$(sample, "yyyy-mm-dd")                   ' prints 2001-09-11
    
End Sub

```

Note that `DateSerial()` will accept "invalid" dates and calculate a valid date from it. This can be used creatively for good:

**Positive Example**

```vb
Sub GoodDateSerialExample()

    'Calculate 45 days from today
    Dim today As Date
    today = DateSerial (2001, 9, 11)
    Dim futureDate As Date
    futureDate = DateSerial(Year(today), Month(today), Day(today) + 45)
    Debug.Print Format$(futureDate, "yyyy-mm-dd")            'prints 2009-10-26

End Sub

```

However, it is more likely to cause grief when attempting to create a date from unvalidated user input:

**Negative Example**

```vb
Sub BadDateSerialExample()

    'Allow user to enter unvalidate date information
    Dim myYear As Long
    myYear = InputBox("Enter Year")                                         
            'Assume user enters 2009
    Dim myMonth As Long
    myMonth = InputBox("Enter Month")                                       
            'Assume user enters 2
    Dim myDay As Long
    myDay = InputBox("Enter Day")                                           
            'Assume user enters 31
    Debug.Print Format$(DateSerial(myYear, myMonth, myDay), "yyyy-mm-dd")   
            'prints  2009-03-03

End Sub

```

