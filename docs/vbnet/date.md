---
metaTitle: "Visual Basic .NET - Date"
description: "Converting (Parsing) a String to a Date, Converting a Date To A String"
---

# Date



## Converting (Parsing) a String to a Date


If you know the format of the string you are converting (parsing) you should use `DateTime.ParseExact`

```vb
Dim dateString As String = "12.07.2003"
Dim dateFormat As String = "dd.MM.yyyy"
Dim dateValue As Date

dateValue = DateTime.ParseExact(dateString, dateFormat, Globalization.CultureInfo.InvariantCulture)

```

If you are not certain for the format of the string, you can use `DateTime.TryParseExact` and test the result to see if parsed or not:

```vb
Dim dateString As String = "23-09-2013"
Dim dateFormat As String = "dd-MM-yyyy"
Dim dateValue As Date

If DateTime.TryParseExact(dateString, dateFormat, Globalization.CultureInfo.InvariantCulture, DateTimeStyles.None, dateValue) Then
    'the parse worked and the dateValue variable now holds the datetime that was parsed as it is passing in ByRef
Else
    'the parse failed
End If

```



## Converting a Date To A String


Simply use the `.ToString` overload of a `DateTime` object to get the format you require:

```vb
Dim dateValue As DateTime = New DateTime(2001, 03, 06)
Dim dateString As String = dateValue.ToString("yyyy-MM-dd") '2001-03-06

```

