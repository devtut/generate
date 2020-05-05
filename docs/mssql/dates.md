---
metaTitle: "Dates"
description: "Date & Time Formatting using CONVERT, Date & Time Formatting using FORMAT, DATEADD for adding and subtracting time periods, Get the current DateTime, Getting the last day of a month, Create function to calculate a person's age on a specific date, DATEDIFF for calculating time period differences, DATEPART & DATENAME, Return just Date from a DateTime, CROSS PLATFORM DATE OBJECT, Date parts reference, Date Format  Extended"
---

# Dates



## Date & Time Formatting using CONVERT


You can use the CONVERT function to cast a datetime datatype to a formatted string.

```sql
SELECT GETDATE() AS [Result]                                -- 2016-07-21 07:56:10.927
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),100) AS [Result] -- Jul 21 2016  7:56AM
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),101) AS [Result] -- 07/21/2016
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),102) AS [Result] -- 2016.07.21
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),103) AS [Result] -- 21/07/2016
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),104) AS [Result] -- 21.07.2016
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),105) AS [Result] -- 21-07-2016
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),106) AS [Result] -- 21 Jul 2016
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),107) AS [Result] -- Jul 21, 2016
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),108) AS [Result] -- 07:57:05
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),109) AS [Result] -- Jul 21 2016  7:57:45:707AM
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),110) AS [Result] -- 07-21-2016
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),111) AS [Result] -- 2016/07/21
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),112) AS [Result] -- 20160721
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),113) AS [Result] -- 21 Jul 2016 07:57:59:553
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),114) AS [Result] -- 07:57:59:553
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),120) AS [Result] -- 2016-07-21 07:57:59
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),121) AS [Result] -- 2016-07-21 07:57:59.553
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),126) AS [Result] -- 2016-07-21T07:58:34.340
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),127) AS [Result] -- 2016-07-21T07:58:34.340
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),130) AS [Result] -- 16 ???? 1437  7:58:34:340AM
UNION SELECT CONVERT(VARCHAR(30),GETDATE(),131) AS [Result] -- 16/10/1437  7:58:34:340AM

```



## Date & Time Formatting using FORMAT


You can utilize the new function: [`FORMAT()`](https://msdn.microsoft.com/en-us/library/hh213505.aspx).

Using this you can transform your `DATETIME` fields to your own custom `VARCHAR` format.

**Example**

```sql
DECLARE @Date DATETIME = '2016-09-05 00:01:02.333'

SELECT FORMAT(@Date, N'dddd, MMMM dd, yyyy hh:mm:ss tt')

```

> 
Monday, September 05, 2016 12:01:02 AM


**Arguments**

Given the `DATETIME` being formatted is `2016-09-05 00:01:02.333`, the following chart shows what their output would be for the provided argument.

|Argument|Output
|---|---|---|---|---
|yyyy|2016
|yy|16
|MMMM|September
|MM|09
|M|9
|dddd|Monday
|ddd|Mon
|dd|05
|d|5
|HH|00
|H|0
|hh|12
|h|12
|mm|01
|m|1
|ss|02
|s|2
|tt|AM
|t|A
|fff|333
|ff|33
|f|3

You can also supply a single argument to the `FORMAT()` function to generate a pre-formatted output:

```sql
DECLARE @Date DATETIME = '2016-09-05 00:01:02.333'

SELECT FORMAT(@Date, N'U')

```

> 
Monday, September 05, 2016 4:01:02 AM


|Single Argument|Output
|---|---|---|---|---
|D|Monday, September 05, 2016
|d|9/5/2016
|F|Monday, September 05, 2016 12:01:02 AM
|f|Monday, September 05, 2016 12:01 AM
|G|9/5/2016 12:01:02 AM
|g|9/5/2016 12:01 AM
|M|September 05
|O|2016-09-05T00:01:02.3330000
|R|Mon, 05 Sep 2016 00:01:02 GMT
|s|2016-09-05T00:01:02
|T|12:01:02 AM
|t|12:01 AM
|U|Monday, September 05, 2016 4:01:02 AM
|u|2016-09-05 00:01:02Z
|Y|September, 2016

**Note: The above list is using the `en-US` culture.  A different culture can be specified for the `FORMAT()` via the third parameter:**

```sql
DECLARE @Date DATETIME = '2016-09-05 00:01:02.333'

SELECT FORMAT(@Date, N'U', 'zh-cn')

```

> 
2016年9月5日 4:01:02




## DATEADD for adding and subtracting time periods


General syntax:

```sql
DATEADD (datepart , number , datetime_expr)  

```

To add a time measure, the `number` must be positive. To subtract a time measure, the `number` must be negative.

Examples

```sql
DECLARE @now DATETIME2 = GETDATE();
SELECT @now;                        --2016-07-21 14:39:46.4170000
SELECT DATEADD(YEAR, 1, @now)       --2017-07-21 14:39:46.4170000
SELECT DATEADD(QUARTER, 1, @now)    --2016-10-21 14:39:46.4170000
SELECT DATEADD(WEEK, 1, @now)       --2016-07-28 14:39:46.4170000
SELECT DATEADD(DAY, 1, @now)        --2016-07-22 14:39:46.4170000
SELECT DATEADD(HOUR, 1, @now)       --2016-07-21 15:39:46.4170000
SELECT DATEADD(MINUTE, 1, @now)     --2016-07-21 14:40:46.4170000
SELECT DATEADD(SECOND, 1, @now)     --2016-07-21 14:39:47.4170000
SELECT DATEADD(MILLISECOND, 1, @now)--2016-07-21 14:39:46.4180000

```

NOTE: `DATEADD` also accepts abbreviations in the `datepart` parameter. Use of these abbreviations is generally discouraged as they can be confusing (`m` vs `mi`, `ww` vs `w`, etc.).



## Get the current DateTime


The built-in functions [`GETDATE`](https://msdn.microsoft.com/en-us/library/ms188383.aspx) and [`GETUTCDATE`](https://msdn.microsoft.com/en-us/library/ms178635.aspx) each return the current date and time without a time zone offset.

The return value of both functions is based on the operating system of the computer on which the instance of SQL Server is running.

The return value of GETDATE represents the current time in the same timezone as operating system. The return value of GETUTCDATE represents the current UTC time.

Either function can be included in the `SELECT` clause of a query or as part of boolean expression in the `WHERE` clause.

Examples:

```sql
-- example query that selects the current time in both the server time zone and UTC
SELECT GETDATE() as SystemDateTime, GETUTCDATE() as UTCDateTime

-- example query records with EventDate in the past.
SELECT * FROM MyEvents WHERE EventDate < GETDATE() 

```

There are a few other built-in functions that return different variations of the current date-time:

```sql
SELECT 
    GETDATE(),          --2016-07-21 14:27:37.447
    GETUTCDATE(),       --2016-07-21 18:27:37.447
    CURRENT_TIMESTAMP,  --2016-07-21 14:27:37.447
    SYSDATETIME(),      --2016-07-21 14:27:37.4485768
    SYSDATETIMEOFFSET(),--2016-07-21 14:27:37.4485768 -04:00
    SYSUTCDATETIME()    --2016-07-21 18:27:37.4485768

```



## Getting the last day of a month


Using the `DATEADD` and `DATEDIFF` functions, it's possible to return the last date of a month.

```sql
SELECT DATEADD(d, -1, DATEADD(m, DATEDIFF(m, 0, '2016-09-23') + 1, 0))
-- 2016-09-30 00:00:00.000

```

The `EOMONTH` function provides a more concise way to return the last date of a month, and has an optional parameter to offset the month.

```sql
SELECT EOMONTH('2016-07-21')        --2016-07-31
SELECT EOMONTH('2016-07-21', 4)     --2016-11-30
SELECT EOMONTH('2016-07-21', -5)    --2016-02-29

```



## Create function to calculate a person's age on a specific date


This function will take 2 datetime parameters, the DOB, and a date to check the age at

```

 CREATE FUNCTION [dbo].[Calc_Age]
    (
    @DOB datetime , @calcDate datetime 
    )
    RETURNS int
    AS
   BEGIN
declare @age int

IF (@calcDate < @DOB  )
RETURN -1

-- If a DOB is supplied after the comparison date, then return -1
SELECT @age = YEAR(@calcDate) - YEAR(@DOB) +
  CASE WHEN DATEADD(year,YEAR(@calcDate) - YEAR(@DOB)
  ,@DOB) > @calcDate THEN -1 ELSE 0 END
    

RETURN @age
    

END

```

eg to check the age today of someone born on 1/1/2000

```sql
SELECT  dbo.Calc_Age('2000-01-01',Getdate())

```



## DATEDIFF for calculating time period differences


General syntax:

```sql
DATEDIFF (datepart, datetime_expr1, datetime_expr2)

```

It will return a positive number if `datetime_expr` is in the past relative to `datetime_expr2`, and a negative number otherwise.

Examples

```sql
DECLARE @now DATETIME2 = GETDATE();
DECLARE @oneYearAgo DATETIME2 = DATEADD(YEAR, -1, @now);
SELECT @now                                    --2016-07-21 14:49:50.9800000
SELECT @oneYearAgo                             --2015-07-21 14:49:50.9800000
SELECT DATEDIFF(YEAR, @oneYearAgo, @now)       --1
SELECT DATEDIFF(QUARTER, @oneYearAgo, @now)    --4
SELECT DATEDIFF(WEEK, @oneYearAgo, @now)       --52
SELECT DATEDIFF(DAY, @oneYearAgo, @now)        --366
SELECT DATEDIFF(HOUR, @oneYearAgo, @now)       --8784
SELECT DATEDIFF(MINUTE, @oneYearAgo, @now)     --527040
SELECT DATEDIFF(SECOND, @oneYearAgo, @now)     --31622400

```

NOTE: `DATEDIFF` also accepts abbreviations in the `datepart` parameter. Use of these abbreviations is generally discouraged as they can be confusing (`m` vs `mi`, `ww` vs `w`, etc.).

`DATEDIFF` can also be used to determine the offset between UTC and the local time of the SQL Server.  The following statement can be used to calculate the offset between UTC and local time (including timezone).

```sql
select  DATEDIFF(hh, getutcdate(), getdate()) as 'CentralTimeOffset'

```



## DATEPART & DATENAME


`DATEPART` returns the specified `datepart` of the specified datetime expression as a numeric value.

`DATENAME` returns a character string that represents the specified `datepart` of the specified date. In practice `DATENAME` is mostly useful for getting the name of the month or the day of the week.

There are also some shorthand functions to get the year, month or day of a datetime expression, which behave like `DATEPART` with their respective `datepart` units.

Syntax:

```sql
DATEPART ( datepart , datetime_expr )
DATENAME ( datepart , datetime_expr )
DAY ( datetime_expr )
MONTH ( datetime_expr )
YEAR ( datetime_expr )

```

Examples:

```sql
DECLARE @now DATETIME2 = GETDATE();
SELECT @now                       --2016-07-21 15:05:33.8370000
SELECT DATEPART(YEAR, @now)       --2016
SELECT DATEPART(QUARTER, @now)    --3
SELECT DATEPART(WEEK, @now)       --30
SELECT DATEPART(HOUR, @now)       --15
SELECT DATEPART(MINUTE, @now)     --5
SELECT DATEPART(SECOND, @now)     --33
-- Differences between DATEPART and DATENAME:
SELECT DATEPART(MONTH, @now)      --7
SELECT DATENAME(MONTH, @now)      --July
SELECT DATEPART(WEEKDAY, @now)    --5
SELECT DATENAME(WEEKDAY, @now)    --Thursday
--shorthand functions
SELECT DAY(@now)    --21
SELECT MONTH(@now)  --7
SELECT YEAR(@now)   --2016

```

NOTE: `DATEPART` and `DATENAME` also accept abbreviations in the `datepart` parameter. Use of these abbreviations is generally discouraged as they can be confusing (`m` vs `mi`, `ww` vs `w`, etc.).



## Return just Date from a DateTime


There are many ways to return a Date from a DateTime object

1. `SELECT CONVERT(Date, GETDATE())`
1. `SELECT DATEADD(dd, 0, DATEDIFF(dd, 0, GETDATE()))` returns 2016-07-21 00:00:00.000
1. `SELECT CAST(GETDATE() AS DATE)`
1. `SELECT CONVERT(CHAR(10),GETDATE(),111)`
1. `SELECT FORMAT(GETDATE(), 'yyyy-MM-dd')`

Note that options 4 and 5 returns a string, not a date.



## CROSS PLATFORM DATE OBJECT


In Transact SQL , you may define an object as `Date` (or `DateTime`) using the `[DATEFROMPARTS][1]` (or `[DATETIMEFROMPARTS][1]`) function like following:

```

DECLARE @myDate DATE=DATEFROMPARTS(1988,11,28) 
 DECLARE @someMoment DATETIME=DATEFROMPARTS(1988,11,28,10,30,50,123)

```

The parameters you provide are Year, Month, Day for the `DATEFROMPARTS` function and, for the `DATETIMEFROMPARTS` function you will need to provide year, month, day, hour, minutes, seconds and milliseconds.

These methods are useful and worth being used because using the plain string to build a date(or datetime) may fail depending on the host machine region, location or date format settings.



## Date parts reference


These are the `datepart` values available to date & time functions:

|datepart|Abbreviations
|---|---|---|---|---
|year|yy, yyyy
|quarter|qq, q
|month|mm, m
|dayofyear|dy, y
|day|dd, d
|week|wk, ww
|weekday|dw, w
|hour|hh
|minute|mi, n
|second|ss, s
|millisecond|ms
|microsecond|mcs
|nanosecond|ns

**NOTE**: Use of abbreviations is generally discouraged as they can be confusing (`m` vs `mi`, `ww` vs `w`, etc.). The long version of the `datepart` representation promotes clarity and readability, and should be used whenever possible (`month`, `minute`, `week`, `weekday`, etc.).



## Date Format  Extended


|Date Format|SQL Statement|Sample Output
|---|---|---|---|---
|YY-MM-DD|SELECT RIGHT(CONVERT(VARCHAR(10), SYSDATETIME(), 20), 8) AS [YY-MM-DD]<br/>SELECT REPLACE(CONVERT(VARCHAR(8), SYSDATETIME(), 11), '/', '-') AS [YY-MM-DD]|11-06-08
|YYYY-MM-DD|SELECT CONVERT(VARCHAR(10), SYSDATETIME(), 120) AS [YYYY-MM-DD]<br/>SELECT REPLACE(CONVERT(VARCHAR(10), SYSDATETIME(), 111), '/', '-') AS [YYYY-MM-DD]|2011-06-08
|YYYY-M-D|SELECT CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) + '-' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '-' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) AS [YYYY-M-D]|2011-6-8
|YY-M-D|SELECT RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) + '-' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '-' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) AS [YY-M-D]|11-6-8
|M-D-YYYY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '-' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '-' + CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) AS [M-D-YYYY]|6-8-2011
|M-D-YY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '-' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '-' + RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) AS [M-D-YY]|6-8-11
|D-M-YYYY|SELECT CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '-' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '-' + CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) AS [D-M-YYYY]|8-6-2011
|D-M-YY|SELECT CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '-' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '-' + RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) AS [D-M-YY]|8-6-11
|YY-MM|SELECT RIGHT(CONVERT(VARCHAR(7), SYSDATETIME(), 20), 5) AS [YY-MM]<br>SELECT SUBSTRING(CONVERT(VARCHAR(10), SYSDATETIME(), 120), 3, 5) AS [YY-MM]|11-06
|YYYY-MM|SELECT CONVERT(VARCHAR(7), SYSDATETIME(), 120) AS [YYYY-MM]|2011-06
|YY-M|SELECT RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) + '-' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) AS [YY-M]|11-6
|YYYY-M|SELECT CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) + '-' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) AS [YYYY-M]|2011-6
|MM-YY|SELECT RIGHT(CONVERT(VARCHAR(8), SYSDATETIME(), 5), 5) AS [MM-YY]<br>SELECT SUBSTRING(CONVERT(VARCHAR(8), SYSDATETIME(), 5), 4, 5) AS [MM-YY]|06-11
|MM-YYYY|SELECT RIGHT(CONVERT(VARCHAR(10), SYSDATETIME(), 105), 7) AS [MM-YYYY]|06-2011
|M-YY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '-' + RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) AS [M-YY]|6-11
|M-YYYY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '-' + CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) AS [M-YYYY]|6-2011
|MM-DD|SELECT CONVERT(VARCHAR(5), SYSDATETIME(), 10) AS [MM-DD]|06-08
|DD-MM|SELECT CONVERT(VARCHAR(5), SYSDATETIME(), 5) AS [DD-MM]|08-06
|M-D|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '-' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) AS [M-D]|6-8
|D-M|SELECT CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '-' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) AS [D-M]|8-6
|M/D/YYYY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '/' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '/' + CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) AS [M/D/YYYY]|6/8/2011
|M/D/YY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '/' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '/' + RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) AS [M/D/YY]|6/8/11
|D/M/YYYY|SELECT CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '/' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '/' + CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) AS [D/M/YYYY]|8/6/2011
|D/M/YY|SELECT CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '/' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '/' + RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) AS [D/M/YY]|8/6/11
|YYYY/M/D|SELECT CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) + '/' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '/' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) AS [YYYY/M/D]|2011/6/8
|YY/M/D|SELECT RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) + '/' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '/' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) AS [YY/M/D]|11/6/8
|MM/YY|SELECT RIGHT(CONVERT(VARCHAR(8), SYSDATETIME(), 3), 5) AS [MM/YY]|06/11
|MM/YYYY|SELECT RIGHT(CONVERT(VARCHAR(10), SYSDATETIME(), 103), 7) AS [MM/YYYY]|06/2011
|M/YY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '/' + RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) AS [M/YY]|6/11
|M/YYYY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '/' + CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) AS [M/YYYY]|6/2011
|YY/MM|SELECT CONVERT(VARCHAR(5), SYSDATETIME(), 11) AS [YY/MM]|11/06
|YYYY/MM|SELECT CONVERT(VARCHAR(7), SYSDATETIME(), 111) AS [YYYY/MM]|2011/06
|YY/M|SELECT RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) + '/' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) AS [YY/M]|11/6
|YYYY/M|SELECT CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) + '/' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) AS [YYYY/M]|2011/6
|MM/DD|SELECT CONVERT(VARCHAR(5), SYSDATETIME(), 1) AS [MM/DD]|06/08
|DD/MM|SELECT CONVERT(VARCHAR(5), SYSDATETIME(), 3) AS [DD/MM]|08/06
|M/D|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '/' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) AS [M/D]|6/8
|D/M|SELECT CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '/' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) AS [D/M]|8/6
|MM.DD.YYYY|SELECT REPLACE(CONVERT(VARCHAR(10), SYSDATETIME(), 101), '/', '.') AS [MM.DD.YYYY]|06.08.2011
|MM.DD.YY|SELECT REPLACE(CONVERT(VARCHAR(8), SYSDATETIME(), 1), '/', '.') AS [MM.DD.YY]|06.08.11
|M.D.YYYY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '.' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '.' + CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) AS [M.D.YYYY]|6.8.2011
|M.D.YY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '.' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '.' + RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) AS [M.D.YY]|6.8.11
|DD.MM.YYYY|SELECT CONVERT(VARCHAR(10), SYSDATETIME(), 104) AS [DD.MM.YYYY]|08.06.2011
|DD.MM.YY|SELECT CONVERT(VARCHAR(10), SYSDATETIME(), 4) AS [DD.MM.YY]|08.06.11
|D.M.YYYY|SELECT CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '.' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '.' + CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) AS [D.M.YYYY]|8.6.2011
|D.M.YY|SELECT CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + '.' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '.' + RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) AS [D.M.YY]|8.6.11
|YYYY.M.D|SELECT CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) + '.' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '.' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) AS [YYYY.M.D]|2011.6.8
|YY.M.D|SELECT RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) + '.' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '.' + CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) AS [YY.M.D]|11.6.8
|MM.YYYY|SELECT RIGHT(CONVERT(VARCHAR(10), SYSDATETIME(), 104), 7) AS [MM.YYYY]|06.2011
|MM.YY|SELECT RIGHT(CONVERT(VARCHAR(8), SYSDATETIME(), 4), 5) AS [MM.YY]|06.11
|M.YYYY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '.' + CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) AS [M.YYYY]|6.2011
|M.YY|SELECT CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) + '.' + RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) AS [M.YY]|6.11
|YYYY.MM|SELECT CONVERT(VARCHAR(7), SYSDATETIME(), 102) AS [YYYY.MM]|2011.06
|YY.MM|SELECT CONVERT(VARCHAR(5), SYSDATETIME(), 2) AS [YY.MM]|11.06
|YYYY.M|SELECT CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)) + '.' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) AS [YYYY.M]|2011.6
|YY.M|SELECT RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) + '.' + CAST(MONTH(SYSDATETIME()) AS VARCHAR(2)) AS [YY.M]|11.6
|MM.DD|SELECT RIGHT(CONVERT(VARCHAR(8), SYSDATETIME(), 2), 5) AS [MM.DD]|06.08
|DD.MM|SELECT CONVERT(VARCHAR(5), SYSDATETIME(), 4) AS [DD.MM]|08.06
|MMDDYYYY|SELECT REPLACE(CONVERT(VARCHAR(10), SYSDATETIME(), 101), '/', '') AS [MMDDYYYY]|06082011
|MMDDYY|SELECT REPLACE(CONVERT(VARCHAR(8), SYSDATETIME(), 1), '/', '') AS [MMDDYY]|060811
|DDMMYYYY|SELECT REPLACE(CONVERT(VARCHAR(10), SYSDATETIME(), 103), '/', '') AS [DDMMYYYY]|08062011
|DDMMYY|SELECT REPLACE(CONVERT(VARCHAR(8), SYSDATETIME(), 3), '/', '') AS [DDMMYY]|080611
|MMYYYY|SELECT RIGHT(REPLACE(CONVERT(VARCHAR(10), SYSDATETIME(), 103), '/', ''), 6) AS [MMYYYY]|062011
|MMYY|SELECT RIGHT(REPLACE(CONVERT(VARCHAR(8), SYSDATETIME(), 3), '/', ''), 4) AS [MMYY]|0611
|YYYYMM|SELECT CONVERT(VARCHAR(6), SYSDATETIME(), 112) AS [YYYYMM]|201106
|YYMM|SELECT CONVERT(VARCHAR(4), SYSDATETIME(), 12) AS [YYMM]|1106
|Month DD, YYYY|SELECT DATENAME(MONTH, SYSDATETIME())+ ' ' + RIGHT('0' + DATENAME(DAY, SYSDATETIME()), 2) + ', ' + DATENAME(YEAR, SYSDATETIME())    AS [Month DD, YYYY]|June 08, 2011
|Mon YYYY|SELECT LEFT(DATENAME(MONTH, SYSDATETIME()), 3) + ' ' + DATENAME(YEAR, SYSDATETIME()) AS [Mon YYYY]|Jun 2011
|Month YYYY|SELECT DATENAME(MONTH, SYSDATETIME()) + ' ' + DATENAME(YEAR, SYSDATETIME()) AS [Month YYYY]|June 2011
|DD Month|SELECT RIGHT('0' + DATENAME(DAY, SYSDATETIME()), 2) + ' ' + DATENAME(MONTH, SYSDATETIME())  AS  [DD Month]|08 June
|Month DD|SELECT DATENAME(MONTH, SYSDATETIME()) + ' ' + RIGHT('0' + DATENAME(DAY, SYSDATETIME()), 2) AS    [Month DD]|June 08
|DD Month YY|SELECT CAST(DAY(SYSDATETIME()) AS VARCHAR(2)) + ' ' + DATENAME(MM, SYSDATETIME()) + ' ' + RIGHT(CAST(YEAR(SYSDATETIME()) AS VARCHAR(4)), 2) AS [DD Month YY]|08 June 11
|DD Month YYYY|SELECT RIGHT('0' + DATENAME(DAY, SYSDATETIME()), 2) + ' ' + DATENAME(MONTH, SYSDATETIME())+ ' ' + DATENAME(YEAR, SYSDATETIME()) AS [DD Month YYYY]|08 June 2011
|Mon-YY|SELECT REPLACE(RIGHT(CONVERT(VARCHAR(9), SYSDATETIME(), 6), 6), ' ', '-') AS  [Mon-YY]|Jun-08
|Mon-YYYY|SELECT REPLACE(RIGHT(CONVERT(VARCHAR(11), SYSDATETIME(), 106), 8), ' ', '-') AS [Mon-YYYY]|Jun-2011
|DD-Mon-YY|SELECT REPLACE(CONVERT(VARCHAR(9), SYSDATETIME(), 6), ' ', '-') AS [DD-Mon-YY]|08-Jun-11
|DD-Mon-YYYY|SELECT REPLACE(CONVERT(VARCHAR(11), SYSDATETIME(), 106), ' ', '-') AS [DD-Mon-YYYY]|08-Jun-2011



#### Syntax


- EOMONTH (**start_date** [, month_to_add ] )



#### Remarks


as per [https://msdn.microsoft.com/en-us/library/ms187819.aspx](https://msdn.microsoft.com/en-us/library/ms187819.aspx), `DateTime`s are only precise to 3ms.

Rounding of datetime Fractional Second Precision
datetime values are rounded to increments of .000, .003, or .007 seconds, as shown in the following table.

|User-specified value|System stored value
|---|---|---|---|---
|01/01/98 23:59:59.999|1998-01-02 00:00:00.000
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---
|01/01/98 23:59:59.995|1998-01-01 23:59:59.997
|01/01/98 23:59:59.996|
|01/01/98 23:59:59.997|
|01/01/98 23:59:59.998|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---
|01/01/98 23:59:59.992|1998-01-01 23:59:59.993
|01/01/98 23:59:59.993|
|01/01/98 23:59:59.994|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---
|01/01/98 23:59:59.990|1998-01-01 23:59:59.990
|01/01/98 23:59:59.991|
|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---|---

If more precision is required, `time`, `datetime2` or `datetimeoffset` should be used.

