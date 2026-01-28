---
metaTitle: "Java - Date Class"
description: "A basic date output, Convert java.util.Date to java.sql.Date, Java 8 LocalDate and LocalDateTime objects, Converting Date to a certain String format, Creating a Specific Date, Creating Date objects, Comparing Date objects, Converting String into Date, Convert formatted string representation of date to Date object, Time Zones and java.util.Date, LocalTime"
---

# Date Class



## A basic date output


Using the following code with the format string `yyyy/MM/dd hh:mm.ss`, we will receive the following output

> 
2016/04/19 11:45.36


```java
// define the format to use
String formatString = "yyyy/MM/dd hh:mm.ss";

// get a current date object
Date date = Calendar.getInstance().getTime();

// create the formatter
SimpleDateFormat simpleDateFormat = new SimpleDateFormat(formatString);

// format the date
String formattedDate = simpleDateFormat.format(date);

// print it
System.out.println(formattedDate);


// single-line version of all above code
System.out.println(new SimpleDateFormat("yyyy/MM/dd hh:mm.ss").format(Calendar.getInstance().getTime()));

```



## Convert java.util.Date to java.sql.Date


`java.util.Date` to `java.sql.Date` conversion is usually necessary when a Date object needs to be written in a database.

`java.sql.Date` is a wrapper around millisecond value and is used by `JDBC` to identify an `SQL DATE` type

In the below example, we use the `java.util.Date()` constructor, that creates a Date object and initializes it to represent time to the nearest millisecond. This date is used in the `convert(java.util.Date utilDate)` method to return a `java.sql.Date` object

**Example**

```java
public class UtilToSqlConversion {
    
    public static void main(String args[])
    {
        java.util.Date utilDate = new java.util.Date();
        System.out.println("java.util.Date is : " + utilDate);
        java.sql.Date sqlDate = convert(utilDate);
        System.out.println("java.sql.Date is : " + sqlDate);
        DateFormat df = new SimpleDateFormat("dd/MM/YYYY - hh:mm:ss");
        System.out.println("dateFormated date is : " + df.format(utilDate));
    }

    private static java.sql.Date convert(java.util.Date uDate) {
        java.sql.Date sDate = new java.sql.Date(uDate.getTime());
        return sDate;
    }

}

```

**Output**

```java
java.util.Date is : Fri Jul 22 14:40:35 IST 2016
java.sql.Date is : 2016-07-22
dateFormated date is : 22/07/2016 - 02:40:35

```

`java.util.Date` has both date and time information, whereas `java.sql.Date` only has date information



## Java 8 LocalDate and LocalDateTime objects


Date and LocalDate objects **cannot** be **exactly** converted between each other since a Date object represents both a specific day and time, while a LocalDate object does not contain time or timezone information. However, it can be useful to convert between the two if you only care about the actual date information and not the time information.

**Creates a LocalDate**

```java
// Create a default date
LocalDate lDate = LocalDate.now();

// Creates a date from values
lDate = LocalDate.of(2017, 12, 15);
    
// create a date from string
lDate = LocalDate.parse("2017-12-15");

// creates a date from zone
LocalDate.now(ZoneId.systemDefault());

```

**Creates a LocalDateTime**

```java
// Create a default date time
LocalDateTime lDateTime = LocalDateTime.now();

// Creates a date time from values
lDateTime = LocalDateTime.of(2017, 12, 15, 11, 30);

// create a date time from string
lDateTime = LocalDateTime.parse("2017-12-05T11:30:30");

// create a date time from zone 
LocalDateTime.now(ZoneId.systemDefault());

```

**LocalDate to Date and vice-versa**

```java
Date date = Date.from(Instant.now());
ZoneId defaultZoneId = ZoneId.systemDefault();

// Date to LocalDate
LocalDate localDate = date.toInstant().atZone(defaultZoneId).toLocalDate();

// LocalDate to Date
Date.from(localDate.atStartOfDay(defaultZoneId).toInstant());

```

**LocalDateTime to Date and vice-versa**

```java
Date date = Date.from(Instant.now());
ZoneId defaultZoneId = ZoneId.systemDefault();

// Date to LocalDateTime
LocalDateTime localDateTime =  date.toInstant().atZone(defaultZoneId).toLocalDateTime();

// LocalDateTime to Date
Date out = Date.from(localDateTime.atZone(defaultZoneId).toInstant());

```



## Converting Date to a certain String format


`format()` from `SimpleDateFormat` class helps to convert a `Date` object into certain format `String` object by using the supplied **pattern string**.

```java
Date today = new Date();
    
SimpleDateFormat dateFormat = new SimpleDateFormat("dd-MMM-yy"); //pattern is specified here
System.out.println(dateFormat.format(today)); //25-Feb-16 

```

Patterns can be applied again by using `applyPattern()`

```java
dateFormat.applyPattern("dd-MM-yyyy");
System.out.println(dateFormat.format(today)); //25-02-2016

dateFormat.applyPattern("dd-MM-yyyy HH:mm:ss E");
System.out.println(dateFormat.format(today)); //25-02-2016 06:14:33 Thu

```

**Note:** Here `mm` (small letter m) denotes minutes and `MM` (capital M) denotes month. Pay careful attention when formatting years: capital "Y" (`Y`) indicates the "week in the year" while lower-case "y" (`y`) indicates the year.



## Creating a Specific Date


While the Java Date class has several constructors, you'll notice that most are deprecated. The only acceptable way of creating a Date instance directly is either by using the empty constructor or passing in a long (number of milliseconds since standard base time). Neither are handy unless you're looking for the current date or have another Date instance already in hand.

To create a new date, you will need a Calendar instance. From there you can set the Calendar instance to the date that you need.

```java
Calendar c = Calendar.getInstance();

```

This returns a new Calendar instance set to the current time. Calendar has many methods for mutating it's date and time or setting it outright. In this case, we'll set it to a specific date.

```java
c.set(1974, 6, 2, 8, 0, 0);
Date d = c.getTime();

```

The `getTime` method returns the Date instance that we need. Keep in mind that the Calendar set methods only set one or more fields, they do not set them all. That is, if you set the year, the other fields remain unchanged.

**PITFALL**

In many cases, this code snippet fulfills its purpose, but keep in mind that two important parts of the date/time are not defined.

- the `(1974, 6, 2, 8, 0, 0)` parameters are interpreted within the default timezone, defined somewhere else,
- the milliseconds are not set to zero, but filled from the system clock at the time the Calendar instance is created.



## Creating Date objects


```java
Date date = new Date();
System.out.println(date); // Thu Feb 25 05:03:59 IST 2016

```

Here this `Date` object contains the current date and time when this object was created.

```java
Calendar calendar = Calendar.getInstance();
calendar.set(90, Calendar.DECEMBER, 11);
Date myBirthDate = calendar.getTime();
System.out.println(myBirthDate); // Mon Dec 31 00:00:00 IST 1990

```

`Date` objects are best created through a `Calendar` instance since the use of the data constructors is deprecated and discouraged. To do se we need to get an instance of the `Calendar` class from the factory method. Then we can set year, month and day of month by using numbers or in case of months constants provided py the Calendar class to improve readability and reduce errors.

```java
calendar.set(90, Calendar.DECEMBER, 11, 8, 32, 35);
Date myBirthDatenTime = calendar.getTime();
System.out.println(myBirthDatenTime); // Mon Dec 31 08:32:35 IST 1990

```

Along with date, we can also pass time in the order of hour, minutes and seconds.



## Comparing Date objects


### Calendar, Date, and LocalDate

### before, after, compareTo and equals methods

```java
//Use of Calendar and Date objects    
final Date today = new Date();
final Calendar calendar = Calendar.getInstance();
calendar.set(1990, Calendar.NOVEMBER, 1, 0, 0, 0);
Date birthdate = calendar.getTime();

final Calendar calendar2 = Calendar.getInstance();
calendar2.set(1990, Calendar.NOVEMBER, 1, 0, 0, 0);
Date samebirthdate = calendar2.getTime();

//Before example
System.out.printf("Is %1$tF before %2$tF? %3$b%n", today, birthdate, Boolean.valueOf(today.before(birthdate)));
System.out.printf("Is %1$tF before %1$tF? %3$b%n", today, today, Boolean.valueOf(today.before(today)));
System.out.printf("Is %2$tF before %1$tF? %3$b%n", today, birthdate, Boolean.valueOf(birthdate.before(today)));

//After example
System.out.printf("Is %1$tF after %2$tF? %3$b%n", today, birthdate, Boolean.valueOf(today.after(birthdate)));
System.out.printf("Is %1$tF after %1$tF? %3$b%n", today, birthdate, Boolean.valueOf(today.after(today)));
System.out.printf("Is %2$tF after %1$tF? %3$b%n", today, birthdate, Boolean.valueOf(birthdate.after(today)));

//Compare example
System.out.printf("Compare %1$tF to %2$tF: %3$d%n", today, birthdate, Integer.valueOf(today.compareTo(birthdate)));
System.out.printf("Compare %1$tF to %1$tF: %3$d%n", today, birthdate, Integer.valueOf(today.compareTo(today)));
System.out.printf("Compare %2$tF to %1$tF: %3$d%n", today, birthdate, Integer.valueOf(birthdate.compareTo(today)));

//Equal example
System.out.printf("Is %1$tF equal to %2$tF? %3$b%n", today, birthdate, Boolean.valueOf(today.equals(birthdate)));
System.out.printf("Is %1$tF equal to %2$tF? %3$b%n", birthdate, samebirthdate,
            Boolean.valueOf(birthdate.equals(samebirthdate)));
System.out.printf(
            "Because birthdate.getTime() -> %1$d is different from samebirthdate.getTime() -> %2$d, there are millisecondes!%n",
            Long.valueOf(birthdate.getTime()), Long.valueOf(samebirthdate.getTime()));

//Clear ms from calendars
calendar.clear(Calendar.MILLISECOND);
calendar2.clear(Calendar.MILLISECOND);
birthdate = calendar.getTime();
samebirthdate = calendar2.getTime();

System.out.printf("Is %1$tF equal to %2$tF after clearing ms? %3$b%n", birthdate, samebirthdate,
            Boolean.valueOf(birthdate.equals(samebirthdate)));

```

### isBefore, isAfter, compareTo and equals methods

```java
//Use of LocalDate
final LocalDate now = LocalDate.now();
final LocalDate birthdate2 = LocalDate.of(2012, 6, 30);
final LocalDate birthdate3 = LocalDate.of(2012, 6, 30);

//Hours, minutes, second and nanoOfsecond can also be configured with an other class LocalDateTime
//LocalDateTime.of(year, month, dayOfMonth, hour, minute, second, nanoOfSecond);

//isBefore example
System.out.printf("Is %1$tF before %2$tF? %3$b%n", now, birthdate2, Boolean.valueOf(now.isBefore(birthdate2)));
System.out.printf("Is %1$tF before %1$tF? %3$b%n", now, birthdate2, Boolean.valueOf(now.isBefore(now)));
System.out.printf("Is %2$tF before %1$tF? %3$b%n", now, birthdate2, Boolean.valueOf(birthdate2.isBefore(now)));

//isAfter example
System.out.printf("Is %1$tF after %2$tF? %3$b%n", now, birthdate2, Boolean.valueOf(now.isAfter(birthdate2)));
System.out.printf("Is %1$tF after %1$tF? %3$b%n", now, birthdate2, Boolean.valueOf(now.isAfter(now)));
System.out.printf("Is %2$tF after %1$tF? %3$b%n", now, birthdate2, Boolean.valueOf(birthdate2.isAfter(now)));

//compareTo example
System.out.printf("Compare %1$tF to %2$tF %3$d%n", now, birthdate2, Integer.valueOf(now.compareTo(birthdate2)));
System.out.printf("Compare %1$tF to %1$tF %3$d%n", now, birthdate2, Integer.valueOf(now.compareTo(now)));
System.out.printf("Compare %2$tF to %1$tF %3$d%n", now, birthdate2, Integer.valueOf(birthdate2.compareTo(now)));

//equals example
System.out.printf("Is %1$tF equal to %2$tF? %3$b%n", now, birthdate2, Boolean.valueOf(now.equals(birthdate2)));
System.out.printf("Is %1$tF to %2$tF? %3$b%n", birthdate2, birthdate3, Boolean.valueOf(birthdate2.equals(birthdate3)));

//isEqual example
System.out.printf("Is %1$tF equal to %2$tF? %3$b%n", now, birthdate2, Boolean.valueOf(now.isEqual(birthdate2)));
System.out.printf("Is %1$tF to %2$tF? %3$b%n", birthdate2, birthdate3, Boolean.valueOf(birthdate2.isEqual(birthdate3)));

```

### Date comparison before Java 8

Before Java 8, dates could be compared using [java.util.Calendar](https://docs.oracle.com/javase/7/docs/api/java/util/Calendar.html) and [java.util.Date](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html) classes.
Date class offers 4 methods to compare dates :

- [after(Date when)](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html#after(java.util.Date))
- [before(Date when)](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html#before(java.util.Date))
- [compareTo(Date anotherDate)](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html#compareTo(java.util.Date))
- [equals(Object obj)](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html#equals(java.lang.Object))

`after`, `before`, `compareTo` and `equals` methods compare the values returned by [getTime()](https://docs.oracle.com/javase/7/docs/api/java/util/Date.html#getTime()) method for each date.

`compareTo` method returns positive integer.

- Value greater than 0 : when the Date is after the Date argument
- Value greater than 0 : when the Date is before the Date argument
- Value equals to 0 : when the Date is equal to the Date argument

`equals` results can be surprising as shown in the example because values, like milliseconds, are not initialize with the same value if not explicitly given.

### Since Java 8

With Java 8 a new Object to work with Date is available [java.time.LocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html).
`LocalDate` implements [ChronoLocalDate](https://docs.oracle.com/javase/8/docs/api/java/time/chrono/ChronoLocalDate.html), the abstract representation of a date where the Chronology, or calendar system, is pluggable.

To have the date time precision the Object [java.time.LocalDateTime](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDateTime.html) has to be used. `LocalDate` and `LocalDateTime` use the same methods name for comparing.

Comparing dates using a `LocalDate` is different from using `ChronoLocalDate` because  the chronology, or calendar system are not taken in account the first one.

Because most application should use `LocalDate`, `ChronoLocalDate` is not included in examples. Further reading [here](https://docs.oracle.com/javase/8/docs/api/java/time/chrono/ChronoLocalDate.html).

> 
Most applications should declare method signatures, fields and variables as LocalDate, not this[ChronoLocalDate] interface.


`LocalDate` has 5 methods to compare dates :

- [isAfter(ChronoLocalDate other)](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html#isAfter-java.time.chrono.ChronoLocalDate-)
- [isBefore(ChronoLocalDate other)](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html#isBefore-java.time.chrono.ChronoLocalDate-)
- [isEqual(ChronoLocalDate other)](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html#isEqual-java.time.chrono.ChronoLocalDate-)
- [compareTo(ChronoLocalDate other)](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html#compareTo-java.time.chrono.ChronoLocalDate-)
- [equals(Object obj)](https://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html#equals-java.lang.Object-)

In case of `LocalDate` parameter, `isAfter`, `isBefore`, `isEqual`, `equals` and `compareTo` now use this method:

```java
int compareTo0(LocalDate otherDate) {
    int cmp = (year - otherDate.year);
    if (cmp == 0) {
        cmp = (month - otherDate.month);
        if (cmp == 0) {
            cmp = (day - otherDate.day);
        }
    }
    return cmp;
}

```

`equals` method check if the parameter reference equals the date first whereas `isEqual` directly calls `compareTo0`.

In case of an other class instance of `ChronoLocalDate` the dates are compared using the `Epoch Day`. The Epoch Day count is a simple incrementing count of days where day 0 is 1970-01-01 (ISO).



## Converting String into Date


`parse()` from `SimpleDateFormat` class helps to convert a `String` pattern into a `Date` object.

```java
DateFormat dateFormat = DateFormat.getDateInstance(DateFormat.SHORT, Locale.US);
String dateStr = "02/25/2016"; // input String
Date date = dateFormat.parse(dateStr);
System.out.println(date.getYear()); // 116

```

There are 4 different styles for the text format, `SHORT`, `MEDIUM` (this is the default), `LONG` and `FULL`, all of which depend on the locale. If no locale is specified, the system default locale is used.

|Style|Locale.US|Locale.France
|---|---|---|---|---|---|---|---|---|---
|SHORT|6/30/09|30/06/09
|MEDIUM|Jun 30, 2009|30 juin 2009
|LONG|June 30, 2009|30 juin 2009
|FULL|Tuesday, June 30, 2009|mardi 30 juin 2009



## Convert formatted string representation of date to Date object


This method can be used to convert a formatted string representation of a date into a `Date` object.

```

   /**
     * Parses the date using the given format.
     *
     * @param formattedDate the formatted date string
     * @param dateFormat the date format which was used to create the string.
     * @return the date
     */
    public static Date parseDate(String formattedDate, String dateFormat) {
        Date date = null;
        SimpleDateFormat objDf = new SimpleDateFormat(dateFormat);
        try {
          date = objDf.parse(formattedDate);
        } catch (ParseException e) {
          // Do what ever needs to be done with exception.
        }
        return date;
    }

```



## Time Zones and java.util.Date


A `java.util.Date` object **does not** have a concept of time zone.

- There is no way to **set** a timezone for a Date
- There is no way to **change** the timezone of a Date object
- A Date object created with the `new Date()` default constructor will be initialised with the current time in the system default timezone

However, it is possible to display the date represented by the point in time described by the Date object in a different time zone using e.g. `java.text.SimpleDateFormat`:

```java
Date date = new Date();
//print default time zone
System.out.println(TimeZone.getDefault().getDisplayName());
SimpleDateFormat sdf = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"); //note: time zone not in format!
//print date in the original time zone
System.out.println(sdf.format(date));
//current time in London
sdf.setTimeZone(TimeZone.getTimeZone("Europe/London"));
System.out.println(sdf.format(date));

```

Output:

```java
Central European Time
2016-07-21 22:50:56
2016-07-21 21:50:56

```



## LocalTime


To use just the time part of a Date use LocalTime. You can instantiate a LocalTime object in a couple ways

1. `LocalTime time = LocalTime.now();`
1. `time = LocalTime.MIDNIGHT;`
1. `time = LocalTime.NOON;`
1. `time = LocalTime.of(12, 12, 45);`

`LocalTime` also has a built in toString method that displays the format very nicely.

```java
System.out.println(time); 

```

you can also get, add and subtract hours, minutes, seconds, and nanoseconds from the LocalTime object i.e.

```java
time.plusMinutes(1);
time.getMinutes();
time.minusMinutes(1);

```

You can turn it into a Date object with the following code:

```java
LocalTime lTime = LocalTime.now();
Instant instant = lTime.atDate(LocalDate.of(A_YEAR, A_MONTH, A_DAY)).
        atZone(ZoneId.systemDefault()).toInstant();
Date time = Date.from(instant);

```

this class works very nicely within a timer class to simulate an alarm clock.



#### Syntax


- `Date object = new Date();`
- `Date object = new Date(long date);`



#### Parameters


|Parameter|Explanation
|---|---|---|---|---|---|---|---|---|---
|No parameter|Creates a new Date object using the allocation time (to the nearest millisecond)
|long date|Creates a new Date object with the time set to the number of milliseconds since "the epoch" (January 1, 1970, 00:00:00 GMT)



#### Remarks


**Representation**

Internally, a Java Date object is represented as a long; it is the number of milliseconds since a specific time (referred to as the **epoch**). The original Java Date class had methods for dealing with time zones, etc., but these were deprecated in favor of the then-new Calendar class.

So if all you want to do in your code is represent a specific time, you can create a Date class and store it, etc. If you want to print out a human-readable version of that date, however, you create a Calendar class and use its formatting to produce hours, minutes, seconds, days, time zones, etc. Keep in mind that a specific millisecond is displayed as different hours in different time zones; normally you want to display one in the "local" time zone, but the formatting methods have to take into account that you may want to display it for some other one.

Also be aware that the clocks used by JVMs do not usually have millisecond accuracy; the clock might only "tick" every 10 milliseconds, and therefore, if timing things, you cannot rely on measuring things accurately at that level.

**Import Statement**

```java
import java.util.Date;

```

The `Date` class may be imported from `java.util` package.

**Caution**

`Date` instances are mutable, so using them can make it difficult to write
thread-safe code or can accidentally provide write access to internal state. For example, in the below class, the `getDate()` method allows the caller to modify the transaction date:

```java
public final class Transaction {
  private final Date date;

  public Date getTransactionDate() {
    return date;
  }
}

```

The solution is to either return a copy of the `date` field or use the new APIs in `java.time` introduced in Java 8.

Most of the constructor methods in the `Date` class have been deprecated and should not be used. In almost all cases, it is advisable to use `Calendar` class for date operations.

**Java 8**

Java 8 introduces new time and date API in the package `java.time`, including [LocalDate](http://docs.oracle.com/javase/8/docs/api/java/time/LocalDate.html) and [LocalTime](http://docs.oracle.com/javase/8/docs/api/java/time/LocalTime.html). The classes in the `java.time` package provide an overhauled API that is easier to use. If you are writing to Java 8 it is strongly encouraged that you use this new API. See [Dates and Time (java.time.*)](http://stackoverflow.com/documentation/java/4813/dates-and-time-java-time#t=201611231701424697591) .

