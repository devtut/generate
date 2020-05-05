---
metaTitle: "Java - Calendar and its Subclasses"
description: "Creating Calendar objects, Increasing / Decreasing calendar fields, Finding AM/PM, Subtracting calendars"
---

# Calendar and its Subclasses



## Creating Calendar objects


`Calendar` objects can be created by using `getInstance()` or by using the constructor `GregorianCalendar`.

It's important to notice that months in `Calendar` are zero based, which means that JANUARY is represented by an `int` value 0. In order to provide a better code, always use `Calendar` constants, such as `Calendar.JANUARY` to avoid misunderstandings.

```java
Calendar calendar = Calendar.getInstance();
Calendar gregorianCalendar = new GregorianCalendar();
Calendar gregorianCalendarAtSpecificDay = new GregorianCalendar(2016, Calendar.JANUARY, 1);
Calendar gregorianCalendarAtSpecificDayAndTime = new GregorianCalendar(2016, Calendar.JANUARY, 1, 6, 55, 10);

```

**Note**: Always use the month constants: The numeric representation is [misleading](http://stackoverflow.com/questions/344380/why-is-january-month-0-in-java-calendar), e.g. `Calendar.JANUARY` has the value `0`



## Increasing / Decreasing calendar fields


`add()` and `roll()` can be used to increase/decrease `Calendar` fields.

```java
Calendar calendar = new GregorianCalendar(2016, Calendar.MARCH, 31); // 31 March 2016

```

The `add()` method affects all fields, and behaves effectively if one were to add or subtract actual dates from the calendar

```java
calendar.add(Calendar.MONTH, -6);

```

The above operation removes six months from the calendar, taking us back to 30 September 2015.

To change a particular field without affecting the other fields, use `roll()`.

```java
calendar.roll(Calendar.MONTH, -6);

```

The above operation removes six months from the current **month**, so the month is identified as September.  No other fields have been adjusted; the year has not changed with this operation.



## Finding AM/PM


With Calendar class it is easy to find AM or PM.

```

 Calendar cal = Calendar.getInstance();
  cal.setTime(new Date());
  if (cal.get(Calendar.AM_PM) == Calendar.PM)
      System.out.println("It is PM");

```



## Subtracting calendars


To get a difference between two `Calendar`s, use `getTimeInMillis()` method:

```java
Calendar c1 = Calendar.getInstance();
Calendar c2 = Calendar.getInstance();
c2.set(Calendar.DATE, c2.get(Calendar.DATE) + 1);

System.out.println(c2.getTimeInMillis() - c1.getTimeInMillis()); //outputs 86400000 (24 * 60 * 60 * 1000)

```



#### Remarks


As of Java 8, `Calendar` and its subclasses have been superseded by the [java.time](http://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html) package and its subpackages.  They should be preferred, unless a legacy API requires Calendar.

