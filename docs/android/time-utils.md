---
metaTitle: "Android - Time Utils"
description: "Convert Date Format into Milliseconds, To check within a period, GetCurrentRealTime"
---

# Time Utils



## Convert Date Format into Milliseconds


To Convert you date in dd/MM/yyyy format into milliseconds you call this function with data as String

```java
public long getMilliFromDate(String dateFormat) {
    Date date = new Date();
    SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
    try {
        date = formatter.parse(dateFormat);
    } catch (ParseException e) {
        e.printStackTrace();
    }
    System.out.println("Today is " + date);
    return date.getTime();
}

```

This method converts milliseconds to Time-stamp Format date :

```java
public String getTimeStamp(long timeinMillies) {
    String date = null;
    SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss"); // modify format
    date = formatter.format(new Date(timeinMillies));
    System.out.println("Today is " + date);

    return date;
}

```

This Method will convert given specific day,month and year into milliseconds. It will be very help when using `Timpicker` or `Datepicker`

```java
public static long getTimeInMillis(int day, int month, int year) {
    Calendar calendar = Calendar.getInstance();
    calendar.set(year, month, day);
    return calendar.getTimeInMillis();
}

```

It will return milliseconds from date

```java
public static String getNormalDate(long timeInMillies) {
    String date = null;
    SimpleDateFormat formatter = new SimpleDateFormat("dd/MM/yyyy");
    date = formatter.format(timeInMillies);
    System.out.println("Today is " + date);
    return date;
}

```

It will return current date

```java
public static String getCurrentDate() {
    Calendar c = Calendar.getInstance();
    System.out.println("Current time => " + c.getTime());
    SimpleDateFormat df = new SimpleDateFormat("dd/MM/yyyy");
    String formattedDate = df.format(c.getTime());
    return formattedDate;
}

```

**Note :** Java Provides numbers of date format support [Date Pattern](https://developer.android.com/reference/java/text/SimpleDateFormat.html#number)



## To check within a period


This example will help to verify the given time is within a period or not.

**To check the time is today, We can use **DateUtils** class**

```java
boolean isToday = DateUtils.isToday(timeInMillis);

```

To check the time is within a week,

```java
private static boolean isWithinWeek(final long millis) {
    return System.currentTimeMillis() - millis <= (DateUtils.WEEK_IN_MILLIS - DateUtils.DAY_IN_MILLIS);
}

```

To check the time is within a year,

```java
private static boolean isWithinYear(final long millis) {
    return System.currentTimeMillis() - millis <= DateUtils.YEAR_IN_MILLIS;
}

```

To check the time is within a number day of day including today,

```java
public static boolean isWithinDay(long timeInMillis, int day) {
    long diff = System.currentTimeMillis() - timeInMillis;

    float dayCount = (float) (diff / DateUtils.DAY_IN_MILLIS);

    return dayCount < day;
}

```

Note : DateUtils is **android.text.format.DateUtils**



## GetCurrentRealTime


This calculate current device time and add/subtract difference between real and device time

```java
public static Calendar getCurrentRealTime() {

    long bootTime = networkTime - SystemClock.elapsedRealtime();
    Calendar calInstance = Calendar.getInstance();
    calInstance.setTimeZone(getUTCTimeZone());
    long currentDeviceTime = bootTime + SystemClock.elapsedRealtime();
    calInstance.setTimeInMillis(currentDeviceTime);
    return calInstance;
}

```

get UTC based timezone.

```java
public static TimeZone getUTCTimeZone() {
    return TimeZone.getTimeZone("GMT");
}

```

