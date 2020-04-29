---
metaTitle: "LocalTime"
description: "Amount of time between two LocalTime, Intro, Time Modification, Time Zones and their time difference"
---

# LocalTime



## Amount of time between two LocalTime


There are two equivalent ways to calculate the amount of time unit between two `LocalTime`: (1) through `until(Temporal, TemporalUnit)` method and through (2) `TemporalUnit.between(Temporal, Temporal)`.

```java
import java.time.LocalTime;
import java.time.temporal.ChronoUnit;

public class AmountOfTime {

    public static void main(String[] args) {

        LocalTime start = LocalTime.of(1, 0, 0); // hour, minute, second
        LocalTime end = LocalTime.of(2, 10, 20); // hour, minute, second
        
        long halfDays1 = start.until(end, ChronoUnit.HALF_DAYS); // 0
        long halfDays2 = ChronoUnit.HALF_DAYS.between(start, end); // 0
        
        long hours1 = start.until(end, ChronoUnit.HOURS); // 1
        long hours2 = ChronoUnit.HOURS.between(start, end); // 1
        
        long minutes1 = start.until(end, ChronoUnit.MINUTES); // 70
        long minutes2 = ChronoUnit.MINUTES.between(start, end); // 70
        
        long seconds1 = start.until(end, ChronoUnit.SECONDS); // 4220
        long seconds2 = ChronoUnit.SECONDS.between(start, end); // 4220
        
        long millisecs1 = start.until(end, ChronoUnit.MILLIS); // 4220000
        long millisecs2 = ChronoUnit.MILLIS.between(start, end); // 4220000

        long microsecs1 = start.until(end, ChronoUnit.MICROS); // 4220000000
        long microsecs2 = ChronoUnit.MICROS.between(start, end); // 4220000000

        long nanosecs1 = start.until(end, ChronoUnit.NANOS); // 4220000000000
        long nanosecs2 = ChronoUnit.NANOS.between(start, end); // 4220000000000
        
        // Using others ChronoUnit will be thrown UnsupportedTemporalTypeException.
        // The following methods are examples thereof.
        long days1 = start.until(end, ChronoUnit.DAYS); 
        long days2 = ChronoUnit.DAYS.between(start, end); 
    }
}

```



## Intro


LocalTime is an immutable class and thread-safe, used to represent time, often viewed as hour-min-sec. Time is represented to nanosecond precision. For example, the value "13:45.30.123456789" can be stored in a LocalTime.

This class does not store or represent a date or time-zone. Instead, it is a description of the local time as seen on a wall clock. It cannot represent an instant on the time-line without additional information such as an offset or time-zone. This is a value based class, equals method should be used for comparisons.

**Fields**

MAX - The maximum supported LocalTime, '23:59:59.999999999'.
MIDNIGHT, MIN, NOON

**Important Static Methods**

now(), now(Clock clock), now(ZoneId zone), parse(CharSequence text)

**Important Instance Methods**

isAfter(LocalTime other), isBefore(LocalTime other), minus(TemporalAmount amountToSubtract), minus(long amountToSubtract, TemporalUnit unit), plus(TemporalAmount amountToAdd), plus(long amountToAdd, TemporalUnit unit)

```java
ZoneId zone = ZoneId.of("Asia/Kolkata");
LocalTime now = LocalTime.now();
LocalTime now1 = LocalTime.now(zone);
LocalTime then = LocalTime.parse("04:16:40");

```

Difference in time can be calculated in any of following ways

```java
long timeDiff = Duration.between(now, now1).toMinutes();
long timeDiff1 = java.time.temporal.ChronoUnit.MINUTES.between(now2, now1);

```

You can also add/subtract hours, minutes or seconds from any object of LocalTime.

**minusHours(long hoursToSubtract), minusMinutes(long hoursToMinutes), minusNanos(long nanosToSubtract), minusSeconds(long secondsToSubtract), plusHours(long hoursToSubtract), plusMinutes(long hoursToMinutes), plusNanos(long nanosToSubtract), plusSeconds(long secondsToSubtract)**

```java
now.plusHours(1L);
now1.minusMinutes(20L);

```



## Time Modification


You can add hours, minutes, seconds and nanoseconds:

```java
LocalTime time = LocalTime.now();
LocalTime addHours = time.plusHours(5); // Add 5 hours
LocaLTime addMinutes = time.plusMinutes(15) // Add 15 minutes
LocalTime addSeconds = time.plusSeconds(30) // Add 30 seconds
LocalTime addNanoseconds = time.plusNanos(150_000_000) // Add 150.000.000ns (150ms)

```



## Time Zones and their time difference


```java
import java.time.LocalTime;
import java.time.ZoneId;
import java.time.temporal.ChronoUnit;

public class Test {
    public static void main(String[] args)
    {
            ZoneId zone1 = ZoneId.of("Europe/Berlin");
            ZoneId zone2 = ZoneId.of("Brazil/East");

            LocalTime now = LocalTime.now();
            LocalTime now1 = LocalTime.now(zone1);
            LocalTime now2 = LocalTime.now(zone2);
            
            System.out.println("Current Time : " + now);
            System.out.println("Berlin Time : " + now1);
            System.out.println("Brazil Time : " + now2);
            
            long minutesBetween = ChronoUnit.MINUTES.between(now2, now1);
            System.out.println("Minutes Between Berlin and Brazil : " + minutesBetween +"mins");          
    }
}

```



#### Syntax


- LocalTime time = LocalTime.now(); // Initializes with current system clock
- LocalTime time = LocalTime.MIDNIGHT; // 00:00
- LocalTime time = LocalTime.NOON; // 12:00
- LocalTime time = LocalTime.of(12, 12, 45); // 12:12:45



#### Parameters


|Method|Output
|------
|`LocalTime.of(13, 12, 11)`|13:12:11
|`LocalTime.MIDNIGHT`|00:00
|`LocalTime.NOON`|12:00
|`LocalTime.now()`|Current time from system clock
|`LocalTime.MAX`|The maximum supported local time 23:59:59.999999999
|`LocalTime.MIN`|The minimum supported local time 00:00
|`LocalTime.ofSecondOfDay(84399)`|23:59:59 , Obtains Time from second-of-day value
|`LocalTime.ofNanoOfDay(2000000000)`|00:00:02 , Obtains Time from nanos-of-day value



#### Remarks


As class name denotes, `LocalTime` represents a time without a time-zone. It doesn't represent a date. It's a simple label for a given time.

The class is value-based and the `equals` method should be used when doing comparisons.

This class is from the package java.time.

