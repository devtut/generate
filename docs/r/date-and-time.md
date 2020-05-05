---
metaTitle: "R - Date and Time"
description: "Current Date and Time, Go to the End of the Month, Go to First Day of the Month, Move a date a number of months consistently by months"
---

# Date and Time


R comes with classes for dates, date-times and time differences; see  `?Dates`, `?DateTimeClasses`, `?difftime` and follow the "See Also" section of those docs for further documentation. Related Docs: [Dates](http://stackoverflow.com/documentation/r/9015) and [Date-Time Classes](http://stackoverflow.com/documentation/r/9027).



## Current Date and Time


R is able to access the current date, time and time zone:

```r
Sys.Date()             # Returns date as a Date object

## [1] "2016-07-21"

Sys.time()             # Returns date & time at current locale as a POSIXct object

## [1] "2016-07-21 10:04:39 CDT"

as.numeric(Sys.time()) # Seconds from UNIX Epoch (1970-01-01 00:00:00 UTC)

## [1] 1469113479

Sys.timezone()         # Time zone at current location

## [1] "Australia/Melbourne"

```

Use `OlsonNames()` to view the time zone names in Olson/IANA database on the current system:

```r
str(OlsonNames())
## chr [1:589] "Africa/Abidjan" "Africa/Accra" "Africa/Addis_Ababa" "Africa/Algiers" "Africa/Asmara" "Africa/Asmera" "Africa/Bamako" ...

```



## Go to the End of the Month


Let's say we want to go to the last day of the month, this function will help on it:

```r
eom <- function(x, p=as.POSIXlt(x)) as.Date(modifyList(p, list(mon=p$mon + 1, mday=0)))

```

Test:

```r
x <- seq(as.POSIXct("2000-12-10"),as.POSIXct("2001-05-10"),by="months")
> data.frame(before=x,after=eom(x))
      before      after
1 2000-12-10 2000-12-31
2 2001-01-10 2001-01-31
3 2001-02-10 2001-02-28
4 2001-03-10 2001-03-31
5 2001-04-10 2001-04-30
6 2001-05-10 2001-05-31
> 

```

Using a date in a string format:

```r
> eom('2000-01-01')
[1] "2000-01-31"

```



## Go to First Day of the Month


Let's say we want to go to the first day of a given month:

```r
date <- as.Date("2017-01-20")

> as.POSIXlt(cut(date, "month"))
[1] "2017-01-01 EST"

```



## Move a date a number of months consistently by months


Let's say we want to move a given date a `num`of months. We can define the following function, that uses the `mondate` package:

```r
moveNumOfMonths <- function(date, num) {
    as.Date(mondate(date) + num)
}

```

It moves consistently the month part of the date and adjusting the day, in case the date refers to the last day of the month.

For example:

Back one month:

```r
> moveNumOfMonths("2017-10-30",-1)
[1] "2017-09-30"

```

Back two months:

```r
> moveNumOfMonths("2017-10-30",-2)
[1] "2017-08-30"

```

Forward two months:

```r
> moveNumOfMonths("2017-02-28", 2)
[1] "2017-04-30"

```

It moves two months from the last day of February, therefore the last day of April.

Let's se how it works for backward and forward operations when it is the last day of the month:

```r
> moveNumOfMonths("2016-11-30", 2)
[1] "2017-01-31"
> moveNumOfMonths("2017-01-31", -2)
[1] "2016-11-30"

```

Because November has 30 days, we get the same date in the backward operation, but:

```r
> moveNumOfMonths("2017-01-30", -2)
[1] "2016-11-30"
> moveNumOfMonths("2016-11-30", 2)
[1] "2017-01-31"

```

Because January has 31 days, then moving two months from last day of November will get the last day of January.



#### Remarks


### Classes

<li>
[POSIXct](http://stackoverflow.com/documentation/r/9027)
A date-time class, POSIXct stores time as seconds since UNIX epoch on `1970-01-01 00:00:00 UTC`. It is the format returned when pulling the current time with `Sys.Time()`.
</li>
<li>
[POSIXlt](http://stackoverflow.com/documentation/r/9027)
A date-time class, stores a list of day, month, year, hour, minute, second, and so on. This is the format returned by `strptime`.
</li>
<li>
<p>[Date](http://stackoverflow.com/documentation/r/9015)
The only date class, stores the date as a floating-point number.</p>
</li>

### Selecting a date-time format

POSIXct is the sole option in the tidyverse and world of UNIX. It is faster and takes up less memory than POSIXlt.

```r
origin = as.POSIXct("1970-01-01 00:00:00", format ="%Y-%m-%d %H:%M:%S", tz = "UTC")

origin
## [1] "1970-01-01 UTC"

origin + 47
## [1] "1970-01-01 00:00:47 UTC"

as.numeric(origin)     # At epoch
## 0

as.numeric(Sys.time()) # Right now (output as of July 21, 2016 at 11:47:37 EDT)
## 1469116057

posixlt = as.POSIXlt(Sys.time(), format ="%Y-%m-%d %H:%M:%S", tz = "America/Chicago")

# Conversion to POISXct
posixct = as.POSIXct(posixlt)
posixct

# Accessing components
posixlt$sec   # Seconds 0-61
posixlt$min   # Minutes 0-59
posixlt$hour  # Hour 0-23
posixlt$mday  # Day of the Month 1-31
posixlt$mon   # Months after the first of the year 0-11
posixlt$year  # Years since 1900.

ct = as.POSIXct("2015-05-25")
lt = as.POSIXlt("2015-05-25")

object.size(ct)
# 520 bytes
object.size(lt)
# 1816 bytes

```

### Specialized packages

- anytime
- data.table IDate and ITime
- fasttime
- [lubridate](http://stackoverflow.com/documentation/r/2496)
- nanotime

