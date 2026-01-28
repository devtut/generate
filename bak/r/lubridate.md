---
metaTitle: "R - lubridate"
description: "Parsing dates and datetimes from strings with lubridate, Difference between period and duration, Instants, Intervals, Durations and Periods, Manipulating date and time in lubridate, Time Zones, Parsing date and time in lubridate, Rounding dates"
---

# lubridate




## Parsing dates and datetimes from strings with lubridate


The `lubridate` package provides convenient functions to format date and datetime objects from character strings. The functions are permutations of

|Letter|Element to parse|Base R equivalent
|---|---|---
|y|year|`%y`, `%Y`
|m (with y and d)|month|`%m`, `%b`, `%h`, `%B`
|d|day|`%d`, `%e`
|h|hour|`%H`, `%I%p`
|m (with h and s)|minute|`%M`
|s|seconds|`%S`

e.g. `ymd()` for parsing a date with the year followed by the month followed by the day, e.g. `"2016-07-22"`, or `ymd_hms()` for parsing a datetime in the order year, month, day, hours, minutes, seconds, e.g. `"2016-07-22 13:04:47"`.

The functions are able to recognize most separators (such as `/`, `-`, and whitespace) without additional arguments. They also work with inconsistent separators.

### Dates

The date functions return an object of class `Date`.

```r
library(lubridate) 

mdy(c(' 07/02/2016 ', '7 / 03 / 2016', ' 7 / 4 / 16 '))
## [1] "2016-07-02" "2016-07-03" "2016-07-04"

ymd(c("20160724","2016/07/23","2016-07-25"))    # inconsistent separators
## [1] "2016-07-24" "2016-07-23" "2016-07-25"

```

### Datetimes

### Utility functions

Datetimes can be parsed using `ymd_hms` variants including `ymd_hm` and `ymd_h`. All datetime functions can accept a `tz` timezone argument akin to that of `as.POSIXct` or `strptime`, but which defaults to `"UTC"` instead of the local timezone.

The datetime functions return an object of class `POSIXct`.

```r
x <- c("20160724 130102","2016/07/23 14:02:01","2016-07-25 15:03:00")
ymd_hms(x, tz="EST")
## [1] "2016-07-24 13:01:02 EST" "2016-07-23 14:02:01 EST"
## [3] "2016-07-25 15:03:00 EST"

ymd_hms(x)
## [1] "2016-07-24 13:01:02 UTC" "2016-07-23 14:02:01 UTC"
## [3] "2016-07-25 15:03:00 UTC"

```

### Parser functions

`lubridate` also includes three functions for parsing datetimes with a formatting string like `as.POSIXct` or `strptime`:

|Function|Output Class|Formatting strings accepted
|---|---|---
|`parse_date_time`|POSIXct|Flexible. Will accept `strptime`-style with `%` or `lubridate` datetime function name style, e.g `"ymd hms"`. Will accept a vector of orders for heterogeneous data and guess which is appropriate.
|`parse_date_time2`|Default POSIXct; if `lt = TRUE`, POSIXlt|Strict. Accepts only `strptime` tokens (with or without `%`) from a limited set.
|`fast_strptime`|Default POSIXlt; if `lt = FALSE`, POSIXct|Strict. Accepts only `%`-delimited `strptime` tokens with delimiters (`-`, `/`, `:`, etc.) from a limited set.

```r
x <- c('2016-07-22 13:04:47', '07/22/2016 1:04:47 pm')

parse_date_time(x, orders = c('mdy Imsp', 'ymd hms'))
## [1] "2016-07-22 13:04:47 UTC" "2016-07-22 13:04:47 UTC"

x <- c('2016-07-22 13:04:47', '2016-07-22 14:47:58')

parse_date_time2(x, orders = 'Ymd HMS')
## [1] "2016-07-22 13:04:47 UTC" "2016-07-22 14:47:58 UTC"

fast_strptime(x, format = '%Y-%m-%d %H:%M:%S')
## [1] "2016-07-22 13:04:47 UTC" "2016-07-22 14:47:58 UTC"

```

`parse_date_time2` and `fast_strptime` use a fast C parser for efficiency.

See `?parse_date_time` for formatting tokens.



## Difference between period and duration


Unlike durations, periods can be used to accurately model clock times without knowing when events such as leap seconds, leap days, and DST changes occur.

```r
start_2012 <- ymd_hms("2012-01-01 12:00:00")
## [1] "2012-01-01 12:00:00 UTC"

# period() considers leap year calculations.
start_2012 + period(1, "years")
## [1] "2013-01-01 12:00:00 UTC"

# Here duration() doesn't consider leap year calculations. 
start_2012 + duration(1)
## [1] "2012-12-31 12:00:00 UTC"

```



## Instants


An instant is a specific moment in time. Any date-time object that refers to a moment of time is recognized as an instant. To test if an object is an instant, use `is.instant`.

```r
library(lubridate)

today_start <- dmy_hms("22.07.2016 12:00:00", tz = "IST") # default tz="UTC"
today_start
## [1] "2016-07-22 12:00:00 IST"
is.instant(today_start)
## [1] TRUE

now_dt <- ymd_hms(now(), tz="IST")
now_dt
## [1] "2016-07-22 13:53:09 IST"
is.instant(now_dt)
## [1] TRUE

is.instant("helloworld")
## [1] FALSE
is.instant(60)
## [1] FALSE

```



## Intervals, Durations and Periods


**Intervals** are simplest way of recording timespans in lubridate. An interval is a span of time that occurs between two specific **instants**.

```r
# create interval by substracting two instants
today_start <- ymd_hms("2016-07-22 12-00-00", tz="IST")
today_start
## [1] "2016-07-22 12:00:00 IST"
today_end <- ymd_hms("2016-07-22 23-59-59", tz="IST")
today_end
## [1] "2016-07-22 23:59:59 IST"
span <- today_end - today_start
span
## Time difference of 11.99972 hours
as.interval(span, today_start)
## [1] 2016-07-22 12:00:00 IST--2016-07-22 23:59:59 IST

# create interval using interval() function
span <- interval(today_start, today_end)
[1] 2016-07-22 12:00:00 IST--2016-07-22 23:59:59 IST

```

**Durations** measure the exact amount of time that occurs between two instants.

```r
duration(60, "seconds")
## [1] "60s"

duration(2, "minutes")
## [1] "120s (~2 minutes)"

```

**Note:** Units larger than weeks are not used due to their variability.

Durations can be created using `dseconds`, `dminutes` and other duration helper functions.<br />
Run `?quick_durations` for complete list.

```r
dseconds(60)
## [1] "60s"

dhours(2)
## [1] "7200s (~2 hours)"

dyears(1)
## [1] "31536000s (~365 days)"

```

Durations can be subtracted and added to instants to get new instants.

```r
today_start + dhours(5)
## [1] "2016-07-22 17:00:00 IST"

today_start + dhours(5) + dminutes(30) + dseconds(15)
## [1] "2016-07-22 17:30:15 IST"

```

Durations can be created from intervals.

```r
as.duration(span)
[1] "43199s (~12 hours)"

```

**Periods** measure the change in clock time that occurs between two instants.

Periods can be created using `period` function as well other helper functions
like `seconds`, `hours`, etc. To get a complete list of period helper functions, Run `?quick_periods`.

```r
period(1, "hour")
## [1] "1H 0M 0S"

hours(1)
## [1] "1H 0M 0S"

period(6, "months")
## [1] "6m 0d 0H 0M 0S"

months(6)
## [1] "6m 0d 0H 0M 0S"

years(1)
## [1] "1y 0m 0d 0H 0M 0S"

```

`is.period` function can be used to check if an object is a period.

```r
is.period(years(1))
## [1] TRUE

is.period(dyears(1))    
## [1] FALSE

```



## Manipulating date and time in lubridate


```r
date <- now()
date
## "2016-07-22 03:42:35 IST"

year(date)
## 2016

minute(date)
## 42

wday(date, label = T, abbr = T)
# [1] Fri
# Levels: Sun < Mon < Tues < Wed < Thurs < Fri < Sat

day(date) <- 31
## "2016-07-31 03:42:35 IST"

# If an element is set to a larger value than it supports, the difference
#  will roll over into the next higher element
day(date) <- 32
## "2016-08-01 03:42:35 IST"

```



## Time Zones


`with_tz` returns a date-time as it would appear in a different time zone.

```r
nyc_time <- now("America/New_York")
nyc_time
## [1] "2016-07-22 05:49:08 EDT"

# corresponding Europe/Moscow time
with_tz(nyc_time, tzone = "Europe/Moscow")
## [1] "2016-07-22 12:49:08 MSK"

```

`force_tz` returns a the date-time that has the same clock time as x in the new time zone.

```r
nyc_time <- now("America/New_York")
nyc_time
## [1] "2016-07-22 05:49:08 EDT"

force_tz(nyc_time, tzone = "Europe/Moscow") # only timezone changes
## [1] "2016-07-22 05:49:08 MSK"

```



## Parsing date and time in lubridate


Lubridate provides `ymd()` series of functions for parsing character strings into dates. The letters y, m, and d correspond to the year, month, and day elements of a date-time.

```r
mdy("07-21-2016")                 # Returns Date

## [1] "2016-07-21"

mdy("07-21-2016", tz = "UTC")     # Returns a vector of class POSIXt

## "2016-07-21 UTC"

dmy("21-07-2016")                 # Returns Date

## [1] "2016-07-21"

dmy(c("21.07.2016", "22.07.2016")) # Returns vector of class Date

## [1] "2016-07-21" "2016-07-22"

```



## Rounding dates


```r
now_dt <- ymd_hms(now(), tz="IST")
now_dt
## [1] "2016-07-22 13:53:09 IST"

```

`round_date()` takes a date-time object and rounds it to the nearest integer value of the specified time unit.

```r
round_date(now_dt, "minute")
## [1] "2016-07-22 13:53:00 IST"

round_date(now_dt, "hour")
## [1] "2016-07-22 14:00:00 IST"

round_date(now_dt, "year")
## [1] "2017-01-01 IST"

```

`floor_date()` takes a date-time object and rounds it down to the nearest integer value of the specified time unit.

```r
floor_date(now_dt, "minute")
## [1] "2016-07-22 13:53:00 IST"

floor_date(now_dt, "hour")
## [1] "2016-07-22 13:00:00 IST"

floor_date(now_dt, "year")
## [1] "2016-01-01 IST"

```

`ceiling_date()` takes a date-time object and rounds it up to the nearest integer value of the specified time unit.

```r
ceiling_date(now_dt, "minute")
## [1] "2016-07-22 13:54:00 IST"

ceiling_date(now_dt, "hour")
## [1] "2016-07-22 14:00:00 IST"

ceiling_date(now_dt, "year")
## [1] "2017-01-01 IST"

```



#### Syntax


- ymd_hms(..., quiet = FALSE, tz = "UTC", locale = Sys.getlocale("LC_TIME"))
- now(tzone = "")
- interval(start, end, tzone = attr(start, "tzone"))
- duration(num = NULL, units = "seconds", ...)
- period(num = NULL, units = "second", ...)



#### Remarks


To install package from CRAN:

```r
install.packages("lubridate")

```

To install development version from Github:

```r
library(devtools)
# dev mode allows testing of development packages in a sandbox, without interfering
# with the other packages you have installed.
dev_mode(on=T)
install_github("hadley/lubridate")
dev_mode(on=F)

```

To get vignettes on lubridate package:

```r
vignette("lubridate")

```

To get help about some function `foo`:

```r
help(foo)     # help about function foo
?foo          # same thing

# Example
# help("is.period")
# ?is.period

```

To get examples for a function `foo`:

```r
example("foo")

# Example
# example("interval")

```

