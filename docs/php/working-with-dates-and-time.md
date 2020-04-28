---
metaTitle: "Working with Dates and Time"
description: "Getting the difference between two dates / times, Convert a date into another format, Parse English date descriptions into a Date format, Using Predefined Constants for Date Format"
---

# Working with Dates and Time



## Getting the difference between two dates / times


The most feasible way is to use, the `DateTime` class.

An example:

```
<?php
// Create a date time object, which has the value of ~ two years ago
$twoYearsAgo = new DateTime("2014-01-18 20:05:56");
// Create a date time object, which has the value of ~ now
$now = new DateTime("2016-07-21 02:55:07");

// Calculate the diff
$diff = $now->diff($twoYearsAgo);

// $diff->y contains the difference in years between the two dates
$yearsDiff = $diff->y;
// $diff->m contains the difference in minutes between the two dates
$monthsDiff = $diff->m;
// $diff->d contains the difference in days between the two dates
$daysDiff = $diff->d;
// $diff->h contains the difference in hours between the two dates
$hoursDiff = $diff->h;
// $diff->i contains the difference in minutes between the two dates
$minsDiff = $diff->i;
// $diff->s contains the difference in seconds between the two dates
$secondsDiff = $diff->s;

// Total Days Diff, that is the number of days between the two dates
$totalDaysDiff = $diff->days;

// Dump the diff altogether just to get some details ;)
var_dump($diff);

```

Also, comparing two dates is much easier, just use the [Comparison operators](http://stackoverflow.com/documentation/php/1687/operators/6231/comparison-operators) , like:

```
<?php
// Create a date time object, which has the value of ~ two years ago
$twoYearsAgo = new DateTime("2014-01-18 20:05:56");
// Create a date time object, which has the value of ~ now
$now = new DateTime("2016-07-21 02:55:07");
var_dump($now > $twoYearsAgo); // prints bool(true)
var_dump($twoYearsAgo > $now); // prints bool(false)
var_dump($twoYearsAgo <= $twoYearsAgo); // prints bool(true)
var_dump($now == $now); // prints bool(true)

```



## Convert a date into another format


**The Basics**

The simplist way to convert one date format into another is to use [`strtotime()`](http://docs.php.net/manual/en/function.strtotime.php) with [`date()`](http://docs.php.net/manual/en/function.date.php). `strtotime()` will convert the date into a [Unix Timestamp](http://en.wikipedia.org/wiki/Unix_time). That Unix Timestamp can then be passed to `date()` to convert it to the new format.

```
$timestamp = strtotime('2008-07-01T22:35:17.02');
$new_date_format = date('Y-m-d H:i:s', $timestamp);

```

Or as a one-liner:

```
$new_date_format = date('Y-m-d H:i:s', strtotime('2008-07-01T22:35:17.02'));

```

Keep in mind that `strtotime()` requires the date to be in a [valid format](https://php.net/manual/en/datetime.formats.php). Failure to provide a valid format will result in `strtotime()` returning false which will cause your date to be 1969-12-31.

**Using `DateTime()`**

As of PHP 5.2, PHP offered the [`DateTime()`](http://docs.php.net/manual/en/class.datetime.php) class which offers us more powerful tools for working with dates (and time). We can rewrite the above code using `DateTime()` as so:

```
$date = new DateTime('2008-07-01T22:35:17.02');
$new_date_format = $date->format('Y-m-d H:i:s');

```

**Working with Unix timestamps**

`date()` takes a Unix timestamp as its second parameter and returns a formatted date for you:

```
$new_date_format = date('Y-m-d H:i:s', '1234567890');

```

DateTime() works with Unix timestamps by adding an `@` before the timestamp:

```
$date = new DateTime('@1234567890');
$new_date_format = $date->format('Y-m-d H:i:s');

```

If the timestamp you have is in milliseconds (it may end in `000` and/or the timestamp is thirteen characters long) you will need to convert it to seconds before you can can convert it to another format. There's two ways to do this:

- Trim the last three digits off using [`substr()`](http://php.net/manual/en/function.substr.php)

Trimming the last three digits can be acheived several ways, but using `substr()` is the easiest:

```
$timestamp = substr('1234567899000', -3);

```

- Divide the substr by 1000

You can also convert the timestamp into seconds by dividing by 1000. Because the timestamp is too large for 32 bit systems to do math on you will need to use the [BCMath](http://php.net/manual/en/book.bc.php) library to do the math as strings:

```
$timestamp = bcdiv('1234567899000', '1000');

```

To get a Unix Timestamp you can use `strtotime()` which returns a Unix Timestamp:

```
$timestamp = strtotime('1973-04-18');

```

With DateTime() you can use [`DateTime::getTimestamp()`](http://php.net/manual/en/datetime.gettimestamp.php)

```
$date = new DateTime('2008-07-01T22:35:17.02');
$timestamp = $date->getTimestamp();

```

If you're running PHP 5.2 you can use the `U` formatting option instead:

```
$date = new DateTime('2008-07-01T22:35:17.02');
$timestamp = $date->format('U');

```

**Working with non-standard and ambiguous date formats**

Unfortunately not all dates that a developer has to work with are in a standard format. Fortunately PHP 5.3 provided us with a solution for that. [`DateTime::createFromFormat()`](http://docs.php.net/manual/en/datetime.createfromformat.php) allows us to tell PHP what format a date string is in so it can be successfully parsed into a DateTime object for further manipulation.

```
$date = DateTime::createFromFormat('F-d-Y h:i A', 'April-18-1973 9:48 AM');
$new_date_format = $date->format('Y-m-d H:i:s');

```

In PHP 5.4 we gained the ability to do class member access on instantiation has been added which allows us to turn our `DateTime()` code into a one-liner:

```
$new_date_format = (new DateTime('2008-07-01T22:35:17.02'))->format('Y-m-d H:i:s');

```

Unfortunately this does not work with `DateTime::createFromFormat()` yet.



## Parse English date descriptions into a Date format


Using the [`strtotime()`](https://secure.php.net/manual/en/function.strtotime.php) function combined with [`date()`](https://secure.php.net/manual/en/function.date.php) you can parse different English text descriptions to dates:

```
// Gets the current date
echo date("m/d/Y", strtotime("now")), "\n"; // prints the current date
echo date("m/d/Y", strtotime("10 September 2000")), "\n"; // prints September 10, 2000 in the m/d/Y format
echo date("m/d/Y", strtotime("-1 day")), "\n"; // prints yesterday's date
echo date("m/d/Y", strtotime("+1 week")), "\n"; // prints the result of the current date + a week
echo date("m/d/Y", strtotime("+1 week 2 days 4 hours 2 seconds")), "\n"; // same as the last example but with extra days, hours, and seconds added to it
echo date("m/d/Y", strtotime("next Thursday")), "\n"; // prints next Thursday's date
echo date("m/d/Y", strtotime("last Monday")), "\n"; // prints last Monday's date
echo date("m/d/Y", strtotime("First day of next month")), "\n"; // prints date of first day of next month
echo date("m/d/Y", strtotime("Last day of next month")), "\n"; // prints date of last day of next month
echo date("m/d/Y", strtotime("First day of last month")), "\n"; // prints date of first day of last month
echo date("m/d/Y", strtotime("Last day of last month")), "\n"; // prints date of last day of last month

```



## Using Predefined Constants for Date Format


We can use Predefined Constants for Date format in `date()` instead of the conventional date format strings since PHP 5.1.0.

**Predefined Date Format Constants Available**

`DATE_ATOM` - Atom (2016-07-22T14:50:01+00:00)

`DATE_COOKIE` - HTTP Cookies (Friday, 22-Jul-16 14:50:01 UTC)

`DATE_RSS` - RSS (Fri, 22 Jul 2016 14:50:01 +0000)

`DATE_W3C` - World Wide Web Consortium (2016-07-22T14:50:01+00:00)

`DATE_ISO8601` - ISO-8601 (2016-07-22T14:50:01+0000)

`DATE_RFC822` - RFC 822 (Fri, 22 Jul 16 14:50:01 +0000)

`DATE_RFC850` - RFC 850 (Friday, 22-Jul-16 14:50:01 UTC)

`DATE_RFC1036` - RFC 1036 (Fri, 22 Jul 16 14:50:01 +0000)

`DATE_RFC1123` - RFC 1123 (Fri, 22 Jul 2016 14:50:01 +0000)

`DATE_RFC2822` - RFC 2822 (Fri, 22 Jul 2016 14:50:01 +0000)

`DATE_RFC3339` - Same as DATE_ATOM (2016-07-22T14:50:01+00:00)

**Usage Examples**

```
echo date(DATE_RFC822);

```

> 
This will output: **Fri, 22 Jul 16 14:50:01 +0000**


```
echo date(DATE_ATOM,mktime(0,0,0,8,15,1947));

```

> 
This will output: **1947-08-15T00:00:00+05:30**




#### Syntax


- string date ( string $format [, int $timestamp = time() ] )
- int strtotime ( string $time [, int $now ] )

