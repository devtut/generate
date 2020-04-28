---
metaTitle: "Datetime Class"
description: "getTimestamp, setDate, Add or Subtract Date Intervals, Create DateTime from custom format, Printing DateTimes, Create Immutable version of DateTime from Mutable prior PHP 5.6"
---

# Datetime Class



## getTimestamp


`getTimeStemp` is a unix representation of a datetime object.

```
$date = new DateTime();
echo $date->getTimestamp(); 

```

this will out put an integer indication the seconds that have elapsed since 00:00:00 UTC, Thursday, 1 January 1970.



## setDate


`setDate` sets the date in a DateTime object.

```
$date = new DateTime();
$date->setDate(2016, 7, 25);

```

this example sets the date to be the twenty-fifth of July, 2015, it will produce the following result:

```
2016-07-25 17:52:15.819442

```



## Add or Subtract Date Intervals


We can use the class [DateInterval](http://php.net/manual/pt_BR/class.dateinterval.php) to add or subtract some interval in a DateTime object.

See the example below, where we are adding an interval of 7 days and printing a message on the screen:

```
$now = new DateTime();// empty argument returns the current date
$interval = new DateInterval('P7D');//this objet represents a 7 days interval
$lastDay = $now->add($interval); //this will return a DateTime object
$formatedLastDay = $lastDay->format('Y-m-d');//this method format the DateTime object and returns a String
echo "Samara says: Seven Days. You'll be happy on $formatedLastDay.";

```

This will output (running on Aug 1st, 2016):

> 
Samara says: Seven Days. You'll be happy on 2016-08-08.


We can use the sub method in a similar way to subtract dates

```
$now->sub($interval);
echo "Samara says: Seven Days. You were happy last on $formatedLastDay.";

```

This will output (running on Aug 1st, 2016):

> 
Samara says: Seven Days. You were happy last on 2016-07-25.




## Create DateTime from custom format


PHP is able to parse [a number of date formats](https://secure.php.net/manual/en/datetime.formats.php). If you want to parse a non-standard format, or if you want your code to explicitly state the format to be used, then you can use the static [`DateTime::createFromFormat`](https://php.net/manual/en/datetime.createfromformat.php) method:

**Object oriented style**

```
$format = "Y,m,d";
$time = "2009,2,26";
$date = DateTime::createFromFormat($format, $time);

```

**Procedural style**

```
$format = "Y,m,d";
$time = "2009,2,26";
$date = date_create_from_format($format, $time);

```



## Printing DateTimes


PHP 4+ supplies a method, format that converts a DateTime object into a string with a desired format. According to PHP Manual, this is the object oriented function:

```
public string DateTime::format ( string $format )

```

The function date() takes one parameters - a format, which is a string

### Format

The format is a string, and uses single characters to define the format:

- **Y**: four digit representation of the year (eg: 2016)
- **y**: two digit representation of the year (eg: 16)
- **m**: month, as a number (01 to 12)
- **M**: month, as three letters (Jan, Feb, Mar, etc)
- **j**: day of the month, with no leading zeroes (1 to 31)
- **D**: day of the week, as three letters (Mon, Tue, Wed, etc)
- **h**: hour (12-hour format) (01 to 12)
- **H**: hour (24-hour format) (00 to 23)
- **A**: either AM or PM
- **i**: minute, with leading zeroes (00 to 59)
- **s**: second, with leading zeroes (00 to 59)
- The complete list can be found [here](http://php.net/manual/en/function.date.php)

### Usage

These characters can be used in various combinations to display times in virtually any format. Here are some examples:

```
$date = new DateTime('2000-05-26T13:30:20'); /* Friday, May 26, 2000 at 1:30:20 PM */

$date->format("H:i");
/* Returns 13:30 */

$date->format("H i s");
/* Returns 13 30 20 */

$date->format("h:i:s A");
/* Returns 01:30:20 PM */

$date->format("j/m/Y");
/* Returns 26/05/2000 */

$date->format("D, M j 'y - h:i A");
/* Returns Fri, May 26 '00 - 01:30 PM */

```

### Procedural

The procedural format is similar:

### Object-Oriented

```
$date->format($format)

```

### Procedural Equivalent

```
date_format($date, $format)

```



## Create Immutable version of DateTime from Mutable prior PHP 5.6


To create `\DateTimeImmutable` in PHP 5.6+ use:

```
\DateTimeImmutable::createFromMutable($concrete);

```

Prior PHP 5.6 you can use:

```
\DateTimeImmutable::createFromFormat(\DateTime::ISO8601, $mutable->format(\DateTime::ISO8601), $mutable->getTimezone());

```

