---
metaTitle: "MySQL - Time with subsecond precision"
description: "Get the current time with millisecond precision, Get the current time in a form that looks like a Javascript timestamp., Create a  table with columns to store sub-second time., Convert a millisecond-precision date / time value to text., Store a Javascript timestamp into a TIMESTAMP column"
---

# Time with subsecond precision



## Get the current time with millisecond precision


```sql
SELECT NOW(3)

```

does the trick.



## Get the current time in a form that looks like a Javascript timestamp.


Javascript timestamps are based on the venerable UNIX `time_t` data type, and show the number of milliseconds since `1970-01-01 00:00:00` UTC.

This expression gets the current time as a Javascript timestamp integer. (It does so correctly regardless of the current time_zone setting.)

```

ROUND(UNIX_TIMESTAMP(NOW(3)) * 1000.0, 0)

```

If you have `TIMESTAMP` values stored in a column, you can retrieve them as integer Javascript timestamps using the UNIX_TIMESTAMP() function.

```

 SELECT ROUND(UNIX_TIMESTAMP(column) * 1000.0, 0)

```

If your column contains `DATETIME` columns and you retrieve them as Javascript timestamps, those timestamps will be offset by the time zone offset of the time zone they're stored in.



## Create a  table with columns to store sub-second time.


```sql
CREATE TABLE times (
     dt DATETIME(3), 
     ts TIMESTAMP(3)
 );

```

makes a table with millisecond-precision date / time fields.

```

INSERT INTO times VALUES (NOW(3), NOW(3));

```

inserts a row containing `NOW()` values with millisecond precision into the table.

```

INSERT INTO times VALUES ('2015-01-01 16:34:00.123','2015-01-01 16:34:00.128');

```

inserts specific millisecond precision values.

**Notice** that you must use `NOW(3)` rather than `NOW()` if you use that function to insert high-precision time values.



## Convert a millisecond-precision date / time value to text.


`%f` is the fractional precision format specifier for [the DATE_FORMAT() function](https://dev.mysql.com/doc/refman/5.7/en/date-and-time-functions.html#function_date-format).

```sql
SELECT DATE_FORMAT(NOW(3), '%Y-%m-%d %H:%i:%s.%f')

```

displays a value like `2016-11-19 09:52:53.248000` with fractional microseconds. Because we used `NOW(3)`, the final three digits in the fraction are 0.



## Store a Javascript timestamp into a TIMESTAMP column


If you have a Javascript timestamp value, for example `1478960868932`, you can convert that to a MySQL fractional time value like this:

```sql
FROM_UNIXTIME(1478960868932 * 0.001)

```

It's simple to use that kind of expression to store your Javascript timestamp into a MySQL table. Do this:

```sql
INSERT INTO table (col) VALUES (FROM_UNIXTIME(1478960868932 * 0.001))

```

(Obviously, you'll want to insert other columns.)



#### Remarks


You need to be at MySQL version 5.6.4 or later to declare columns with fractional-second time datatypes.

For example, `DATETIME(3)` will give you millisecond resolution in your timestamps, and `TIMESTAMP(6)` will give you microsecond resolution on a *nix-style timestamp.

Read this: [http://dev.mysql.com/doc/refman/5.7/en/fractional-seconds.html](http://dev.mysql.com/doc/refman/5.7/en/fractional-seconds.html)

`NOW(3)` will give you the present time from your MySQL server's operating system with millisecond precision.

(Notice that MySQL internal fractional arithmetic, like * 0.001, is always handled as IEEE754 double precision floating point, so it's unlikely you'll lose precision before the Sun becomes a white dwarf star.)

