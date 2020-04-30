---
metaTitle: "Handling Time Zones"
description: "Retrieve the current date and time in a particular time zone., Convert a stored `DATE` or `DATETIME` value to another time zone., Retrieve stored `TIMESTAMP` values in a particular time zone, What is my server's local time zone setting?, What time_zone values are available in my server?"
---

# Handling Time Zones



## Retrieve the current date and time in a particular time zone.


This fetches the value of `NOW()` in local time, in India Standard Time, and then again in UTC.

```sql
SELECT NOW();
SET time_zone='Asia/Kolkata'; 
SELECT NOW();
SET time_zone='UTC'; 
SELECT NOW();

```



## Convert a stored `DATE` or `DATETIME` value to another time zone.


If you have a stored `DATE` or `DATETIME` (in a column somewhere) it was stored with respect to some time zone, but in MySQL the time zone is **not** stored with the value.  So, if you want to convert it to another time zone, you can, but you must know the original time zone. Using `CONVERT_TZ()` does the conversion.  This example shows rows sold in California in local time.

```sql
SELECT CONVERT_TZ(date_sold,'UTC','America/Los_Angeles') date_sold_local
  FROM sales
 WHERE state_sold = 'CA'

```



## Retrieve stored `TIMESTAMP` values in a particular time zone


This is really easy. All `TIMESTAMP` values are stored in universal time, and always converted to the present `time_zone` setting whenever they are rendered.

```sql
SET SESSION time_zone='America/Los_Angeles'; 
SELECT timestamp_sold
  FROM sales
 WHERE state_sold = 'CA'

```

Why is this? `TIMESTAMP` values are based on the venerable [UNIX `time_t` data type](https://en.wikipedia.org/wiki/Unix_time). Those UNIX timestamps are stored as a number of seconds since `1970-01-01 00:00:00` UTC.

**Notice** `TIMESTAMP` values are stored in universal time. `DATE` and `DATETIME` values are stored in whatever local time was in effect when they were stored.



## What is my server's local time zone setting?


Each server has a default global time_zone setting, configured by the owner of the server machine. You can find out the current time zone setting this way:

```sql
SELECT @@time_zone

```

Unfortunately, that usually yields the value `SYSTEM`, meaning the MySQL time is governed by the server OS's time zone setting.

This sequence of queries (yes, [it's a hack](https://en.wikipedia.org/wiki/Kludge#Computer_science)) gives you back the offset in minutes between the server's time zone setting and UTC.

```sql
CREATE TEMPORARY TABLE times (dt DATETIME, ts TIMESTAMP);
SET time_zone = 'UTC';
INSERT INTO times VALUES(NOW(), NOW());
SET time_zone = 'SYSTEM';
SELECT dt, ts, TIMESTAMPDIFF(MINUTE, dt, ts)offset FROM times;
DROP TEMPORARY TABLE times;

```

How does this work? The two columns in the temporary table with different data types is the clue.  `DATETIME` data types are always stored in local time in tables, and `TIMESTAMP`s in UTC.  So the `INSERT` statement, performed when the time_zone is set to UTC, stores two identical date / time values.

Then, the SELECT statement, is done when the time_zone is set to server local time. `TIMESTAMP`s are always translated from their stored UTC form to local time in SELECT statements.  `DATETIME`s are not. So the [`TIMESTAMPDIFF(MINUTE...)` operation](https://dev.mysql.com/doc/refman/5.7/en/date-and-time-functions.html#function_timestampdiff) computes the difference between local and universal time.



## What time_zone values are available in my server?


To get a list of possible time_zone values in your MySQL server instance, use this command.

```

SELECT mysql.time_zone_name.name

```

Ordinarily, this shows the [ZoneInfo list of time zones](https://www.iana.org/time-zones) maintained by Paul Eggert at the [Internet Assigned Numbers Authority](https://www.iana.org/). Worldwide there are appproximately 600 time zones.

Unix-like operating systems (Linux distributions, BSD distributions, and modern Mac OS distributions, for example) receive routine updates. Installing these updates on an operating system lets the MySQL instances running there track the changes in time zone and daylight / standard time changeovers.

If you get a much shorter list of time zone names, your server is either incompletely configured or running on Windows.  [Here are instructions](https://dev.mysql.com/doc/refman/5.7/en/time-zone-support.html) for your server administrator to install and maintain the ZoneInfo list.



#### Remarks


When you need to handle time information for a worldwide user base in MySQL, use the TIMESTAMP data type in your tables.

For each user, store a user-preference timezone column. VARCHAR(64) is a good data type for that column. When a user registers to use your system, ask for the time zone value. Mine is Atlantic Time, `America/Edmonton`. Yours might or might not be `Asia/Kolkata` or `Australia/NSW`. For a user interface for this user-preference setting, the WordPress.org software has a good example.

Finally, whenever you establish a connection from your host program (Java, php, whatever) to your DBMS on behalf of a user, issue the SQL command

```

SET SESSION time_zone='(whatever tz string the user gave you)'

```

before you handle any user data involving times. Then all the `TIMESTAMP` times you have install will render in the user's local time.

This will cause all times going in to your tables to be converted to UTC, and all times coming out to be translated to local. It works properly for NOW() and CURDATE(). Again, you must use TIMESTAMP and not DATETIME or DATE data types for this.

Make sure your server OS and default MySQL time zones are set to UTC. If you don't do this before you start loading information into your database, it will be almost impossible to fix. If you use a vendor to run MySQL, insist they get this right.

