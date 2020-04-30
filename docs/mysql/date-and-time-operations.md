---
metaTitle: "Date and Time Operations"
description: "Date arithmetic, SYSDATE(), NOW(), CURDATE(), Now(), Testing against a date range, Extract Date from Given Date or DateTime Expression, Using an index for a date and time lookup"
---

# Date and Time Operations




## Date arithmetic


```sql
NOW() + INTERVAL 1 DAY  -- This time tomorrow

CURDATE() - INTERVAL 4 DAY -- Midnight 4 mornings ago

```

Show the mysql questions stored that were
asked 3 to 10 hours ago (180 to 600 minutes ago):

```sql
SELECT qId,askDate,minuteDiff  
FROM 
(   SELECT qId,askDate,
    TIMESTAMPDIFF(MINUTE,askDate,now()) as minuteDiff 
    FROM questions_mysql 
) xDerived 
WHERE minuteDiff BETWEEN 180 AND 600 
ORDER BY qId DESC 
LIMIT 50;

+----------+---------------------+------------+
| qId      | askDate             | minuteDiff |
+----------+---------------------+------------+
| 38546828 | 2016-07-23 22:06:50 |        182 |
| 38546733 | 2016-07-23 21:53:26 |        195 |
| 38546707 | 2016-07-23 21:48:46 |        200 |
| 38546687 | 2016-07-23 21:45:26 |        203 |
| ...      |                     |            |
+----------+---------------------+------------+

```

MySQL manual pages for [`TIMESTAMPDIFF()`](https://dev.mysql.com/doc/refman/5.7/en/date-and-time-functions.html#function_timestampdiff).

**Beware** Do not try to use expressions like `CURDATE() + 1` for date arithmetic in MySQL. They don't return what you expect, especially if you're accustomed to the Oracle database product. Use `CURDATE() + INTERVAL 1 DAY` instead.



## SYSDATE(), NOW(), CURDATE()


```

 SELECT SYSDATE();

```

This function returns the current date and time as a value in `'YYYY-MM-DD HH:MM:SS'` or `YYYYMMDDHHMMSS` format, depending on whether the function is used in a string or numeric context. It returns the date and time in the current time zone.

```

 SELECT NOW();

```

This function is a synonym for `SYSDATE()`.

```

 SELECT CURDATE();

```

This function returns the current date, without any time,  as a value in `'YYYY-MM-DD'` or `YYYYMMDD` format, depending on whether the function is used in a string or numeric context. It returns the date in the current time zone.



## Now()


```sql
Select Now();

```

Shows the current server date and time.

```sql
Update `footable` set mydatefield = Now();

```

This will update the field `mydatefield` with current server date and time in server's configured timezone, e.g.

```sql
'2016-07-21 12:00:00'

```



## Testing against a date range


Although it is very tempting to use `BETWEEN` ... `AND` ... for a date range, it is problematical.  Instead, this pattern avoids most problems:

```sql
WHERE x >= '2016-02-25'
  AND x  < '2016-02-25' + INTERVAL 5 DAY

```

Advantages:

- `BETWEEN` is 'inclusive' thereby including the final date or second.
- `23:59:59` is clumsy and wrong if you have microsecond resolution on a `DATETIME`.
- This pattern avoid dealing with leap years and other data calculations.
- It works whether `x` is `DATE`, `DATETIME` or `TIMESTAMP`.



## Extract Date from Given Date or DateTime Expression


```sql
SELECT DATE('2003-12-31 01:02:03');

```

The output will be:

```sql
2003-12-31

```



## Using an index for a date and time lookup


Many real-world database tables have many rows with `DATETIME` OR `TIMESTAMP` column values spanning a lot of time, including years or even decades. Often it's necessary to use a `WHERE` clause to retrieve some subset of that timespan. For example, we might want to retrieve rows for the date 1-September-2016 from a table.

An inefficient way to do that is this:

```

WHERE DATE(x) = '2016-09-01'   /* slow! */

```

It's inefficient because it applies a function -- `DATE()` -- to the values of a column. That means MySQL must examine each value of `x`, and an index cannot be used.

A better way to do the operation is this

```

WHERE x >= '2016-09-01'
   AND x <  '2016-09-01' + INTERVAL 1 DAY

```

This selects a range of values of `x` lying anywhere on the day in question, up until but **not including** (hence `<`) midnight on the next day.

If the table has an index on the `x` column, then the database server can perform a range scan on the index. That means it can quickly find the first relevant value of x, and then scan the index sequentially until it finds the last relevant value. An index range scan is much more efficient than the full table scan required by `DATE(x) = '2016-09-01`.

Don't be tempted to use this, even though it looks more efficient.

```

 WHERE x BETWEEN '2016-09-01' AND '2016-09-01' + INTERVAL 1 DAY  /*  wrong! */

```

It has the same efficiency as the range scan, but it will select rows with values of `x` falling exactly at midnight on 2-Sept-2016, which is not what you want.

