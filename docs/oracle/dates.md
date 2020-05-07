---
metaTitle: "Oracle Database - Dates"
description: "Date Arithmetic - Difference between Dates in Days, Hours, Minutes and/or Seconds, Setting the Default Date Format Model, Date Arithmetic - Difference between Dates in Months or Years, Generating Dates with No Time Component, Generating Dates with a Time Component, The Format of a Date, Converting Dates to a String, Changing How SQL/Plus or SQL Developer Display Dates, Extract the Year, Month, Day, Hour, Minute or Second Components of a Date, Time Zones and Daylight Savings Time, Leap Seconds, Getting the Day of the Week"
---

# Dates



## Date Arithmetic - Difference between Dates in Days, Hours, Minutes and/or Seconds


In oracle, the difference (in days and/or fractions thereof) between two `DATE`s can be found using subtraction:

```sql
SELECT DATE '2016-03-23' - DATE '2015-12-25' AS difference FROM DUAL;

```

Outputs the number of days between the two dates:

```sql
DIFFERENCE
----------
        89

```

And:

```sql
SELECT TO_DATE( '2016-01-02 01:01:12', 'YYYY-MM-DD HH24:MI:SS' )
         - TO_DATE( '2016-01-01 00:00:00', 'YYYY-MM-DD HH24:MI:SS' )
         AS difference
FROM   DUAL

```

Outputs the fraction of days between two dates:

```sql
DIFFERENCE
----------
    1.0425

```

The difference in hours, minutes or seconds can be found by multiplying this number by `24`, `24*60` or `24*60*60` respectively.

The previous example can be changed to get the days, hours, minutes and seconds between two dates using:

```sql
SELECT TRUNC( difference                       ) AS days,
       TRUNC( MOD( difference * 24,       24 ) ) AS hours,
       TRUNC( MOD( difference * 24*60,    60 ) ) AS minutes,
       TRUNC( MOD( difference * 24*60*60, 60 ) ) AS seconds
FROM   (
  SELECT TO_DATE( '2016-01-02 01:01:12', 'YYYY-MM-DD HH24:MI:SS' )
         - TO_DATE( '2016-01-01 00:00:00', 'YYYY-MM-DD HH24:MI:SS' )
         AS difference
  FROM   DUAL

```

);

**(Note: [`TRUNC()`](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions201.htm) is used rather than [`FLOOR()`](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions058.htm) to correctly handle negative differences.)**

Outputs:

```sql
DAYS HOURS MINUTES SECONDS
---- ----- ------- -------
   1     1       1      12

```

The previous example can also be solved by converting the numeric difference to an [interval](https://docs.oracle.com/cd/B19306_01/server.102/b14200/sql_elements003.htm#i38598) using [`NUMTODSINTERVAL()`](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions103.htm):

```sql
SELECT EXTRACT( DAY    FROM difference ) AS days,
       EXTRACT( HOUR   FROM difference ) AS hours,
       EXTRACT( MINUTE FROM difference ) AS minutes,
       EXTRACT( SECOND FROM difference ) AS seconds
FROM   (
  SELECT NUMTODSINTERVAL(
           TO_DATE( '2016-01-02 01:01:12', 'YYYY-MM-DD HH24:MI:SS' )
             - TO_DATE( '2016-01-01 00:00:00', 'YYYY-MM-DD HH24:MI:SS' ),
           'DAY'
         ) AS difference
  FROM   DUAL
);

```



## Setting the Default Date Format Model


When Oracle implicitly converts from a `DATE` to a string or vice-versa (or when [`TO_CHAR()`](http://docs.oracle.com/cd/B19306_01/server.102/b14200/functions180.htm) or [`TO_DATE()`](http://docs.oracle.com/cd/B19306_01/server.102/b14200/functions183.htm) are explicitly called without a format model) the `NLS_DATE_FORMAT` session parameter will be used as the format model in the conversion. If the literal does not match the format model then an exception will be raised.

You can review this parameter using:

```sql
SELECT VALUE FROM NLS_SESSION_PARAMETERS WHERE PARAMETER = 'NLS_DATE_FORMAT';

```

You can set this value within your current session using:

```sql
ALTER SESSION SET NLS_DATE_FORMAT = 'YYYY-MM-DD HH24:MI:SS';

```

**(Note: this does not change the value for any other users.)**

If you rely on the `NLS_DATE_FORMAT` to provide the format mask in `TO_DATE()` or `TO_CHAR()` then you should not be surprised when your queries break if this value is ever changed.



## Date Arithmetic - Difference between Dates in Months or Years


The difference in months between two dates can be found using the [`MONTHS_BETWEEN( date1, date2 )`](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions089.htm):

```sql
SELECT MONTHS_BETWEEN( DATE '2016-03-10', DATE '2015-03-10' ) AS difference FROM DUAL;

```

Outputs:

```sql
DIFFERENCE
----------
        12

```

If the difference includes part months then it will return the fraction of the month based on there being **31** days in each month:

```sql
SELECT MONTHS_BETWEEN( DATE '2015-02-15', DATE '2015-01-01' ) AS difference FROM DUAL;

```

Outputs:

```sql
DIFFERENCE
----------
 1.4516129

```

Due to `MONTHS_BETWEEN` assuming 31 days per month when there can be fewer days per month then this can result in different values for differences spanning the boundaries between months.

Example:

```sql
SELECT MONTHS_BETWEEN( DATE'2016-02-01', DATE'2016-02-01' - INTERVAL '1' DAY ) AS "JAN-FEB",
       MONTHS_BETWEEN( DATE'2016-03-01', DATE'2016-03-01' - INTERVAL '1' DAY ) AS "FEB-MAR",
       MONTHS_BETWEEN( DATE'2016-04-01', DATE'2016-04-01' - INTERVAL '1' DAY ) AS "MAR-APR",
       MONTHS_BETWEEN( DATE'2016-05-01', DATE'2016-05-01' - INTERVAL '1' DAY ) AS "APR-MAY"
FROM   DUAL;

```

Output:

```sql
JAN-FEB FEB-MAR MAR-APR APR-MAY
------- ------- ------- -------
0.03226 0.09677 0.03226 0.06452

```

The difference in years can be found by dividing the month difference by 12.



## Generating Dates with No Time Component


All `DATE`s have a time component; however, it is customary to store dates which do not need to include time information with the hours/minutes/seconds set to zero (i.e. midnight).

Use an [ANSI `DATE` literal](https://docs.oracle.com/cd/B19306_01/server.102/b14200/sql_elements003.htm#BABGIGCJ) (using [ISO 8601 Date format](https://en.wikipedia.org/wiki/ISO_8601#Calendar_dates)):

```sql
SELECT DATE '2000-01-01' FROM DUAL;

```

Convert it from a string literal using [`TO_DATE()`](http://docs.oracle.com/cd/B19306_01/server.102/b14200/functions183.htm):

```sql
SELECT TO_DATE( '2001-01-01', 'YYYY-MM-DD' ) FROM DUAL;

```

**(More information on the [date format models](http://docs.oracle.com/cd/B19306_01/server.102/b14200/sql_elements004.htm#i34924) can be found in the Oracle documentation.)**

or:

```sql
SELECT TO_DATE(
         'January 1, 2000, 00:00 A.M.',
         'Month dd, YYYY, HH12:MI A.M.',
         'NLS_DATE_LANGUAGE = American'
       )
FROM   DUAL;

```

**(If you are converting language specific terms such as month names then it is good practice to include the 3rd `nlsparam` parameter to the `TO_DATE()` function and specify the language to be expected.)**



## Generating Dates with a Time Component


Convert it from a string literal using [`TO_DATE()`](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions183.htm):

```sql
SELECT TO_DATE( '2000-01-01 12:00:00', 'YYYY-MM-DD HH24:MI:SS' ) FROM DUAL;

```

Or use a [`TIMESTAMP` literal](https://docs.oracle.com/cd/B19306_01/server.102/b14200/sql_elements003.htm#sthref367):

```sql
CREATE TABLE date_table(
  date_value DATE
);

INSERT INTO date_table ( date_value ) VALUES ( TIMESTAMP '2000-01-01 12:00:00' );

```

Oracle will implicitly cast a `TIMESTAMP` to a `DATE` when storing it in a `DATE` column of a table; however you can explicitly [`CAST()`](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions016.htm) the value to a `DATE`:

```sql
SELECT CAST( TIMESTAMP '2000-01-01 12:00:00' AS DATE ) FROM DUAL;

```



## The Format of a Date


In Oracle a `DATE` data type does not have a format; when Oracle sends a `DATE` to the client program (SQL/Plus, SQL/Developer, Toad, Java, Python, etc) it will send 7- or 8- bytes which represent the date.

A `DATE` which is not stored in a table (i.e. generated by `SYSDATE` and having "type 13" when using the `DUMP()` command) has 8-bytes and has the structure (the numbers on the right are the internal representation of `2012-11-26 16:41:09`):

```sql
BYTE VALUE                           EXAMPLE
---- ------------------------------- --------------------------------------
1    Year modulo 256                 220
2    Year multiples of 256           7   (7 * 256 + 220 = 2012)
3    Month                           11
4    Day                             26
5    Hours                           16
6    Minutes                         41
7    Seconds                         9
8    Unused                          0

```

A `DATE` which is stored in a table ("type 12" when using the `DUMP()` command) has 7-bytes and has the structure (the numbers on the right are the internal representation of `2012-11-26 16:41:09`):

```sql
BYTE VALUE                           EXAMPLE
---- ------------------------------- --------------------------------------
1    ( Year multiples of 100 ) + 100 120
2    ( Year modulo 100 ) + 100       112 ((120-100)*100 + (112-100) = 2012)
3    Month                           11
4    Day                             26
5    Hours + 1                       17
6    Minutes + 1                     42
7    Seconds + 1                     10

```

If you want the date to have a specific format then you will need to convert it to something that has a format (i.e. a string). The SQL client may implicitly do this or you can explicitly [convert the value to a string](http://stackoverflow.com/documentation/oracle/2087/dates/6849/converting-dates-to-a-string) using [`TO_CHAR( date, format_model, nls_params )`](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions180.htm).



## Converting Dates to a String


Use [`TO_CHAR( date [, format_model [, nls_params]] )`](http://docs.oracle.com/cd/B19306_01/server.102/b14200/functions180.htm):

**(Note: if a [format model](https://docs.oracle.com/cd/B28359_01/server.111/b28286/sql_elements004.htm#i34924) is not provided then the `NLS_DATE_FORMAT` session parameter will be used as the [default format model](http://stackoverflow.com/documentation/oracle/2087/dates/6850/setting-the-default-date-format-model); this can be different for every session so should not be relied on. It is good practice to always specify the format model.)**

```sql
CREATE TABLE table_name (
  date_value DATE
);

INSERT INTO table_name ( date_value ) VALUES ( DATE '2000-01-01' );
INSERT INTO table_name ( date_value ) VALUES ( TIMESTAMP '2016-07-21 08:00:00' );
INSERT INTO table_name ( date_value ) VALUES ( SYSDATE );

```

Then:

```sql
SELECT TO_CHAR( date_value, 'YYYY-MM-DD' ) AS formatted_date FROM table_name;

```

Outputs:

```sql
FORMATTED_DATE
--------------
2000-01-01
2016-07-21
2016-07-21

```

And:

```sql
SELECT TO_CHAR(
         date_value,
         'FMMonth d yyyy, hh12:mi:ss AM',
         'NLS_DATE_LANGUAGE = French'
       ) AS formatted_date
FROM   table_name;

```

Outputs:

```sql
FORMATTED_DATE
-----------------------------
Janvier   01 2000, 12:00:00 AM
Juillet   21 2016, 08:00:00 AM
Juillet   21 2016, 19:08:31 PM

```



## Changing How SQL/Plus or SQL Developer Display Dates


When SQL/Plus or SQL Developer display dates they will perform an implicit conversion to a string using the default date format model (see the [Setting the Default Date Format Model](http://stackoverflow.com/documentation/oracle/2087/dates/6850/setting-the-default-date-format-model) example).

You can change how a date is displayed by changing the `NLS_DATE_FORMAT` parameter.



## Extract the Year, Month, Day, Hour, Minute or Second Components of a Date


The year, month or day components of a `DATE` data type can be found using the [`EXTRACT( [ YEAR | MONTH | DAY ] FROM datevalue )`](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions050.htm)

```sql
SELECT EXTRACT (YEAR  FROM DATE '2016-07-25') AS YEAR,
       EXTRACT (MONTH FROM DATE '2016-07-25') AS MONTH,
       EXTRACT (DAY   FROM DATE '2016-07-25') AS DAY
FROM DUAL;

```

Outputs:

```sql
YEAR MONTH DAY
---- ----- ---
2016     7  25

```

The time (hour, minute or second) components can be found by either:

- Using [`CAST( datevalue AS TIMESTAMP )`](http://docs.oracle.com/cd/B19306_01/server.102/b14200/functions016.htm) to convert the `DATE` to a `TIMESTAMP` and then using [`EXTRACT( [ HOUR | MINUTE | SECOND ] FROM timestampvalue )`](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions050.htm); or
- Using [`TO_CHAR( datevalue, format_model )`](http://docs.oracle.com/cd/B19306_01/server.102/b14200/functions180.htm) to get the value as a string.

For example:

```sql
SELECT EXTRACT( HOUR   FROM CAST( datetime AS TIMESTAMP ) ) AS Hours,
       EXTRACT( MINUTE FROM CAST( datetime AS TIMESTAMP ) ) AS Minutes,
       EXTRACT( SECOND FROM CAST( datetime AS TIMESTAMP ) ) AS Seconds
FROM   (
  SELECT TO_DATE( '2016-01-01 09:42:01', 'YYYY-MM-DD HH24:MI:SS' ) AS datetime FROM DUAL
);

```

Outputs:

```sql
HOURS MINUTES SECONDS
----- ------- -------
    9      42       1

```



## Time Zones and Daylight Savings Time


The `DATE` data type does not handle time zones or changes in daylight savings time.

Either:

- use the [`TIMESTAMP WITH TIME ZONE` data type](https://docs.oracle.com/cd/B19306_01/server.102/b14225/ch4datetime.htm); or
- handle the changes in your application logic.

A `DATE` can be stored as Coordinated Universal Time (UTC) and converted to the current session time zone like this:

```sql
SELECT FROM_TZ(
         CAST(
           TO_DATE( '2016-01-01 12:00:00', 'YYYY-MM-DD HH24:MI:SS' )
           AS TIMESTAMP
         ),
         'UTC'
       )
       AT LOCAL AS time
FROM   DUAL;

```

If you run `ALTER SESSION SET TIME_ZONE = '+01:00';` then the output is:

```sql
TIME
------------------------------------
2016-01-01 13:00:00.000000000 +01:00

```

and `ALTER SESSION SET TIME_ZONE = 'PST';` then the output is:

```sql
TIME
------------------------------------
2016-01-01 04:00:00.000000000 PST

```



## Leap Seconds


Oracle [does not handle leap seconds](http://stackoverflow.com/questions/31136211/how-to-handle-leap-seconds-in-oracle). See My Oracle Support note `2019397.2` and `730795.1` for more details.



## Getting the Day of the Week


You can use [`TO_CHAR( date_value, 'D' )`](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions180.htm) to get the day-of-week.

However, this is dependent on the `NLS_TERRITORY` session parameter:

```sql
ALTER SESSION SET NLS_TERRITORY = 'AMERICA';        -- First day of week is Sunday
SELECT TO_CHAR( DATE '1970-01-01', 'D' ) FROM DUAL;

```

Outputs `5`

```sql
ALTER SESSION SET NLS_TERRITORY = 'UNITED KINGDOM'; -- First day of week is Monday
SELECT TO_CHAR( DATE '1970-01-01', 'D' ) FROM DUAL;

```

Outputs `4`

To do this independent of the `NLS` settings, you can truncate the date to midnight of the current day (to remove any fractions of days) and subtract the date truncated to the start of the current iso-week (which always starts on Monday):

```sql
SELECT TRUNC( date_value ) - TRUNC( date_value, 'IW' ) + 1 FROM DUAL

```

