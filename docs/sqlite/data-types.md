---
metaTitle: "Data types"
description: "Date/time types, Enforcing column types, TYPEOF function, Using booleans"
---

# Data types



## Date/time types


SQLite has no separate data type for date or time values.

### ISO8601 strings

The built-in keywords `CURRENT_DATE`, `CURRENT_TIME`, and `CURRENT_TIMESTAMP` return strings in ISO8601 format:

```sql
> SELECT CURRENT_DATE, CURRENT_TIME, CURRENT_TIMESTAMP;
CURRENT_DATE  CURRENT_TIME  CURRENT_TIMESTAMP  
------------  ------------  -------------------
2016-07-08    12:34:56      2016-07-08 12:34:56

```

Such values are also understood by all [built-in date/time functions](https://www.sqlite.org/lang_datefunc.html):

```sql
> SELECT strftime('%Y', '2016-07-08');
2016

```

### Julian day numbers

The [built-in date/time functions](https://www.sqlite.org/lang_datefunc.html) interpret numbers as [Julian days](https://en.wikipedia.org/wiki/Julian_day):

```sql
> SELECT datetime(2457578.02425926);
2016-07-08 12:34:56

```

The `julianday()` function converts any supported date/time value into a Julian day number:

```sql
> SELECT julianday('2016-07-08 12:34:56');
2457578.02425926

```

### Unix timestamps

The [built-in date/time functions](https://www.sqlite.org/lang_datefunc.html) can interpret numbers as [Unix timestamps](https://en.wikipedia.org/wiki/Unix_time) with the `unixepoch` modifier:

```sql
> SELECT datetime(0, 'unixepoch');
1970-01-01 00:00:00 

```

The `strftime()` function can convert any supported date/time value into a Unix timestamp:

```sql
> SELECT strftime('%s', '2016-07-08 12:34:56');
1467981296 

```

### unsupported formats

It would be possible to store date/time values in any other format in the database, but the built-in date/time functions will not parse them, and return NULL:

```sql
> SELECT time('1:30:00');   -- not two digits

> SELECT datetime('8 Jul 2016');
 

```



## Enforcing column types


SQLite uses [dynamic typing](http://www.sqlite.org/datatype3.html) and ignores declared column types:

```sql
> CREATE TABLE Test (
      Col1 INTEGER,
      Col2 VARCHAR(2),       -- length is ignored, too
      Col3 BLOB,
      Col4,                  -- no type required
      Col5 FLUFFY BUNNIES    -- use whatever you want
  );
> INSERT INTO Test VALUES (1, 1, 1, 1, 1);
> INSERT INTO Test VALUES ('xxx', 'xxx', 'xxx', 'xxx', 'xxx');
> SELECT * FROM Test;
1   1   1   1   1         
xxx xxx xxx xxx xxx       

```

(However, declared column types are used for [type affinity](http://www.sqlite.org/datatype3.html#affinity).)

To enforce types, you have to add a constraint with the [typeof() function](http://stackoverflow.com/documentation/sqlite/5252/data-types/18671/typeof-function):

```sql
CREATE TABLE Tab (
    Col1 TEXT   CHECK (typeof(Col1) = 'text' AND length(Col1) <= 10),
    [...]
);

```

(If such a column should be NULLable, you have to explicitly allow `'null'`.)



## TYPEOF function


```sql
sqlite> SELECT TYPEOF(NULL);
null
sqlite> SELECT TYPEOF(42);
integer
sqlite> SELECT TYPEOF(3.141592653589793);
real
sqlite> SELECT TYPEOF('Hello, world!');
text
sqlite> SELECT TYPEOF(X'0123456789ABCDEF');
blob

```



## Using booleans


For booleans, SQLite uses integers `0` and `1`:

```sql
sqlite> SELECT 2 + 2 = 4;
1
sqlite> SELECT 'a' = 'b';
0
sqlite> SELECT typeof('a' = 'b');
integer

```

```sql
> CREATE TABLE Users ( Name, IsAdmin );
> INSERT INTO Users VALUES ('root', 1);
> INSERT INTO Users VALUES ('john', 0);
> SELECT Name FROM Users WHERE IsAdmin;
root

```



#### Remarks


official documentation: [Datatypes In SQLite Version 3](http://www.sqlite.org/datatype3.html)

