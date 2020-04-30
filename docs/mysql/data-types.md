---
metaTitle: "Data Types"
description: "Implicit / automatic casting, VARCHAR(255) -- or not, INT as AUTO_INCREMENT, Others, Introduction (numeric), Integer Types, Fixed Point Types, Floating Point Types, Bit Value Type, CHAR(n), DATE, DATETIME, TIMESTAMP, YEAR, and TIME"
---

# Data Types




## Implicit / automatic casting


```sql
select '123' * 2;

```

To make the **multiplication** with `2` MySQL automatically converts the string `123` into a number.

Return value:

> 
246


The conversion to a number starts from left to right. If the conversion is not possible the result is `0`

```sql
select '123ABC' * 2

```

Return value:

> 
246


```sql
select 'ABC123' * 2

```

Return value:

> 
0




## VARCHAR(255) -- or not


**Suggested max len**

First, I will mention some common strings that are always hex, or otherwise limited to ASCII.  For these, you should specify `CHARACTER SET ascii` (`latin1` is ok) so that it will not waste space:

```sql
UUID CHAR(36) CHARACTER SET ascii -- or pack into BINARY(16)
country_code CHAR(2) CHARACTER SET ascii
ip_address CHAR(39) CHARACTER SET ascii -- or pack into BINARY(16)
phone VARCHAR(20) CHARACTER SET ascii -- probably enough to handle extension
postal_code VARCHAR(20) CHARACTER SET ascii -- (not 'zip_code') (don't know the max

city VARCHAR(100) -- This Russian town needs 91:
    Poselok Uchebnogo Khozyaystva Srednego Professionalno-Tekhnicheskoye Uchilishche Nomer Odin
country VARCHAR(50) -- probably enough
name VARCHAR(64) -- probably adequate; more than some government agencies allow

```

**Why not simply 255?**
There are two reasons to avoid the common practice of using (255) for everything.

- When a complex `SELECT` needs to create temporary table (for a subquery, `UNION`, `GROUP BY`, etc), the preferred choice is to use the `MEMORY` engine, which puts the data in RAM.  But `VARCHARs` are turned into `CHAR` in the process.  This makes `VARCHAR(255) CHARACTER SET utf8mb4` take 1020 bytes.  That can lead to needing to spill to disk, which is slower.
- In certain situations, InnoDB will look at the potential size of the columns in a table and decide that it will be too big, aborting a `CREATE TABLE`.

**VARCHAR** versus **TEXT**

Usage hints for `*TEXT`, `CHAR`, and `VARCHAR`, plus some Best Practice:

- Never use `TINYTEXT`.
- Almost never use `CHAR` -- it is fixed length; each character is the max length of the `CHARACTER SET` (eg, 4 bytes/character for utf8mb4).
- With `CHAR`, use `CHARACTER SET ascii` unless you know otherwise.
- `VARCHAR(n)` will truncate at n **characters**; `TEXT` will truncate at some number of **bytes**.  (But, do you want truncation?)
- `*TEXT` **may** slow down complex `SELECTs` due to how temp tables are handled.



## INT as AUTO_INCREMENT


Any size of `INT` may be used for `AUTO_INCREMENT`.  `UNSIGNED` is always appropriate.

Keep in mind that certain operations "burn" `AUTO_INCREMENT` ids.  This could lead to an unexpected gap.  Examples:  `INSERT IGNORE` and `REPLACE`.  They **may** preallocate an id **before** realizing that it won't be needed. This is expected behavior and by design in the InnoDB engine and should not discourage their use.



## Others


There is already a separate entry for "FLOAT, DOUBLE, and DECIMAL" and "ENUM".  A single page on datatypes is likely to be unwieldy -- I suggest "Field types" (or should it be called "Datatypes"?) be an overview, then split into these topic pages:

- INTs
- FLOAT, DOUBLE, and DECIMAL
- Strings (CHARs, TEXT, etc)
- BINARY and BLOB
- DATETIME, TIMESTAMP, and friends
- ENUM and SET
- Spatial data
- [JSON type](http://stackoverflow.com/documentation/mysql/2985/json#t=20170211153143344074) (MySQL 5.7.8+)
- How to represent Money, and other common 'types' that need shoehorning into existing datatypes

Where appropriate, each topic page should include, in addition to syntax and examples:

- Considerations when ALTERing
- Size (bytes)
- Contrast with non-MySQL engines (low priority)
- Considerations when using the datatype in a PRIMARY KEY or secondary key
- other Best Practice
- other Performance issues

(I assume this "example" will self-distruct when my suggestions have been satisfied or vetoed.)



## Introduction (numeric)


MySQL offers a number of different numeric types. These can be broken down into

|Group|Types
|------
|Integer Types|`INTEGER`, `INT`, `SMALLINT`, `TINYINT`, `MEDIUMINT`, `BIGINT`
|Fixed Point Types|`DECIMAL`, `NUMERIC`
|Floating Point Types|`FLOAT`, `DOUBLE`
|Bit Value Type|`BIT`



## Integer Types


Minimal unsigned value is always 0.

|Type|Storage<br>(Bytes)|Minimum Value<br>(Signed)|Maximum Value<br>(Signed)|Maximum Value<br>(Unsigned)
|------
|`TINYINT`|1|-2<sup>7</sup><br>-128|2<sup>7</sup>-1<br>127|2<sup>8</sup>-1<br>255
|`SMALLINT`|2|-2<sup>15</sup><br>-32,768|2<sup>15</sup>-1<br>32,767|2<sup>16</sup>-1<br>65,535
|`MEDIUMINT`|3|-2<sup>23</sup><br>-8,388,608|2<sup>23</sup>-1<br>8,388,607|2<sup>24</sup>-1<br>16,777,215
|`INT`|4|-2<sup>31</sup><br>-2,147,483,648|2<sup>31</sup>-1<br>2,147,483,647|2<sup>32</sup>-1<br>4,294,967,295
|`BIGINT`|8|-2<sup>63</sup><br>-9,223,372,036,854,775,808|2<sup>63</sup>-1<br> 9,223,372,036,854,775,807|2<sup>64</sup>-1<br>18,446,744,073,709,551,615



## Fixed Point Types


MySQL's `DECIMAL` and `NUMERIC` types store exact numeric data values. It is recommended to use these types to preserve exact precision, such as for money.

### Decimal

These values are stored in binary format. In a column declaration, the precision and scale should be specified

Precision represents the number of significant digits that are stored for values.

Scale represents the number of digits stored **after** the decimal

```sql
salary DECIMAL(5,2)

```

5 represents the `precision` and 2 represents the `scale`. For this example, the range of values that can be stored in this column is `-999.99 to 999.99`

If the scale parameter is omitted, it defaults to 0

This data type can store up to 65 digits.

The number of bytes taken by `DECIMAL(M,N)` is **approximately** `M/2`.



## Floating Point Types


`FLOAT` and `DOUBLE` represent **approximate** data types.

|Type|Storage|Precision|Range
|------
|FLOAT|4 bytes|23 significant bits / ~7 decimal digits|10^+/-38
|DOUBLE|8 bytes|53 significant bits / ~16 decimal digits|10^+/-308

`REAL` is a synonym for `FLOAT`.  `DOUBLE PRECISION` is a synonym for `DOUBLE`.

Although MySQL also permits (M,D) qualifier, do **not** use it.  (M,D) means that values can be stored with up to M total digits, where D can be after the decimal.  **Numbers will be rounded twice or truncated; this will cause more trouble than benefit.**

Because floating-point values are approximate and not stored as exact values, attempts to treat them as exact in comparisons may lead to problems.  Note in particular that a `FLOAT` value rarely equals a `DOUBLE` value.



## Bit Value Type


The `BIT` type is useful for storing bit-field values. `BIT(M)` allows storage of up to M-bit values where M is in the range of `1 to 64`

You can also specify values with `bit value` notation.

```sql
b'111'      -> 7
b'10000000' -> 128

```

Sometimes it is handy to use 'shift' to construct a single-bit value, for example `(1 << 7)` for 128.

The maximum combined size of all BIT columns in an `NDB` table is 4096.



## CHAR(n)


`CHAR(n)` is a string of a **fixed** length of `n` **characters**.  If it is `CHARACTER SET utf8mb4`, that means it occupies exactly `4*n` **bytes**, regardless of what text is in it.

Most use cases for `CHAR(n)` involve strings that contain English characters, hence should be `CHARACTER SET ascii`.  (`latin1` will do just as good.)

```sql
country_code CHAR(2) CHARACTER SET ascii,
postal_code  CHAR(6) CHARACTER SET ascii,
uuid    CHAR(39) CHARACTER SET ascii,  -- more discussion elsewhere

```



## DATE, DATETIME, TIMESTAMP, YEAR, and TIME


The `DATE` datatype comprises the date but no time component. Its format is `'YYYY-MM-DD'` with a range of '1000-01-01' to '9999-12-31'.

The `DATETIME` type includes the time with a format of 'YYYY-MM-DD HH:MM:SS'. It has a range from '1000-01-01 00:00:00' to '9999-12-31 23:59:59'.

The `TIMESTAMP` type is an integer type comprising date and time with an effective range from '1970-01-01 00:00:01' UTC to '2038-01-19 03:14:07' UTC.

The `YEAR` type represents a year and holds a range from 1901 to 2155.

The `TIME` type represents a time with a format of 'HH:MM:SS' and holds a range from '-838:59:59' to '838:59:59'.

Storage Requirements:

```sql
|-----------|--------------------|----------------------------------------|
| Data Type | Before MySQL 5.6.4 | as of MySQL 5.6.4                      |
|-----------|--------------------|----------------------------------------|
| YEAR      |      1 byte        |  1 byte                                |
| DATE      |      3 bytes       |  3 bytes                               |
| TIME      |      3 bytes       |  3 bytes + fractional seconds storage  |
| DATETIME  |      8 bytes       |  5 bytes + fractional seconds storage  |
| TIMESTAMP |      4 bytes       |  4 bytes + fractional seconds storage  |
|-----------|--------------------|----------------------------------------|

```

Fractional Seconds (as of Version 5.6.4):

```sql
|------------------------------|------------------|
| Fractional Seconds Precision | Storage Required |
|------------------------------|------------------|
|              0               |      0 bytes     |
|              1,2             |      1 byte      |
|              3,4             |      2 byte      |
|              5,6             |      3 byte      |
|------------------------------|------------------|

```

See the MySQL Manual Pages [DATE, DATETIME, and TIMESTAMP Types](http://dev.mysql.com/doc/refman/5.7/en/datetime.html), [Data Type Storage Requirements](http://dev.mysql.com/doc/refman/5.7/en/storage-requirements.html), and [Fractional Seconds in Time Values](http://dev.mysql.com/doc/refman/5.7/en/fractional-seconds.html).

