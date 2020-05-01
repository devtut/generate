---
metaTitle: "Data Types"
description: "Numeric Types, Date/ Time Types, Geometric Types, Network Adress Types, Character Types, Arrays"
---

# Data Types


PostgreSQL has a rich set of native data types available to users. Users can add new types to PostgreSQL using the CREATE TYPE command.

[https://www.postgresql.org/docs/9.6/static/datatype.html](https://www.postgresql.org/docs/9.6/static/datatype.html)



## Numeric Types


|Name|Storage  Size|Description|Range
|---|---|---|---|---|---|---
|`smallint`|2 bytes|small-range integer|-32768 to +32767
|`integer`|4 bytes|ypical choice for integer|-2147483648 to +2147483647
|`bigint`|8 bytes|large-range integer|-9223372036854775808 to +9223372036854775807
|`decimal`|variable|user-specified precision, exact|up to 131072 digits before the decimal point; up to 16383 digits after the decimal point
|`numeric`|variable|user-specified precision, exact|up to 131072 digits before the decimal point; up to 16383 digits after the decimal point
|`real`|4 bytes|variable-precision, inexact|6 decimal digits precision
|`double precision`|8 bytes|variable-precision, inexact|15 decimal digits precision
|`smallserial`|2 bytes|small autoincrementing integer|1 to 32767
|`serial`|4 bytes|autoincrementing integer|1 to 2147483647
|`bigserial`|8 bytes|large autoincrementing integer|1 to 9223372036854775807
|`int4range`||Range of integer|
|`int8range`||Range of bigint|
|`numrange`||Range of numeric|



## Date/ Time Types


|Name|Storage Size|Description|Low Value|High Value|Resolution
|---|---|---|---|---|---|---
|`timestamp` (without time zone)|8 bytes|both date and time (no time zone)|4713 BC|294276 AD|1 microsecond / 14 digits
|`timestamp` (with time zone)|8 bytes|both date and time, with time zone|4713 BC|294276 AD|1 microsecond / 14 digits
|`date`|4 bytes|date (no time of day)|4713 BC|5874897 AD|1 day
|`time` (without time zone)|8 bytes|time of day (no date)|00:00:00|24:00:00|1 microsecond / 14 digits
|`time` (with time zone)|12 bytes|times of day only, with time zone|00:00:00+1459|24:00:00-1459|1 microsecond / 14 digits
|`interval`|16 bytes|time interval|-178000000 years|178000000 years|1 microsecond / 14 digits
|`tsrange`||range of timestamp without time zone|||
|`tstzrange`||range of timestamp with time zone|||
|`daterange`||range of date|||



## Geometric Types


|Name|Storage Size|Description|Representation
|---|---|---|---|---|---|---
|`point`|16 bytes|Point on a plane|(x,y)
|`line`|32 bytes|Infinite line|{A,B,C}
|`lseg`|32 bytes|Finite line segment|((x1,y1),(x2,y2))
|`box`|32 bytes|Rectangular box|((x1,y1),(x2,y2))
|`path`|16+16n bytes|Closed path (similar to polygon)|((x1,y1),...)
|`path`|16+16n bytes|Open path|[(x1,y1),...]
|`polygon`|40+16n bytes|Polygon (similar to closed path)|((x1,y1),...)
|`circle`|24 bytes|Circle|<(x,y),r> (center point and radius)



## Network Adress Types


|Name|Storage Size|Description
|---|---|---|---|---|---|---
|`cidr`|7 or 19 bytes|IPv4 and IPv6 networks
|`inet`|7 or 19 bytes|IPv4 and IPv6 hosts and networks
|`macaddr`|6 bytes|MAC addresses



## Character Types


|Name|Description
|---|---|---|---|---|---|---
|`character varying(n)`, `varchar(n)`|variable-length with limit
|`character(n)`, `char(n)`|fixed-length, blank padded
|`text`|variable unlimited length



## Arrays


In PostgreSQL you can create Arrays of any built-in, user-defined or enum type. In default there is no limit to an Array, but you **can** specify it.

### Declaring an Array

```sql
SELECT integer[];
SELECT integer[3];
SELECT integer[][];
SELECT integer[3][3];
SELECT integer ARRAY;
SELECT integer ARRAY[3];

```

### Creating an Array

```sql
SELECT '{0,1,2}';
SELECT '{{0,1},{1,2}}';
SELECT ARRAY[0,1,2];
SELECT ARRAY[ARRAY[0,1],ARRAY[1,2]];

```

### Accessing an Array

By default PostgreSQL uses a one-based numbering convention for arrays, that is, an array of n elements starts with `array[1]` and ends with `array[n]`.

```sql
--accesing a spefific element
WITH arr AS (SELECT ARRAY[0,1,2] int_arr) SELECT int_arr[1] FROM arr;

int_arr
---------
        0
(1 row)

--sclicing an array
WITH arr AS (SELECT ARRAY[0,1,2] int_arr) SELECT int_arr[1:2] FROM arr;

int_arr
---------
    {0,1}
(1 row)

```

### Getting information about an array

```sql
--array dimensions (as text)
with arr as (select ARRAY[0,1,2] int_arr) select array_dims(int_arr) from arr;

array_dims
------------
       [1:3]
(1 row)

--length of an array dimension
 WITH arr AS (SELECT ARRAY[0,1,2] int_arr) SELECT array_length(int_arr,1) FROM arr;

 array_length
 --------------
              3
 (1 row)

--total number of elements across all dimensions
 WITH arr AS (SELECT ARRAY[0,1,2] int_arr) SELECT cardinality(int_arr) FROM arr;
 
 cardinality
 -------------
             3
 (1 row)

```

### Array functions

> 
will be added


