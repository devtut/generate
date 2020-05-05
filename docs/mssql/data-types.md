---
metaTitle: "Microsoft SQL Server - Data Types"
description: "Exact Numerics, Approximate Numerics, Date and Time, Character Strings, Unicode Character Strings, Binary Strings, Other Data Types"
---

# Data Types


This section discusses the data types that SQL Server can use, including their data range, length, and limitations (if any.)



## Exact Numerics


There are two basic classes of exact numeric data types - **Integer**, and **Fixed Precision and Scale**.

**Integer Data Types**

- bit
- tinyint
- smallint
- int
- bigint

**Integers** are numeric values that never contain a fractional portion, and always use a fixed amount of storage. The range and storage sizes of the integer data types are shown in this table:

|Data type|Range|Storage
|---|---|---|---
|bit|0 or 1|1 bit **
|tinyint|0 to 255|1 byte
|smallint|-2^15 (-32,768) to 2^15-1 (32,767)|2 bytes
|int|-2^31 (-2,147,483,648) to 2^31-1 (2,147,483,647)|4 bytes
|bigint|-2^63 (-9,223,372,036,854,775,808) to 2^63-1 (9,223,372,036,854,775,807)|8 bytes

**Fixed Precision and Scale Data Types**

- numeric
- decimal
- smallmoney
- money

These data types are useful for representing numbers exactly. As long as the values can fit within the range of the values storable in the data type, the value will not have rounding issues. This is useful for any financial calculations, where rounding errors will result in clinical insanity for accountants.

Note that **decimal** and **numeric** are synonyms for the same data type.

|Data type|Range|Storage
|---|---|---|---
|Decimal [(p [, s])] or Numeric [(p [, s])]|-10^38 + 1 to 10^38 - 1|See **Precision** table

When defining a **decimal** or **numeric** data type, you may need to specify the Precision [p] and Scale [s].

Precision is the number of digits that can be stored. For example, if you needed to store values between 1 and 999, you would need a Precision of 3 (to hold the three digits in 100). If you do not specify a precision, the default precision is 18.

Scale is the number of digits after the decimal point. If you needed to store a number between 0.00 and 999.99, you would need to specify a Precision of 5 (five digits) and a Scale of 2 (two digits after the decimal point). You must specify a precision to specify a scale. The default scale is zero.

The Precision of a **decimal** or **numeric** data type defines the number of bytes required to store the value, as shown below:

**Precision Table**

|Precision|Storage bytes
|---|---|---|---
|1 - 9|5
|10-19|9
|20-28|13
|29-38|17

**Monetary Fixed Data Types**

These data types are intended specifically for accounting and other monetary data. These type have a fixed Scale of 4 - you will always see four digits after the decimal place. For most systems working with most currencies, using a **numeric** value with a Scale of 2 will be sufficient. Note that no information about the type of currency represented is stored with the value.

|Data type|Range|Storage
|---|---|---|---
|money|-922,337,203,685,477.5808 to 922,337,203,685,477.5807|8 bytes
|smallmoney|-214,748.3648 to 214,748.3647|4 bytes



## Approximate Numerics


- float [(**n**)]
- real

These data types are used to store floating point numbers. Since these types are intended to hold approximate numeric values only, these should not be used in cases where any rounding error is unacceptable. However, if you need to handle very large numbers, or numbers with an indeterminate number of digits after the decimal place, these may be your best option.

|Data type|Range|Size
|---|---|---|---
|float|-1.79E+308 to -2.23E-308, 0 and 2.23E-308 to 1.79E+308|depends on **n** in table below
|real|-3.40E + 38 to -1.18E - 38, 0 and 1.18E - 38 to 3.40E + 38|4 Bytes

**n** value table for **float** numbers. If no value is specified in the declaration of the float, the default value of 53 will be used. Note that **float(24)** is the equivalent of a **real** value.

|n value|Precision|Size
|---|---|---|---
|1-24|7 digits|4 bytes
|25-53|15 digits|8 bytes



## Date and Time


These types are in all versions of SQL Server

- datetime
- smalldatetime

These types are in all versions of SQL Server after SQL Server 2012

- date
- datetimeoffset
- datetime2
- time



## Character Strings


- char
- varchar
- text



## Unicode Character Strings


- nchar
- nvarchar
- ntext



## Binary Strings


- binary
- varbinary
- image



## Other Data Types


- cursor
- timestamp
- hierarchyid
- uniqueidentifier
- sql_variant
- xml
- table
- Spatial Types

