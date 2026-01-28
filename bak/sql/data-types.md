---
metaTitle: "SQL - Data Types"
description: "DECIMAL and NUMERIC, FLOAT and REAL, Integers, MONEY and SMALLMONEY, BINARY and VARBINARY, CHAR and VARCHAR, NCHAR and NVARCHAR, UNIQUEIDENTIFIER"
---

# Data Types



## DECIMAL and NUMERIC


Fixed precision and scale decimal numbers. `DECIMAL` and `NUMERIC` are functionally equivalent.

Syntax:

```sql
DECIMAL ( precision [ , scale] )
NUMERIC ( precision [ , scale] )

```

Examples:

```sql
SELECT CAST(123 AS DECIMAL(5,2)) --returns 123.00
SELECT CAST(12345.12 AS NUMERIC(10,5)) --returns 12345.12000

```



## FLOAT and REAL


Approximate-number data types for use with floating point numeric data.

```sql
SELECT CAST( PI() AS FLOAT) --returns 3.14159265358979
SELECT CAST( PI() AS REAL) --returns 3.141593

```



## Integers


Exact-number data types that use integer data.

|Data type|Range|Storage
|---|---|---|---
|bigint|-2^63 (-9,223,372,036,854,775,808) to 2^63-1 (9,223,372,036,854,775,807)|8 Bytes
|int|-2^31 (-2,147,483,648) to 2^31-1 (2,147,483,647)|4 Bytes
|smallint|-2^15 (-32,768) to 2^15-1 (32,767)|2 Bytes
|tinyint|0 to 255|1 Byte



## MONEY and SMALLMONEY


Data types that represent monetary or currency values.

|Data type|Range|Storage
|---|---|---|---
|money|-922,337,203,685,477.5808 to 922,337,203,685,477.5807|8 bytes
|smallmoney|-214,748.3648 to 214,748.3647|4 bytes



## BINARY and VARBINARY


Binary data types of either fixed length or variable length.

Syntax:

```sql
BINARY [ ( n_bytes ) ]
VARBINARY [ ( n_bytes | max ) ]

```

`n_bytes` can be any number from 1 to 8000 bytes. `max` indicates that the maximum storage space is 2^31-1.

Examples:

```sql
SELECT CAST(12345 AS BINARY(10)) -- 0x00000000000000003039
SELECT CAST(12345 AS VARBINARY(10)) -- 0x00003039

```



## CHAR and VARCHAR


String data types of either fixed length or variable length.

Syntax:

```sql
CHAR [ ( n_chars ) ]
VARCHAR [ ( n_chars ) ]

```

Examples:

```sql
SELECT CAST('ABC' AS CHAR(10)) -- 'ABC       ' (padded with spaces on the right)
SELECT CAST('ABC' AS VARCHAR(10)) -- 'ABC' (no padding due to variable character)
SELECT CAST('ABCDEFGHIJKLMNOPQRSTUVWXYZ' AS CHAR(10))  -- 'ABCDEFGHIJ' (truncated to 10 characters)

```



## NCHAR and NVARCHAR


UNICODE string data types of either fixed length or variable length.

Syntax:

```sql
NCHAR [ ( n_chars ) ]
NVARCHAR [ ( n_chars | MAX ) ]

```

Use `MAX` for very long strings that may exceed 8000 characters.



## UNIQUEIDENTIFIER


A 16-byte GUID / UUID.

```sql
DECLARE @GUID UNIQUEIDENTIFIER = NEWID(); 
SELECT @GUID -- 'E28B3BD9-9174-41A9-8508-899A78A33540'
DECLARE @bad_GUID_string VARCHAR(100) = 'E28B3BD9-9174-41A9-8508-899A78A33540_foobarbaz'
SELECT 
    @bad_GUID_string,   -- 'E28B3BD9-9174-41A9-8508-899A78A33540_foobarbaz'
    CONVERT(UNIQUEIDENTIFIER, @bad_GUID_string) -- 'E28B3BD9-9174-41A9-8508-899A78A33540'

```

