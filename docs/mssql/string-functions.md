---
metaTitle: "String Functions"
description: "Quotename, Replace, Substring, String_Split, Left, Right, Soundex, Format, String_escape, ASCII, Char, Concat, LTrim, RTrim, PatIndex, Space, Difference, Len, Lower, Upper, Unicode, NChar, Reverse, Replicate, Str, CharIndex"
---

# String Functions




## Quotename


Returns a Unicode string surrounded by delimiters to make it a valid SQL Server delimited identifier.

Parameters:

1. character string. A string of Unicode data, up to 128 characters (`sysname`). If an input string is longer than 128 characters function returns `null`.
1. quote character. **Optional**. A single character to use as a delimiter. Can be a single quotation mark (`'` or ``), a left or right bracket (`{`,`[`,`(`,`<` or `>`,`)`,`]`,`}`) or a double quotation mark (`"`). Any other value will return null. Default value is square brackets.

```sql
SELECT QUOTENAME('what''s my name?')      -- Returns [what's my name?]

SELECT QUOTENAME('what''s my name?', '[') -- Returns [what's my name?]
SELECT QUOTENAME('what''s my name?', ']') -- Returns [what's my name?]

SELECT QUOTENAME('what''s my name?', '''') -- Returns 'what''s my name?'

SELECT QUOTENAME('what''s my name?', '"') -- Returns "what's my name?"

SELECT QUOTENAME('what''s my name?', ')') -- Returns (what's my name?)
SELECT QUOTENAME('what''s my name?', '(') -- Returns (what's my name?)

SELECT QUOTENAME('what''s my name?', '<') -- Returns <what's my name?>
SELECT QUOTENAME('what''s my name?', '>') -- Returns <what's my name?>

SELECT QUOTENAME('what''s my name?', '{') -- Returns {what's my name?}
SELECT QUOTENAME('what''s my name?', '}') -- Returns {what's my name?}

SELECT QUOTENAME('what''s my name?', '`') -- Returns `what's my name?`

```



## Replace


Returns a string (`varchar` or `nvarchar`) where all occurrences of a specified sub string is replaced with another sub string.

Parameters:

1. string expression. This is the string that would be searched. It can be a character or binary data type.
1. pattern. This is the sub string that would be replaced. It can be a character or binary data type. The pattern argument cannot be an empty string.
1. replacement. This is the sub string that would replace the pattern sub string. It can be a character or binary data.

```sql
SELECT REPLACE('This is my string', 'is', 'XX') -- Returns 'ThXX XX my string'.

```

**Notes:**

- If string expression is not of type `varchar(max)` or `nvarchar(max)`, the `replace` function truncates the return value at 8,000 chars.
- Return data type depends on input data types - returns `nvarchar` if one of the input values is `nvarchar`, or `varchar` otherwise.
- Return `NULL` if any of the input parameters is `NULL`



## Substring


Returns a substring that starts with the char that's in the specified start index and the specified max length.

Parameters:

1. Character expression. The character expression can be of any data type that can be implicitly converted to `varchar` or `nvarchar`, except for `text` or `ntext`.
1. Start index. A number (`int` or `bigint`) that specifies the start index of the requested substring. (**Note:** strings in sql server are base 1 index, meaning that the first character of the string is index 1). This number can be less then 1. In this case, If the sum of start index and max length is greater then 0, the return string would be a string starting from the first char of the character expression and with the length of (start index + max length - 1). If it's less then 0, an empty string would be returned.
1. Max length. An integer number between 0 and `bigint` max value (9,223,372,036,854,775,807). If the max length parameter is negative, an error will be raised.

```sql
SELECT SUBSTRING('This is my string', 6, 5) -- returns 'is my'

```

If the max length + start index is more then the number of characters in the string, the entier string is returned.

```sql
SELECT SUBSTRING('Hello World',1,100) -- returns 'Hello World'

```

If the start index is bigger then the number of characters in the string, an empty string is returned.

```sql
SELECT SUBSTRING('Hello World',15,10) -- returns ''

```



## String_Split


Splits a string expression using a character separator. Note that `STRING_SPLIT()` is a table-valued function and therefore must be used within `FROM` clause.

Parameters:

1. string. Any character type expression (`char`, `nchar`, `varchar` or `nvarchar`)
1. seperator. A single character expression of any type (`char(1)`, `nchar(1)`, `varchar(1)` or `nvarchar(1)`).

Returns a single column table where each row contains a fragment of the string. The name of the columns is `value`, and the datatype is `nvarchar` if any of the parameters is either `nchar` or `nvarchar`, otherwise `varchar`.

The following example splits a string using space as a separator:

```sql
SELECT value FROM STRING_SPLIT('Lorem ipsum dolor sit amet.', ' ');

```

Result:

```sql
value
-----
Lorem
ipsum
dolor
sit
amet.

```

Remarks:

> 
The `STRING_SPLIT` function is available only under compatibility level **130**. If your database compatibility level is lower than 130, SQL Server will not be able to find and execute `STRING_SPLIT` function. You can change the compatibility level of a database using the following command:


```sql
ALTER DATABASE [database_name] SET COMPATIBILITY_LEVEL = 130

```

Older versions of sql server does not have a built in split string function.
There are many user defined functions that handles the problem of splitting a string.
You can read Aaron Bertrand's article [Split strings the right way – or the next best way](http://sqlperformance.com/2012/07/t-sql-queries/split-strings) for a comprehensive comparison of some of them.



## Left


Returns a sub string starting with the left most char of a string and up to the maximum length specified.

Parameters:

1. character expression. The character expression can be of any data type that can be implicitly converted to `varchar` or `nvarchar`, except for `text` or `ntext`
<li>max length. An integer number between 0 and `bigint` max value (9,223,372,036,854,775,807).<br />
If the max length parameter is negative, an error will be raised.</li>

```sql
SELECT LEFT('This is my string', 4) -- result: 'This'

```

If the max length is more then the number of characters in the string, the entier string is returned.

```sql
SELECT LEFT('This is my string', 50) -- result: 'This is my string'

```



## Right


Returns a sub string that is the right most part of the string, with the specified max length.

Parameters:

1. character expression. The character expression can be of any data type that can be implicitly converted to `varchar` or `nvarchar`, except for `text` or `ntext`
<li>max length. An integer number between 0 and `bigint` max value (9,223,372,036,854,775,807).
If the max length parameter is negative, an error will be raised.</li>

```sql
SELECT RIGHT('This is my string', 6) -- returns 'string'

```

If the max length is more then the number of characters in the string, the entier string is returned.

```sql
SELECT RIGHT('This is my string', 50) -- returns 'This is my string'

```



## Soundex


Returns a four-character code (`varchar`) to evaluate the phonetic similarity of two strings.

Parameters:

1. character expression. An alphanumeric expression of character data.

The soundex function creates a four-character code that is based on how the character expression would sound when spoken. the first char is the the upper case version of the first character of the parameter, the rest 3 characters are numbers representing the letters in the expression (except a, e, i, o, u, h, w and y that are ignored).

```sql
SELECT SOUNDEX ('Smith') -- Returns 'S530'

SELECT SOUNDEX ('Smythe') -- Returns 'S530'

```



## Format


Returns a `NVARCHAR` value formatted with the specified format and culture (if specified). This is primarily used for converting date-time types to strings.

Parameters:

1. `value`. An expression of a supported data type to format. valid types are listed below.
1. `format`. An `NVARCHAR` format pattern. See Microsoft official documentation for [standard](https://msdn.microsoft.com/library/dwhawy9k.aspx) and [custom](https://msdn.microsoft.com/library/0c899ak8.aspx) format strings.
1. `culture`. **Optional**. `nvarchar` argument specifying a culture. The default value is the culture of the current session.

**DATE**

Using standard format strings:

```sql
DECLARE @d DATETIME = '2016-07-31';  

SELECT 
    FORMAT ( @d, 'd', 'en-US' ) AS 'US English Result' -- Returns '7/31/2016'
   ,FORMAT ( @d, 'd', 'en-gb' ) AS 'Great Britain English Result' -- Returns '31/07/2016'
   ,FORMAT ( @d, 'd', 'de-de' ) AS 'German Result' -- Returns '31.07.2016'
   ,FORMAT ( @d, 'd', 'zh-cn' ) AS 'Simplified Chinese (PRC) Result' -- Returns '2016/7/31'
   ,FORMAT ( @d, 'D', 'en-US' ) AS 'US English Result' -- Returns 'Sunday, July 31, 2016'
   ,FORMAT ( @d, 'D', 'en-gb' ) AS 'Great Britain English Result' -- Returns '31 July 2016'
   ,FORMAT ( @d, 'D', 'de-de' ) AS 'German Result' -- Returns 'Sonntag, 31. Juli 2016'

```

Using custom format strings:

```sql
SELECT FORMAT( @d, 'dd/MM/yyyy', 'en-US' ) AS 'DateTime Result' -- Returns '31/07/2016'
      ,FORMAT(123456789,'###-##-####') AS 'Custom Number Result' -- Returns '123-45-6789',
      ,FORMAT( @d,'dddd, MMMM dd, yyyy hh:mm:ss tt','en-US') AS 'US' -- Returns 'Sunday, July 31, 2016 12:00:00 AM'
      ,FORMAT( @d,'dddd, MMMM dd, yyyy hh:mm:ss tt','hi-IN') AS 'Hindi' -- Returns रविवार, जुलाई 31, 2016 12:00:00 पूर्वाह्न
      ,FORMAT ( @d, 'dddd', 'en-US' )  AS 'US' -- Returns 'Sunday'
      ,FORMAT ( @d, 'dddd', 'hi-IN' )  AS 'Hindi' -- Returns 'रविवार'

```

`FORMAT` can also be used for formatting `CURRENCY`,`PERCENTAGE` and `NUMBERS`.

**CURRENCY**

```sql
DECLARE @Price1 INT = 40
SELECT FORMAT(@Price1,'c','en-US') AS 'CURRENCY IN US Culture' -- Returns '$40.00'      
       ,FORMAT(@Price1,'c','de-DE') AS 'CURRENCY IN GERMAN Culture' -- Returns '40,00 €'

```

We can specify the number of digits after the decimal.

```sql
DECLARE @Price DECIMAL(5,3) = 40.356
SELECT FORMAT( @Price, 'C') AS 'Default', -- Returns '$40.36'
       FORMAT( @Price, 'C0') AS 'With 0 Decimal', -- Returns '$40'
       FORMAT( @Price, 'C1') AS 'With 1 Decimal', -- Returns '$40.4'
       FORMAT( @Price, 'C2') AS 'With 2 Decimal', -- Returns '$40.36'

```

**PERCENTAGE**

```

  DECLARE @Percentage float = 0.35674
   SELECT FORMAT( @Percentage, 'P') AS '% Default', -- Returns '35.67 %'
   FORMAT( @Percentage, 'P0') AS '% With 0 Decimal', -- Returns '36 %'
   FORMAT( @Percentage, 'P1') AS '% with 1 Decimal'  -- Returns '35.7 %'

```

**NUMBER**

```sql
DECLARE @Number AS DECIMAL(10,2) = 454545.389
SELECT FORMAT( @Number, 'N','en-US') AS 'Number Format in US', -- Returns '454,545.39'
FORMAT( @Number, 'N','en-IN')  AS 'Number Format in INDIA', -- Returns '4,54,545.39'
FORMAT( @Number, '#.0')     AS 'With 1 Decimal', -- Returns '454545.4'
FORMAT( @Number, '#.00')    AS 'With 2 Decimal', -- Returns '454545.39'
FORMAT( @Number, '#,##.00') AS 'With Comma and 2 Decimal', -- Returns '454,545.39'
FORMAT( @Number, '##.00')   AS 'Without Comma and 2 Decimal', -- Returns '454545.39'
FORMAT( @Number, '000000000') AS 'Left-padded to nine digits' -- Returns '000454545'

```

Valid value types list: ([source](https://msdn.microsoft.com/en-us/library/hh213505.aspx#Anchor_3))

```sql
Category         Type             .Net type
-------------------------------------------
Numeric          bigint           Int64
Numeric          int              Int32
Numeric          smallint         Int16
Numeric          tinyint          Byte
Numeric          decimal          SqlDecimal
Numeric          numeric          SqlDecimal
Numeric          float            Double
Numeric          real             Single
Numeric          smallmoney       Decimal
Numeric          money            Decimal
Date and Time    date             DateTime
Date and Time    time             TimeSpan
Date and Time    datetime         DateTime
Date and Time    smalldatetime    DateTime
Date and Time    datetime2        DateTime
Date and Time    datetimeoffset   DateTimeOffset

```

**Important Notes:**

- `FORMAT` returns `NULL` for errors other than a culture that is not valid. For example, `NULL` is returned if the value specified in format is not valid.
- `FORMAT` relies on the presence of the .NET Framework Common Language Runtime (CLR).
- `FORMAT` relies upon CLR formatting rules which dictate that colons and periods must be escaped. Therefore, when the format string (second parameter) contains a colon or period, the colon or period must be escaped with backslash when an input value (first parameter) is of the time data type.

See also [Date & Time Formatting using FORMAT](http://stackoverflow.com/documentation/sql-server/1471/dates/8084/date-time-formatting-using-format) documentation example.



## String_escape


Escapes special characters in texts and returns text (`nvarchar(max)`) with escaped characters.

Parameters:

<li>
text. is a `nvarchar` expression representing the string that should be escaped.
</li>
<li>
type. Escaping rules that will be applied. Currently the only supported value is `'json'`.
</li>

```sql
SELECT STRING_ESCAPE('\   /  
\\    "     ', 'json') -- returns '\\\t\/\n\\\\\t\"\t'

```

List of characters that will be escaped:

```sql
Special character    Encoded sequence
-------------------------------------
Quotation mark (")   \"
Reverse solidus (\)  \\
Solidus (/)          \/
Backspace            \b
Form feed            \f
New line             \n
Carriage return      \r
Horizontal tab       \t


Control character    Encoded sequence
------------------------------------
CHAR(0)            \u0000
CHAR(1)            \u0001
...                ...
CHAR(31)           \u001f

```



## ASCII


Returns an int value representing the ASCII code of the leftmost character of a string.

```sql
SELECT ASCII('t') -- Returns 116
SELECT ASCII('T') -- Returns 84
SELECT ASCII('This') -- Returns 84

```

If the string is Unicode and the leftmost character is not ASCII but representable in the current collation, a value greater than 127 can be returned:

```sql
SELECT ASCII(N'ï') -- returns 239 when `SERVERPROPERTY('COLLATION') = 'SQL_Latin1_General_CP1_CI_AS'`

```

If the string is Unicode and the leftmost character cannot be represented in the current collation, the int value of 63 is returned: (which represents question mark in ASCII):

```sql
SELECT ASCII(N'߷') -- returns 63
SELECT ASCII(nchar(2039)) -- returns 63

```



## Char


Returns a char represented by an int ASCII code.

```sql
SELECT CHAR(116) -- Returns 't'
SELECT CHAR(84)  -- Returns 'T'

```

This can be used to introduce new line/line feed `CHAR(10)`, carriage returns `CHAR(13)`, etc. See [AsciiTable.com](http://www.asciitable.com/) for reference.

If the argument value is not between 0 and 255, the CHAR function returns `NULL`.<br />
The return data type of the `CHAR` function is `char(1)`



## Concat


Returns a string that is the result of two or more strings joined together.   `CONCAT` accepts two or more arguments.

```sql
SELECT CONCAT('This', ' is', ' my', ' string') -- returns 'This is my string'

```

Note: Unlike concatenating strings using the string concatenation operator (`+`), when passing a null value to the `concat` function it will implicitly convert it to an empty string:

```sql
SELECT CONCAT('This', NULL, ' is', ' my', ' string'), -- returns 'This is my string'
       'This' + NULL + ' is' + ' my' + ' string' -- returns NULL.

```

Also arguments of a non-string type will be implicitly converted to a string:

```sql
SELECT CONCAT('This', ' is my ', 3, 'rd string') -- returns 'This is my 3rd string'

```

Non-string type variables will also be converted to string format, no need to manually covert or cast it to string:

```sql
DECLARE @Age INT=23;
SELECT CONCAT('Ram is ', @Age,' years old');  -- returns 'Ram is 23 years old'

```

Older versions do not support `CONCAT` function and must use the string concatenation operator (`+`) instead. Non-string types must be cast or converted to string types in order to concatenate them this way.

```sql
SELECT 'This is the number ' + CAST(42 AS VARCHAR(5)) --returns 'This is the number 42'

```



## LTrim


Returns a character expression (`varchar` or `nvarchar`) after removing all leading white spaces, i.e., white spaces from the left through to the first non-white space character.

Parameters:

1. character expression. Any expression of character or binary data that can be implicitly converted to `varcher`, except `text`, `ntext` and `image`.

```sql
SELECT LTRIM('    This is my string') -- Returns 'This is my string'

```



## RTrim


Returns a character expression (`varchar` or `nvarchar`) after removing all trailing white spaces, i.e., spaces from the right end of the string up until the first non-white space character to the left.

Parameters:

1. character expression. Any expression of character or binary data that can be implicitly converted to `varcher`, except `text`, `ntext` and `image`.

```sql
SELECT RTRIM('This is my string     ') -- Returns 'This is my string'

```



## PatIndex


Returns the starting position of the first occurrence of a the specified pattern in the specified expression.

Parameters:

<li>
pattern. A character expression the contains the sequence to be found. Limited to A maximum length of 8000 chars. Wildcards (`%`, `_`) can be used in the pattern. If the pattern does not start with a wildcard, it may only match whatever is in the beginning of the expression. If it doesn't end with a wildcard, it may only match whatever is in the end of the expression.
</li>
<li>
expression. Any string data type.
</li>

```sql
SELECT PATINDEX('%ter%', 'interesting') -- Returns 3. 

SELECT PATINDEX('%t_r%t%', 'interesting') -- Returns 3. 

SELECT PATINDEX('ter%', 'interesting') -- Returns 0, since 'ter' is not at the start. 

SELECT PATINDEX('inter%', 'interesting') -- Returns 1. 

SELECT PATINDEX('%ing', 'interesting') -- Returns 9. 

```



## Space


Returns a string (`varchar`) of repeated spaces.

Parameters:

1. integer expression. Any integer expression, up to 8000. If negative, `null` is returned. if 0, an empty string is returned. (To return a string longer then 8000 spaces, use Replicate.

```sql
SELECT SPACE(-1) -- Returns NULL
SELECT SPACE(0)  -- Returns an empty string
SELECT SPACE(3)  -- Returns '   ' (a string containing 3 spaces)

```



## Difference


Returns an integer (`int`) value that indicates the difference between the soundex values of two character expressions.

Parameters:

1. character expression 1.
1. character expression 2.

Both parameters are alphanumeric expressions of character data.

The integer returned is the number of chars in the soundex values of the parameters that are the same, so 4 means that the expressions are very similar and 0 means that they are very different.

```sql
SELECT  SOUNDEX('Green'),  -- G650
        SOUNDEX('Greene'),  -- G650
        DIFFERENCE('Green','Greene') -- Returns 4
        
SELECT  SOUNDEX('Blotchet-Halls'),  -- B432
        SOUNDEX('Greene'),  -- G650
        DIFFERENCE('Blotchet-Halls', 'Greene') -- Returns 0

```



## Len


Returns the number of characters of a string.<br />
Note: the `LEN` function ignores trailing spaces:

```sql
SELECT LEN('My string'), -- returns 9
       LEN('My string   '), -- returns 9
       LEN('   My string') -- returns 12

```

If the length including trailing spaces is desired there are several techniques to achieve this, although each has its drawbacks. One technique is to append a single character to the string, and then use the `LEN` minus one:

```sql
DECLARE @str varchar(100) = 'My string   '
SELECT LEN(@str + 'x') - 1 -- returns 12

```

The drawback to this is if the type of the string variable or column is of the maximum length, the append of the extra character is discarded, and the resulting length will still not count trailing spaces. To address that, the following modified version solves the problem, and gives the correct results in all cases at the expense of a small amount of additional execution time, and because of this (correct results, including with surrogate pairs, and reasonable execution speed) appears to be the best technique to use:

```sql
SELECT LEN(CONVERT(NVARCHAR(MAX), @str) + 'x') - 1

```

Another technique is to use the`DATALENGTH` function.

```sql
DECLARE @str varchar(100) = 'My string   '
SELECT DATALENGTH(@str) -- returns 12

```

It's important to note though that `DATALENGTH` returns the length in bytes of the string in memory. This will be different for `varchar` vs. `nvarchar`.

```sql
DECLARE @str nvarchar(100) = 'My string   '
SELECT DATALENGTH(@str) -- returns 24

```

You can adjust for this by dividing the datalength of the string by the datalength of a single character (which must be of the same type). The example below does this, and also handles the case where the target string happens to be empty, thus avoiding a divide by zero.

```sql
DECLARE @str nvarchar(100) = 'My string   '
SELECT DATALENGTH(@str) / DATALENGTH(LEFT(LEFT(@str, 1) + 'x', 1)) -- returns 12

```

Even this, though, has a problem in SQL Server 2012 and above. It will produce incorrect results when the string contains surrogate pairs (some characters can occupy more bytes than other characters in the same string).

Another technique is to use `REPLACE` to convert spaces to a non-space character, and take the `LEN` of the result. This gives correct results in all cases, but has very poor execution speed with long strings.



## Lower


Returns a character expression (`varchar` or `nvarchar`) after converting all uppercase characters to lowercase.

Parameters:

1. Character expression. Any expression of character or binary data that can be implicitly converted to `varchar`.

```sql
SELECT LOWER('This IS my STRING') -- Returns 'this is my string'

DECLARE @String nchar(17) = N'This IS my STRING';
SELECT LOWER(@String) -- Returns 'this is my string'

```



## Upper


Returns a character expression (`varchar` or `nvarchar`) after converting all lowercase characters to uppercase.

Parameters:

1. Character expression. Any expression of character or binary data that can be implicitly converted to `varchar`.

```sql
SELECT UPPER('This IS my STRING') -- Returns 'THIS IS MY STRING'

DECLARE @String nchar(17) = N'This IS my STRING';
SELECT UPPER(@String) -- Returns 'THIS IS MY STRING'

```



## Unicode


Returns the integer value representing the Unicode value of the first character of the input expression.

Parameters:

1. Unicode character expression. Any valid `nchar` or `nvarchar` expression.

```sql
SELECT UNICODE(N'Ɛ') -- Returns 400

DECLARE @Unicode nvarchar(11) = N'Ɛ is a char'
SELECT UNICODE(@Unicode) -- Returns 400

```



## NChar


Returns the Unicode character(s) (`nchar(1)` or `nvarchar(2)`) corresponding to the integer argument it receives, as defined by the Unicode standard.

Parameters:

1. integer expression. Any integer expression that is a positive number between 0 and 65535, or if the collation of the database supports supplementary character (CS) flag, the supported range is between 0 to 1114111. If the integer expression does not fall inside this range, `null` is returned.

```sql
SELECT NCHAR(257) -- Returns 'ā'
SELECT NCHAR(400) -- Returns 'Ɛ'

```



## Reverse


Returns a string value in reversed order.

Parameters:

1. string expression. Any string or binary data that can be implicitly converted to `varchar`.

```sql
Select REVERSE('Sql Server') -- Returns 'revreS lqS'

```



## Replicate


Repeats a string value a specified number of times.

Parameters:

1. string expression. String expression can be a character string or binary data.
1. integer expression. Any integer type, including `bigint`. If negative, `null` is returned. If 0, an empty string is returned.

```sql
SELECT REPLICATE('a', -1)  -- Returns NULL

SELECT REPLICATE('a', 0)  -- Returns ''

SELECT REPLICATE('a', 5)  -- Returns 'aaaaa'

SELECT REPLICATE('Abc', 3) -- Returns 'AbcAbcAbc'

```

**Note:** If string expression is not of type `varchar(max)` or `nvarchar(max)`, the return value will not exceed 8000 chars. Replicate will stop before adding the string that will cause the return value to exceed that limit:

```sql
SELECT LEN(REPLICATE('a b c d e f g h i j k l', 350)) -- Returns 7981

SELECT LEN(REPLICATE(cast('a b c d e f g h i j k l' as varchar(max)), 350)) -- Returns 8050

```



## Str


Returns character data (`varchar`) converted from numeric data.

Parameters:

1. float expression. An approximate numeric data type with a decimal point.
1. length. **optional.** The total length of the string expression that would return, including digits, decimal point and leading spaces (if needed). The default value is 10.
1. decimal. **optional.** The number of digits to the right of the decimal point. If higher then 16, the result would be truncated to sixteen places to the right of the decimal point.

```sql
SELECT STR(1.2) -- Returns '         1'

SELECT STR(1.2, 3) -- Returns '  1'

SELECT STR(1.2, 3, 2) -- Returns '1.2'

SELECT STR(1.2, 5, 2) -- Returns ' 1.20'

SELECT STR(1.2, 5, 5) -- Returns '1.200'

SELECT STR(1, 5, 2) -- Returns ' 1.00'

SELECT STR(1) -- Returns '         1'

```



## CharIndex


Returns the start index of a the first occurrence of string expression inside another string expression.

Parameters list:

1. String to find (up to 8000 chars)
1. String to search (any valid character data type and length, including binary)
1. (Optional) index to start. A number of type int or big int. If omitted or less then 1, the search starts at the beginning of the string.

If the string to search is `varchar(max)`, `nvarchar(max)` or `varbinary(max)`, the `CHARINDEX` function will return a `bigint` value. Otherwise, it will return an `int`.

```sql
SELECT CHARINDEX('is', 'this is my string') -- returns 3
SELECT CHARINDEX('is', 'this is my string', 4) -- returns 6
SELECT CHARINDEX(' is', 'this is my string') -- returns 5

```



#### Remarks


List of string functions (Alphabetically sorted):

<li>
[Ascii](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14342/ascii)
</li>
<li>
[Char](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14344/char)
</li>
<li>
[Charindex](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14343/charindex)
</li>
<li>
[Concat](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14346/concat)
</li>
<li>
[Difference](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/17326/difference)
</li>
<li>
[Format](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/17327/format)
</li>
<li>
[Left](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14339/left)
</li>
<li>
[Len](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14345/len)
</li>
<li>
[Lower](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14347/lower)
</li>
<li>
[Ltrim](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14349/ltrim)
</li>
<li>
[Nchar](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14352/nchar)
</li>
<li>
[Patindex](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14638/patindex)
</li>
<li>
[Quotename](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/15914/quotename)
</li>
<li>
[Replace](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14964/replace)
</li>
<li>
[Replicate](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14643/replicate)
</li>
<li>
[Reverse](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14637/reverse)
</li>
<li>
[Right](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14340/right)
</li>
<li>
[Rtrim](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14350/rtrim)
</li>
<li>
[Soundex](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/17325/soundex)
</li>
<li>
[Space](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14642/space)
</li>
<li>
[Str](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/15913/str)
</li>
<li>
[String_escape](http://stackoverflow.com/documentation/sql-server/4113/string-functions/17987/string-escape)
</li>
<li>
[String_split](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/15779/string-split)
</li>
<li>
[Stuff](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14965/stuff)
</li>
<li>
[Substring](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14341/substring)
</li>
<li>
[Unicode](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14351/unicode)
</li>
<li>
[Upper](http://stackoverflow.com/documentation/sql-server/4113/varchar-functions/14348/upper)
</li>

