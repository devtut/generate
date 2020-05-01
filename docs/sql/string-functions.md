---
metaTitle: "String Functions"
description: "Concatenate, Length, Trim empty spaces, Upper & lower case, Split, Replace, Substring, Stuff, REGEXP, LEFT - RIGHT, REVERSE, REPLICATE, Replace function in sql Select and Update query, PARSENAME , INSTR"
---

# String Functions


String functions perform operations on string values and return either numeric or string values.

Using string functions, you can, for example, combine data, extract a substring, compare strings, or convert a string to all uppercase or lowercase characters.



## Concatenate


In (standard ANSI/ISO) SQL, the operator for string concatenation is `||`.  This syntax is supported by all major databases except SQL Server:

```sql
SELECT 'Hello' || 'World' || '!'; --returns HelloWorld!

```

Many databases support a `CONCAT` function to join strings:

```sql
SELECT CONCAT('Hello', 'World'); --returns 'HelloWorld'

```

Some databases support using `CONCAT` to join more than two strings (Oracle does not):

```sql
SELECT CONCAT('Hello', 'World', '!'); --returns 'HelloWorld!'

```

In some databases, non-string types must be cast or converted:

```sql
SELECT CONCAT('Foo', CAST(42 AS VARCHAR(5)), 'Bar'); --returns 'Foo42Bar'

```

Some databases (e.g., Oracle) perform implicit lossless conversions.  For example, a `CONCAT` on a `CLOB` and `NCLOB` yields a `NCLOB`.  A `CONCAT` on a number and a `varchar2` results in a `varchar2`, etc.:

```sql
SELECT CONCAT(CONCAT('Foo', 42), 'Bar') FROM dual; --returns Foo42Bar

```

Some databases can use the non-standard `+` operator (but in most, `+` works only for numbers):

```sql
SELECT 'Foo' + CAST(42 AS VARCHAR(5)) + 'Bar';

```

On SQL Server < 2012, where `CONCAT` is not supported, `+` is the only way to join strings.



## Length


****SQL Server****

The LEN doesn't count the trailing space.

```sql
SELECT LEN('Hello') -- returns 5

SELECT LEN('Hello '); -- returns 5

```

The DATALENGTH counts the trailing space.

```sql
SELECT DATALENGTH('Hello') -- returns 5

SELECT DATALENGTH('Hello '); -- returns 6

```

It should be noted though, that DATALENGTH returns the length of the underlying byte representation of the string, which depends, i.a., on the charset used to store the string.

```sql
DECLARE @str varchar(100) = 'Hello ' --varchar is usually an ASCII string, occupying 1 byte per char
SELECT DATALENGTH(@str) -- returns 6

DECLARE @nstr nvarchar(100) = 'Hello ' --nvarchar is a unicode string, occupying 2 bytes per char
SELECT DATALENGTH(@nstr) -- returns 12

```

****Oracle****

Syntax: Length ( char )

Examples:

```sql
SELECT Length('Bible') FROM dual; --Returns 5
SELECT Length('righteousness') FROM dual; --Returns 13
SELECT Length(NULL) FROM dual; --Returns NULL

```

See Also: LengthB, LengthC, Length2, Length4



## Trim empty spaces


Trim is used to remove write-space at the beginning or end of selection

In MSSQL there is no single `TRIM()`

```sql
SELECT LTRIM('  Hello  ') --returns 'Hello  '
SELECT RTRIM('  Hello  ') --returns '  Hello'
SELECT LTRIM(RTRIM('  Hello  ')) --returns 'Hello'

```

MySql and Oracle

```sql
SELECT TRIM('  Hello  ') --returns 'Hello'

```



## Upper & lower case


```sql
SELECT UPPER('HelloWorld') --returns 'HELLOWORLD'
SELECT LOWER('HelloWorld') --returns 'helloworld'

```



## Split


Splits a string expression using a character separator. Note that `STRING_SPLIT()` is a table-valued function.

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



## Replace


Syntax:

`REPLACE(` String to search `,` String to search for and replace `,` String to place into the original string `)`

Example:

```sql
SELECT REPLACE( 'Peter Steve Tom', 'Steve', 'Billy' ) --Return Values: Peter Billy Tom

```



## Substring


Syntax is: `SUBSTRING ( string_expression, start, length )`. Note that SQL strings are 1-indexed.

```sql
SELECT SUBSTRING('Hello', 1, 2) --returns 'He'
SELECT SUBSTRING('Hello', 3, 3) --returns 'llo'

```

This is often used in conjunction with the `LEN()` function to get the last `n` characters of a string of unknown length.

```sql
DECLARE @str1 VARCHAR(10) = 'Hello', @str2 VARCHAR(10) = 'FooBarBaz';
SELECT SUBSTRING(@str1, LEN(@str1) - 2, 3) --returns 'llo'
SELECT SUBSTRING(@str2, LEN(@str2) - 2, 3) --returns 'Baz'

```



## Stuff


Stuff a string into another, replacing 0 or more characters at a certain position.

Note: `start` position is 1-indexed (you start indexing at 1, not 0).

Syntax:

```sql
STUFF ( character_expression , start , length , replaceWith_expression )  

```

Example:

```sql
SELECT STUFF('FooBarBaz', 4, 3, 'Hello') --returns 'FooHelloBaz'

```



## REGEXP


Checks if a string matches a regular expression (defined by another string).

```sql
SELECT 'bedded' REGEXP '[a-f]' -- returns True

SELECT 'beam' REGEXP '[a-f]' -- returns False

```



## LEFT - RIGHT


Syntax is:   <BR>LEFT ( string-expression , integer )<br />
RIGHT ( string-expression , integer )

```sql
SELECT LEFT('Hello',2)  --return He  
SELECT RIGHT('Hello',2) --return lo

```

Oracle SQL doesn't have LEFT and RIGHT functions.  They can be emulated with SUBSTR and LENGTH.<BR>SUBSTR ( string-expression, 1, integer )
<BR>SUBSTR ( string-expression, length(string-expression)-integer+1, integer)

```sql
SELECT SUBSTR('Hello',1,2)  --return He  
SELECT SUBSTR('Hello',LENGTH('Hello')-2+1,2) --return lo

```



## REVERSE


Syntax is: REVERSE ( string-expression )

```sql
SELECT REVERSE('Hello') --returns olleH

```



## REPLICATE


The `REPLICATE` function concatenates a string with itself a specified number of times.

Syntax is: REPLICATE ( string-expression , integer )

```sql
SELECT REPLICATE ('Hello',4) --returns 'HelloHelloHelloHello'

```



## Replace function in sql Select and Update query


The Replace function in SQL is used to update the content of a string. The function call is REPLACE( ) for MySQL, Oracle, and SQL Server. <br/><br/>The syntax of the Replace function is:

```sql
REPLACE (str, find, repl)

```

The following example replaces occurrences of `South` with `Southern` in Employees table:

|FirstName|Address
|---|---|---|---
|James|South New York
|John|South Boston
|Michael|South San Diego

**Select Statement :**

If we apply the following Replace function:

```sql
SELECT 
    FirstName, 
    REPLACE (Address, 'South', 'Southern') Address
FROM Employees 
ORDER BY FirstName 

```

Result:

|FirstName|Address
|---|---|---|---
|James|Southern New York
|John|Southern Boston
|Michael|Southern San Diego

**Update Statement :**

We can use a replace function to make permanent changes in our table through following approach.

```sql
Update Employees 
Set city = (Address, 'South', 'Southern');

```

A more common approach is to use this in conjunction with a WHERE clause like this:

```sql
Update Employees 
Set Address = (Address, 'South', 'Southern')
Where Address LIKE 'South%';

```



## PARSENAME 


**DATABASE** : SQL Server

**<strong>PARSENAME**</strong> function returns the specific part of given string(object name). object name may contains string like object name,owner name, database name and server name.

More details <kbd>[MSDN:PARSENAME](https://msdn.microsoft.com/en-us/library/ms188006.aspx)</kbd>

**Syntax**

```sql
PARSENAME('NameOfStringToParse',PartIndex)

```

**Example**

To get object name use part index  `1`

```sql
SELECT PARSENAME('ServerName.DatabaseName.SchemaName.ObjectName',1)  // returns `ObjectName`
SELECT PARSENAME('[1012-1111].SchoolDatabase.school.Student',1)     // returns `Student`

```

To get schema name use part index  `2`

```sql
SELECT PARSENAME('ServerName.DatabaseName.SchemaName.ObjectName',2)  // returns `SchemaName`
SELECT PARSENAME('[1012-1111].SchoolDatabase.school.Student',2)     // returns `school`

```

To get database name use part index  `3`

```sql
SELECT PARSENAME('ServerName.DatabaseName.SchemaName.ObjectName',3) // returns `DatabaseName`
SELECT PARSENAME('[1012-1111].SchoolDatabase.school.Student',3)    // returns `SchoolDatabase` 

```

To get server name use part index  `4`

```sql
SELECT PARSENAME('ServerName.DatabaseName.SchemaName.ObjectName',4)  // returns `ServerName`
SELECT PARSENAME('[1012-1111].SchoolDatabase.school.Student',4)     // returns `[1012-1111]`

```

PARSENAME will returns null is specified part is not present in given object name string



## INSTR


Return the index of the first occurrence of a substring (zero if not found)

Syntax: INSTR ( string, substring )

```sql
SELECT INSTR('FooBarBar', 'Bar') -- return 4
SELECT INSTR('FooBarBar', 'Xar') -- return 0

```



#### Syntax


- CONCAT ( string_value1, string_value2 [, string_valueN ] )
- LTRIM ( character_expression )
- RTRIM ( character_expression )
- SUBSTRING ( expression ,start , length )
- ASCII ( character_expression )
- REPLICATE ( string_expression ,integer_expression )
- REVERSE ( string_expression )
- UPPER ( character_expression )
- TRIM ( [ characters FROM ] string )
- STRING_SPLIT ( string , separator )
- STUFF ( character_expression , start , length , replaceWith_expression )
- REPLACE ( string_expression , string_pattern , string_replacement )



#### Remarks


[String functions reference for Transact-SQL / Microsoft](https://msdn.microsoft.com/en-us/library/ms181984.aspx#)

[String functions reference for MySQL](http://dev.mysql.com/doc/refman/5.7/en/string-functions.html)

[String functions reference for PostgreSQL](https://www.postgresql.org/docs/current/static/functions-string.html)

