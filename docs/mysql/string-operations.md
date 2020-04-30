---
metaTitle: "String operations"
description: "Find element in comma separated list, STR_TO_DATE - Convert string to date, LOWER() / LCASE(), REPLACE(), SUBSTRING(), UPPER() / UCASE(), LENGTH(), CHAR_LENGTH(), HEX(str)"
---

# String operations



## Find element in comma separated list


```sql
SELECT FIND_IN_SET('b','a,b,c');

```

Return value:

> 
2


```sql
SELECT FIND_IN_SET('d','a,b,c');

```

Return value:

> 
0




## STR_TO_DATE - Convert string to date


With a column of one of the string types, named `my_date_field` with a value such as [the string] `07/25/2016`, the following statement demonstrates the use of the `STR_TO_DATE` function:

```sql
SELECT STR_TO_DATE(my_date_field, '%m/%d/%Y') FROM my_table;

```

You could use this function as part of `WHERE` clause as well.



## LOWER() / LCASE()


Convert in lowercase the string argument

Syntax: LOWER(str)

```sql
LOWER('fOoBar') -- 'foobar'
LCASE('fOoBar') -- 'foobar'

```



## REPLACE()


Convert in lowercase the string argument

Syntax: REPLACE(str, from_str, to_str)

```sql
REPLACE('foobarbaz', 'bar', 'BAR') -- 'fooBARbaz'
REPLACE('foobarbaz', 'zzz', 'ZZZ') -- 'foobarbaz'

```



## SUBSTRING()


SUBSTRING (or equivalent: SUBSTR) returns the substring starting from the specified position and, optionally, with the specified length

Syntax: `SUBSTRING(str, start_position)`

```sql
SELECT SUBSTRING('foobarbaz', 4); -- 'barbaz'
SELECT SUBSTRING('foobarbaz' FROM 4); -- 'barbaz'

-- using negative indexing
SELECT SUBSTRING('foobarbaz', -6); -- 'barbaz'
SELECT SUBSTRING('foobarbaz' FROM -6); -- 'barbaz'

```

Syntax: `SUBSTRING(str, start_position, length)`

```sql
SELECT SUBSTRING('foobarbaz', 4, 3); -- 'bar'
SELECT SUBSTRING('foobarbaz', FROM 4 FOR 3); -- 'bar'

-- using negative indexing
SELECT SUBSTRING('foobarbaz', -6, 3); -- 'bar'
SELECT SUBSTRING('foobarbaz' FROM -6 FOR 3); -- 'bar'

```



## UPPER() / UCASE()


Convert in uppercase the string argument

Syntax: UPPER(str)

```sql
UPPER('fOoBar') -- 'FOOBAR'
UCASE('fOoBar') -- 'FOOBAR'

```



## LENGTH()


Return the length of the string in bytes. Since some characters may be encoded using more than one byte, if you want the length in characters see CHAR_LENGTH()

Syntax: LENGTH(str)

```sql
LENGTH('foobar') -- 6
LENGTH('fööbar') -- 8 -- contrast with CHAR_LENGTH(...) = 6

```



## CHAR_LENGTH()


Return the number of characters in the string

Syntax: CHAR_LENGTH(str)

```sql
CHAR_LENGTH('foobar') -- 6
CHAR_LENGTH('fööbar') -- 6 -- contrast with LENGTH(...) = 8

```



## HEX(str)


Convert the argument to hexadecimal.  This is used for strings.

```sql
HEX('fööbar') -- 66F6F6626172 -- in "CHARACTER SET latin1" because "F6" is hex for ö
HEX('fööbar') -- 66C3B6C3B6626172  -- in "CHARACTER SET utf8 or utf8mb4" because "C3B6" is hex for ö

```



#### Parameters


|Name|Description
|------
|ASCII()|Return numeric value of left-most character
|BIN()|Return a string containing binary representation of a number
|BIT_LENGTH()|Return length of argument in bits
|CHAR()|Return the character for each integer passed
|CHAR_LENGTH()|Return number of characters in argument
|CHARACTER_LENGTH()|Synonym for CHAR_LENGTH()
|CONCAT()|Return concatenated string
|CONCAT_WS()|Return concatenate with separator
|ELT()|Return string at index number
|EXPORT_SET()|Return a string such that for every bit set in the value bits, you get an on string and for every unset bit, you get an off string
|FIELD()|Return the index (position) of the first argument in the subsequent arguments
|FIND_IN_SET()|Return the index position of the first argument within the second argument
|FORMAT()|Return a number formatted to specified number of decimal places
|FROM_BASE64()|Decode to a base-64 string and return result
|HEX()|Return a hexadecimal representation of a decimal or string value
|INSERT()|Insert a substring at the specified position up to the specified number of characters
|INSTR()|Return the index of the first occurrence of substring
|LCASE()|Synonym for LOWER()
|LEFT()|Return the leftmost number of characters as specified
|LENGTH()|Return the length of a string in bytes
|LIKE|Simple pattern matching
|LOAD_FILE()|Load the named file
|LOCATE()|Return the position of the first occurrence of substring
|LOWER()|Return the argument in lowercase
|LPAD()|Return the string argument, left-padded with the specified string
|LTRIM()|Remove leading spaces
|MAKE_SET()|Return a set of comma-separated strings that have the corresponding bit in bits set
|MATCH|Perform full-text search
|MID()|Return a substring starting from the specified position
|NOT LIKE|Negation of simple pattern matching
|NOT REGEXP|Negation of REGEXP
|OCT()|Return a string containing octal representation of a number
|OCTET_LENGTH()|Synonym for LENGTH()
|ORD()|Return character code for leftmost character of the argument
|POSITION()|Synonym for LOCATE()
|QUOTE()|Escape the argument for use in an SQL statement
|REGEXP|Pattern matching using regular expressions
|REPEAT()|Repeat a string the specified number of times
|REPLACE()|Replace occurrences of a specified string
|REVERSE()|Reverse the characters in a string
|RIGHT()|Return the specified rightmost number of characters
|RLIKE|Synonym for REGEXP
|RPAD()|Append string the specified number of times
|RTRIM()|Remove trailing spaces
|SOUNDEX()|Return a soundex string
|SOUNDS LIKE|Compare sounds
|SPACE()|Return a string of the specified number of spaces
|STRCMP()|Compare two strings
|SUBSTR()|Return the substring as specified
|SUBSTRING()|Return the substring as specified
|SUBSTRING_INDEX()|Return a substring from a string before the specified number of occurrences of the delimiter
|TO_BASE64()|Return the argument converted to a base-64 string
|TRIM()|Remove leading and trailing spaces
|UCASE()|Synonym for UPPER()
|UNHEX()|Return a string containing hex representation of a number
|UPPER()|Convert to uppercase
|WEIGHT_STRING()|Return the weight string for a string

