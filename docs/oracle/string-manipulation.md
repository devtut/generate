---
metaTitle: "Oracle Database - String Manipulation"
description: "INITCAP, Concatenation: Operator || or concat() function, UPPER, LOWER, Regular expression, SUBSTR, LTRIM / RTRIM"
---

# String Manipulation




## INITCAP


The `INITCAP` function converts the case of a string so that each word starts with a capital letter and all subsequent letters are in lowercase.

```sql
SELECT INITCAP('HELLO mr macdonald!') AS NEW FROM dual;

```

Output

```sql
NEW
-------------------
Hello Mr Macdonald!

```



## Concatenation: Operator || or concat() function


The Oracle SQL and PL/SQL `||` operator allows you to concatenate 2 or more strings together.

**Example:**

Assuming the following `customers` table:

```

id  firstname    lastname
---  -----------  ----------
  1  Thomas       Woody

```

Query:

```

SELECT firstname || ' ' || lastname || ' is in my database.' as "My Sentence" 
   FROM customers;

```

Output:

```sql
My Sentence
---------------------------------
Thomas Woody is in my database.

```

Oracle also supports the standard SQL `CONCAT(str1, str2)` function:

**Example:**

Query:

```

SELECT CONCAT(firstname, ' is in my database.') from customers;

```

Output:

```sql
Expr1
---------------------------------
Thomas is in my database.

```



## UPPER


The UPPER function allows you to convert all lowercase letters in a string to uppercase.

```sql
SELECT UPPER('My text 123!') AS result FROM dual;

```

Output:

```sql
RESULT    
------------
MY TEXT 123!

```



## LOWER


LOWER converts all uppercase letters in a string to lowercase.

```sql
SELECT LOWER('HELLO World123!') text FROM dual;

```

Outputs:

|text
|---|---|---|---|---|---|---|---|---|---
|hello world123!



## Regular expression


Let's say we want to replace only numbers with 2 digits: regular expression will find them with `(\d\d)`

```sql
SELECT REGEXP_REPLACE ('2, 5, and 10 are numbers in this example', '(\d\d)', '#')
FROM dual;

```

Results in:

```sql
'2, 5, and # are numbers in this example'

```

If I want to swap parts of the text, I use `\1`, `\2`, `\3` to call for the matched strings:

```

SELECT REGEXP_REPLACE ('swap around 10 in that one ', '(.*)(\d\d )(.*)', '\3\2\1\3')
 FROM dual;

```



## SUBSTR


`SUBSTR` retrieves part of a string by indicating the starting position and the number of characters to extract

```sql
SELECT SUBSTR('abcdefg',2,3) FROM DUAL;

```

returns:

```sql
bcd

```

To count from the end of the string, `SUBSTR` accepts a negative number as the second parameter, e.g.

```sql
SELECT SUBSTR('abcdefg',-4,2) FROM DUAL;

```

returns:

```sql
de

```

To get the last character in a string: `SUBSTR(mystring,-1,1)`



## LTRIM / RTRIM


`LTRIM` and `RTRIM` remove characters from the beginning or the end (respectively) of a string. A set of one or more characters may be supplied (default is a space) to remove.

For example,

```sql
select LTRIM('<===>HELLO<===>', '=<>')
      ,RTRIM('<===>HELLO<===>', '=<>')
from dual;

```

Returns:

```sql
HELLO<===>
<===>HELLO

```

