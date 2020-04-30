---
metaTitle: "The STUFF Function"
description: "Using FOR XML to Concatenate Values from Multiple Rows, Basic Character Replacement with STUFF(), Basic Example of STUFF() function., Obtain column names separated with comma (not a list), stuff for comma separated in sql server"
---

# The STUFF Function



## Using FOR XML to Concatenate Values from Multiple Rows


One common use for the `FOR XML` function is to concatenate the values of multiple rows.

Here's an example using the [Customers table](http://stackoverflow.com/documentation/sql/280/example-databases/1015/customers-table#t=201607211440098397043):

```sql
SELECT 
    STUFF( (SELECT ';' + Email
        FROM Customers 
        where (Email is not null and Email <> '')
        ORDER BY Email ASC 
        FOR XML PATH('')), 
    1, 1, '')

```

In the example above, `FOR XML PATH(''))` is being used to concatenate email addresses, using `;` as the delimiter character. Also, the purpose of `STUFF` is to remove the leading `;` from the concatenated string. `STUFF` is also implicitly casting the concatenated string from XML to varchar.

Note: the result from the above example will be XML-encoded, meaning it will replace `<` characters with `&lt;` etc. If you don't want this, change `FOR XML PATH(''))` to `FOR XML PATH, TYPE).value('.[1]','varchar(MAX)')`, e.g.:

```sql
SELECT 
    STUFF( (SELECT ';' + Email
        FROM Customers 
        where (Email is not null and Email <> '')
        ORDER BY Email ASC 
        FOR XML PATH, TYPE).value('.[1]','varchar(900)'),
    1, 1, '')

```

This can be used to achieve a result similar to `GROUP_CONCAT` in MySQL or `string_agg` in PostgreSQL 9.0+, although we use subqueries instead of GROUP BY aggregates. (As an alternative, you can install a user-defined aggregate such as [this one](https://groupconcat.codeplex.com/) if you're looking for functionality closer to that of `GROUP_CONCAT`).



## Basic Character Replacement with STUFF()


The `STUFF()` function inserts a string into another string by first deleting a specified number of characters.  The following example, deletes "Svr" and replaces it with "Server". This happens by specifying the `start_position` and `length` of the replacement.

```sql
SELECT STUFF('SQL Svr Documentation', 5, 3, 'Server')

```

Executing this example will result in returning `SQL Server Documentation` instead of `SQL Svr Documentation.`



## Basic Example of STUFF() function.


STUFF(Original_Expression, Start, Length, Replacement_expression)

STUFF() function inserts Replacement_expression, at the start position specified, along with removing the characters specified using Length parameter.

```

Select FirstName, LastName,Email, STUFF(Email, 2, 3, '*****') as StuffedEmail From Employee

```

**Executing this example will result in returning the given table**

|FirstName|LastName|Email|StuffedEmail
|------
|Jomes|Hunter|James@hotmail.com|J*****s@hotmail.com
|Shyam|rathod|Shyam@hotmail.com|S*****m@hotmail.com
|Ram|shinde|Ram@hotmail.com|R*****hotmail.com



## Obtain column names separated with comma (not a list)


```sql
/*
The result can be use for fast way to use columns on Insertion/Updates.
Works with tables and views.

Example: eTableColumns  'Customers'
ColumnNames
------------------------------------------------------
Id, FName, LName, Email, PhoneNumber, PreferredContact

INSERT INTO Customers (Id, FName, LName, Email, PhoneNumber, PreferredContact)
    VALUES (5, 'Ringo', 'Star', 'two@beatles.now', NULL, 'EMAIL')
*/
CREATE PROCEDURE eTableColumns (@Table VARCHAR(100))
AS
SELECT ColumnNames = 
   STUFF( (SELECT ', ' +  c.name
FROM    
    sys.columns c
INNER JOIN 
    sys.types t ON c.user_type_id = t.user_type_id
WHERE
    c.object_id = OBJECT_ID( @Table)
        FOR XML PATH, TYPE).value('.[1]','varchar(2000)'),
    1, 1, '')
GO

```



## stuff for comma separated in sql server


`FOR XML PATH` and `STUFF` to concatenate the multiple rows into a single row:

```

 select distinct t1.id,
      STUFF(
             (SELECT ', ' + convert(varchar(10), t2.date, 120)
              FROM yourtable t2
              where t1.id = t2.id
              FOR XML PATH (''))
              , 1, 1, '')  AS date
    from yourtable t1;

```



#### Parameters


|Parameter|Details
|------
|character_expression|the existing string in your data
|start_position|the position in `character_expression` to delete `length` and then insert the `replacement_string`
|length|the number of characters to delete from `character_expression`
|replacement_string|the sequence of characters to insert in `character_expression`

