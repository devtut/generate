---
metaTitle: "NULLs"
description: "COALESCE (), NULL comparison, ANSI NULLS, ISNULL(), Is null / Is not null, NULL with NOT IN SubQuery"
---

# NULLs


In SQL Server, `NULL` represents data that is missing, or unknown.  This means that `NULL` is not really a value; it's better described as a placeholder for a value. This is also the reason why you can't compare `NULL` with any value, and not even with another `NULL`.



## COALESCE ()


`COALESCE ()` Evaluates the arguments in order and returns the current value of the first expression that initially does not evaluate to `NULL`.

```sql
DECLARE @MyInt int -- variable is null until it is set with value.
DECLARE @MyInt2 int -- variable is null until it is set with value.
DECLARE @MyInt3 int -- variable is null until it is set with value.

SET @MyInt3  = 3

SELECT COALESCE (@MyInt, @MyInt2 ,@MyInt3 ,5) -- Returns 3 : value of @MyInt3.

```

Although ISNULL() operates similarly to COALESCE(), the ISNULL() function only accepts two parameters - one to check, and one to use if the first parameter is NULL.
See also `ISNULL`, below



## NULL comparison


`NULL` is a special case when it comes to comparisons.

Assume the following data.

```sql
id someVal
 ----
 0 NULL
 1 1
 2 2

```

With a query:

```

SELECT id
 FROM table
 WHERE someVal = 1

```

would return id `1`

```

SELECT id
 FROM table
 WHERE someVal <> 1

```

would return id `2`

```

SELECT id
 FROM table
 WHERE someVal IS NULL

```

would return id `0`

```

SELECT id
 FROM table
 WHERE someVal IS NOT NULL

```

would return both ids `1` and `2`.

If you wanted NULLs to be "counted" as values in a `=`, `<>` comparison, it must first be converted to a countable data type:

```

SELECT id
 FROM table
 WHERE ISNULL(someVal, -1) <> 1

```

OR

```

SELECT id
 FROM table
 WHERE someVal IS NULL OR someVal <> 1

```

returns `0` and `2`

Or you can change your [ANSI Null](http://stackoverflow.com/a/411868/2312877) setting.



## ANSI NULLS


From [MSDN](https://msdn.microsoft.com/en-us/library/ms188048.aspx)

> 
In a future version of SQL Server, ANSI_NULLS will always be ON and any applications that explicitly set the option to OFF will generate an error. Avoid using this feature in new development work, and plan to modify applications that currently use this feature.


`ANSI NULLS` being set to off allows for a `=`/`<>` comparison of null values.

Given the following data:

```sql
id someVal
 ----
 0 NULL
 1 1
 2 2

```

And with ANSI NULLS on, this query:

```

SELECT id
 FROM table
 WHERE someVal = NULL

```

would produce no results.  However the same query, with ANSI NULLS off:

```

set ansi_nulls off

 SELECT id
 FROM table
 WHERE someVal = NULL

```

Would return id `0`.



## ISNULL()


The `IsNull()` function accepts two parameters, and returns the second parameter if the first one is `null`.

Parameters:

1. check expression. Any expression of any data type.
1. replacement value. This is the value that would be returned if the check expression is null. The replacement value must be of a data type that can be implicitly converted to the data type of the check expression.

The `IsNull()` function returns the same data type as the check expression.

```sql
DECLARE @MyInt int -- All variables are null until they are set with values.

SELECT ISNULL(@MyInt, 3) -- Returns 3.

```

See also `COALESCE`, above



## Is null / Is not null


Since null is not a value, you can't use comparison operators with nulls.<br />
To check if a column or variable holds null, you need to use `is null`:

```sql
DECLARE @Date date = '2016-08-03'

```

The following statement will select the value `6`, since all comparisons with null values evaluates to false or unknown:

```sql
SELECT CASE WHEN @Date = NULL THEN 1
            WHEN @Date <> NULL THEN 2
            WHEN @Date > NULL THEN 3
            WHEN @Date < NULL THEN 4
            WHEN @Date IS NULL THEN 5
            WHEN @Date IS NOT NULL THEN 6

```

Setting the content of the @Date variable to `null` and try again, the following statement will return `5`:

```sql
SET @Date = NULL -- Note that the '=' here is an assignment operator!

SELECT CASE WHEN @Date = NULL THEN 1
            WHEN @Date <> NULL THEN 2
            WHEN @Date > NULL THEN 3
            WHEN @Date < NULL THEN 4
            WHEN @Date IS NULL THEN 5
            WHEN @Date IS NOT NULL THEN 6

```



## NULL with NOT IN SubQuery


While handling not in sub-query with null in the sub-query we need to eliminate NULLS to get your expected results

```sql
create table #outertable (i int)
create table #innertable (i int)

insert into #outertable (i) values (1), (2),(3),(4), (5)
insert into #innertable (i) values (2), (3), (null)

select * from #outertable where i in (select i from #innertable)
--2
--3
--So far so good

select * from #outertable where i not in (select i from #innertable)
--Expectation here is to get 1,4,5 but it is not. It will get empty results because of the NULL it executes as {select * from #outertable where i not in (null)}

--To fix this 
select * from #outertable where i not in (select i from #innertable where i is not null)
--you will get expected results
--1
--4
--5

```

While handling not in sub-query with null be cautious with your expected output



#### Remarks


SQL Server provides other methods to handle nulls, such as `IS NULL`, `IS NOT NULL`, `ISNULL()`, `COALESCE()` and others.

