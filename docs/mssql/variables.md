---
metaTitle: "Variables"
description: "Updating variables using SELECT, Declare multiple variables at once, with initial values, Declare a Table Variable, Updating a variable using SET, Compound assignment operators, Updating variables by selecting from a table"
---

# Variables



## Updating variables using SELECT


Using `SELECT`, you can update multiple variables at once.

```sql
DECLARE @Variable1 INT, @Variable2 VARCHAR(10)
SELECT @Variable1 = 1, @Variable2 = 'Hello'
PRINT @Variable1
PRINT @Variable2

```

> 
1
Hello


When using `SELECT` to update a variable from a table column, if there are multiple values, it will use the **last** value. (Normal order rules apply - if no sort is given, the order is not guaranteed.)

```sql
CREATE TABLE #Test (Example INT)
INSERT INTO #Test VALUES (1), (2)

DECLARE @Variable INT
SELECT @Variable = Example
FROM #Test
ORDER BY Example ASC

PRINT @Variable

```

> 
2


```sql
SELECT TOP 1 @Variable = Example
FROM #Test
ORDER BY Example ASC

PRINT @Variable

```

> 
1


If there are no rows returned by the query, the variable's value won't change:

```sql
SELECT TOP 0 @Variable = Example
FROM #Test
ORDER BY Example ASC

PRINT @Variable

```

> 
1




## Declare multiple variables at once, with initial values


```sql
DECLARE 
  @Var1 INT = 5, 
  @Var2 NVARCHAR(50) = N'Hello World', 
  @Var3 DATETIME = GETDATE()

```



## Declare a Table Variable


```sql
DECLARE @Employees TABLE
(
    EmployeeID INT NOT NULL PRIMARY KEY,
    FirstName NVARCHAR(50) NOT NULL,
    LastName NVARCHAR(50) NOT NULL,
    ManagerID INT NULL
)

```

When you create a normal table, you use `CREATE TABLE Name (Columns)` syntax. When creating a table variable, you use `DECLARE @Name TABLE (Columns)` syntax.

To reference the table variable inside a `SELECT` statement, SQL Server requires that you give the table variable an alias, otherwise you'll get an error:

> 
Must declare the scalar variable "@TableVariableName".


i.e.

```sql
DECLARE @Table1 TABLE (Example INT)
DECLARE @Table2 TABLE (Example INT)

/*
-- the following two commented out statements would generate an error:
SELECT *
FROM @Table1
INNER JOIN @Table2 ON @Table1.Example = @Table2.Example

SELECT *
FROM @Table1
WHERE @Table1.Example = 1
*/

-- but these work fine:
SELECT *
FROM @Table1 T1
INNER JOIN @Table2 T2 ON T1.Example = T2.Example

SELECT *
FROM @Table1 Table1
WHERE Table1.Example = 1

```



## Updating a variable using SET


```sql
DECLARE @VariableName INT
SET @VariableName = 1
PRINT @VariableName

```

> 
1


Using `SET`, you can only update one variable at a time.



## Compound assignment operators


Supported compound operators:

> 
`+=` Add and assign
`-=` Subtract and assign
`*=` Multiply and assign
`/=` Divide and assign
`%=` Modulo and assign
`&=` Bitwise AND and assign
`^=` Bitwise XOR and assign
`|=` Bitwise OR and assign


Example usage:

```sql
DECLARE @test INT = 42;
SET @test += 1;
PRINT @test;    --43
SET @test -= 1;
PRINT @test;    --42
SET @test *= 2
PRINT @test;    --84
SET @test /= 2;
PRINT @test;    --42

```



## Updating variables by selecting from a table


Depending on the structure of your data, you can create variables that update dynamically.

```sql
DECLARE @CurrentID int = (SELECT TOP 1 ID FROM Table ORDER BY CreateDate desc)


DECLARE @Year int = 2014
DECLARE @CurrentID int = (SELECT ID FROM Table WHERE Year = @Year)

```

In most cases, you will want to ensure that your query returns only one value when using this method.



#### Syntax


- DECLARE @VariableName DataType [ = Value ] ;
- SET @VariableName = Value ;

