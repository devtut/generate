---
metaTitle: "Getting started with Microsoft SQL Server"
description: "INSERT / SELECT / UPDATE / DELETE: the basics of Data Manipulation Language, SELECT all rows and columns from a table, UPDATE Specific Row, Comments in code, DELETE All Rows, PRINT, Select rows that match a condition, UPDATE All Rows, Retrieve Basic Server Information, TRUNCATE TABLE, Create new table and insert records from old table, Using Transactions to change data safely, Getting Table Row Count"
---

# Getting started with Microsoft SQL Server



## INSERT / SELECT / UPDATE / DELETE: the basics of Data Manipulation Language


**D**ata **M**anipulation **L**anguage (DML for short) includes operations such as `INSERT`, `UPDATE` and `DELETE`:

```sql
-- Create a table HelloWorld

CREATE TABLE HelloWorld (
    Id INT IDENTITY,
    Description VARCHAR(1000)
)


-- DML Operation INSERT, inserting a row into the table
INSERT INTO HelloWorld (Description) VALUES ('Hello World')


-- DML Operation SELECT, displaying the table 
SELECT * FROM HelloWorld  


-- Select a specific column from table
SELECT Description FROM HelloWorld


-- Display number of records in the table
SELECT Count(*) FROM HelloWorld


-- DML Operation UPDATE, updating a specific row in the table
UPDATE HelloWorld SET Description = 'Hello, World!' WHERE Id = 1


-- Selecting rows from the table (see how the Description has changed after the update?)
SELECT * FROM HelloWorld


-- DML Operation - DELETE, deleting a row from the table
DELETE FROM HelloWorld WHERE Id = 1


-- Selecting the table. See table content after DELETE operation 
SELECT * FROM HelloWorld

```

In this script we're **creating a table** to demonstrate some basic queries.

The following examples are showing how to **query tables:**

```sql
USE Northwind;
GO
SELECT TOP 10 * FROM Customers 
ORDER BY CompanyName

```

will select the first 10 records of the `Customer` table, ordered by the column `CompanyName` from the database `Northwind` (which is one of Microsoft's sample databases, it can be downloaded from **[here](https://technet.microsoft.com/en-us/library/ms143221(v=sql.105).aspx)**):

[<img src="https://i.stack.imgur.com/6xeX4.jpg" alt="Northwind database query" />](https://i.stack.imgur.com/6xeX4.jpg)

**Note** that `Use Northwind;` changes the default database for all subsequent queries.
You can still reference the database by using the fully qualified syntax in the form of [Database].[Schema].[Table]:

```sql
SELECT TOP 10 * FROM Northwind.dbo.Customers 
ORDER BY CompanyName

SELECT TOP 10 * FROM Pubs.dbo.Authors
ORDER BY City

```

This is useful if you're querying data from different databases. Note that `dbo`, specified "in between" is called a schema and needs to be specified while using the fully qualified syntax. You can think of it as a folder within your database. `dbo` is the default schema. The default schema may be omitted. All other user defined schemas need to be specified.

If the database table contains columns which are named like reserved words, e.g. `Date`, you need to enclose the column name in brackets, like this:

```sql
-- descending order
SELECT TOP 10 [Date] FROM dbo.MyLogTable
ORDER BY [Date] DESC

```

The same applies if the column name contains spaces in its name (which is not recommended). An alternative syntax is to use double quotes instead of square brackets, e.g.:

```sql
-- descending order
SELECT top 10 "Date" from dbo.MyLogTable
order by "Date" desc 

```

is equivalent but not so commonly used. Notice the difference between double quotes and single quotes: Single quotes are used for strings, i.e.

```sql
-- descending order
SELECT top 10 "Date" from dbo.MyLogTable
where UserId='johndoe'
order by "Date" desc 

```

is a valid syntax. Notice that T-SQL has a `N` prefix for NChar and NVarchar data types, e.g.

```sql
SELECT TOP 10 * FROM Northwind.dbo.Customers 
WHERE CompanyName LIKE N'AL%'
ORDER BY CompanyName

```

returns all companies having a company name starting with `AL` (`%` is a wild card, use it as you would use the asterisk in a DOS command line, e.g. `DIR AL*`). For `LIKE`, there are a couple of wildcards available, look **[here](https://msdn.microsoft.com/en-us/library/ms179859.aspx)** to find out more details.

### **Joins**

Joins are useful if you want to query fields which don't exist in one single table, but in multiple tables. For example: You want to query all columns from the `Region` table in the `Northwind` database. But you notice that you require also the `RegionDescription`, which is stored in a different table, `Region`. However, there is a common key, `RgionID` which you can use to combine this information in a single query as follows (`Top 5` just returns the first 5 rows, omit it to get all rows):

```sql
SELECT TOP 5 Territories.*, 
    Regions.RegionDescription 
FROM Territories 
INNER JOIN Region 
    ON Territories.RegionID=Region.RegionID
ORDER BY TerritoryDescription

```

will show all columns from `Territories` plus the `RegionDescription` column from `Region`. The result is ordered by `TerritoryDescription`.

### **Table Aliases**

When your query requires a reference to two or more tables, you may find it useful to use a Table Alias. Table aliases are shorthand references to tables that can be used in place of a full table name, and can reduce typing and editing. The syntax for using an alias is:

```sql
<TableName> [as] <alias>

```

Where `as` is an optional keyword. For example, the previous query can be rewritten as:

```sql
SELECT TOP 5 t.*, 
    r.RegionDescription 
FROM Territories t
INNER JOIN Region r 
    ON t.RegionID = r.RegionID
ORDER BY TerritoryDescription

```

Aliases must be unique for all tables in a query, even if you use the same table twice. For example, if your Employee table included a SupervisorId field, you can use this query to return an employee and his supervisor's name:

```sql
SELECT e.*, 
    s.Name as SupervisorName -- Rename the field for output
FROM Employee e
INNER JOIN Employee s
    ON e.SupervisorId = s.EmployeeId
WHERE e.EmployeeId = 111

```

### **Unions**

As we have seen before, a Join adds columns from different table sources. But what if you want to combine rows from different sources? In this case you can use a UNION. Suppose you're planning a party and want to invite not only employees but also the customers. Then you could run this query to do it:

```sql
SELECT FirstName+' '+LastName as ContactName, Address, City FROM Employees
UNION
SELECT ContactName, Address, City FROM Customers

```

It will return names, addresses and cities from the employees and customers in one single table. Note that duplicate rows (if there should be any) are automatically eliminated (if you don't want this, use a `UNION ALL` instead). The column number, column names, order and data type must match across all the select statements that are part of the union - this is why the first SELECT combines `FirstName` and `LastName` from Employee into `ContactName`.

### **Table Variables**

It can be useful, if you need to deal with temporary data (especially in a stored procedure), to use table variables: The difference between a "real" table and a table variable is that it just exists in memory for temporary processing.

**Example:**

```sql
DECLARE @Region TABLE
(
  RegionID int, 
  RegionDescription NChar(50)
)

```

creates a table in memory. In this case the `@` prefix is mandatory because it is a variable. You can perform all DML operations mentioned above to insert, delete and select rows, e.g.

```sql
INSERT INTO @Region values(3,'Northern')
INSERT INTO @Region values(4,'Southern')

```

But normally, you would populate it based on a real table like

```sql
INSERT INTO @Region
SELECT * FROM dbo.Region WHERE RegionID>2;

```

which would read the filtered values from the real table `dbo.Region` and insert it into the memory table `@Region` - where it can be used for further processing. For example, you could use it in a join like

```sql
SELECT * FROM Territories t
JOIN @Region r on t.RegionID=r.RegionID

```

which would in this case return all `Northern` and `Southern` territories.
More detailed information can be found [here](http://odetocode.com/Articles/365.aspx). Temporary tables are discussed [here](https://www.simple-talk.com/sql/t-sql-programming/temporary-tables-in-sql-server/), if you are interested to read more about that topic.

**NOTE:** Microsoft only recommends the use of table variables if the number of rows of data in the table variable are less than 100. If you will be working with larger amounts of data, use a **temporary table**, or temp table, instead.



## SELECT all rows and columns from a table


Syntax:

```sql
SELECT *
FROM table_name

```

Using the asterisk operator `*` serves as a shortcut for selecting all the columns in the table. All rows will also be selected because this `SELECT` statement does not have a `WHERE` clause, to specify any filtering criteria.

This would also work the same way if you added an alias to the table, for instance `e` in this case:

```sql
SELECT *
FROM Employees AS e

```

Or if you wanted to select all from a specific table you can use the alias + " .* ":

```sql
SELECT e.*, d.DepartmentName
FROM Employees AS e
    INNER JOIN Department AS d 
        ON e.DepartmentID = d.DepartmentID

```

Database objects may also be accessed using fully qualified names:

```sql
SELECT * FROM [server_name].[database_name].[schema_name].[table_name]

```

This is not necessarily recommended, as changing the server and/or database names would cause the queries using fully-qualified names to no longer execute due to invalid object names.

Note that the fields before `table_name` can be omitted in many cases if the queries are executed on a single server, database and schema, respectively. However, it is common for a database to have multiple schema, and in these cases the schema name should not be omitted when possible.

**Warning:** Using `SELECT *` in production code or stored procedures can lead to problems later on (as new columns are added to the table, or if columns are rearranged in the table), especially if your code makes simple assumptions about the order of columns, or number of columns returned.  So it's safer to always explicitly specify column names in SELECT statements for production code.

```sql
SELECT col1, col2, col3
FROM table_name

```



## UPDATE Specific Row


```sql
UPDATE HelloWorlds
SET HelloWorld = 'HELLO WORLD!!!'
WHERE Id = 5

```

The above code updates the value of the field "HelloWorld" with "HELLO WORLD!!!" for the record where "Id = 5" in HelloWorlds table.

Note: In an update statement, It is advised to use a "where" clause to avoid updating the whole table unless and until your requirement is different.



## Comments in code


Transact-SQL supports two forms of comment writing.  Comments are ignored by the database engine, and are meant for people to read.

**Comments** are preceded by `--` and are ignored until a new line is encountered:

```sql
-- This is a comment
SELECT *
FROM MyTable -- This is another comment
WHERE Id = 1;

```

**Slash star comments** begin with `/*` and end with `*/`. All text between those delimiters is considered as a comment block.

```sql
/* This is
a multi-line
comment block. */
SELECT Id = 1, [Message] = 'First row'
UNION ALL
SELECT 2, 'Second row'
/* This is a one liner */
SELECT 'More';

```

Slash star comments have the advantage of keeping the comment usable if the SQL Statement loses new line characters.  This can happen when SQL is captured during troubleshooting.

Slash star comments can be nested and a starting `/*` inside a slash star comment needs to be ended with a `*/` to be valid. The following code will result in an error

```sql
/*
SELECT *
FROM CommentTable
WHERE Comment = '/*'
*/

```

The slash star even though inside the quote is considered as the start of a comment. Hence it needs to be ended with another closing star slash.
The correct way would be

```sql
/*
SELECT *
FROM CommentTable
WHERE Comment = '/*'
*/  */

```



## DELETE All Rows


```sql
DELETE
FROM Helloworlds

```

This will delete all the data from the table. The table will contain no rows after you run this code. Unlike `DROP TABLE`, this preserves the table itself and its structure and you can continue to insert new rows into that table.

Another way to delete all rows in table is truncate it, as follow:

```sql
TRUNCATE TABLE HelloWords

```

Difference with DELETE operation are several:

1. Truncate operation doesn't store in transaction log file
1. If exists `IDENTITY` field, this will be reset
1. TRUNCATE can be applied on whole table and no on part of it (instead with `DELETE` command you can associate a `WHERE` clause)

**Restrictions Of TRUNCATE**

1. Cannot TRUNCATE a table if there is a `FOREIGN KEY` reference
1. If the table is participated in an `INDEXED VIEW`
1. If the table is published by using `TRANSACTIONAL REPLICATION` or `MERGE REPLICATION`
1. It will not fire any TRIGGER defined in the table

[[sic]](https://msdn.microsoft.com/en-us/library/ms177570.aspx)



## PRINT


Display a message to the output console. Using SQL Server Management Studio, this will be displayed in the messages tab, rather than the results tab:

```sql
PRINT 'Hello World!';

```



## Select rows that match a condition


Generally, the syntax is:

```sql
SELECT <column names>
FROM <table name>
WHERE <condition>

```

For example:

```sql
SELECT FirstName, Age
FROM Users
WHERE LastName = 'Smith'

```

Conditions can be complex:

```sql
SELECT FirstName, Age
FROM Users
WHERE LastName = 'Smith' AND (City = 'New York' OR City = 'Los Angeles')

```



## UPDATE All Rows


A simple form of updating is incrementing all the values in a given field of the table.
In order to do so, we need to define the field and the increment value

The following is an example that increments the `Score` field by 1 (in all rows):

```sql
UPDATE Scores
SET score = score + 1  

```

This can be dangerous since you can corrupt your data if you accidentally make an  UPDATE for a **specific Row** with an  UPDATE for **All rows** in the table.



## Retrieve Basic Server Information


```sql
SELECT @@VERSION

```

Returns the version of MS SQL Server running on the instance.

```sql
SELECT @@SERVERNAME

```

Returns the name of the MS SQL Server instance.

```sql
SELECT @@SERVICENAME

```

Returns the name of the Windows service MS SQL Server is running as.

```sql
SELECT serverproperty('ComputerNamePhysicalNetBIOS');

```

Returns the physical name of the machine where SQL Server is running. Useful to identify the node in a failover cluster.

```sql
SELECT * FROM fn_virtualservernodes();

```

In a failover cluster returns every node where SQL Server can run on. It returns nothing if not a cluster.



## TRUNCATE TABLE


```sql
TRUNCATE TABLE Helloworlds 

```

This code will delete all the data from the table Helloworlds. Truncate table is almost similar to `Delete from Table` code. The difference is that you can not use where clauses with Truncate. Truncate table is considered better than delete because it uses less transaction log spaces.

Note that if an identity column exists, it is reset to the initial seed value (for example, auto-incremented ID will restart from 1). This can lead to inconsistency if the identity columns is used as a foreign key in another table.



## Create new table and insert records from old table


```sql
SELECT * INTO NewTable FROM OldTable

```

Creates a new table with structure of old table and inserts all rows into the new table.

**Some Restrictions**

> 
<ol>
- You cannot specify a table variable or table-valued parameter as the new table.
<li>You cannot use SELECT…INTO to create a partitioned table, even when the source table   is partitioned. SELECT...INTO does not use the
partition scheme of the source table; instead, the new table is
created in the default filegroup. To insert rows into a partitioned
table, you must first create the partitioned table and then use the
INSERT INTO...SELECT FROM statement.</li>
<li>Indexes, constraints, and triggers defined in the source table are not transferred to the new table, nor can they be specified in the
SELECT...INTO statement. If these objects are required, you can create
them after executing the SELECT...INTO statement.</li>
<li>Specifying an ORDER BY clause does not guarantee the rows are inserted in the specified order. When a sparse column is included in
the select list,  the sparse column property does not transfer to the
column in the new table. If this property is required in the new
table, alter the column definition after executing the SELECT...INTO
statement to include this property.</li>
<li>When a computed column is included in the select list, the corresponding column in the new table is not a computed column. The
values in the new column are the values that were computed at the time
SELECT...INTO was executed.</li>
</ol>


[**[sic](https://msdn.microsoft.com/en-us/library/ms188029.aspx)**]



## Using Transactions to change data safely


Whenever you change data, in a Data Manipulation Language(DML) command, you can wrap your changes in a transaction. DML includes `UPDATE`, `TRUNCATE`, `INSERT` and `DELETE`. One of the ways that you can make sure that you're changing the right data would be to use a transaction.

DML changes will take a lock on the rows affected. When you begin a transaction, you must end the transaction or all objects being changed in the DML will remain locked by whoever began the transaction. You can end your transaction with either `ROLLBACK` or `COMMIT`. `ROLLBACK` returns everything within the transaction to its original state. `COMMIT` places the data into a final state where you cannot undo your changes without another DML statement.

**Example:**

```sql
--Create a test table

USE [your database]
GO
CREATE TABLE test_transaction (column_1 varchar(10))
GO

INSERT INTO 
 dbo.test_transaction
        ( column_1 )
VALUES
        ( 'a' )

BEGIN TRANSACTION --This is the beginning of your transaction

UPDATE dbo.test_transaction
SET column_1 = 'B'
OUTPUT INSERTED.*
WHERE column_1 = 'A'
  

ROLLBACK TRANSACTION  --Rollback will undo your changes
           --Alternatively, use COMMIT to save your results

SELECT * FROM dbo.test_transaction   --View the table after your changes have been run

DROP TABLE dbo.test_transaction

```

**Notes:**

- This is a **simplified example** which does not include error handling. But any database operation can fail and hence throw an exception. **[Here is an example](http://stackoverflow.com/a/2128105/1016343)** how such a required error handling might look like. You should **never** use transactions **without an error handler**, otherwise you might leave the transaction in an unknown state.
- Depending on the **[isolation level](https://technet.microsoft.com/en-us/library/ms189122(v=sql.105).aspx)**, transactions are putting locks on the data being queried or changed. You need to ensure that transactions are not running for a long time, because they will lock records in a database and can lead to **[deadlocks](https://www.simple-talk.com/sql/performance/sql-server-deadlocks-by-example/)** with other parallel running transactions. Keep the operations encapsulated in transactions as short as possible and minimize the impact with the amount of data you're locking.



## Getting Table Row Count


The following example can be used to find the total row count for a specific table in a database if `table_name` is replaced by the the table you wish to query:

```sql
SELECT COUNT(*) AS [TotalRowCount] FROM table_name;

```

It is also possible to get the row count for all tables by joining back to the table's partition based off the tables' HEAP (index_id = 0) or cluster clustered index (index_id = 1) using the following script:

```sql
SELECT  [Tables].name                AS [TableName],
        SUM( [Partitions].[rows] )    AS [TotalRowCount]
FROM    sys.tables AS [Tables]
JOIN    sys.partitions AS [Partitions]
    ON  [Tables].[object_id]    =    [Partitions].[object_id]
    AND [Partitions].index_id IN ( 0, 1 )
--WHERE    [Tables].name = N'table name' /* uncomment to look for a specific table */
GROUP BY    [Tables].name;

```

This is possible as every table is essentially a single partition table, unless extra partitions are added to it.
This script also has the benefit of not interfering with read/write operations to the tables rows'.



#### Remarks


This is a set of examples highlighting basic usage of SQL Server.

