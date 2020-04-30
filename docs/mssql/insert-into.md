---
metaTitle: "INSERT INTO"
description: "INSERT multiple rows of data, Use OUTPUT to get the new Id, INSERT from SELECT Query Results, INSERT a single row of data, INSERT on specific columns, INSERT Hello World INTO table"
---

# INSERT INTO


The INSERT INTO statement is used to insert new records in a table.



## INSERT multiple rows of data


To insert multiple rows of data in SQL Server 2008 or later:

```sql
INSERT INTO USERS VALUES
(2, 'Michael', 'Blythe'),
(3, 'Linda', 'Mitchell'),
(4, 'Jillian', 'Carson'),
(5, 'Garrett', 'Vargas');

```

To insert multiple rows of data in earlier versions of SQL Server, use "UNION ALL" like so:

```sql
INSERT INTO USERS (FIRST_NAME, LAST_NAME)
SELECT 'James', 'Bond' UNION ALL
SELECT 'Miss', 'Moneypenny' UNION ALL
SELECT 'Raoul', 'Silva'

```

Note, the "INTO" keyword is optional in INSERT queries. Another warning is that SQL server only supports 1000 rows in one INSERT so you have to split them in batches.



## Use OUTPUT to get the new Id


When INSERTing, you can use `OUTPUT INSERTED.ColumnName` to get values from the newly inserted row, for example the newly generated Id - useful if you have an `IDENTITY` column or any sort of default or calculated value.

When programatically calling this (e.g., from ADO.net) you would treat it as a normal query and read the values as if you would've made a `SELECT`-statement.

```sql
-- CREATE TABLE OutputTest ([Id] INT NOT NULL PRIMARY KEY IDENTITY, [Name] NVARCHAR(50))

INSERT INTO OutputTest ([Name])
OUTPUT INSERTED.[Id]
VALUES ('Testing')

```

If the ID of the recently added row is required inside the same set of query or stored procedure.

```sql
-- CREATE a table variable having column with the same datatype of the ID

DECLARE @LastId TABLE ( id int);

INSERT INTO OutputTest ([Name])
OUTPUT INSERTED.[Id] INTO @LastId
VALUES ('Testing')

SELECT id FROM @LastId

-- We can set the value in a variable and use later in procedure

DECLARE @LatestId int = (SELECT id FROM @LastId)

```



## INSERT from SELECT Query Results


To insert data retrieved from SQL query (single or multiple rows)

```sql
INSERT INTO Table_name (FirstName, LastName, Position)
SELECT FirstName, LastName, 'student' FROM Another_table_name

```

Note, 'student' in SELECT is a string constant that will be inserted in each row.

If required, you can select and insert data from/into the same table



## INSERT a single row of data


A single row of data can be inserted in two ways:

```sql
INSERT INTO USERS(Id, FirstName, LastName)
VALUES (1, 'Mike', 'Jones');

```

Or

```sql
INSERT INTO USERS
VALUES (1, 'Mike', 'Jones');

```

Note that the second insert statement only allows the values in exactly the same order as the table columns whereas in the first insert, the order of the values can be changed like:

```sql
INSERT INTO USERS(FirstName, LastName, Id)
VALUES ('Mike', 'Jones', 1);

```



## INSERT on specific columns


To do an insert on specific columns (as opposed to all of them) you must specify the columns you want to update.

```sql
INSERT INTO USERS (FIRST_NAME, LAST_NAME)
VALUES ('Stephen', 'Jiang');

```

This will only work if the columns that you did not list are nullable, identity, timestamp data type or computed columns; or columns that have a default value constraint. Therefore, if any of them are non-nullable, non-identity, non-timestamp, non-computed, non-default valued columns...then attempting this kind of insert will trigger an error message telling you that you have to provide a value for the applicable field(s).



## INSERT Hello World INTO table


```sql
CREATE TABLE MyTableName
(
    Id INT,
    MyColumnName NVARCHAR(1000)
)
GO

INSERT INTO MyTableName (Id, MyColumnName)
VALUES (1, N'Hello World!')
GO    

```

