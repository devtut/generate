---
metaTitle: "Table Valued Parameters"
description: "Using a table valued parameter to insert multiple rows to a table"
---

# Table Valued Parameters



## Using a table valued parameter to insert multiple rows to a table


First, define a [used defined table type](http://stackoverflow.com/documentation/sql-server/5280/user-defined-table-types) to use:

```sql
CREATE TYPE names as TABLE
(
    FirstName varchar(10),
    LastName varchar(10)
)
GO

```

Create the stored procedure:

```sql
CREATE PROCEDURE prInsertNames
(
    @Names dbo.Names READONLY -- Note: You must specify the READONLY
)
AS

INSERT INTO dbo.TblNames (FirstName, LastName)
SELECT FirstName, LastName
FROM @Names 
GO

```

Executing the stored procedure:

```sql
DECLARE @names dbo.Names
INSERT INTO @Names VALUES
('Zohar', 'Peled'),
('First', 'Last')

EXEC dbo.prInsertNames @Names

```



#### Remarks


Table valued parameters (TVP for short) are parameters passed to a stored procedure or function that contains data that is table structured.
Using table valued parameters requires creating a [user defined table type](http://stackoverflow.com/documentation/sql-server/5280/user-defined-table-types) for the parameter being used.

Tabled valued parameters are readonly parameters.

