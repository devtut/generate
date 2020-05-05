---
metaTitle: "Microsoft SQL Server - COALESCE"
description: "Using COALESCE to Build Comma-Delimited String, Coalesce basic Example, Getting the first not null from a list of column values"
---

# COALESCE




## Using COALESCE to Build Comma-Delimited String


We can get a comma delimited string from multiple rows using coalesce as shown below.

Since table variable is used, we need to execute whole query once. So to make easy to understand, I have added BEGIN and END block.

```sql
BEGIN

    --Table variable declaration to store sample records
    DECLARE @Table TABLE (FirstName varchar(256), LastName varchar(256))

    --Inserting sample records into table variable @Table
    INSERT INTO @Table (FirstName, LastName)
    VALUES
    ('John','Smith'),
    ('Jane','Doe')

    --Creating variable to store result          
    DECLARE @Names varchar(4000)

    --Used COLESCE function, so it will concatenate comma seperated FirstName into @Names varible
    SELECT @Names = COALESCE(@Names + ',', '') + FirstName
    FROM @Table

    --Now selecting actual result 
    SELECT @Names
    END

```



## Coalesce basic Example


`COALESCE()` returns the first `NON NULL` value in a list of arguments. Suppose we had a table containing phone numbers, and cell phone numbers and wanted to return only one for each user. In order to only obtain one, we can get the first `NON NULL` value.

```sql
DECLARE @Table TABLE (UserID int, PhoneNumber varchar(12), CellNumber varchar(12))
INSERT INTO @Table (UserID, PhoneNumber, CellNumber)
VALUES
(1,'555-869-1123',NULL),
(2,'555-123-7415','555-846-7786'),
(3,NULL,'555-456-8521')


SELECT
    UserID,
    COALESCE(PhoneNumber, CellNumber)
FROM
    @Table

```



## Getting the first not null from a list of column values


```sql
SELECT COALESCE(NULL, NULL, 'TechOnTheNet.com', NULL, 'CheckYourMath.com');
Result: 'TechOnTheNet.com'

SELECT COALESCE(NULL, 'TechOnTheNet.com', 'CheckYourMath.com');
Result: 'TechOnTheNet.com'

SELECT COALESCE(NULL, NULL, 1, 2, 3, NULL, 4);
Result: 1

```



#### Syntax


- COALESCE([Column1],[Column2]....[ColumnN]

