---
metaTitle: "Last Inserted Identity"
description: "SCOPE_IDENTITY(), @@IDENTITY, IDENT_CURRENT('tablename'), @@IDENTITY and MAX(ID)"
---

# Last Inserted Identity



## SCOPE_IDENTITY()


```sql
CREATE TABLE dbo.logging_table(log_id INT IDENTITY(1,1) PRIMARY KEY, 
                               log_message VARCHAR(255))

CREATE TABLE dbo.person(person_id INT IDENTITY(1,1) PRIMARY KEY, 
                        person_name VARCHAR(100) NOT NULL)
GO;

CREATE TRIGGER dbo.InsertToADifferentTable ON dbo.person  
AFTER INSERT  
AS
    INSERT INTO dbo.logging_table(log_message)
    VALUES('Someone added something to the person table')
GO;

INSERT INTO dbo.person(person_name)
VALUES('John Doe')  

SELECT SCOPE_IDENTITY();

```

This will return the most recently added identity value produced on the same connection, within the current scope. In this case, 1, for the first row in the dbo.person table.



## @@IDENTITY


```sql
CREATE TABLE dbo.logging_table(log_id INT IDENTITY(1,1) PRIMARY KEY, 
                               log_message VARCHAR(255))

CREATE TABLE dbo.person(person_id INT IDENTITY(1,1) PRIMARY KEY, 
                        person_name VARCHAR(100) NOT NULL)
GO;

CREATE TRIGGER dbo.InsertToADifferentTable ON dbo.person  
AFTER INSERT  
AS
    INSERT INTO dbo.logging_table(log_message)
    VALUES('Someone added something to the person table')
GO;

INSERT INTO dbo.person(person_name)
VALUES('John Doe')    

SELECT @@IDENTITY;

```

This will return the most recently-added identity on the same connection, regardless of scope. In this case, whatever the current value of the identity column on logging_table is, assuming no other activity is occurring on the instance of SQL Server and no other triggers fire from this insert.



## IDENT_CURRENT('tablename')


```sql
SELECT IDENT_CURRENT('dbo.person');

```

This will select the most recently-added identity value on the selected table, regardless of connection or scope.



## @@IDENTITY and MAX(ID)


```sql
SELECT MAX(Id) FROM Employees -- Display the value of Id in the last row in Employees table.
GO
INSERT INTO Employees (FName, LName, PhoneNumber) -- Insert a new row 
VALUES ('John', 'Smith', '25558696525') 
GO  
SELECT @@IDENTITY 
GO  
SELECT MAX(Id) FROM Employees -- Display the value of Id of the newly inserted row.  
GO

```

The last two SELECT statements values are the same.

