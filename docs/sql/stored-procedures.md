---
metaTitle: "Stored Procedures"
description: "Create and call a stored procedure"
---

# Stored Procedures



## Create and call a stored procedure


Stored procedures can be created through a database management GUI ([SQL Server example](https://msdn.microsoft.com/en-us/library/ms345415.aspx)), or through a SQL statement as follows:

```sql
-- Define a name and parameters
CREATE PROCEDURE Northwind.getEmployee
    @LastName nvarchar(50),   
    @FirstName nvarchar(50)   
AS   

-- Define the query to be run
SELECT FirstName, LastName, Department  
FROM Northwind.vEmployeeDepartment
WHERE FirstName = @FirstName AND LastName = @LastName  
AND EndDate IS NULL;  

```

Calling the procedure:

```sql
EXECUTE Northwind.getEmployee N'Ackerman', N'Pilar';

-- Or  
EXEC Northwind.getEmployee @LastName = N'Ackerman', @FirstName = N'Pilar';  
GO  

-- Or  
EXECUTE Northwind.getEmployee @FirstName = N'Pilar', @LastName = N'Ackerman';  
GO  

```



#### Remarks


Stored Procedures are SQL statements stored in the database that can be executed or called in queries. Using a stored procedure allows encapsulation of complicated or frequently used logic, and improves query performance by utilizing cached query plans. They can return any value a standard query can return.

Other benefits over dynamic SQL expressions are listed on [Wikipeida](https://en.wikipedia.org/wiki/Stored_procedure#Comparison_with_dynamic_SQL).

