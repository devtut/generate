---
metaTitle: "Basic DDL Operations in MS SQL Server"
description: "Getting started"
---

# Basic DDL Operations in MS SQL Server



## Getting started


This section describes some basic **DDL** (="**D**ata **D**efinition **L**anguage") commands to create a database, a table within a database, a view and finally a stored procedure.

### Create Database

The following SQL command creates a new database `Northwind` on the current server, using path`C:\Program Files\Microsoft SQL Server\MSSQL11.INSTSQL2012\MSSQL\DATA\`:

```sql
USE [master]
GO

CREATE DATABASE [Northwind]
 CONTAINMENT = NONE
 ON  PRIMARY 
 ( 
  NAME = N'Northwind', 
  FILENAME = N'C:\Program Files\Microsoft SQL Server\MSSQL11.INSTSQL2012\MSSQL\DATA\Northwind.mdf' , SIZE = 5120KB , MAXSIZE = UNLIMITED, FILEGROWTH = 1024KB 
 )
 LOG ON 
 ( 
  NAME = N'Northwind_log', 
  FILENAME = N'C:\Program Files\Microsoft SQL Server\MSSQL11.INSTSQL2012\MSSQL\DATA\Northwind_log.ldf' , SIZE = 1536KB , MAXSIZE = 2048GB , FILEGROWTH = 10%
 )
GO

ALTER DATABASE [Northwind] SET COMPATIBILITY_LEVEL = 110
GO

```

**Note:** A T-SQL database consists of two files, the database file `*.mdf`, and its transaction log `*.ldf`. Both need to be specified when a new database is created.

### Create Table

The following SQL command creates a new table `Categories` in the current database, using schema `dbo` (you can switch database context with `Use <DatabaseName>`):

```sql
CREATE TABLE dbo.Categories(
    CategoryID int IDENTITY NOT NULL,
    CategoryName nvarchar(15) NOT NULL,
    Description ntext NULL,
    Picture image NULL,
      CONSTRAINT PK_Categories PRIMARY KEY CLUSTERED 
      (
         CategoryID ASC
      )
      WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, IGNORE_DUP_KEY = OFF, 
            ALLOW_ROW_LOCKS = ON, ALLOW_PAGE_LOCKS = ON) ON PRIMARY
) ON PRIMARY TEXTIMAGE_ON PRIMARY

```

### Create View

The following SQL command creates a new view `Summary_of_Sales_by_Year` in the current database, using schema `dbo` (you can switch database context with `Use <DatabaseName>`):

```sql
CREATE VIEW dbo.Summary_of_Sales_by_Year AS
  SELECT ord.ShippedDate, ord.OrderID, ordSub.Subtotal
  FROM Orders ord
  INNER JOIN [Order Subtotals] ordSub ON ord.OrderID = ordSub.OrderID

```

This will join tables `Orders` and `[Order Subtotals]` to display the columns `ShippedDate`, `OrderID` and `Subtotal`. Because table `[Order Subtotals]` has a blank in its name in the Northwind database, it needs to be enclosed in square brackets.

### Create Procedure

The following SQL command creates a new stored procedure `CustOrdersDetail` in the current database, using schema `dbo` (you can switch database context with `Use <DatabaseName>`):

```sql
CREATE PROCEDURE dbo.MyCustOrdersDetail @OrderID int, @MinQuantity int=0
AS BEGIN
  SELECT ProductName,
    UnitPrice=ROUND(Od.UnitPrice, 2),
    Quantity,
    Discount=CONVERT(int, Discount * 100), 
    ExtendedPrice=ROUND(CONVERT(money, Quantity * (1 - Discount) * Od.UnitPrice), 2)
  FROM Products P, [Order Details] Od
  WHERE Od.ProductID = P.ProductID and Od.OrderID = @OrderID 
  and Od.Quantity>=@MinQuantity
END

```

This stored procedure, after it has been created, can be invoked as follows:

```sql
exec dbo.MyCustOrdersDetail 10248

```

which will return all order details with @OrderId=10248 (and quantity >=0 as default).
Or you can specify the optional parameter

```sql
exec dbo.MyCustOrdersDetail 10248, 10

```

which will return only orders with a minimum quantity of 10 (or more).

