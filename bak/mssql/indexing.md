---
metaTitle: "Microsoft SQL Server - Indexing"
description: "Create Clustered index, Create Non-Clustered index, Show index info, Drop index, Index on view, Returns size and fragmentation indexes , Reorganize and rebuild index, Rebuild or reorganize all indexes on a table, Rebuild all index database, Index investigations"
---

# Indexing



## Create Clustered index


With a clustered index the leaf pages contain the actual table rows. Therefore, there can be only one clustered index.

```sql
CREATE TABLE Employees
(
    ID CHAR(900),
    FirstName NVARCHAR(3000),
    LastName NVARCHAR(3000),
    StartYear CHAR(900)
)
GO

CREATE CLUSTERED INDEX IX_Clustered 
ON Employees(ID)
GO

```



## Create Non-Clustered index


Non-clustered indexes have a structure separate from the data rows. A non-clustered index contains the non-clustered index key values and each key value entry has a pointer to the data row that contains the key value. There can be maximum 999 non-clustered index on SQL Server 2008/ 2012.

Link for reference: [https://msdn.microsoft.com/en-us/library/ms143432.aspx](https://msdn.microsoft.com/en-us/library/ms143432.aspx)

```sql
CREATE TABLE Employees
(
    ID CHAR(900),
    FirstName NVARCHAR(3000),
    LastName NVARCHAR(3000),
    StartYear CHAR(900)
)
GO

CREATE NONCLUSTERED INDEX IX_NonClustered
ON Employees(StartYear)
GO

```



## Show index info


```sql
SP_HELPINDEX tableName

```



## Drop index


```sql
DROP INDEX IX_NonClustered ON Employees

```



## Index on view


```sql
CREATE VIEW  View_Index02
WITH SCHEMABINDING
AS 
SELECT c.CompanyName, o.OrderDate, o.OrderID, od.ProductID 
     FROM dbo.Customers C 
        INNER JOIN dbo.orders O ON c.CustomerID=o.CustomerID  
            INNER JOIN dbo.[Order Details] od ON o.OrderID=od.OrderID   
GO

CREATE UNIQUE CLUSTERED INDEX IX1 ON 
    View_Index02(OrderID, ProductID) 

```



## Returns size and fragmentation indexes 


```sql
sys.dm_db_index_physical_stats (   
    { database_id | NULL | 0 | DEFAULT }  
  , { object_id | NULL | 0 | DEFAULT }  
  , { index_id | NULL | 0 | -1 | DEFAULT }  
  , { partition_number | NULL | 0 | DEFAULT }  
  , { mode | NULL | DEFAULT }  
)  


Sample :

SELECT * FROM sys.dm_db_index_physical_stats  
    (DB_ID(N'DBName'), OBJECT_ID(N'IX_NonClustered '), NULL, NULL , 'DETAILED');  

```



## Reorganize and rebuild index


|avg_fragmentation_in_percent value|Corrective statement
|---|---|---|---
|**>5% and < = 30%**|REORGANIZE
|**>30%**|REBUILD

```sql
ALTER INDEX IX_NonClustered ON tableName REORGANIZE;  


ALTER INDEX ALL ON Production.Product
 REBUILD WITH (FILLFACTOR = 80, SORT_IN_TEMPDB = ON,
          STATISTICS_NORECOMPUTE = ON);

```



## Rebuild or reorganize all indexes on a table


Rebuilding indexes is done using the following statement

```sql
ALTER INDEX All ON tableName REBUILD;

```

This drops the index and recreates it, removing fragementation, reclaims disk space and reorders index pages.

One can also reorganize an index using

```sql
ALTER INDEX All ON tableName REORGANIZE;

```

which will use minimal system resources and defragments the leaf level of clustered and nonclustered indexes on tables and views by physically reordering the leaf-level pages to match the logical, left to right, order of the leaf nodes



## Rebuild all index database


```sql
EXEC sp_MSForEachTable 'ALTER INDEX ALL ON ? REBUILD'

```



## Index investigations


You could use "SP_HELPINDEX Table_Name", but Kimberly Tripp has a stored procedure (that can be found [here](http://www.sqlskills.com/blogs/kimberly/use-this-new-sql-server-2012-rewrite-for-sp_helpindex/)), which is better example, as it shows more about the indexes, including columns and filter definition, for example:<br>
Usage:

```sql
USE Adventureworks 
EXEC sp_SQLskills_SQL2012_helpindex 'dbo.Product'

```

Alternatively, Tibor Karaszi has a stored procedure (found [here](http://www.karaszi.com/SQLServer/util_sp_indexinfo.asp)). The later will show information on index usage too, and optionally provide a list of index suggestions.
Usage:

```sql
USE Adventureworks 
EXEC sp_indexinfo 'dbo.Product' 

```

