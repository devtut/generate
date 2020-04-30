---
metaTitle: "In-Memory OLTP (Hekaton)"
description: "Declare Memory-Optimized Table Variables, Create Memory Optimized Table, Show created .dll files and tables for Memory Optimized Tables, Create Memory Optimized System-Versioned Temporal Table, Memory-Optimized Table Types  and Temp tables"
---

# In-Memory OLTP (Hekaton)



## Declare Memory-Optimized Table Variables


For faster performance you can memory-optimize your table variable.
Here is the T-SQL for a traditional table variable:

```sql
DECLARE @tvp TABLE  
( 
    col1   INT NOT NULL ,  
    Col2   CHAR(10) 
);  

```

To define memory-optimized variables, you must first create a memory-optimized table type and then declare a variable from it:

```sql
CREATE TYPE dbo.memTypeTable 
AS TABLE  
(  
    Col1  INT NOT NULL INDEX ix1,  
    Col2  CHAR(10)  
)  
WITH  
    (MEMORY_OPTIMIZED = ON);  

```

Then we can use the table type like this:

```sql
DECLARE @tvp memTypeTable
insert INTO @tvp
values (1,'1'),(2,'2'),(3,'3'),(4,'4'),(5,'5'),(6,'6')

SELECT * FROM @tvp

```

Result:

```sql
Col1    Col2
1       1         
2       2         
3       3         
4       4         
5       5         
6       6        

```



## Create Memory Optimized Table


```sql
-- Create demo database
CREATE DATABASE SQL2016_Demo
 ON  PRIMARY
( 
    NAME = N'SQL2016_Demo', 
    FILENAME = N'C:\Dump\SQL2016_Demo.mdf', 
    SIZE = 5120KB, 
    FILEGROWTH = 1024KB 
 )
 LOG ON 
 ( 
    NAME = N'SQL2016_Demo_log', 
    FILENAME = N'C:\Dump\SQL2016_Demo_log.ldf', 
    SIZE = 1024KB, 
    FILEGROWTH = 10%
 )
GO

use SQL2016_Demo
go

-- Add Filegroup by MEMORY_OPTIMIZED_DATA type 
ALTER DATABASE SQL2016_Demo 
    ADD FILEGROUP MemFG CONTAINS MEMORY_OPTIMIZED_DATA 
GO


--Add a file to defined filegroup
ALTER DATABASE SQL2016_Demo ADD FILE
    ( 
        NAME = MemFG_File1,
        FILENAME = N'C:\Dump\MemFG_File1' -- your file path, check directory exist before executing this code
    ) 
TO FILEGROUP MemFG
GO

--Object Explorer -- check database created
GO

-- create memory optimized table 1
CREATE TABLE dbo.MemOptTable1  
(  
    Column1     INT         NOT NULL,  
    Column2     NVARCHAR(4000)  NULL,  
    SpidFilter  SMALLINT    NOT NULL   DEFAULT (@@spid),  

    INDEX ix_SpidFiler NONCLUSTERED (SpidFilter),  
    INDEX ix_SpidFilter HASH (SpidFilter) WITH (BUCKET_COUNT = 64),  
      
    CONSTRAINT CHK_soSessionC_SpidFilter  
        CHECK ( SpidFilter = @@spid ),  
)  
    WITH  
        (MEMORY_OPTIMIZED = ON,  
         DURABILITY = SCHEMA_AND_DATA);  --or DURABILITY = SCHEMA_ONLY
go  

-- create memory optimized table 2
CREATE TABLE MemOptTable2
(
    ID INT NOT NULL PRIMARY KEY NONCLUSTERED HASH WITH (BUCKET_COUNT = 10000),
    FullName NVARCHAR(200) NOT NULL, 
    DateAdded DATETIME NOT NULL
) WITH (MEMORY_OPTIMIZED = ON, DURABILITY = SCHEMA_AND_DATA)
GO

```



## Show created .dll files and tables for Memory Optimized Tables


```sql
SELECT
    OBJECT_ID('MemOptTable1') AS MemOptTable1_ObjectID,
    OBJECT_ID('MemOptTable2') AS MemOptTable2_ObjectID
GO

SELECT 
    name,description 
FROM sys.dm_os_loaded_modules
WHERE name LIKE '%XTP%'
GO

```

Show all Memory Optimized Tables:

```sql
SELECT 
    name,type_desc,durability_desc,Is_memory_Optimized 
FROM sys.tables
    WHERE Is_memory_Optimized = 1
GO

```



## Create Memory Optimized System-Versioned Temporal Table


```sql
CREATE TABLE [dbo].[MemOptimizedTemporalTable]
(
    [BusinessDocNo] [bigint] NOT NULL,
    [ProductCode] [int] NOT NULL,
    [UnitID] [tinyint] NOT NULL,
    [PriceID] [tinyint] NOT NULL,
    [SysStartTime] [datetime2](7) GENERATED ALWAYS AS ROW START NOT NULL,
    [SysEndTime] [datetime2](7) GENERATED ALWAYS AS ROW END NOT NULL,
    PERIOD FOR SYSTEM_TIME ([SysStartTime], [SysEndTime]),

    CONSTRAINT [PK_MemOptimizedTemporalTable]  PRIMARY KEY NONCLUSTERED 
    (
        [BusinessDocNo] ASC,
        [ProductCode] ASC
    )
)
WITH ( 
    MEMORY_OPTIMIZED = ON , DURABILITY = SCHEMA_AND_DATA, -- Memory Optimized Option ON
    SYSTEM_VERSIONING = ON (HISTORY_TABLE = [dbo].[MemOptimizedTemporalTable_History] , DATA_CONSISTENCY_CHECK = ON ) 
)

```

[more informations](https://msdn.microsoft.com/en-us/library/mt620110.aspx)



## Memory-Optimized Table Types  and Temp tables


For example, this is traditional tempdb-based table type:

```sql
CREATE TYPE dbo.testTableType AS TABLE
(
   col1 INT NOT NULL,
   col2 CHAR(10)
);

```

To memory-optimize this table type simply add the option `memory_optimized=on`, and add an index if there is none on the original type:

```sql
CREATE TYPE dbo.testTableType AS TABLE
(
   col1 INT NOT NULL,
   col2 CHAR(10)
)WITH (MEMORY_OPTIMIZED=ON);

```

Global temporary table is like this:

```sql
CREATE TABLE ##tempGlobalTabel 
(  
    Col1   INT   NOT NULL ,  
    Col2   NVARCHAR(4000)  
);  

```

Memory-optimized global temporary table:

```sql
CREATE TABLE dbo.tempGlobalTabel 
(  
    Col1   INT   NOT NULL   INDEX ix NONCLUSTERED,  
    Col2   NVARCHAR(4000)  
)  
    WITH  
        (MEMORY_OPTIMIZED = ON,  
         DURABILITY = SCHEMA_ONLY);  

```

To memory-optimize global temp tables (##temp):

<li>Create a new `SCHEMA_ONLY` memory-optimized table with the same schema as the global `##temp` table
<ul>
1. Ensure the new table has at least one index
</ul>
</li>
1. Change all references to `##temp` in your Transact-SQL statements to the new memory-optimized table temp
1. Replace the `DROP TABLE ##temp` statements in your code with `DELETE FROM temp`, to clean up the contents
1. Remove the `CREATE TABLE ##temp` statements from your code – these are now redundant

[more informations](https://blogs.msdn.microsoft.com/sqlserverstorageengine/2016/03/21/improving-temp-table-and-table-variable-performance-using-memory-optimization/)

