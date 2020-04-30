---
metaTitle: "Retrieve information about the database"
description: "Retrieve a List of all Stored Procedures, Get the list of all databases on a server, Count the Number of Tables in a Database, Database Files, Determine a Windows Login's Permission Path, See if Enterprise-specific features are being used, Retrieve Database Options, Show Size of All Tables in Current Database , Retrieve Tables Containing Known Column, Search and Return All Tables and Columns Containing a Specified Column Value, Get all schemas, tables, columns and indexes, Return a list of SQL Agent jobs, with schedule information, Find every mention of a field in the database, Retrieve information on backup and restore operations"
---

# Retrieve information about the database



## Retrieve a List of all Stored Procedures


The following queries will return a list of all Stored Procedures in the database, with basic information about each Stored Procedure:

```sql
SELECT *
FROM INFORMATION_SCHEMA.ROUTINES
WHERE ROUTINE_TYPE = 'PROCEDURE'

```

The `ROUTINE_NAME`, `ROUTINE_SCHEMA` and `ROUTINE_DEFINITION` columns are generally the most useful.

```sql
SELECT *
FROM sys.objects
WHERE type = 'P'

```

```sql
SELECT *
FROM sys.procedures

```

Note that this version has an advantage over selecting from sys.objects since it includes the additional columns `is_auto_executed`, `is_execution_replicated`, `is_repl_serializable`, and `skips_repl_constraints`.

```sql
SELECT *
FROM sysobjects
WHERE type = 'P'

```

Note that the output contains many columns that will never relate to a stored procedure.

The next set of queries will return all Stored Procedures in the database that include the string 'SearchTerm':

```sql
SELECT o.name
FROM syscomments c
INNER JOIN sysobjects o
    ON c.id=o.id
WHERE o.xtype = 'P'
    AND c.TEXT LIKE '%SearchTerm%'

```

```sql
SELECT p.name
FROM sys.sql_modules AS m
INNER JOIN sys.procedures AS p
    ON m.object_id = p.object_id
WHERE definition LIKE '%SearchTerm%'

```



## Get the list of all databases on a server


**Method 1:** Below query will be applicable for SQL Server 2000+ version (Contains 12 columns)

```sql
SELECT * FROM dbo.sysdatabases

```

**Method 2:** Below query extract information about databases with more informations (ex: State, Isolation, recovery model etc.)

Note: This is a catalog view and will be available SQL SERVER 2005+ versions

```sql
SELECT * FROM sys.databases

```

**Method 3:** To see just database names you can use undocumented sp_MSForEachDB

```sql
EXEC sp_MSForEachDB 'SELECT ''?'' AS DatabaseName'

```

**Method 4:** Below SP will help you to provide database size along with databases name , owner, status etc. on the server

```sql
EXEC sp_helpdb

```

**Method 5** Similarly, below stored procedure will give database name, database size and Remarks

```sql
EXEC sp_databases

```



## Count the Number of Tables in a Database


This query will return the number of tables in the specified database.

```sql
USE YourDatabaseName
SELECT COUNT(*) from INFORMATION_SCHEMA.TABLES
WHERE TABLE_TYPE = 'BASE TABLE' 

```

Following is another way this can be done for all user tables with SQL Server 2008+. The reference is [here](https://msdn.microsoft.com/en-us/library/ms187406.aspx?f=255&MSPPError=-2147217396).

```sql
SELECT COUNT(*) FROM sys.tables

```



## Database Files


Display all data files for all databases with size and growth info

```sql
SELECT  d.name AS 'Database',
        d.database_id,  
        SF.fileid,
        SF.name AS 'LogicalFileName',   
        CASE SF.status & 0x100000  
            WHEN 1048576 THEN 'Percentage' 
            WHEN 0 THEN 'MB' 
        END AS 'FileGrowthOption',
        Growth AS GrowthUnit,
        ROUND(((CAST(Size AS FLOAT)*8)/1024)/1024,2) [SizeGB], -- Convert 8k pages to GB
        Maxsize,        
        filename AS PhysicalFileName

FROM    Master.SYS.SYSALTFILES SF
Join    Master.SYS.Databases d on sf.fileid = d.database_id

Order by d.name

```



## Determine a Windows Login's Permission Path


This will show the user type and permission path (which windows group the user is getting its permissions from).

```sql
xp_logininfo 'DOMAIN\user'

```



## See if Enterprise-specific features are being used


It is sometimes useful to verify that your work on Developer edition hasn't introduced a dependency on any features restricted to Enterprise edition.

You can do this using the `sys.dm_db_persisted_sku_features` system view, like so:

```sql
SELECT * FROM sys.dm_db_persisted_sku_features

```

Against the database itself.

This will list the features being used, if any.



## Retrieve Database Options


The following query returns the database options and metadata:

```sql
select * from sys.databases WHERE name = 'MyDatabaseName';

```



## Show Size of All Tables in Current Database 


```sql
SELECT
    s.name + '.' + t.NAME AS TableName,
    SUM(a.used_pages)*8 AS 'TableSizeKB'  --a page in SQL Server is 8kb
FROM sys.tables t
    JOIN sys.schemas s on t.schema_id = s.schema_id
    LEFT JOIN sys.indexes i ON t.OBJECT_ID = i.object_id
    LEFT JOIN sys.partitions p ON i.object_id = p.OBJECT_ID AND i.index_id = p.index_id
    LEFT JOIN sys.allocation_units a ON p.partition_id = a.container_id
GROUP BY
    s.name, t.name
ORDER BY
    --Either sort by name:
    s.name + '.' + t.NAME
    --Or sort largest to smallest:
    --SUM(a.used_pages) desc

```



## Retrieve Tables Containing Known Column


This query will return all `COLUMNS` and their associated `TABLES` for a given column name. It is designed to show you what tables (unknown) contain a specified column (known)

```sql
SELECT 
    c.name AS ColName, 
    t.name AS TableName
FROM 
    sys.columns c
    JOIN sys.tables t ON c.object_id = t.object_id
WHERE 
    c.name LIKE '%MyName%'

```



## Search and Return All Tables and Columns Containing a Specified Column Value


This script, from [here](http://stackoverflow.com/questions/1796506/search-all-tables-all-columns-for-a-specific-value-sql-server) and [here](http://thesitedoctor.co.uk/blog/search-every-table-and-field-in-a-sql-server-database-updated), will return all Tables and Columns where a specified value exists. This is powerful in finding out where a certain value is in a database. It can be taxing, so it is suggested that it be executed in a backup / test enviroment first.

```sql
DECLARE @SearchStr nvarchar(100)
SET @SearchStr = '## YOUR STRING HERE ##'
 
 
    -- Copyright © 2002 Narayana Vyas Kondreddi. All rights reserved.
    -- Purpose: To search all columns of all tables for a given search string
    -- Written by: Narayana Vyas Kondreddi
    -- Site: http://vyaskn.tripod.com
    -- Updated and tested by Tim Gaunt
    -- http://www.thesitedoctor.co.uk
    -- http://blogs.thesitedoctor.co.uk/tim/2010/02/19/Search+Every+Table+And+Field+In+A+SQL+Server+Database+Updated.aspx
    -- Tested on: SQL Server 7.0, SQL Server 2000, SQL Server 2005 and SQL Server 2010
    -- Date modified: 03rd March 2011 19:00 GMT
    CREATE TABLE #Results (ColumnName nvarchar(370), ColumnValue nvarchar(3630))
 
    SET NOCOUNT ON
 
    DECLARE @TableName nvarchar(256), @ColumnName nvarchar(128), @SearchStr2 nvarchar(110)
    SET  @TableName = ''
    SET @SearchStr2 = QUOTENAME('%' + @SearchStr + '%','''')
 
    WHILE @TableName IS NOT NULL
     
    BEGIN
        SET @ColumnName = ''
        SET @TableName =
        (
            SELECT MIN(QUOTENAME(TABLE_SCHEMA) + '.' + QUOTENAME(TABLE_NAME))
            FROM     INFORMATION_SCHEMA.TABLES
            WHERE         TABLE_TYPE = 'BASE TABLE'
                AND    QUOTENAME(TABLE_SCHEMA) + '.' + QUOTENAME(TABLE_NAME) > @TableName
                AND    OBJECTPROPERTY(
                        OBJECT_ID(
                            QUOTENAME(TABLE_SCHEMA) + '.' + QUOTENAME(TABLE_NAME)
                             ), 'IsMSShipped'
                               ) = 0
        )
 
        WHILE (@TableName IS NOT NULL) AND (@ColumnName IS NOT NULL)
             
        BEGIN
            SET @ColumnName =
            (
                SELECT MIN(QUOTENAME(COLUMN_NAME))
                FROM     INFORMATION_SCHEMA.COLUMNS
                WHERE         TABLE_SCHEMA    = PARSENAME(@TableName, 2)
                    AND    TABLE_NAME    = PARSENAME(@TableName, 1)
                    AND    DATA_TYPE IN ('char', 'varchar', 'nchar', 'nvarchar', 'int', 'decimal')
                    AND    QUOTENAME(COLUMN_NAME) > @ColumnName
            )
     
            IF @ColumnName IS NOT NULL
             
            BEGIN
                INSERT INTO #Results
                EXEC
                (
                    'SELECT ''' + @TableName + '.' + @ColumnName + ''', LEFT(' + @ColumnName + ', 3630) FROM ' + @TableName + ' (NOLOCK) ' +
                    ' WHERE ' + @ColumnName + ' LIKE ' + @SearchStr2
                )
            END
        END   
    END
 
    SELECT ColumnName, ColumnValue FROM #Results
 
DROP TABLE #Results
- See more at: http://thesitedoctor.co.uk/blog/search-every-table-and-field-in-a-sql-server-database-updated#sthash.bBEqfJVZ.dpuf

```



## Get all schemas, tables, columns and indexes


```sql
SELECT 
    s.name AS [schema],
    t.object_id AS [table_object_id], 
    t.name AS [table_name],
    c.column_id,
    c.name AS [column_name],
    i.name AS [index_name],
    i.type_desc AS [index_type]
FROM sys.schemas AS s
INNER JOIN sys.tables AS t 
    ON s.schema_id = t.schema_id
INNER JOIN sys.columns AS c 
    ON t.object_id = c.object_id
LEFT JOIN sys.index_columns AS ic 
    ON c.object_id = ic.object_id and c.column_id = ic.column_id
LEFT JOIN sys.indexes AS i 
    ON ic.object_id = i.object_id and ic.index_id = i.index_id
ORDER BY [schema], [table_name], c.column_id;

```



## Return a list of SQL Agent jobs, with schedule information


```sql
USE msdb
Go


SELECT dbo.sysjobs.Name AS 'Job Name', 
      'Job Enabled' = CASE dbo.sysjobs.Enabled
            WHEN 1 THEN 'Yes'
            WHEN 0 THEN 'No'
      END,
      'Frequency' = CASE dbo.sysschedules.freq_type
            WHEN 1 THEN 'Once'
            WHEN 4 THEN 'Daily'
            WHEN 8 THEN 'Weekly'
            WHEN 16 THEN 'Monthly'
            WHEN 32 THEN 'Monthly relative'
            WHEN 64 THEN 'When SQLServer Agent starts'
      END, 
      'Start Date' = CASE active_start_date
            WHEN 0 THEN null
            ELSE
            substring(convert(varchar(15),active_start_date),1,4) + '/' + 
            substring(convert(varchar(15),active_start_date),5,2) + '/' + 
            substring(convert(varchar(15),active_start_date),7,2)
      END,
      'Start Time' = CASE len(active_start_time)
            WHEN 1 THEN cast('00:00:0' + right(active_start_time,2) as char(8))
            WHEN 2 THEN cast('00:00:' + right(active_start_time,2) as char(8))
            WHEN 3 THEN cast('00:0' 
                        + Left(right(active_start_time,3),1)  
                        +':' + right(active_start_time,2) as char (8))
            WHEN 4 THEN cast('00:' 
                        + Left(right(active_start_time,4),2)  
                        +':' + right(active_start_time,2) as char (8))
            WHEN 5 THEN cast('0' 
                        + Left(right(active_start_time,5),1) 
                        +':' + Left(right(active_start_time,4),2)  
                        +':' + right(active_start_time,2) as char (8))
            WHEN 6 THEN cast(Left(right(active_start_time,6),2) 
                        +':' + Left(right(active_start_time,4),2)  
                        +':' + right(active_start_time,2) as char (8))
      END,

      CASE len(run_duration)
            WHEN 1 THEN cast('00:00:0'
                        + cast(run_duration as char) as char (8))
            WHEN 2 THEN cast('00:00:'
                        + cast(run_duration as char) as char (8))
            WHEN 3 THEN cast('00:0' 
                        + Left(right(run_duration,3),1)  
                        +':' + right(run_duration,2) as char (8))
            WHEN 4 THEN cast('00:' 
                        + Left(right(run_duration,4),2)  
                        +':' + right(run_duration,2) as char (8))
            WHEN 5 THEN cast('0' 
                        + Left(right(run_duration,5),1) 
                        +':' + Left(right(run_duration,4),2)  
                        +':' + right(run_duration,2) as char (8))
            WHEN 6 THEN cast(Left(right(run_duration,6),2) 
                        +':' + Left(right(run_duration,4),2)  
                        +':' + right(run_duration,2) as char (8))
      END as 'Max Duration',
    CASE(dbo.sysschedules.freq_subday_interval)
            WHEN 0 THEN 'Once'
            ELSE cast('Every ' 
                        + right(dbo.sysschedules.freq_subday_interval,2) 
                        + ' '
                        +     CASE(dbo.sysschedules.freq_subday_type)
                                          WHEN 1 THEN 'Once'
                                          WHEN 4 THEN 'Minutes'
                                          WHEN 8 THEN 'Hours'
                                    END as char(16))
    END as 'Subday Frequency'
FROM dbo.sysjobs 
LEFT OUTER JOIN dbo.sysjobschedules 
ON dbo.sysjobs.job_id = dbo.sysjobschedules.job_id
INNER JOIN dbo.sysschedules ON dbo.sysjobschedules.schedule_id = dbo.sysschedules.schedule_id 
LEFT OUTER JOIN (SELECT job_id, max(run_duration) AS run_duration
            FROM dbo.sysjobhistory
            GROUP BY job_id) Q1
ON dbo.sysjobs.job_id = Q1.job_id
WHERE Next_run_time = 0

UNION

SELECT dbo.sysjobs.Name AS 'Job Name', 
      'Job Enabled' = CASE dbo.sysjobs.Enabled
            WHEN 1 THEN 'Yes'
            WHEN 0 THEN 'No'
      END,
      'Frequency' = CASE dbo.sysschedules.freq_type
            WHEN 1 THEN 'Once'
            WHEN 4 THEN 'Daily'
            WHEN 8 THEN 'Weekly'
            WHEN 16 THEN 'Monthly'
            WHEN 32 THEN 'Monthly relative'
            WHEN 64 THEN 'When SQLServer Agent starts'
      END, 
      'Start Date' = CASE next_run_date
            WHEN 0 THEN null
            ELSE
            substring(convert(varchar(15),next_run_date),1,4) + '/' + 
            substring(convert(varchar(15),next_run_date),5,2) + '/' + 
            substring(convert(varchar(15),next_run_date),7,2)
      END,
      'Start Time' = CASE len(next_run_time)
            WHEN 1 THEN cast('00:00:0' + right(next_run_time,2) as char(8))
            WHEN 2 THEN cast('00:00:' + right(next_run_time,2) as char(8))
            WHEN 3 THEN cast('00:0' 
                        + Left(right(next_run_time,3),1)  
                        +':' + right(next_run_time,2) as char (8))
            WHEN 4 THEN cast('00:' 
                        + Left(right(next_run_time,4),2)  
                        +':' + right(next_run_time,2) as char (8))
            WHEN 5 THEN cast('0' + Left(right(next_run_time,5),1) 
                        +':' + Left(right(next_run_time,4),2)  
                        +':' + right(next_run_time,2) as char (8))
            WHEN 6 THEN cast(Left(right(next_run_time,6),2) 
                        +':' + Left(right(next_run_time,4),2)  
                        +':' + right(next_run_time,2) as char (8))
      END,

      CASE len(run_duration)
            WHEN 1 THEN cast('00:00:0'
                        + cast(run_duration as char) as char (8))
            WHEN 2 THEN cast('00:00:'
                        + cast(run_duration as char) as char (8))
            WHEN 3 THEN cast('00:0' 
                        + Left(right(run_duration,3),1)  
                        +':' + right(run_duration,2) as char (8))
            WHEN 4 THEN cast('00:' 
                        + Left(right(run_duration,4),2)  
                        +':' + right(run_duration,2) as char (8))
            WHEN 5 THEN cast('0' 
                        + Left(right(run_duration,5),1) 
                        +':' + Left(right(run_duration,4),2)  
                        +':' + right(run_duration,2) as char (8))
            WHEN 6 THEN cast(Left(right(run_duration,6),2) 
                        +':' + Left(right(run_duration,4),2)  
                        +':' + right(run_duration,2) as char (8))
      END as 'Max Duration',
    CASE(dbo.sysschedules.freq_subday_interval)
            WHEN 0 THEN 'Once'
            ELSE cast('Every ' 
                        + right(dbo.sysschedules.freq_subday_interval,2) 
                        + ' '
                        +     CASE(dbo.sysschedules.freq_subday_type)
                                          WHEN 1 THEN 'Once'
                                          WHEN 4 THEN 'Minutes'
                                          WHEN 8 THEN 'Hours'
                                    END as char(16))
    END as 'Subday Frequency'
FROM dbo.sysjobs 
LEFT OUTER JOIN dbo.sysjobschedules ON dbo.sysjobs.job_id = dbo.sysjobschedules.job_id
INNER JOIN dbo.sysschedules ON dbo.sysjobschedules.schedule_id = dbo.sysschedules.schedule_id 
LEFT OUTER JOIN (SELECT job_id, max(run_duration) AS run_duration
            FROM dbo.sysjobhistory
            GROUP BY job_id) Q1
ON dbo.sysjobs.job_id = Q1.job_id
WHERE Next_run_time <> 0

ORDER BY [Start Date],[Start Time]

```



## Find every mention of a field in the database


```sql
SELECT DISTINCT
 o.name AS Object_Name,o.type_desc
 FROM sys.sql_modules m 
    INNER JOIN sys.objects  o ON m.object_id=o.object_id
 WHERE m.definition Like '%myField%'
 ORDER BY 2,1

```

Will find mentions of `myField` in SProcs, Views, etc.



## Retrieve information on backup and restore operations


To get the list of all backup operations performed on the current database instance:

```sql
SELECT sdb.Name AS DatabaseName,
    COALESCE(CONVERT(VARCHAR(50), bus.backup_finish_date, 120),'-') AS LastBackUpDateTime
FROM sys.sysdatabases sdb
    LEFT OUTER JOIN msdb.dbo.backupset bus ON bus.database_name = sdb.name
ORDER BY sdb.name, bus.backup_finish_date DESC

```

To get the list of all restore operations performed on the current database instance:

```sql
SELECT 
    [d].[name] AS database_name, 
    [r].restore_date AS last_restore_date, 
    [r].[user_name], 
    [bs].[backup_finish_date] AS backup_creation_date, 
    [bmf].[physical_device_name] AS [backup_file_used_for_restore] 
FROM master.sys.databases [d] 
    LEFT OUTER JOIN msdb.dbo.[restorehistory] r ON r.[destination_database_name] = d.Name 
    INNER JOIN msdb.dbo.backupset [bs] ON [r].[backup_set_id] = [bs].[backup_set_id] 
    INNER JOIN msdb.dbo.backupmediafamily bmf ON [bs].[media_set_id] = [bmf].[media_set_id] 
ORDER BY [d].[name], [r].restore_date DESC

```



#### Remarks


As with other relational database systems, SQL Server exposes metadata about your databases.

This is provided through the ISO Standard `INFORMATION_SCHEMA` schema, or the SQL Server-specific `sys` catalog views.

