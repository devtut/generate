---
metaTitle: "Microsoft SQL Server - Temporal Tables"
description: "CREATE Temporal Tables, FOR SYSTEM_TIME ALL, Creating a Memory-Optimized System-Versioned Temporal Table and cleaning up the SQL Server history table, How do I query temporal data?, Return actual value specified point in time(FOR SYSTEM_TIME AS OF <date_time>), FOR SYSTEM_TIME BETWEEN <start_date_time> AND <end_date_time>, FOR SYSTEM_TIME FROM <start_date_time> TO <end_date_time>, FOR SYSTEM_TIME CONTAINED IN (<start_date_time> , <end_date_time>)"
---

# Temporal Tables



## CREATE Temporal Tables


```sql
CREATE TABLE dbo.Employee   
(    
  [EmployeeID] int NOT NULL PRIMARY KEY CLUSTERED   
  , [Name] nvarchar(100) NOT NULL  
  , [Position] varchar(100) NOT NULL   
  , [Department] varchar(100) NOT NULL  
  , [Address] nvarchar(1024) NOT NULL  
  , [AnnualSalary] decimal (10,2) NOT NULL  
  , [ValidFrom] datetime2 (2) GENERATED ALWAYS AS ROW START  
  , [ValidTo] datetime2 (2) GENERATED ALWAYS AS ROW END  
  , PERIOD FOR SYSTEM_TIME (ValidFrom, ValidTo)  
 )    
 WITH (SYSTEM_VERSIONING = ON (HISTORY_TABLE = dbo.EmployeeHistory));  

```

**INSERTS:** On an **INSERT**, the system sets the value for the **ValidFrom** column to the begin time of the current transaction (in the UTC time zone) based on the system clock and assigns the value for the **ValidTo** column to the maximum value of 9999-12-31. This marks the row as open.

**UPDATES:** On an **UPDATE**, the system stores the previous value of the row in the history table and sets the value for the **ValidTo** column to the begin time of the current transaction (in the UTC time zone) based on the system clock. This marks the row as closed, with a period recorded for which the row was valid. In the current table, the row is updated with its new value and the system sets the value for the **ValidFrom** column to the begin time for the transaction (in the UTC time zone) based on the system clock. The value for the updated row in the current table for the **ValidTo** column remains the maximum value of 9999-12-31.

**DELETES**: On a **DELETE**, the system stores the previous value of the row in the history table and sets the value for the **ValidTo** column to the begin time of the current transaction (in the UTC time zone) based on the system clock. This marks the row as closed, with a period recorded for which the previous row was valid. In the current table, the row is removed. Queries of the current table will not return this row. Only queries that deal with history data return data for which a row is closed.

**MERGE**: On a **MERGE**, the operation behaves exactly as if up to three statements (an **INSERT**, an **UPDATE**, and/or a **DELETE**) executed, depending on what is specified as actions in the **MERGE** statement.

**Tip :** The times recorded in the system datetime2 columns are based on the begin time of the transaction itself. For example, all rows inserted within a single transaction will have the same UTC time recorded in the column corresponding to the start of the **SYSTEM_TIME** period.



## FOR SYSTEM_TIME ALL


Returns the union of rows that belong to the current and the history table.

```sql
SELECT * FROM Employee
    FOR SYSTEM_TIME ALL

```



## Creating a Memory-Optimized System-Versioned Temporal Table and cleaning up the SQL Server history table


Creating a temporal table with a default history table is a convenient option when you want to control naming and still rely on system to create history table with default configuration. In the example below, a new system-versioned memory-optimized temporal table linked to a new disk-based history table.

```sql
CREATE SCHEMA History  
GO  
CREATE TABLE dbo.Department   
(  
    DepartmentNumber char(10) NOT NULL PRIMARY KEY NONCLUSTERED,   
    DepartmentName varchar(50) NOT NULL,   
    ManagerID int  NULL,   
    ParentDepartmentNumber char(10) NULL,   
    SysStartTime datetime2 GENERATED ALWAYS AS ROW START HIDDEN NOT NULL,   
    SysEndTime datetime2 GENERATED ALWAYS AS ROW END HIDDEN NOT NULL,     
    PERIOD FOR SYSTEM_TIME (SysStartTime,SysEndTime)     
)  
WITH   
    (  
        MEMORY_OPTIMIZED = ON, DURABILITY = SCHEMA_AND_DATA,  
        SYSTEM_VERSIONING = ON ( HISTORY_TABLE = History.DepartmentHistory )   
    );  

```

**Cleaning up the SQL Server history table**
Over time the history table can grow significantly. Since inserting, updating or deleting data from the history table are not allowed, the only way to clean up the history table is first to disable system versioning:

```sql
ALTER TABLE dbo.Employee

```

SET (SYSTEM_VERSIONING = OFF);
GO

Delete unnecessary data from the history table:

```

   DELETE FROM dbo.EmployeeHistory

```

WHERE EndTime <= '2017-01-26 14:00:29';

and then re-enable system versioning:

```sql
ALTER TABLE dbo.Employee

```

SET (SYSTEM_VERSIONING = ON (HISTORY_TABLE = [dbo].[EmployeeHistory], DATA_CONSISTENCY_CHECK = ON));

Cleaning the history table in Azure SQL Databases is a little different, since Azure SQL databases have built-in support for cleaning of the history table. First, temporal history retention cleanup need to be enable on a database level:

```sql
ALTER DATABASE CURRENT

```

SET TEMPORAL_HISTORY_RETENTION ON
GO

Then set the retention period per table:

```sql
ALTER TABLE dbo.Employee

```

SET (SYSTEM_VERSIONING = ON (HISTORY_RETENTION_PERIOD = 90 DAYS));

This will delete all data in the history table older than 90 days.
SQL Server 2016 on-premise databases do not support TEMPORAL_HISTORY_RETENTION and HISTORY_RETENTION_PERIOD and either of the above two queries are executed on the SQL Server 2016 on-premise databases the following errors will occur.

For TEMPORAL_HISTORY_RETENTION error will be:

```sql
Msg 102, Level 15, State 6, Line 34

```

Incorrect syntax near ‘TEMPORAL_HISTORY_RETENTION’.

For HISTORY_RETENTION_PERIOD error will be:

```sql
Msg 102, Level 15, State 1, Line 39

```

Incorrect syntax near ‘HISTORY_RETENTION_PERIOD’.



## How do I query temporal data?


```sql
SELECT * FROM Employee   
    FOR SYSTEM_TIME    
        BETWEEN '2014-01-01 00:00:00.0000000' AND '2015-01-01 00:00:00.0000000'   
            WHERE EmployeeID = 1000 ORDER BY ValidFrom;  

```



## Return actual value specified point in time(FOR SYSTEM_TIME AS OF <date_time>)


Returns a table with a rows containing the values that were actual (current) at the specified point in time in the past.

```sql
SELECT * FROM Employee   
    FOR SYSTEM_TIME AS  OF '2016-08-06 08:32:37.91'

```



## FOR SYSTEM_TIME BETWEEN <start_date_time> AND <end_date_time>


Same as above in the FOR SYSTEM_TIME FROM <start_date_time>TO <end_date_time> description, except the table of rows returned includes rows that became active on the upper boundary defined by the <end_date_time> endpoint.

```sql
SELECT * FROM Employee   
    FOR SYSTEM_TIME BETWEEN  '2015-01-01' AND '2015-12-31' 

```



## FOR SYSTEM_TIME FROM <start_date_time> TO <end_date_time>


Returns a table with the values for all row versions that were active within the specified time range, regardless of whether they started being active before the <start_date_time> parameter value for the FROM argument or ceased being active after the <end_date_time> parameter value for the TO argument. Internally, a union is performed between the temporal table and its history table and the results are filtered to return the values for all row versions that were active at any time during the time range specified. Rows that became active exactly on the lower boundary defined by the FROM endpoint are included and records that became active exactly on the upper boundary defined by the TO endpoint are not included.

```sql
SELECT * FROM Employee   
    FOR SYSTEM_TIME FROM '2015-01-01' TO '2015-12-31' 

```



## FOR SYSTEM_TIME CONTAINED IN (<start_date_time> , <end_date_time>)


Returns a table with the values for all row versions that were opened and closed within the specified time range defined by the two datetime values for the CONTAINED IN argument. Rows that became active exactly on the lower boundary or ceased being active exactly on the upper boundary are included.

```sql
SELECT * FROM Employee
    FOR SYSTEM_TIME CONTAINED IN ('2015-04-01', '2015-09-25')   

```



#### Remarks


SQL Server 2016 introduces support for system-versioned temporal tables as a database feature that brings built-in support for providing information about data stored in the table at any point in time rather than only the data that is correct at the current moment in time.

A system-versioned temporal table is a new type of user table in SQL Server 2016, designed to keep a full history of data changes and allow easy point in time analysis. This type of temporal table is referred to as a system-versioned temporal table because the period of validity for each row is managed by the system (i.e. database engine).
Every temporal table has two explicitly defined columns, each with a datetime2 data type. These columns are referred to as period columns. These period columns are used exclusively by the system to record period of validity for each row whenever a row is modified.

