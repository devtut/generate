---
metaTitle: "Computed Columns"
description: "A column is computed from an expression, Simple example we normally use in log tables"
---

# Computed Columns



## A column is computed from an expression


A computed column is computed from an expression that can use other columns in the same table. The expression can be a noncomputed column name, constant, function, and any combination of these connected by one or more operators.

Create table with a computed column

```sql
Create table NetProfit
(
    SalaryToEmployee            int,    
    BonusDistributed            int,
    BusinessRunningCost         int,    
    BusinessMaintenanceCost     int,
    BusinessEarnings            int,
    BusinessNetIncome
                As BusinessEarnings - (SalaryToEmployee          + 
                                       BonusDistributed          + 
                                       BusinessRunningCost       +
                                       BusinessMaintenanceCost    )
                                           
)

```

Value is computed and stored in the computed column automatically on inserting other values.

```sql
Insert Into NetProfit
    (SalaryToEmployee,
     BonusDistributed,
     BusinessRunningCost,
     BusinessMaintenanceCost,
     BusinessEarnings)
Values        
    (1000000,
     10000,
     1000000,
     50000,
     2500000)    

```



## Simple example we normally use in log tables


```sql
CREATE TABLE [dbo].[ProcessLog](
[LogId] [int] IDENTITY(1,1) NOT NULL,
[LogType] [varchar](20) NULL,
[StartTime] [datetime] NULL,
[EndTime] [datetime] NULL,
[RunMinutes]  AS (datediff(minute,coalesce([StartTime],getdate()),coalesce([EndTime],getdate())))

```

This gives run difference in minutes for runtime which will be very handy..

