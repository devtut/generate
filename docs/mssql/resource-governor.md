---
metaTitle: "Resource Governor"
description: "Reading the Statistics, Create a pool for adhoc queries"
---

# Resource Governor



## Reading the Statistics


```sql
select *
from sys.dm_resource_governor_workload_groups

select *
from sys.dm_resource_governor_resource_pools

```



## Create a pool for adhoc queries


First create a resource pool besides the default one

```sql
CREATE RESOURCE POOL [PoolAdhoc] WITH(min_cpu_percent=0, 
        max_cpu_percent=50, 
        min_memory_percent=0, 
        max_memory_percent=50)
GO

```

Create the worload group for the pool

```sql
CREATE WORKLOAD GROUP [AdhocMedium] WITH(importance=Medium) USING [PoolAdhoc]

```

Create the function that contains the logic for the resource governor and attach it

```sql
create function [dbo].[ufn_ResourceGovernorClassifier]()
  returns sysname with schemabinding
as
begin
    return CASE
                WHEN APP_NAME() LIKE 'Microsoft Office%'                        THEN 'AdhocMedium'        -- Excel
                WHEN APP_NAME() LIKE 'Microsoft SQL Server Management Studio%'    THEN 'AdhocMedium'        -- Adhoc SQL
                WHEN SUSER_NAME() LIKE 'DOMAIN\username'                    THEN 'AdhocMedium'                -- Ssis
                ELSE 'default'
            END
end

GO

alter resource governor 
with (classifier_function = dbo.ufn_ResourceGovernorClassifier)

GO

alter resource governor reconfigure

GO

```



#### Remarks


Resource Governor in SQL Server is a feature that allows you to manage resource usage by different applications and users. It kicks in realtime by setting CPU and memory limits. It will help preventing that one heavy process will eat up all system resources while for example smaller tasks are awaiting them.

Only available in Enterprise Editions

