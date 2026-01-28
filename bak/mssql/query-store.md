---
metaTitle: "Microsoft SQL Server - Query Store"
description: "Enable query store on database, Get execution statistics for SQL queries/plans, Remove data from query store, Forcing plan for query"
---

# Query Store



## Enable query store on database


Query store can be enabled on database by using the following command:

```sql
ALTER DATABASE tpch SET QUERY_STORE = ON

```

SQL Server/Azure SQL Database will collect information about executed queries and provide information in sys.query_store views:

- sys.query_store_query
- sys.query_store_query_text
- sys.query_store_plan
- sys.query_store_runtime_stats
- sys.query_store_runtime_stats_interval
- sys.database_query_store_options
- sys.query_context_settings



## Get execution statistics for SQL queries/plans


The following query will return informationa about qeries, their plans and average statistics regarding their duration, CPU time, physical and logical io reads.

```sql
SELECT Txt.query_text_id, Txt.query_sql_text, Pl.plan_id,
        avg_duration, avg_cpu_time, 
        avg_physical_io_reads, avg_logical_io_reads
FROM sys.query_store_plan AS Pl  
JOIN sys.query_store_query AS Qry  
    ON Pl.query_id = Qry.query_id  
JOIN sys.query_store_query_text AS Txt  
    ON Qry.query_text_id = Txt.query_text_id
JOIN sys.query_store_runtime_stats Stats
    ON Pl.plan_id = Stats.plan_id

```



## Remove data from query store


If you want to remove some query or query plan from query store, you can use the following commands:

```sql
EXEC sp_query_store_remove_query 4;
EXEC sp_query_store_remove_plan 3; 

```

Parameters for these stored procedures are query/plan id retrieved from system views.

You can also just remove execution statistics for particular plan without removing the plan from the store:

```sql
EXEC sp_query_store_reset_exec_stats 3;  

```

Parameter provided to this procedure plan id.



## Forcing plan for query


SQL Query optimizer will choose the baes possible plan that he can find for some query. If you can find some plan that works optimally for some query, you can force QO to always use that plan using the following stored procedure:

```sql
EXEC sp_query_store_unforce_plan @query_id, @plan_id

```

From this point, QO will always use plan provided for the query.

If you want to remove this binding, you can use the following stored procedure:

```sql
EXEC sp_query_store_force_plan @query_id, @plan_id

```

From this point, QO will again try to find the best plan.

