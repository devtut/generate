---
metaTitle: "Microsoft SQL Server - Partitioning"
description: "Retrieve Partition Boundary Values, Switching Partitions, Retrieve partition table,column, scheme, function, total and min-max boundry values using single query"
---

# Partitioning




## Retrieve Partition Boundary Values


```sql
SELECT        ps.name AS PartitionScheme
            , fg.name AS [FileGroup]
            , prv.*            
            , LAG(prv.Value) OVER (PARTITION BY ps.name ORDER BY ps.name, boundary_id) AS PreviousBoundaryValue

FROM        sys.partition_schemes ps
INNER JOIN    sys.destination_data_spaces dds
            ON dds.partition_scheme_id = ps.data_space_id
INNER JOIN    sys.filegroups fg
            ON dds.data_space_id = fg.data_space_id
INNER JOIN    sys.partition_functions f
            ON f.function_id = ps.function_id
INNER JOIN    sys.partition_range_values prv
            ON f.function_id = prv.function_id
            AND dds.destination_id = prv.boundary_id

```



## Switching Partitions


According to this [TechNet Microsoft page][1],

> 
**Partitioning data** enables you to manage and access subsets of your data quickly and efficiently while maintaining the integrity of the entire data collection.


When you call the following query the data is not physically moved; only the metadata about the location of the data changes.

`ALTER TABLE [SourceTable] SWITCH TO [TargetTable]`

The tables must have the same columns with the same data types and NULL settings, they need to be in the same file group and the new target table must be empty. See the page link above for more info on switching partitions.

[1]: [https://technet.microsoft.com/en-us/library/ms191160(v=sql.105).aspx](https://technet.microsoft.com/en-us/library/ms191160(v=sql.105).aspx)  The column `IDENTITY` property may differ.



## Retrieve partition table,column, scheme, function, total and min-max boundry values using single query


```sql
SELECT DISTINCT
    object_name(i.object_id) AS [Object Name],
    c.name AS [Partition Column],
    s.name AS [Partition Scheme],
    pf.name AS [Partition Function],
    prv.tot AS [Partition Count],
    prv.miVal AS [Min Boundry Value],
    prv.maVal AS [Max Boundry Value]
FROM sys.objects o 
INNER JOIN sys.indexes i ON i.object_id = o.object_id
INNER JOIN sys.columns c ON c.object_id = o.object_id
INNER JOIN sys.index_columns ic ON ic.object_id = o.object_id
    AND ic.column_id = c.column_id
    AND ic.partition_ordinal = 1
INNER JOIN sys.partition_schemes s ON i.data_space_id = s.data_space_id
INNER JOIN sys.partition_functions pf ON pf.function_id = s.function_id
OUTER APPLY(SELECT 
                COUNT(*) tot, MIN(value) miVal, MAX(value) maVal 
            FROM sys.partition_range_values prv 
            WHERE prv.function_id = pf.function_id) prv
--WHERE object_name(i.object_id) = 'table_name'
ORDER BY OBJECT_NAME(i.object_id)

```

Just un-comment `where` clause and replace `table_name` with `actual table name` to view the detail of desired object.

