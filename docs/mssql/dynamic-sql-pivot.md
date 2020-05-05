---
metaTitle: "Microsoft SQL Server - Dynamic SQL Pivot"
description: "Basic Dynamic SQL Pivot"
---

# Dynamic SQL Pivot


This topic covers how to do a dynamic pivot in SQL Server.



## Basic Dynamic SQL Pivot


```sql
if object_id('tempdb.dbo.#temp') is not null drop table #temp
create table #temp
(
    dateValue datetime,
    category varchar(3),
    amount decimal(36,2)
)

insert into #temp values ('1/1/2012', 'ABC', 1000.00)
insert into #temp values ('2/1/2012', 'DEF', 500.00)
insert into #temp values ('2/1/2012', 'GHI', 800.00)
insert into #temp values ('2/10/2012', 'DEF', 700.00)
insert into #temp values ('3/1/2012', 'ABC', 1100.00)


DECLARE 
    @cols AS NVARCHAR(MAX),
    @query  AS NVARCHAR(MAX);

SET @cols = STUFF((SELECT distinct ',' + QUOTENAME(c.category) 
            FROM #temp c 
            FOR XML PATH(''), TYPE
            ).value('.', 'NVARCHAR(MAX)') 
        ,1,1,'')

set @query = '
            SELECT 
                dateValue, 
                ' + @cols + ' 
            from 
            (
                select
                     dateValue,
                     amount,
                     category
                from #temp
           ) x
            pivot 
            (
                 sum(amount)
                for category in (' + @cols + ')
            ) p '


exec sp_executeSql @query

```

