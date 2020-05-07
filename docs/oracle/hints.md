---
metaTitle: "Oracle Database - Hints"
description: "Parallel Hint, USE_NL, APPEND HINT, USE_HASH, FULL, Result Cache"
---

# Hints



## Parallel Hint


Statement-level parallel hints are the easiest:

```sql
SELECT /*+ PARALLEL(8) */ first_name, last_name FROM employee emp;

```

Object-level parallel hints give more control but are more prone to errors; developers often forget to use the alias instead of the object name, or they forget to include some objects.

```sql
SELECT /*+ PARALLEL(emp,8) */ first_name, last_name FROM employee emp;

```

`SELECT /*+ PARALLEL(table_alias,Degree of Parallelism) */ FROM table_name table_alias;`

Let's say a query takes 100 seconds to execute without using parallel hint.
If we change DOP to 2 for same query, then **ideally** the same query with parallel hint will take 50 second. Similarly using DOP as 4 will take 25 seconds.

In practice, parallel execution depends on many other factors and does not scale linearly.  This is especially true for small run times where the parallel overhead may be larger than the gains from running in multiple parallel servers.



## USE_NL


Use Nested Loops.

Usage : `use_nl(A B)`

This hint will ask the engine to use nested loop method to join the tables A and B. That is row by row comparison. The hint does not force the order of the join, just asks for NL.

```sql
SELECT /*+use_nl(e d)*/ *
FROM Employees E
JOIN Departments D on E.DepartmentID = D.ID

```



## APPEND HINT


"Use DIRECT PATH method for inserting new rows".

The `APPEND` hint instructs the engine to use [direct path load](http://docs.oracle.com/cd/E11882_01/server.112/e41084/sql_elements006.htm#SQLRF50901). This means that the engine will not use a conventional insert using memory structures and standard locks, but will write directly to the tablespace the data. Always creates new blocks which are appended to the table's segment. This will be faster, but have some limitations:

- You cannot read from the table you appended in the same session until you commmit or rollback the transaction.
- If there are triggers defined on the table Oracle [will not use direct path](https://asktom.oracle.com/pls/apex/f?p=100:11:0::::P11_QUESTION_ID:1211797200346279484#2096268200346987629)(it's a different story for sqlldr loads).
- others

Example.

```sql
INSERT /*+append*/ INTO Employees
SELECT *
FROM Employees;

```



## USE_HASH


Instructs the engine to use hash method to join tables in the argument.

Usage : `use_hash(TableA [TableB] ... [TableN])`

As [explained](https://jonathanlewis.wordpress.com/2013/09/07/hash-joins/) in [many](https://docs.oracle.com/database/121/TGSQL/glossary.htm#GUID-FF45796B-8A90-45C6-8A40-0B308B72AF7C) [places](http://logicalread.solarwinds.com/oracle-11g-hash-joins-mc02/#.V5Wm4_mnoUI),  "in a HASH join, Oracle accesses one table (usually the smaller of the joined results) and builds a hash table on the join key in memory. It then scans the other table in the join (usually the larger one) and probes the hash table for matches to it."

It is preferred against Nested Loops method when the tables are big, no indexes are at hand, etc.

**Note**: The hint does not force the order of the join, just asks for HASH JOIN method.

Example of usage:

```sql
SELECT /*+use_hash(e d)*/ *
FROM Employees E
JOIN Departments D on E.DepartmentID = D.ID

```



## FULL


The FULL hint tells Oracle to perform a full table scan on a specified table, no matter if an index can be used.

```sql
create table fullTable(id) as select level from dual connect by level < 100000;
create index idx on fullTable(id);

```

With no hints, the index is used:

```sql
select count(1) from fullTable f where id between 10 and 100;
--------------------------------------------------------------------------
| Id  | Operation         | Name | Rows  | Bytes | Cost (%CPU)| Time     |
--------------------------------------------------------------------------
|   0 | SELECT STATEMENT  |      |     1 |    13 |     3   (0)| 00:00:01 |
|   1 |  SORT AGGREGATE   |      |     1 |    13 |            |          |
|*  2 |   INDEX RANGE SCAN| IDX  |     2 |    26 |     3   (0)| 00:00:01 |
--------------------------------------------------------------------------

```

FULL hint forces a full scan:

```sql
select /*+ full(f) */ count(1) from fullTable f where id between 10 and 100;
--------------------------------------------------------------------------------
| Id  | Operation          | Name      | Rows  | Bytes | Cost (%CPU)| Time     |
--------------------------------------------------------------------------------
|   0 | SELECT STATEMENT   |           |     1 |    13 |    47   (3)| 00:00:01 |
|   1 |  SORT AGGREGATE    |           |     1 |    13 |            |          |
|*  2 |   TABLE ACCESS FULL| FULLTABLE |     2 |    26 |    47   (3)| 00:00:01 |
--------------------------------------------------------------------------------

```



## Result Cache


Oracle (**11g and above**) allows the SQL queries to be cached in the [SGA](https://docs.oracle.com/cd/B19306_01/server.102/b14220/memory.htm#i10093) and reused to improve performance. It queries the data from cache rather than database. Subsequent execution of same query is faster because now the data is being pulled from cache.

```sql
SELECT /*+ result_cache */ number FROM main_table;

```

Output -

```sql
Number
------
   1
   2
   3
   4
   5
   6
   7
   8
   9
   10

Elapsed: 00:00:02.20

```

If I run the same query again now, the time to execute will reduce since the data is now fetched from cache which was set during the first execution.

Output -

```sql
Number
------
   1
   2
   3
   4
   5
   6
   7
   8
   9
   10

Elapsed: 00:00:00.10

```

Notice how the elapsed time reduced from **2.20 seconds** to **0.10 seconds**.

> 
Result Cache holds the cache until the data in database is updated/altered/deleted. Any change will release the cache.




#### Parameters


|Parameters|Details
|---|---|---|---|---|---|---|---|---|---
|Degree of Parallelism (DOP)|It is the number of parallel connection/processes which you want your query to open up. It is usually 2, 4, 8, 16 so on.
|Table Name|The name of the table on which parallel hint will be applied.

