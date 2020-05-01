---
metaTitle: "Limit Result Set"
description: "Limiting With PERCENT, Limiting With TOP, Limiting with FETCH"
---

# Limit Result Set


As database tables grow, it's often useful to limit the results of queries to a fixed number or percentage. This can be achieved using SQL Server's `TOP` keyword or `OFFSET FETCH` clause.



## Limiting With PERCENT


This example limits `SELECT` result to 15 percentage of total row count.

```sql
SELECT TOP 15 PERCENT *
FROM table_name

```



## Limiting With TOP


This example limits `SELECT` result to 100 rows.

```sql
SELECT TOP 100 *
FROM table_name;

```

It is also possible to use a variable to specify the number of rows:

```sql
DECLARE @CountDesiredRows int = 100;
SELECT TOP (@CountDesiredRows) *
FROM table_name;

```



## Limiting with FETCH


`FETCH` is generally more useful for pagination, but can be used as an alternative to `TOP`:

```sql
SELECT *
FROM table_name
ORDER BY 1
OFFSET 0 ROWS
FETCH NEXT 50 ROWS ONLY

```



#### Parameters


|Parameter|Details
|---|---
|`TOP`|Limiting keyword. Use with a number.
|`PERCENT`|Percentage keyword. Comes after `TOP` and limiting number.



#### Remarks


If `ORDER BY` clause is used, limiting applies to the ordered result set.

