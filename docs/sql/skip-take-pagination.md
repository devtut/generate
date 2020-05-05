---
metaTitle: "SQL - SKIP TAKE (Pagination)"
description: "Limiting amount of results, Skipping then taking some results (Pagination), Skipping some rows from result"
---

# SKIP TAKE (Pagination)



## Limiting amount of results


ISO/ANSI SQL:

```sql
SELECT * FROM TableName FETCH FIRST 20 ROWS ONLY;

```

MySQL; PostgreSQL; SQLite:

```sql
SELECT * FROM TableName LIMIT 20; 

```

Oracle:

```sql
SELECT Id,
   Col1
FROM (SELECT Id,
           Col1,
           row_number() over (order by Id) RowNumber
      FROM TableName)
WHERE RowNumber <= 20

```

SQL Server:

```sql
SELECT TOP 20 * 
FROM dbo.[Sale]

```



## Skipping then taking some results (Pagination)


ISO/ANSI SQL:

```sql
SELECT Id, Col1
FROM TableName
ORDER BY Id
OFFSET 20 ROWS FETCH NEXT 20 ROWS ONLY;

```

MySQL:

```sql
SELECT * FROM TableName LIMIT 20, 20; -- offset, limit

```

Oracle; SQL Server:

```sql
SELECT Id,
   Col1
 FROM (SELECT Id,
           Col1,
           row_number() over (order by Id) RowNumber
      FROM TableName)
WHERE RowNumber BETWEEN 21 AND 40

```

PostgreSQL; SQLite:

```sql
SELECT * FROM TableName LIMIT 20 OFFSET 20;

```



## Skipping some rows from result


ISO/ANSI SQL:

```sql
SELECT Id, Col1
FROM TableName
ORDER BY Id
OFFSET 20 ROWS

```

MySQL:

```sql
SELECT * FROM TableName LIMIT 20, 42424242424242;
-- skips 20 for take use very large number that is more than rows in table

```

Oracle:

```sql
SELECT Id,
   Col1
FROM (SELECT Id,
           Col1,
           row_number() over (order by Id) RowNumber
      FROM TableName)
WHERE RowNumber > 20

```

PostgreSQL:

```sql
SELECT * FROM TableName OFFSET 20;

```

SQLite:

```sql
SELECT * FROM TableName LIMIT -1 OFFSET 20;

```

