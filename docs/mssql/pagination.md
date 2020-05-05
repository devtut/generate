---
metaTitle: "Microsoft SQL Server - Pagination"
description: "Pagination using ROW_NUMBER with a Common Table Expression, Pagination with OFFSET FETCH, Paginaton with inner query, Paging in Various Versions of SQL Server, SQL Server 2012/2014 using ORDER BY OFFSET and FETCH NEXT"
---

# Pagination




## Pagination using ROW_NUMBER with a Common Table Expression


The `ROW_NUMBER` function can assign an incrementing number to each row in a result set. Combined with a [Common Table Expression](http://stackoverflow.com/documentation/sql-server/1343/common-table-expressions#t=201609211728444423102) that uses a `BETWEEN` operator, it is possible to create 'pages' of result sets. For example: page one containing results 1-10, page two containing results 11-20, page three containing results 21-30, and so on.

```sql
WITH data
AS
(
    SELECT ROW_NUMBER() OVER (ORDER BY name) AS row_id,
        object_id,
        name, 
        type,
        create_date
    FROM sys.objects
)
SELECT *
FROM data
WHERE row_id BETWEEN 41 AND 50

```

Note: It is not possible to use `ROW_NUMBER` in a `WHERE` clause like:

```sql
SELECT object_id,
    name,
    type,
    create_date
FROM sys.objects
WHERE ROW_NUMBER() OVER (ORDER BY name) BETWEEN 41 AND 50

```

Although this would be more convenient, SQL server will return the following error in this case:

> 
Msg 4108, Level 15, State 1, Line 6
Windowed functions can only appear in the SELECT or ORDER BY clauses.




## Pagination with OFFSET FETCH


The `OFFSET FETCH` clause implements pagination in a more concise manner. With it, it's possible to skip N1 rows (specified in `OFFSET`) and return the next N2 rows (specified in `FETCH`):

```sql
SELECT *
FROM sys.objects
ORDER BY object_id
OFFSET 40 ROWS FETCH NEXT 10 ROWS ONLY

```

The `ORDER BY` clause is required in order to provide deterministic results.



## Paginaton with inner query


In earlier versions of SQL Server, developers had to use double sorting combined with the `TOP` keyword to return rows in a page:

```

SELECT TOP 10 *
 FROM
 (
    SELECT
    TOP 50 object_id,
        name,
        type,
        create_date
    FROM sys.objects
    ORDER BY name ASC
) AS data
ORDER BY name DESC

```

The inner query will return the first 50 rows ordered by `name`. Then the outer query will reverse the order of these 50 rows and select the top 10 rows (these will be last 10 rows in the group before the reversal).



## Paging in Various Versions of SQL Server


### **SQL Server 2012 / 2014**

```sql
DECLARE @RowsPerPage INT = 10, @PageNumber INT = 4
SELECT OrderId, ProductId
FROM OrderDetail
ORDER BY OrderId
OFFSET (@PageNumber - 1) * @RowsPerPage ROWS
FETCH NEXT @RowsPerPage ROWS ONLY

```

### **SQL Server 2005/2008/R2**

```sql
DECLARE @RowsPerPage INT = 10, @PageNumber INT = 4
SELECT OrderId, ProductId
FROM (
    SELECT OrderId, ProductId, ROW_NUMBER() OVER (ORDER BY OrderId) AS RowNum
    FROM OrderDetail) AS OD
WHERE OD.RowNum BETWEEN ((@PageNumber - 1 ) * @RowsPerPage) + 1
AND @RowsPerPage * @PageNumber

```

### **SQL Server 2000**

```sql
DECLARE @RowsPerPage INT = 10, @PageNumber INT = 4
SELECT OrderId, ProductId
FROM (SELECT TOP (@RowsPerPage) OrderId, ProductId
        FROM (SELECT TOP ((@PageNumber)*@RowsPerPage) OrderId, ProductId
                FROM OrderDetail
                ORDER BY OrderId) AS OD
    ORDER BY OrderId DESC) AS OD2
ORDER BY OrderId ASC

```



## SQL Server 2012/2014 using ORDER BY OFFSET and FETCH NEXT


For getting the next 10 rows just run this query:

```sql
SELECT * FROM TableName ORDER BY id OFFSET 10 ROWS FETCH NEXT 10 ROWS ONLY;

```

**Key points to consider when using it:**

- `ORDER BY` is mandatory to use `OFFSET` and `FETCH` clause.
<li>`OFFSET` clause is mandatory with `FETCH`. You can never use, `ORDER BY` …
`FETCH`.</li>
<li>`TOP` cannot be combined with `OFFSET` and `FETCH` in the same query
expression.</li>



#### Syntax


- SELECT * FROM TableName ORDER BY id OFFSET 10 ROWS FETCH NEXT 10 ROWS ONLY;

