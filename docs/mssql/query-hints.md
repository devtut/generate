---
metaTitle: "Query Hints"
description: "JOIN Hints, GROUP BY Hints, FAST rows hint, UNION hints, MAXDOP Option, INDEX Hints"
---

# Query Hints



## JOIN Hints


When you join two tables, SQL Server query optimizer (QO) can choose different types of joins that will be used in query:

- HASH join
- LOOP join
- MERGE join

QO will explore plans and choose the optimal operator for joining tables. However, if you are sure that you know what would be the optimal join operator, you can specify what kind of JOIN should be used.
Inner LOOP join will force QO to choose Nested loop join while joining two tables:

```sql
select top 100 *
from Sales.Orders o 
    inner loop join Sales.OrderLines ol 
    on o.OrderID = ol.OrderID

```

inner merge join will force MERGE join operator:

```sql
select top 100 *
from Sales.Orders o 
    inner merge join Sales.OrderLines ol 
    on o.OrderID = ol.OrderID

```

inner hash join will force HASH join operator:

```sql
select top 100 *
from Sales.Orders o 
    inner hash join Sales.OrderLines ol 
    on o.OrderID = ol.OrderID

```



## GROUP BY Hints


When you use GROUP BY clause, SQL Server query optimizer (QO) can choose different types of grouping operators:

- HASH Aggregate that creates hash-map for grouping entries
- Stream Aggregate that works well with pre-ordered inputs

You can explicitly require that QO picks one or another aggregate operator if you know what would be the optimal.
With OPTION (ORDER GROUP), QO will always choose Stream aggregate and add Sort operator in front of Stream aggregate if input is not sorted:

```sql
select OrderID, AVG(Quantity)
from Sales.OrderLines
group by OrderID
OPTION (ORDER GROUP) 

```

With OPTION (HASH GROUP), QO will always choose Hash aggregate :

```sql
select OrderID, AVG(Quantity)
from Sales.OrderLines
group by OrderID
OPTION (HASH GROUP) 

```



## FAST rows hint


Specifies that the query is optimized for fast retrieval of the first number_rows. This is a nonnegative integer. After the first number_rows are returned, the query continues execution and produces its full result set.

```sql
select OrderID, AVG(Quantity)
from Sales.OrderLines
group by OrderID
OPTION (FAST 20) 

```



## UNION hints


When you use UNION operator on two query results, Query optimizer (QO) can use following operators to create a union of two result sets:

- Merge (Union)
- Concat (Union)
- Hash Match (Union)

You can explicitly specify what operator should be used using OPTION() hint:

```sql
select OrderID, OrderDate, ExpectedDeliveryDate, Comments
from Sales.Orders
where OrderDate > DATEADD(day, -1, getdate())
UNION
select PurchaseOrderID as OrderID, OrderDate, ExpectedDeliveryDate, Comments
from Purchasing.PurchaseOrders
where OrderDate > DATEADD(day, -1, getdate())
OPTION(HASH UNION)
-- or OPTION(CONCAT UNION)
-- or OPTION(MERGE UNION)

```



## MAXDOP Option


Specifies the max degree of parallelism for the query specifying this option.

```sql
SELECT OrderID,
    AVG(Quantity)
FROM Sales.OrderLines
GROUP BY OrderID
OPTION (MAXDOP 2);

```

This option overrides the MAXDOP configuration option of sp_configure and Resource Governor. If MAXDOP is set to zero then the server chooses the max degree of parallelism.



## INDEX Hints


Index hints are used to force a query to use a specific index, instead of allowing SQL Server's Query Optimizer to choose what it deems the best index. In some cases you may gain benefits by specifying the index a query must use. Usually SQL Server's Query Optimizer chooses the best index suited for the query, but due to missing/outdated statistics or specific needs you can force it.

```sql
SELECT *
FROM mytable WITH (INDEX (ix_date))
WHERE field1 > 0
    AND CreationDate > '20170101'

```

