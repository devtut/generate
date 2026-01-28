---
metaTitle: "Microsoft SQL Server - OVER Clause"
description: "Cumulative Sum, Using Aggregation functions with OVER, Using Aggregation funtions to find the most recent records, Dividing Data into equally-partitioned buckets using NTILE"
---

# OVER Clause



## Cumulative Sum


Using the [Item Sales Table](http://stackoverflow.com/documentation/sql/280/example-database/1016/cars-table#t=201604051713005624555), we will try to find out how the sales of our items are increasing through dates. To do so we will calculate the **Cumulative Sum** of total sales per Item order by the sale date.

```sql
SELECT item_id, sale_Date 
       SUM(quantity * price) OVER(PARTITION BY item_id ORDER BY sale_Date ROWS BETWEEN UNBOUNDED PRECEDING) AS SalesTotal
  FROM SalesTable

```



## Using Aggregation functions with OVER


Using the [Cars Table](http://stackoverflow.com/documentation/sql/280/example-database/1016/cars-table#t=201604051640186575813), we will calculate the total, max, min and average amount of money each costumer spent and haw many times (COUNT) she brought a car for repairing.

Id    CustomerId    MechanicId    Model    Status    Total Cost

```sql
SELECT CustomerId,  
       SUM(TotalCost) OVER(PARTITION BY CustomerId) AS Total,
       AVG(TotalCost) OVER(PARTITION BY CustomerId) AS Avg,
       COUNT(TotalCost) OVER(PARTITION BY CustomerId) AS Count,
       MIN(TotalCost) OVER(PARTITION BY CustomerId) AS Min,
       MAX(TotalCost) OVER(PARTITION BY CustomerId) AS Max
  FROM CarsTable
 WHERE Status = 'READY'

```

Beware that using OVER in this fashion will not aggregate the rows returned. The above query will return the following:

|CustomerId|Total|Avg|Count|Min|Max
|---|---|---|---|---|---|---
|1|430|215|2|200|230
|1|430|215|2|200|230

The duplicated row(s) may not be that useful for reporting purposes.

If you wish to simply aggregate data, you will be better off using the GROUP BY clause along with the appropriate aggregate functions Eg:

```sql
SELECT CustomerId,  
       SUM(TotalCost) AS Total,
       AVG(TotalCost) AS Avg,
       COUNT(TotalCost) AS Count,
       MIN(TotalCost) AS Min,
       MAX(TotalCost)  AS Max
  FROM CarsTable
 WHERE Status = 'READY'
GROUP BY CustomerId

```



## Using Aggregation funtions to find the most recent records


Using the [Library Database](http://stackoverflow.com/documentation/sql/280/example-databases/4978/library-database#t=201607221318512510656), we try to find the last book added to the database for each author. For this simple example we assume an always incrementing Id for each record added.

```sql
SELECT MostRecentBook.Name, MostRecentBook.Title
FROM ( SELECT Authors.Name,
              Books.Title,
              RANK() OVER (PARTITION BY Authors.Id ORDER BY Books.Id DESC) AS NewestRank
       FROM Authors
       JOIN Books ON Books.AuthorId = Authors.Id
     ) MostRecentBook
WHERE MostRecentBook.NewestRank = 1

```

Instead of RANK, two other functions can be used to order. In the previous example the result will be the same, but they give different results when the ordering gives multiple rows for each rank.

- `RANK()`: duplicates get the same rank, the next rank takes the number of duplicates in the previous rank into account
- `DENSE_RANK()`: duplicates get the same rank, the next rank is always one higher than the previous
- `ROW_NUMBER()`: will give each row a unique 'rank', 'ranking' the duplicates randomly

For example, if the table had a non-unique column CreationDate and the ordering was done based on that, the following query:

```sql
SELECT Authors.Name,
       Books.Title,
       Books.CreationDate,
       RANK() OVER (PARTITION BY Authors.Id ORDER BY Books.CreationDate DESC) AS RANK,
       DENSE_RANK() OVER (PARTITION BY Authors.Id ORDER BY Books.CreationDate DESC) AS DENSE_RANK,
       ROW_NUMBER() OVER (PARTITION BY Authors.Id ORDER BY Books.CreationDate DESC) AS ROW_NUMBER,
FROM Authors
JOIN Books ON Books.AuthorId = Authors.Id

```

Could result in:

|Author|Title|CreationDate|RANK|DENSE_RANK|ROW_NUMBER
|---|---|---|---|---|---|---
|Author 1|Book 1|22/07/2016|1|1|1
|Author 1|Book 2|22/07/2016|1|1|2
|Author 1|Book 3|21/07/2016|3|2|3
|Author 1|Book 4|21/07/2016|3|2|4
|Author 1|Book 5|21/07/2016|3|2|5
|Author 1|Book 6|04/07/2016|6|3|6
|Author 2|Book 7|04/07/2016|1|1|1



## Dividing Data into equally-partitioned buckets using NTILE


Let's say that you have exam scores for several exams and you want to divide them into quartiles per exam.

```sql
-- Setup data:
declare @values table(Id int identity(1,1) primary key, [Value] float, ExamId int)
insert into @values ([Value], ExamId) values
(65, 1), (40, 1), (99, 1), (100, 1), (90, 1), -- Exam 1 Scores
(91, 2), (88, 2), (83, 2), (91, 2), (78, 2), (67, 2), (77, 2) -- Exam 2 Scores

-- Separate into four buckets per exam:
select ExamId, 
       ntile(4) over (partition by ExamId order by [Value] desc) as Quartile, 
       Value, Id 
from @values 
order by ExamId, Quartile

```

[<img src="https://i.stack.imgur.com/jJfx2.png" alt="Our exam data divided into quartiles per exam" />](https://i.stack.imgur.com/jJfx2.png)

`ntile` works great when you really need a set number of buckets and each filled to approximately the same level.  Notice that it would be trivial to separate these scores into percentiles by simply using `ntile(100)`.



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---
|PARTITION BY|The field(s) that follows PARTITION BY is the one that the 'grouping' will be based on



#### Remarks


The OVER clause determines a windows or a subset of row within a query result set. A window function can be applied to set and compute a value for each row in the set. The OVER clause can be used with:

- Ranking functions
- Aggregate functions

so someone can compute aggregated values such as moving averages, cumulative aggregates, running totals, or a top N per group results.

In a very abstract way we can say that OVER behaves like GROUP BY. However OVER is applied per field / column and not to the query as whole as GROUP BY does.

**Note#1:** In SQL Server 2008 (R2) ORDER BY Clause cannot be used with aggregate window functions ([link](https://msdn.microsoft.com/en-us/library/ms189461(v=sql.105).aspx)).

