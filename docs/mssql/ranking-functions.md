---
metaTitle: "Microsoft SQL Server - Ranking Functions"
description: "DENSE_RANK (), RANK()"
---

# Ranking Functions



## DENSE_RANK ()


Same as that of RANK(). It returns rank without any gaps:

```sql
Select  Studentid, Name,Subject,Marks,
DENSE_RANK() over(partition by name order by Marks desc)Rank
From Exam
order by name

Studentid    Name    Subject    Marks    Rank
101          Ivan    Science    80       1
101          Ivan    Maths      70       2
101          Ivan    Social     60       3
102          Ryan    Social     70       1
102          Ryan    Maths      60       2
102          Ryan    Science    50       3
103          Tanvi   Maths      90       1
103          Tanvi   Science    90       1
103          Tanvi   Social     80       2

```



## RANK()


A RANK() Returns the rank of each row in the result set of partitioned column.

Eg :

```sql
Select Studentid,Name,Subject,Marks,
RANK() over(partition by name order by Marks desc)Rank
From Exam
order by name,subject

   Studentid    Name    Subject    Marks    Rank

    101         Ivan    Maths       70       2
    101         Ivan    Science     80       1
    101         Ivan    Social      60       3
    102         Ryan    Maths       60       2
    102         Ryan    Science     50       3
    102         Ryan    Social      70       1
    103         Tanvi   Maths       90       1
    103         Tanvi   Science     90       1
    103         Tanvi   Social      80       3

```



#### Syntax


- DENSE_RANK ( ) OVER ( [ <partition_by_clause> ] < order_by_clause > )
- RANK ( ) OVER ( [ partition_by_clause ] order_by_clause )



#### Parameters


|Arguments|Details
|---|---|---|---
|`<partition_by_clause>`|Divides the result set produced by the [FROM](https://msdn.microsoft.com/en-us/library/ms177634.aspx) clause into partitions to which the `DENSE_RANK` function is applied. For the `PARTITION BY` syntax, see [OVER Clause (Transact-SQL)](https://msdn.microsoft.com/en-us/library/ms189461.aspx).
|`<order_by_clause>`|Determines the order in which the `DENSE_RANK` function is applied to the rows in a partition.
|`OVER ( [ partition_by_clause ] order_by_clause)`|`partition_by_clause` divides the result set produced by the `FROM` clause into partitions to which the function is applied. If not specified, the function treats all rows of the query result set as a single group. order_by_clause determines the order of the data before the function is applied. The order_by_clause is required. The `<rows or range clause>` of the `OVER` clause cannot be specified for the `RANK` function. For more information, see [OVER Clause (Transact-SQL)](https://technet.microsoft.com/en-us/library/ms189461(v=sql.110).aspx).



#### Remarks


If two or more rows tie for a rank in the same partition, each tied rows receives the same rank. For example, if the two top salespeople have the same SalesYTD value, they are both ranked one. The salesperson with the next highest SalesYTD is ranked number two. This is one more than the number of distinct rows that come before this row. Therefore, the numbers returned by the `DENSE_RANK` function do not have gaps and always have consecutive ranks.

The sort order used for the whole query determines the order in which the rows appear in a result. This implies that a row ranked number one does not have to be the first row in the partition.

`DENSE_RANK` is nondeterministic. For more information, see [Deterministic and Nondeterministic Functions](https://msdn.microsoft.com/en-us/library/ms178091.aspx).

