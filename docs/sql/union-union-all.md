---
metaTitle: "UNION / UNION ALL"
description: "Basic UNION ALL query, Simple explanation and Example"
---

# UNION / UNION ALL




## Basic UNION ALL query


```sql
CREATE TABLE HR_EMPLOYEES
(
    PersonID int,
    LastName VARCHAR(30),
    FirstName VARCHAR(30),
    Position VARCHAR(30)
);

CREATE TABLE FINANCE_EMPLOYEES
(
    PersonID INT,
    LastName VARCHAR(30),
    FirstName VARCHAR(30),
    Position VARCHAR(30)
);

```

Let's say we want to extract the names of all the `managers` from our departments.

Using a `UNION` we can get all the employees from both HR and Finance departments, which hold the `position` of a `manager`

```sql
SELECT 
    FirstName, LastName   
FROM 
    HR_EMPLOYEES  
WHERE 
    Position = 'manager'  
UNION ALL  
SELECT 
    FirstName, LastName  
FROM 
    FINANCE_EMPLOYEES  
WHERE 
    Position = 'manager'  

```

The `UNION` statement removes duplicate rows from the query results. Since it is possible to have people having the same Name and position in both departments we are using `UNION ALL`, in order not to remove duplicates.

If you want to use an alias for each output column, you can just put them in the first select statement, as follows:

```sql
SELECT 
    FirstName as 'First Name', LastName as 'Last Name'
FROM 
    HR_EMPLOYEES  
WHERE 
    Position = 'manager'  
UNION ALL  
SELECT 
    FirstName, LastName  
FROM 
    FINANCE_EMPLOYEES  
WHERE 
    Position = 'manager'  

```



## Simple explanation and Example


In simple terms:

- `UNION` joins 2 result sets while removing duplicates from the result set
- `UNION ALL` joins 2 result sets without attempting to remove duplicates

> 
One mistake many people make is to use a `UNION` when they do not need to have the duplicates removed. The additional performance cost against large results sets can be very significant.


**When you might need `UNION`**

Suppose you need to filter a table against 2 different attributes, and you have created separate non-clustered indexes for each column. A `UNION` enables you to leverage both indexes while still preventing duplicates.

```sql
SELECT C1, C2, C3 FROM Table1 WHERE C1 = @Param1
UNION
SELECT C1, C2, C3 FROM Table1 WHERE C2 = @Param2

```

This simplifies your performance tuning since only simple indexes are needed to perform these queries optimally. You may even be able to get by with quite a bit fewer non-clustered indexes improving overall write performance against the source table as well.

**When you might need `UNION ALL`**

Suppose you still need to filter a table against 2 attributes, but you do not need to filter duplicate records (either because it doesn't matter or your data wouldn't produce any duplicates during the union due to your data model design).

```sql
SELECT C1 FROM Table1
UNION ALL
SELECT C1 FROM Table2

```

This is especially useful when creating Views that join data that is designed to be physically partitioned across multiple tables (maybe for performance reasons, but still wants to roll-up records). Since the data is already split, having the database engine remove duplicates adds no value and just adds additional processing time to the queries.



#### Syntax


<li>SELECT column_1 [, column_2 ] FROM table_1 [, table_2 ] [WHERE condition]<br />
**UNION | UNION ALL**<br />
SELECT column_1 [, column_2 ] FROM table_1 [, table_2 ] [WHERE condition]</li>



#### Remarks


`UNION` and `UNION ALL` clauses combine the result-set of two or more identically structured SELECT statements into a single result / table.

Both the column count and column types for each query have to match in order for a `UNION` / `UNION ALL` to work.

The difference between a `UNION` and a `UNION ALL` query is that the `UNION` clause will remove any duplicate rows in the result where the `UNION ALL` will not.

This distinct removal of records can significantly slow queries even if there are no distinct rows to be removed because of this if you know there wont be any duplicates (or don't care) always default to `UNION ALL` for a more optimised query.

