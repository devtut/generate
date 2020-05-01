---
metaTitle: "GROUP BY"
description: "Simple Grouping, GROUP BY multiple columns , GROUP BY with ROLLUP and CUBE, Group by with multiple tables, multiple columns, HAVING"
---

# GROUP BY




## Simple Grouping


Orders Table

|CustomerId|ProductId|Quantity|Price
|---|---|---|---|---
|1|2|5|100
|1|3|2|200
|1|4|1|500
|2|1|4|50
|3|5|6|700

When grouping by a specific column, only unique values of this column are returned.

```sql
SELECT customerId
FROM orders
GROUP BY customerId;

```

Return value:

|customerId
|---|---|---|---|---
|1
|2
|3

Aggregate functions like `count()` apply to each group and not to the complete table:

```sql
SELECT customerId, 
       COUNT(productId) as numberOfProducts,
       sum(price) as totalPrice
FROM orders
GROUP BY customerId;

```

Return value:

|customerId|numberOfProducts|totalPrice
|---|---|---|---|---
|1|3|800
|2|1|50
|3|1|700



## GROUP BY multiple columns 


One might want to GROUP BY more than one column

```sql
declare @temp table(age int, name varchar(15))

insert into @temp
select 18, 'matt' union all
select 21, 'matt' union all
select 21, 'matt' union all
select 18, 'luke' union all
select 18, 'luke' union all
select 21, 'luke' union all
select 18, 'luke' union all
select 21, 'luke'

SELECT Age, Name, count(1) count
FROM @temp 
GROUP BY Age, Name

```

will group by both age and name and will produce:

|Age|Name|count
|---|---|---|---|---
|18|luke|3
|21|luke|2
|18|matt|1
|21|matt|2



## GROUP BY with ROLLUP and CUBE


The ROLLUP operator is useful in generating reports that contain subtotals and totals.

<li>
CUBE generates a result set that shows aggregates for all combinations of values in the selected columns.
</li>
<li>
ROLLUP generates a result set that shows aggregates for a hierarchy of values in the selected columns.
<table><thead>|Item|Color|Quantity
</thead><tbody>|Table|Blue|124
|Table|Red|223
|Chair|Blue|101
|Chair|Red|210
</tbody></table></li>

```sql
SELECT CASE WHEN (GROUPING(Item) = 1) THEN 'ALL'
            ELSE ISNULL(Item, 'UNKNOWN')
       END AS Item,
       CASE WHEN (GROUPING(Color) = 1) THEN 'ALL'
            ELSE ISNULL(Color, 'UNKNOWN')
       END AS Color,
       SUM(Quantity) AS QtySum
FROM Inventory
GROUP BY Item, Color WITH ROLLUP

Item                 Color                QtySum                     
-------------------- -------------------- -------------------------- 
Chair                Blue                 101.00                     
Chair                Red                  210.00                     
Chair                ALL                  311.00                     
Table                Blue                 124.00                     
Table                Red                  223.00                     
Table                ALL                  347.00                     
ALL                  ALL                  658.00 

```

(7 row(s) affected)

If the ROLLUP keyword in the query is changed to CUBE, the CUBE result set is the same, except these two additional rows are returned at the end:

```sql
ALL                  Blue                 225.00                     
ALL                  Red                  433.00 

```

[https://technet.microsoft.com/en-us/library/ms189305(v=sql.90).aspx](https://technet.microsoft.com/en-us/library/ms189305(v=sql.90).aspx)



## Group by with multiple tables, multiple columns


Group by is often used with join statement. Let's assume we have two tables. The first one is the table of students:

|Id|Full Name|Age
|---|---|---|---|---
|1|Matt Jones|20
|2|Frank Blue|21
|3|Anthony Angel|18

Second table is the table of subject each student can take:

|Subject_Id|Subject
|---|---|---|---|---
|1|Maths
|2|P.E.
|3|Physics

And because one student can attend many subjects and one subject can be attended by many students (therefore N:N relationship) we need to have third "bounding" table. Let's call the table Students_subjects:

|Subject_Id|Student_Id
|---|---|---|---|---
|1|1
|2|2
|2|1
|3|2
|1|3
|1|1

Now lets say we want to know the number of subjects each student is attending. Here the standalone `GROUP BY` statement is not sufficient as the information is not available through single table. Therefore we need to use `GROUP BY` with the `JOIN` statement:

```sql
Select Students.FullName, COUNT(Subject Id) as SubjectNumber FROM Students_Subjects
LEFT JOIN Students
ON Students_Subjects.Student_id = Students.Id
GROUP BY Students.FullName

```

The result of the given query is as follows:

|FullName|SubjectNumber
|---|---|---|---|---
|Matt Jones|3
|Frank Blue|2
|Anthony Angel|1

For an even more complex example of GROUP BY usage, let's say student might be able to assign the same subject to his name more than once (as shown in table Students_Subjects).
In this scenario we might be able to count number of times each subject was assigned to a student by GROUPing by more than one column:

```sql
SELECT Students.FullName, Subjects.Subject,
COUNT(Students_subjects.Subject_id) AS NumberOfOrders
FROM ((Students_Subjects
INNER JOIN Students
ON Students_Subjcets.Student_id=Students.Id)
INNER JOIN Subjects
ON Students_Subjects.Subject_id=Subjects.Subject_id)
GROUP BY Fullname,Subject

```

This query gives the following result:

|FullName|Subject|SubjectNumber
|---|---|---|---|---
|Matt Jones|Maths|2
|Matt Jones|P.E|1
|Frank Blue|P.E|1
|Frank Blue|Physics|1
|Anthony Angel|Maths|1



## HAVING


Because the `WHERE` clause is evaluated before `GROUP BY`, you cannot use `WHERE` to pare down results of the grouping (typically an aggregate function, such as `COUNT(*)`). To meet this need, the `HAVING` clause can be used.

For example, using the following data:

```sql
DECLARE @orders TABLE(OrderID INT, Name NVARCHAR(100))

INSERT INTO @orders VALUES
( 1, 'Matt' ),
( 2, 'John' ),
( 3, 'Matt' ),
( 4, 'Luke' ),
( 5, 'John' ),
( 6, 'Luke' ),
( 7, 'John' ),
( 8, 'John' ),
( 9, 'Luke' ),
( 10, 'John' ),
( 11, 'Luke' )

```

If we want to get the number of orders each person has placed, we would use

```sql
SELECT Name, COUNT(*) AS 'Orders'
FROM @orders
GROUP BY Name

```

and get

|Name|Orders
|---|---|---|---|---
|Matt|2
|John|5
|Luke|4

However, if we want to limit this to individuals who have placed more than two orders, we can add a `HAVING` clause.

```sql
SELECT Name, COUNT(*) AS 'Orders'
FROM @orders
GROUP BY Name
HAVING COUNT(*) > 2

```

will yield

|Name|Orders
|---|---|---|---|---
|John|5
|Luke|4

Note that, much like `GROUP BY`, the columns put in `HAVING` must exactly match their counterparts in the `SELECT` statement. If in the above example we had instead said

```sql
SELECT Name, COUNT(DISTINCT OrderID)

```

our `HAVING` clause would have to say

```sql
HAVING COUNT(DISTINCT OrderID) > 2

```

