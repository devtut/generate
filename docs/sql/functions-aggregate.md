---
metaTitle: "SQL - Functions (Aggregate)"
description: "Conditional aggregation, List Concatenation, SUM, AVG(), Count, Max, Min"
---

# Functions (Aggregate)




## Conditional aggregation


Payments Table

|Customer|Payment_type|Amount
|---|---|---|---|---
|Peter|Credit|100
|Peter|Credit|300
|John|Credit|1000
|John|Debit|500

```sql
select customer, 
       sum(case when payment_type = 'credit' then amount else 0 end) as credit,
       sum(case when payment_type = 'debit' then amount else 0 end) as debit
from payments
group by customer

```

Result:

|Customer|Credit|Debit
|---|---|---|---|---
|Peter|400|0
|John|1000|500

```sql
select customer, 
       sum(case when payment_type = 'credit' then 1 else 0 end) as credit_transaction_count,
       sum(case when payment_type = 'debit' then 1 else 0 end) as debit_transaction_count
from payments
group by customer

```

Result:

|Customer|credit_transaction_count|debit_transaction_count
|---|---|---|---|---
|Peter|2|0
|John|1|1



## List Concatenation


Partial credit to [this](http://stackoverflow.com/a/19348687/4896952) SO answer.

List Concatenation aggregates a column or expression by combining the values into a single string for each group. A string to delimit each value (either blank or a comma when omitted) and the order of the values in the result can be specified. While it is not part of the SQL standard, every major relational database vendor supports it in their own way.

### MySQL

```sql
SELECT ColumnA
     , GROUP_CONCAT(ColumnB ORDER BY ColumnB SEPARATOR ',') AS ColumnBs
  FROM TableName
 GROUP BY ColumnA
 ORDER BY ColumnA;

```

### Oracle & DB2

```sql
SELECT ColumnA
     , LISTAGG(ColumnB, ',') WITHIN GROUP (ORDER BY ColumnB) AS ColumnBs
  FROM TableName
 GROUP BY ColumnA
 ORDER BY ColumnA;

```

### PostgreSQL

```sql
SELECT ColumnA
     , STRING_AGG(ColumnB, ',' ORDER BY ColumnB) AS ColumnBs
  FROM TableName
 GROUP BY ColumnA
 ORDER BY ColumnA;

```

### SQL Server

### SQL Server 2016 and earlier

(CTE included to encourage the [DRY principle](https://en.wikipedia.org/wiki/Don%27t_repeat_yourself))

```

 WITH CTE_TableName AS (
       SELECT ColumnA, ColumnB
         FROM TableName)
SELECT t0.ColumnA
     , STUFF((
       SELECT ',' + t1.ColumnB
         FROM CTE_TableName t1
        WHERE t1.ColumnA = t0.ColumnA
        ORDER BY t1.ColumnB
          FOR XML PATH('')), 1, 1, '') AS ColumnBs
  FROM CTE_TableName t0
 GROUP BY t0.ColumnA
 ORDER BY ColumnA;

```

### SQL Server 2017 and SQL Azure

```sql
SELECT ColumnA
     , STRING_AGG(ColumnB, ',') WITHIN GROUP (ORDER BY ColumnB) AS ColumnBs
  FROM TableName
 GROUP BY ColumnA
 ORDER BY ColumnA;

```

### SQLite

without ordering:

```sql
SELECT ColumnA
     , GROUP_CONCAT(ColumnB, ',') AS ColumnBs
  FROM TableName
 GROUP BY ColumnA
 ORDER BY ColumnA;

```

ordering requires a subquery or CTE:

```

 WITH CTE_TableName AS (
       SELECT ColumnA, ColumnB
         FROM TableName
        ORDER BY ColumnA, ColumnB)
SELECT ColumnA
     , GROUP_CONCAT(ColumnB, ',') AS ColumnBs
  FROM CTE_TableName
 GROUP BY ColumnA
 ORDER BY ColumnA;

```



## SUM


`Sum` function sum the value of all the rows in the group. If the group by clause is omitted then sums all the rows.

```sql
select sum(salary) TotalSalary
from employees;

```

|TotalSalary
|---|---|---|---|---
|2500

```sql
select DepartmentId, sum(salary) TotalSalary
from employees
group by DepartmentId;

```

|DepartmentId|TotalSalary
|---|---|---|---|---
|1|2000
|2|500



## AVG()


The aggregate function AVG() returns the average of a given expression, usually numeric values in a column. Assume we have a table containing the yearly calculation of population in cities across the world. The records for New York City look similar to the ones below:

### EXAMPLE TABLE

|city_name|population|year
|---|---|---|---|---
|New York City|8,550,405|2015
|New York City|...|...
|New York City|8,000,906|2005

To select the average population of the New York City, USA from a table containing city names, population measurements, and measurement years for last ten years:

### QUERY

```sql
select city_name, AVG(population) avg_population
from city_population
where city_name = 'NEW YORK CITY';

```

Notice how measurement year is absent from the query since population is being averaged over time.

### RESULTS

|city_name|avg_population
|---|---|---|---|---
|New York City|8,250,754

> 
Note: The AVG() function will convert values to numeric types. This is especially important to keep in mind when working with dates.




## Count


You can count the number of rows:

```sql
SELECT count(*) TotalRows
FROM employees;

```

|TotalRows
|---|---|---|---|---
|4

Or count the employees per department:

```sql
SELECT DepartmentId, count(*) NumEmployees
FROM employees
GROUP BY DepartmentId;

```

|DepartmentId|NumEmployees
|---|---|---|---|---
|1|3
|2|1

You can count over a column/expression with the effect that will not count the `NULL` values:

```sql
SELECT count(ManagerId) mgr
FROM EMPLOYEES;

```

|mgr
|---|---|---|---|---
|3

(There is one null value managerID column)

You can also use **DISTINCT** inside of another function such as **COUNT** to only find the **DISTINCT** members of the set to perform the operation on.

For example:

```

SELECT COUNT(ContinentCode) AllCount
 ,      COUNT(DISTINCT ContinentCode) SingleCount
 FROM Countries;

```

Will return different values. The **SingleCount** will only Count individual Continents once, while the **AllCount** will include duplicates.

|ContinentCode
|---|---|---|---|---
|OC
|EU
|AS
|NA
|NA
|AF
|AF

AllCount: 7
SingleCount: 5



## Max


Find the maximum value of column:

```sql
select max(age) from employee;

```

Above example will return largest value for column `age` of `employee` table.

Syntax:

```sql
SELECT MAX(column_name) FROM table_name;

```



## Min


Find the smallest value of column:

```

select min(age) from employee;

```

Above example will return smallest value for column `age` of `employee` table.

Syntax:

```

SELECT MIN(column_name) FROM table_name;

```



#### Syntax


- Function([**DISTINCT**] expression) -DISTINCT is an optional parameter
- AVG ( [ ALL | DISTINCT ] expression )
- COUNT( { [ALL | DISTINCT ] expression ] | * } )
- GROUPING(<column_expression>)
- MAX ( [ ALL | DISTINCT ] expression )
- MIN ( [ ALL | DISTINCT ] expression )
- SUM ( [ ALL | DISTINCT ] expression )
<li>VAR ( [ ALL | DISTINCT ] expression )<br />
OVER ( [ partition_by_clause ] order_by_clause )</li>
<li>VARP ( [ ALL | DISTINCT ] expression )<br />
OVER ( [ partition_by_clause ] order_by_clause</li>
<li>STDEV ( [ ALL | DISTINCT ] expression )<br />
OVER ( [ partition_by_clause ] order_by_clause )</li>
<li>STDEVP ( [ ALL | DISTINCT ] expression )<br />
OVER ( [ partition_by_clause ] order_by_clause )</li>



#### Remarks


In database management an aggregate function is a function where the values of multiple rows are grouped together as input on certain criteria to form a single value of more significant meaning or measurement such as a set, a bag or a list.

```sql
MIN        returns the smallest value in a given column
MAX        returns the largest value in a given column
SUM        returns the sum of the numeric values in a given column
AVG        returns the average value of a given column
COUNT      returns the total number of values in a given column
COUNT(*)   returns the number of rows in a table
GROUPING   Is a column or an expression that contains a column in a GROUP BY clause.
STDEV      returns the statistical standard deviation of all values in the specified expression.
STDEVP     returns the statistical standard deviation for the population for all values in the specified expression.
VAR        returns the statistical variance of all values in the specified expression. may be followed by the OVER clause.
VARP       returns the statistical variance for the population for all values in the specified expression.

```

> 
<p>Aggregate functions are used to compute against a "returned column of
numeric data" from your `SELECT` statement. They basically summarize
the results of a particular column of selected data. - [SQLCourse2.com](http://www.sqlcourse2.com/agg_functions.html)</p>


All aggregate functions ignore NULL values.

