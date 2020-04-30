---
metaTitle: "Group By"
description: "GROUP BY using HAVING, Group By using Group Concat, Group By Using MIN function, GROUP BY with AGGREGATE functions, GROUP BY USING SUM Function, GROUP BY USING COUNT Function"
---

# Group By



## GROUP BY using HAVING


```sql
SELECT department, COUNT(*) AS "Man_Power"
FROM employees
GROUP BY department
HAVING COUNT(*) >= 10;

```

Using `GROUP BY ... HAVING` to filter aggregate records is analogous to using `SELECT ... WHERE` to filter individual records.

You could also say `HAVING Man_Power >= 10` since `HAVING` understands "aliases".



## Group By using Group Concat


[Group Concat](http://dev.mysql.com/doc/refman/5.7/en/group-by-functions.html#function_group-concat) is used in MySQL to get concatenated values of expressions with more than one result per column. Meaning, there are many rows to be selected back for one column such as `Name(1):Score(*)`

|Name|Score
|------
|Adam|A+
|Adam|A-
|Adam|B
|Adam|C+
|Bill|D-
|John|A-

```sql
SELECT Name, GROUP_CONCAT(Score ORDER BY Score desc SEPERATOR ' ') AS Grades
FROM   Grade
GROUP BY Name

```

Results:

```sql
+------+------------+ 
| Name | Grades     | 
+------+------------+ 
| Adam | C+ B A- A+ | 
| Bill | D-         | 
| John | A-         | 
+------+------------+ 

```



## Group By Using MIN function


Assume a table of employees in which each row is an employee who has a `name`, a `department`, and a `salary`.

```sql
SELECT department, MIN(salary) AS "Lowest salary"
FROM employees
GROUP BY department;

```

This would tell you which department contains the employee with the lowest salary, and what that salary is. Finding the `name` of the employee with the lowest salary in each department is a different problem, beyond the scope of this Example. See "groupwise max".



## GROUP BY with AGGREGATE functions


**Table ORDERS**

```sql
+---------+------------+----------+-------+--------+
| orderid | customerid | customer | total | items  |
+---------+------------+----------+-------+--------+
|       1 |          1 | Bob      |  1300 |     10 |
|       2 |          3 | Fred     |   500 |      2 |
|       3 |          5 | Tess     |  2500 |      8 |
|       4 |          1 | Bob      |   300 |      6 |
|       5 |          2 | Carly    |   800 |      3 |
|       6 |          2 | Carly    |  1000 |     12 |
|       7 |          3 | Fred     |   100 |      1 |
|       8 |          5 | Tess     | 11500 |     50 |
|       9 |          4 | Jenny    |   200 |      2 |
|      10 |          1 | Bob      |   500 |     15 |
+---------+------------+----------+-------+--------+

```


- **COUNT**

Return the **number of rows** that satisfy a specific criteria in `WHERE` clause.

E.g.: Number of orders for each customer.

```sql
SELECT customer, COUNT(*) as orders
FROM orders
GROUP BY customer
ORDER BY customer

```

**Result:**

```sql
+----------+--------+
| customer | orders |
+----------+--------+
| Bob      |      3 |
| Carly    |      2 |
| Fred     |      2 |
| Jenny    |      1 |
| Tess     |      2 |
+----------+--------+

```


- **SUM**

Return the **sum** of the selected column.

E.g.: Sum of the total and items for each customer.

```sql
SELECT customer, SUM(total) as sum_total, SUM(items) as sum_items
FROM orders
GROUP BY customer
ORDER BY customer

```

**Result:**

```sql
+----------+-----------+-----------+
| customer | sum_total | sum_items |
+----------+-----------+-----------+
| Bob      |      2100 |        31 |
| Carly    |      1800 |        15 |
| Fred     |       600 |         3 |
| Jenny    |       200 |         2 |
| Tess     |     14000 |        58 |
+----------+-----------+-----------+

```


- **AVG**

Return the **average** value of a column of numeric value.

E.g.: Average order value for each customers.

```sql
SELECT customer, AVG(total) as avg_total
FROM orders
GROUP BY customer
ORDER BY customer

```

**Result:**

```sql
+----------+-----------+
| customer | avg_total |
+----------+-----------+
| Bob      |       700 |
| Carly    |       900 |
| Fred     |       300 |
| Jenny    |       200 |
| Tess     |      7000 |
+----------+-----------+

```


- **MAX**

Return the **highest** value of a certain column or expression.

E.g.: Highest order total for each customers.

```sql
SELECT customer, MAX(total) as max_total
FROM orders
GROUP BY customer
ORDER BY customer

```

**Result:**

```sql
+----------+-----------+
| customer | max_total |
+----------+-----------+
| Bob      |      1300 |
| Carly    |      1000 |
| Fred     |       500 |
| Jenny    |       200 |
| Tess     |     11500 |
+----------+-----------+

```


- **MIN**

Return the **lowest** value of a certain column or expression.

E.g.: Lowest order total for each customers.

```sql
SELECT customer, MIN(total) as min_total
FROM orders
GROUP BY customer
ORDER BY customer

```

**Result:**

```sql
+----------+-----------+
| customer | min_total |
+----------+-----------+
| Bob      |       300 |
| Carly    |       800 |
| Fred     |       100 |
| Jenny    |       200 |
| Tess     |      2500 |
+----------+-----------+

```



## GROUP BY USING SUM Function


```sql
SELECT product, SUM(quantity) AS "Total quantity"
FROM order_details
GROUP BY product;

```



## GROUP BY USING COUNT Function


```sql
SELECT department, COUNT(*) AS "Man_Power"
FROM employees
GROUP BY department;

```



#### Syntax


1. SELECT expression1, expression2, ... expression_n,
1. aggregate_function (expression)
1. FROM tables
1. [WHERE conditions]
1. GROUP BY expression1, expression2, ... expression_n;



#### Parameters


|Parameter|DETAILS
|------
|expression1, expression2, ... expression_n|The expressions that are not encapsulated within an aggregate function and must be included in the GROUP BY clause.
|aggregate_function|A function such as SUM, COUNT, MIN, MAX, or AVG functions.
|tables|he tables that you wish to retrieve records from. There must be at least one table listed in the FROM clause.
|WHERE conditions|Optional. The conditions that must be met for the records to be selected.



#### Remarks


The MySQL GROUP BY clause is used in a SELECT statement to collect data across multiple records and group the results by one or more columns.

Its behavior is governed in part by the value of [the `ONLY_FULL_GROUP_BY` variable](http://dev.mysql.com/doc/refman/5.7/en/sql-mode.html#sqlmode_only_full_group_by). When this is enabled, `SELECT` statements that group by any column not in the output return an error. ([This is the default as of 5.7.5](http://dev.mysql.com/doc/refman/5.7/en/group-by-handling.html).) Both setting and not setting this variable can cause problems for naive users or users accustomed to other DBMSs.

