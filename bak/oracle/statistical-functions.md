---
metaTitle: "Oracle Database - Statistical functions"
description: "Calculating the median of a set of values, VARIANCE, STDDEV"
---

# Statistical functions



## Calculating the median of a set of values


The [MEDIAN function](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions086.htm)  since Oracle 10g is an easy to use aggregation function:

```sql
SELECT MEDIAN(SAL)
FROM EMP

```

It returns the median of the values

Works on `DATETIME` values too.

> 
The result of MEDIAN is computed by first ordering the rows. Using N as the number of rows in the group, Oracle calculates the row number (RN) of interest with the formula RN = (1 + (0.5*(N-1)). The final result of the aggregate function is computed by linear interpolation between the values from rows at row numbers CRN = CEILING(RN) and FRN = FLOOR(RN).


Since Oracle 9i you can use [PERCENTILE_CONT](https://docs.oracle.com/cd/B19306_01/server.102/b14200/functions110.htm) which works the same as MEDIAN function with percentile value defaults to 0.5

```sql
SELECT PERCENTILE_CONT(.5) WITHIN GROUP(order by SAL) 
FROM EMP

```



## VARIANCE


[Variance measures](https://en.wikipedia.org/wiki/Variance) how far a set numbers is spread out from it's mean. From practical perspective it is squared distance from its mean (center) - the bigger the number the farther the point is.

The following example would return variance of salary values

```sql
SELECT name, salary, VARIANCE(salary) "Variance"
FROM employees 

```



## STDDEV


STDDEV returns the sample standard deviation of expr, a set of numbers. You can use it as both an aggregate and analytic function. It differs from STDDEV_SAMP in that STDDEV returns zero when it has only 1 row of input data, whereas STDDEV_SAMP returns null.

Oracle Database calculates the standard deviation as the square root of the variance defined for the VARIANCE aggregate function.

This function takes as an argument any numeric datatype or any nonnumeric datatype that can be implicitly converted to a numeric datatype. The function returns the same datatype as the numeric datatype of the argument.

If you specify DISTINCT, then you can specify only the query_partition_clause of the analytic_clause. The order_by_clause and windowing_clause are not allowed.

The following example returns the standard deviation of the salaries in the sample **hr.employees** table:

Where hr is Schema and employees is a table name.

```sql
SELECT STDDEV(salary) "Deviation"
FROM employees;

Deviation
----------
3909.36575

```

The query in the following example returns the cumulative standard deviation of the salaries in Department 80 in the sample table hr.employees, ordered by hire_date:

```sql
SELECT last_name, salary, 
STDDEV(salary) OVER (ORDER BY hire_date) "StdDev"
FROM employees  
WHERE department_id = 30; 

LAST_NAME                     SALARY     StdDev
------------------------- ---------- ----------
Raphaely                       11000          0
Khoo                            3100 5586.14357
Tobias                          2800  4650.0896

```

