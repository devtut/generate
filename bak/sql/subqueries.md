---
metaTitle: "SQL - Subqueries"
description: "Subquery in FROM clause, Subquery in WHERE clause, Subquery in SELECT clause, Correlated Subqueries, Subqueries in FROM clause , Subqueries in WHERE clause, Filter query results using query on different table, Subqueries in SELECT clause"
---

# Subqueries




## Subquery in FROM clause


A subquery in a `FROM` clause acts similarly to a temporary table that is generated during the execution of a query and lost afterwards.

```sql
SELECT Managers.Id, Employees.Salary
FROM (
  SELECT Id
  FROM Employees
  WHERE ManagerId IS NULL
) AS Managers
JOIN Employees ON Managers.Id = Employees.Id

```



## Subquery in WHERE clause


Use a subquery to filter the result set. For example this will return all employees with a salary equal to the highest paid employee.

```sql
SELECT *
FROM Employees
WHERE Salary = (SELECT MAX(Salary) FROM Employees)

```



## Subquery in SELECT clause


```sql
SELECT
  Id, 
  FName, 
  LName,
  (SELECT COUNT(*) FROM Cars WHERE Cars.CustomerId = Customers.Id) AS NumberOfCars
FROM Customers

```



## Correlated Subqueries


Correlated (also known as Synchronized or Coordinated) Subqueries are nested queries that make references to the current row of their outer query:

```sql
SELECT EmployeeId
    FROM Employee AS eOuter
    WHERE Salary > (
       SELECT AVG(Salary)
       FROM Employee eInner
       WHERE eInner.DepartmentId = eOuter.DepartmentId
    )

```

Subquery `SELECT AVG(Salary) ...` is **correlated** because it refers to `Employee` row `eOuter` from its outer query.



## Subqueries in FROM clause 


You can use subqueries to define a temporary table  and use it in the FROM clause of an "outer" query.

```sql
SELECT * FROM (SELECT city, temp_hi - temp_lo AS temp_var FROM weather) AS w
WHERE temp_var > 20;

```

The above finds cities from the [weather table](http://stackoverflow.com/documentation/sql/280/example-databases/2641/weather-table) whose daily temperature variation is greater than 20. The result is:

|city|temp_var
|---|---|---|---
|ST LOUIS|21
|LOS ANGELES|31
|LOS ANGELES|23
|LOS ANGELES|31
|LOS ANGELES|27
|LOS ANGELES|28
|LOS ANGELES|28
|LOS ANGELES|32

.



## Subqueries in WHERE clause


The following example finds cities (from the [cities example](http://stackoverflow.com/documentation/sql/280/example-databases/2709/cities-table)) whose population is below the average temperature (obtained via a sub-qquery):

```sql
SELECT name, pop2000 FROM cities 
WHERE pop2000 < (SELECT avg(pop2000)  FROM cities);

```

Here: the subquery (SELECT avg(pop2000)  FROM cities) is used to specify conditions in the WHERE clause. The result is:

|name|pop2000
|---|---|---|---
|San Francisco|776733
|ST LOUIS|348189
|Kansas City|146866



## Filter query results using query on different table


This query selects all employees not on the Supervisors table.

```sql
SELECT *
FROM Employees
WHERE EmployeeID not in (SELECT EmployeeID
                            FROM Supervisors)

```

The same results can be achieved using a LEFT JOIN.

```sql
SELECT *
FROM Employees AS e
LEFT JOIN Supervisors AS s ON s.EmployeeID=e.EmployeeID
WHERE s.EmployeeID is NULL

```



## Subqueries in SELECT clause


Subqueries can also be used in the `SELECT` part of the outer query. The following query
shows all [weather table](http://stackoverflow.com/documentation/sql/280/example-databases/2641/weather-table) columns with the corresponding states from the [cities table](http://stackoverflow.com/documentation/sql/280/example-databases/2709/cities-table).

```sql
SELECT w.*,  (SELECT c.state FROM cities AS c WHERE c.name = w.city ) AS state 
FROM weather AS w;

```



#### Remarks


Subqueries can appear in different clauses of an outer query, or in the set operation.

They must be enclosed in parentheses `()`.
If the result of the subquery is compared to something else, the number of columns must match.
Table aliases are required for subqueries in the FROM clause to name the temporary table.

