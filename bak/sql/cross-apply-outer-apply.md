---
metaTitle: "SQL - cross apply, outer apply"
description: "CROSS APPLY and OUTER APPLY basics"
---

# cross apply, outer apply



## CROSS APPLY and OUTER APPLY basics


Apply will be used when when table valued function in the right expression.

create a Department table to hold information about departments. Then create an Employee table which hold information about the employees. Please note, each employee belongs to a department, hence the Employee table has referential integrity with the Department table.

First query selects data from Department table and uses CROSS APPLY to evaluate the Employee table for each record of the Department table. Second query simply joins the Department table with the Employee table and all the matching records are produced.

```sql
SELECT *
FROM Department D
CROSS APPLY (
    SELECT *
    FROM Employee E
    WHERE E.DepartmentID = D.DepartmentID
) A
GO
SELECT *
FROM Department D
INNER JOIN Employee E
  ON D.DepartmentID = E.DepartmentID

```

If you look at the results they produced, it is the exact same result-set;  How does it differ from a JOIN and how does it help in writing more efficient queries.

The first query in Script #2 selects data from Department table and uses OUTER APPLY to evaluate the Employee table for each record of the Department table.  For those rows for which there is not a match in Employee table, those rows contains NULL values as you can see in case of row 5 and 6. The second query simply uses a LEFT OUTER JOIN between the Department table and the Employee table.  As expected the query returns all rows from Department table; even for those rows for which there is no match in the Employee table.

```sql
SELECT *
FROM Department D
OUTER APPLY (
    SELECT *
    FROM Employee E
    WHERE E.DepartmentID = D.DepartmentID
) A
GO
SELECT *
FROM Department D
LEFT OUTER JOIN Employee E
  ON D.DepartmentID = E.DepartmentID
GO

```

Even though the above two queries return the same information, the execution plan will be bit different. But cost wise there will be not much difference.

Now comes the time to see where the APPLY operator is really required. In Script #3, I am creating a table-valued function which accepts DepartmentID as its parameter and returns all the employees who belong to this department. The next query selects data from Department table and uses CROSS APPLY to join with the function we created.  It passes the DepartmentID for each row from the outer table expression (in our case Department table) and evaluates the function for each row similar to a correlated subquery. The next query uses the OUTER APPLY in place of CROSS APPLY and hence unlike CROSS APPLY which returned only correlated data, the OUTER APPLY returns non-correlated data as well, placing NULLs into the missing columns.

```sql
CREATE FUNCTION dbo.fn_GetAllEmployeeOfADepartment (@DeptID AS int)
RETURNS TABLE
AS
  RETURN
  (
  SELECT
    *
  FROM Employee E
  WHERE E.DepartmentID = @DeptID
  )
GO
SELECT
  *
FROM Department D
CROSS APPLY dbo.fn_GetAllEmployeeOfADepartment(D.DepartmentID)
GO
SELECT
  *
FROM Department D
OUTER APPLY dbo.fn_GetAllEmployeeOfADepartment(D.DepartmentID)
GO

```

So now if you are wondering, can we use a simple join in place of the above queries? Then the answer is NO, if you replace CROSS/OUTER APPLY in the above queries with INNER JOIN/LEFT OUTER JOIN, specify ON clause (something as 1=1) and run the query, you will get "The multi-part identifier "D.DepartmentID" could not be bound."  error. This is because with JOINs the execution context of outer query is different from the execution context of the function (or a derived table), and you can not bind a value/variable from the outer query to the function as a parameter.  Hence the APPLY operator is required for such queries.

