---
metaTitle: "CREATE VIEW"
description: "CREATE Indexed VIEW, CREATE VIEW, CREATE VIEW With Encryption, CREATE VIEW With INNER JOIN, Grouped VIEWs, UNION-ed VIEWs"
---

# CREATE VIEW



## CREATE Indexed VIEW


To create a view with an index, the view must be created using the `WITH SCHEMABINDING` keywords:

```sql
CREATE VIEW view_EmployeeInfo
WITH SCHEMABINDING
AS   
    SELECT EmployeeID,
        FirstName,
        LastName,
        HireDate  
    FROM [dbo].Employee
GO

```

Any clustered or non-clustered indexes can be now be created:

```sql
CREATE UNIQUE CLUSTERED INDEX IX_view_EmployeeInfo
ON view_EmployeeInfo
(
     EmployeeID ASC
)

```

**There Are some limitations to indexed Views:**

<li>
The view definition can reference one or more tables in the same database.
</li>
<li>
Once the unique clustered index is created, additional nonclustered indexes can be created against the view.
</li>
<li>
You can update the data in the underlying tables – including inserts, updates, deletes, and even truncates.
</li>
<li>
You can’t modify the underlying tables and columns. The view is created with the WITH SCHEMABINDING option.
</li>
<li>
It can’t contain COUNT, MIN, MAX, TOP, outer joins, or a few other keywords or elements.
</li>

For more information about creating indexed Views you can read this [MSDN article](https://msdn.microsoft.com/en-us/library/ms191432.aspx)



## CREATE VIEW


```sql
CREATE VIEW view_EmployeeInfo
AS   
    SELECT EmployeeID,
        FirstName,
        LastName,
        HireDate  
    FROM Employee
GO

```

Rows from views can be selected much like tables:

```sql
SELECT FirstName
FROM view_EmployeeInfo

```

You may also create a view with a calculated column. We can modify the view above as follows by adding a calculated column:

```sql
CREATE VIEW view_EmployeeReport
AS   
    SELECT EmployeeID,
        FirstName,
        LastName,
        Coalesce(FirstName,'') + ' ' + Coalesce(LastName,'') as FullName,
        HireDate  
    FROM Employee
GO

```

This view adds an additional column that will appear when you `SELECT` rows from it. The values in this additional column will be dependent on the fields `FirstName` and `LastName` in the table `Employee` and will automatically update behind-the-scenes when those fields are updated.



## CREATE VIEW With Encryption


```sql
CREATE VIEW view_EmployeeInfo
WITH ENCRYPTION
AS   
SELECT EmployeeID, FirstName, LastName, HireDate  
FROM Employee
GO

```



## CREATE VIEW With INNER JOIN


```sql
CREATE VIEW view_PersonEmployee
AS  
    SELECT P.LastName,
        P.FirstName,
        E.JobTitle
    FROM Employee AS E
    INNER JOIN Person AS P  
        ON P.BusinessEntityID = E.BusinessEntityID
GO

```

Views can use joins to select data from numerous sources like tables, table functions, or even other views. This example uses the FirstName and LastName columns from the Person table and the JobTitle column from the Employee table.

This view can now be used to see all corresponding rows for Managers in the database:

```sql
SELECT *
FROM view_PersonEmployee
WHERE JobTitle LIKE '%Manager%'

```



## Grouped VIEWs


A grouped VIEW is based on a query with a GROUP BY clause. Since each of the groups may have more than one row in the base from which it was built, these are necessarily read-only VIEWs. Such VIEWs usually have one or more aggregate functions and they are used for reporting purposes. They are also handy for working around weaknesses in SQL. Consider a VIEW that shows the largest sale in each state. The query is straightforward:

[https://www.simple-talk.com/sql/t-sql-programming/sql-view-beyond-the-basics/](https://www.simple-talk.com/sql/t-sql-programming/sql-view-beyond-the-basics/)

```sql
CREATE VIEW BigSales (state_code, sales_amt_total)
AS SELECT state_code, MAX(sales_amt)
     FROM Sales
    GROUP BY state_code;

```



## UNION-ed VIEWs


VIEWs based on a UNION or UNION ALL operation are read-only because there is no single way to map a change onto just one row in one of the base tables. The UNION operator will remove duplicate rows from the results. Both the UNION and UNION ALL operators hide which table the rows came from. Such VIEWs must use a , because the columns in a UNION [ALL] have no names of their own. In theory, a UNION of two disjoint tables, neither of which has duplicate rows in itself should be updatable.

[https://www.simple-talk.com/sql/t-sql-programming/sql-view-beyond-the-basics/](https://www.simple-talk.com/sql/t-sql-programming/sql-view-beyond-the-basics/)

```sql
CREATE VIEW DepTally2 (emp_nbr, dependent_cnt)
AS (SELECT emp_nbr, COUNT(*)
      FROM Dependents
     GROUP BY emp_nbr)
   UNION
   (SELECT emp_nbr, 0
      FROM Personnel AS P2
     WHERE NOT EXISTS 
          (SELECT *
             FROM Dependents AS D2
            WHERE D2.emp_nbr = P2.emp_nbr));

```

