---
metaTitle: "SQL - Views"
description: "Simple views, Complex views"
---

# Views




## Simple views


A view can filter some rows from the base table or project only some columns from it:

```sql
CREATE VIEW new_employees_details AS
SELECT E.id, Fname, Salary, Hire_date
FROM Employees E
WHERE hire_date > date '2015-01-01';

```

If you select form the view:

```sql
select * from new_employees_details

```

|Id|FName|Salary|Hire_date
|---|---|---|---
|4|Johnathon|500|24-07-2016



## Complex views


A view can be a really complex query(aggregations, joins, subqueries, etc). Just be sure you add column names for everything you select:

```sql
Create VIEW dept_income AS
SELECT d.Name as DepartmentName, sum(e.salary) as TotalSalary
FROM Employees e
JOIN Departments d on e.DepartmentId = d.id
GROUP BY d.Name;

```

Now you can select from it as from any table:

```sql
SELECT * 
FROM dept_income;

```

|DepartmentName|TotalSalary
|---|---|---|---
|HR|1900
|Sales|600

