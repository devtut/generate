---
metaTitle: "Microsoft SQL Server - ORDER BY"
description: "Simple ORDER BY clause, ORDER BY multiple fields, ORDER BY with complex logic, Custom Ordering"
---

# ORDER BY



## Simple ORDER BY clause


Using the [Employees Table](http://stackoverflow.com/documentation/sql/280/example-databases/1014/employees-table#t=201607211314066434211), below is an example to return the Id, FName and LName columns in (ascending) LName order:

```sql
SELECT Id, FName, LName FROM Employees
ORDER BY LName

```

Returns:

|Id|FName|LName
|---|---|---|---
|2|John|Johnson
|1|James|Smith
|4|Johnathon|Smith
|3|Michael|Williams

To sort in descending order add the DESC keyword after the field parameter, e.g. the same query in LName descending order is:

```sql
SELECT Id, FName, LName FROM Employees
ORDER BY LName DESC

```



## ORDER BY multiple fields


Multiple fields can be specified for the `ORDER BY` clause, in either ASCending or DESCending order.

For example, using the [http://stackoverflow.com/documentation/sql/280/example-databases/1207/item-sales-table#t=201607211314066434211](http://stackoverflow.com/documentation/sql/280/example-databases/1207/item-sales-table#t=201607211314066434211) table, we can return a query that sorts by SaleDate in ascending order, and Quantity in descending order.

```sql
SELECT ItemId, SaleDate, Quantity
FROM [Item Sales]
ORDER BY SaleDate ASC, Quantity DESC

```

Note that the `ASC` keyword is optional, and results are sorted in ascending order of a given field by default.



## ORDER BY with complex logic


If we want to order the data differently for per group, we can add a `CASE` syntax to the `ORDER BY`.
In this example, we want to order employees from Department 1 by last name and employees from Department 2 by salary.

|Id|FName|LName|PhoneNumber|ManagerId|DepartmentId|Salary|HireDate
|---|---|---|---|---|---|---|---
|1|James|Smith|1234567890|NULL|1|1000|01-01-2002
|2|John|Johnson|2468101214|1|1|400|23-03-2005
|3|Michael|Williams|1357911131|1|2|600|12-05-2009
|4|Johnathon|Smith|1212121212|2|1|500|24-07-2016
|5|Sam|Saxon|1372141312|2|2|400|25-03-2015

```sql
The following query will provide the required results:
SELECT Id, FName, LName, Salary FROM Employees
ORDER BY Case When DepartmentId = 1 then LName else Salary end

```



## Custom Ordering


If you want to order by a column using something other than alphabetical/numeric ordering, you can use `case` to specify the order you want.

`order by Group` returns:

|Group|Count
|---|---|---|---
|Not Retired|6
|Retired|4
|Total|10

`order by case group when 'Total' then 1 when 'Retired' then 2 else 3 end` returns:

|Group|Count
|---|---|---|---
|Total|10
|Retired|4
|Not Retired|6



#### Remarks


The purpose of the ORDER BY clause is to sort the data returned by a query.

It's important to note that the **order of rows returned by a query is undefined unless there is an ORDER BY clause.**

See MSDN documentation for full details of the ORDER BY clause: [https://msdn.microsoft.com/en-us/library/ms188385.aspx](https://msdn.microsoft.com/en-us/library/ms188385.aspx)

