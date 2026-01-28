---
metaTitle: "SQL - Row number"
description: "Row numbers without partitions, Row numbers with partitions, Delete All But Last Record (1 to Many Table)"
---

# Row number



## Row numbers without partitions


Include a row number according to the order specified.

```sql
SELECT
  ROW_NUMBER() OVER(ORDER BY Fname ASC) AS RowNumber,
  Fname,
  LName
FROM Employees

```



## Row numbers with partitions


Uses a partition criteria to group the row numbering according to it.

```sql
SELECT
  ROW_NUMBER() OVER(PARTITION BY DepartmentId ORDER BY DepartmentId ASC) AS RowNumber,
  DepartmentId, Fname, LName
FROM Employees

```



## Delete All But Last Record (1 to Many Table)


```sql
WITH cte AS (
  SELECT ProjectID,
         ROW_NUMBER() OVER (PARTITION BY ProjectID ORDER BY InsertDate DESC) AS rn
  FROM ProjectNotes
)
DELETE FROM cte WHERE rn > 1;

```



#### Syntax


- ROW_NUMBER ( )
- OVER ( [ PARTITION BY value_expression , ... [ n ] ] order_by_clause )

