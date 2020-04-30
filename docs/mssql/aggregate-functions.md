---
metaTitle: "Aggregate Functions"
description: "SUM(), AVG(), MAX(), MIN(), COUNT(), COUNT(Column_Name) with GROUP BY Column_Name"
---

# Aggregate Functions


Aggregate functions in SQL Server run calculations on sets of values, returning a single value.



## SUM()


Returns sum of numeric values in a given column.

We have table as shown in figure that will be used to perform different aggregate functions. The table name is **Marksheet**.

[<img src="http://i.stack.imgur.com/gUPGI.png" alt="enter image description here" />](http://i.stack.imgur.com/gUPGI.png)

```sql
Select SUM(MarksObtained) From Marksheet

```

The `sum` function doesn't consider rows with NULL value in the field used as parameter

In the above example if we have another row like this:

```sql
106    Italian    NULL

```

This row will not be consider in sum calculation



## AVG()


Returns average of numeric values in a given column.

We have table as shown in figure that will be used to perform different aggregate functions. The table name is **Marksheet**.

[<img src="http://i.stack.imgur.com/gUPGI.png" alt="enter image description here" />](http://i.stack.imgur.com/gUPGI.png)

```sql
Select AVG(MarksObtained) From Marksheet

```

The `average` function doesn't consider rows with NULL value in the field used as parameter

In the above example if we have another row like this:

```sql
106    Italian    NULL

```

This row will not be consider in average calculation



## MAX()


Returns the largest value in a given column.

We have table as shown in figure that will be used to perform different aggregate functions. The table name is **Marksheet**.

[<img src="http://i.stack.imgur.com/gUPGI.png" alt="enter image description here" />](http://i.stack.imgur.com/gUPGI.png)

```sql
Select MAX(MarksObtained) From Marksheet

```



## MIN()


Returns the smallest value in a given column.

We have table as shown in figure that will be used to perform different aggregate functions. The table name is **Marksheet**.

[<img src="http://i.stack.imgur.com/gUPGI.png" alt="enter image description here" />](http://i.stack.imgur.com/gUPGI.png)

```sql
Select MIN(MarksObtained) From Marksheet

```



## COUNT()


Returns the total number of values in a given column.

We have table as shown in figure that will be used to perform different aggregate functions. The table name is **Marksheet**.

[<img src="https://i.stack.imgur.com/gUPGI.png" alt="enter image description here" />](https://i.stack.imgur.com/gUPGI.png)

```sql
Select COUNT(MarksObtained) From Marksheet

```

The `count` function doesn't consider rows with NULL value in the field used as parameter. Usually the count parameter is * (all fields) so only if all fields of row are NULLs this row will not be considered

In the above example if we have another row like this:

```sql
106    Italian    NULL

```

This row will not be consider in count calculation

**NOTE**

The function `COUNT(*)` returns the number of rows in a table. This value can also be obtained by using a constant non-null expression that contains no column references, such as `COUNT(1)`.

Example

```sql
Select COUNT(1) From Marksheet

```



## COUNT(Column_Name) with GROUP BY Column_Name


Most of the time we like to get the total number of occurrence of a column value in a table for example:

TABLE NAME :  REPORTS

|ReportName|ReportPrice
|------
|Test|10.00   $
|Test|10.00   $
|Test|10.00   $
|Test 2|11.00   $
|Test|10.00   $
|Test 3|14.00   $
|Test 3|14.00   $
|Test 4|100.00   $

```sql
SELECT  
    ReportName AS REPORT NAME, 
    COUNT(ReportName) AS COUNT 
FROM     
    REPORTS 
GROUP BY 
    ReportName 

```

|REPORT NAME|COUNT
|------
|Test|4
|Test 2|1
|Test 3|2
|Test 4|1



#### Syntax


- AVG([ALL|DISTINCT]**expression**)
- COUNT([ALL|DISTINCT]**expression**)
- MAX([ALL|DISTINCT]**expression**)
- MIN([ALL|DISTINCT]**expression**)
- SUM([ALL|DISTINCT]**expression**)

