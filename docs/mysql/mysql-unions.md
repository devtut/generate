---
metaTitle: "MySQL Unions"
description: "Union operator, Union ALL, UNION ALL With WHERE"
---

# MySQL Unions



## Union operator


The UNION operator is used to combine the result-set (**only distinct values**) of two or more SELECT statements.

**Query:** (To selects all the different cities (**only distinct values**) from the "Customers" and the "Suppliers" tables)

```sql
SELECT City FROM Customers
UNION
SELECT City FROM Suppliers
ORDER BY City;

```

**Result:**

```sql
Number of Records: 10

City
------
Aachen
Albuquerque
Anchorage
Annecy
Barcelona
Barquisimeto
Bend
Bergamo
Berlin
Bern

```



## Union ALL


UNION ALL to select all (duplicate values also) cities from the "Customers" and "Suppliers" tables.

Query:

```sql
SELECT City FROM Customers
UNION ALL
SELECT City FROM Suppliers
ORDER BY City;

```

Result:

```sql
Number of Records: 12

City
-------
Aachen
Albuquerque
Anchorage
Ann Arbor
Annecy
Barcelona
Barquisimeto
Bend
Bergamo
Berlin
Berlin
Bern

```



## UNION ALL With WHERE


UNION ALL to select all(duplicate values also) German cities from the "Customers" and "Suppliers" tables.
Here `Country="Germany"` is to be specified in the where clause.

**Query:**

```sql
SELECT City, Country FROM Customers
WHERE Country='Germany'
UNION ALL
SELECT City, Country FROM Suppliers
WHERE Country='Germany'
ORDER BY City;

```

Result:

```sql
Number of Records: 14

```sql
|City|Country
|Aachen|Germany
|Berlin|Germany
|Berlin|Germany
|Brandenburg|Germany
|Cunewalde|Germany
|Cuxhaven|Germany
|Frankfurt|Germany
|Frankfurt a.M. |Germany
|Köln|Germany
|Leipzig|Germany
|Mannheim|Germany
|München|Germany
|Münster|Germany
|Stuttgart|Germany



#### Syntax


- SELECT column_name(s) FROM table1 UNION SELECT column_name(s) FROM table2;
<li>SELECT column_name(s) FROM table1
UNION ALL
SELECT column_name(s) FROM table2;</li>
<li>SELECT column_name(s) FROM table1
WHERE col_name="XYZ"
UNION ALL
SELECT column_name(s) FROM table2
WHERE col_name="XYZ";</li>



#### Remarks


`UNION DISTINCT` is the same as `UNION`; it is slower than `UNION ALL` because of a de-duplicating pass.  A good practice is to always spell out `DISTINCT` or `ALL`, thereby signaling that you thought about which to do.

