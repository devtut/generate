---
metaTitle: "SQL - Common Table Expressions"
description: "generating values, recursively enumerating a subtree, Temporary query, recursively going up in a tree, Recursively generate dates, extended to include team rostering as example, Oracle CONNECT BY functionality with recursive CTEs, Refactoring a query to use Common Table Expressions, Example of a complex SQL with Common Table Expression"
---

# Common Table Expressions



## generating values


Most databases do not have a native way of generating a series of numbers for ad-hoc use; however, common table expressions can be used with recursion to emulate that type of function.

The following example generates a common table expression called `Numbers` with a column `i` which has a row for numbers 1-5:

```sql
--Give a table name `Numbers" and a column `i` to hold the numbers
WITH Numbers(i) AS (
    --Starting number/index
    SELECT 1
    --Top-level UNION ALL operator required for recursion
    UNION ALL
    --Iteration expression:
    SELECT i + 1
    --Table expression we first declared used as source for recursion
    FROM Numbers
    --Clause to define the end of the recursion
    WHERE i < 5
)
--Use the generated table expression like a regular table
SELECT i FROM Numbers;

```

|i
|---|---|---|---
|1
|2
|3
|4
|5

This method can be used with any number interval, as well as other types of data.



## recursively enumerating a subtree


```sql
WITH RECURSIVE ManagedByJames(Level, ID, FName, LName) AS (
    -- start with this row
    SELECT 1, ID, FName, LName
    FROM Employees
    WHERE ID = 1

    UNION ALL

    -- get employees that have any of the previously selected rows as manager
    SELECT ManagedByJames.Level + 1,
           Employees.ID,
           Employees.FName,
           Employees.LName
    FROM Employees
    JOIN ManagedByJames
        ON Employees.ManagerID = ManagedByJames.ID

    ORDER BY 1 DESC   -- depth-first search
)
SELECT * FROM ManagedByJames;

```

|Level|ID|FName|LName
|---|---|---|---
|**1**|1|James|Smith
|**2**|2|John|Johnson
|**3**|4|Johnathon|Smith
|**2**|3|Michael|Williams



## Temporary query


These behave in the same manner as nested subqueries but with a different syntax.

```sql
WITH ReadyCars AS (
  SELECT *
  FROM Cars
  WHERE Status = 'READY'
)
SELECT ID, Model, TotalCost
FROM ReadyCars
ORDER BY TotalCost;

```

|ID|Model|TotalCost
|---|---|---|---
|1|Ford F-150|200
|2|Ford F-150|230

**Equivalent subquery syntax**

```sql
SELECT ID, Model, TotalCost
FROM (
  SELECT *
  FROM Cars
  WHERE Status = 'READY'
) AS ReadyCars
ORDER BY TotalCost

```



## recursively going up in a tree


```sql
WITH RECURSIVE ManagersOfJonathon AS (
    -- start with this row
    SELECT *
    FROM Employees
    WHERE ID = 4

    UNION ALL

    -- get manager(s) of all previously selected rows
    SELECT Employees.*
    FROM Employees
    JOIN ManagersOfJonathon
        ON Employees.ID = ManagersOfJonathon.ManagerID
)
SELECT * FROM ManagersOfJonathon;

```

|Id|FName|LName|PhoneNumber|ManagerId|DepartmentId
|---|---|---|---|---|---
|4|Johnathon|Smith|1212121212|**2**|1
|**2**|John|Johnson|2468101214|**1**|1
|**1**|James|Smith|1234567890|NULL|1



## Recursively generate dates, extended to include team rostering as example


```sql
DECLARE @DateFrom DATETIME = '2016-06-01 06:00'
DECLARE @DateTo DATETIME = '2016-07-01 06:00'
DECLARE @IntervalDays INT = 7

-- Transition Sequence = Rest & Relax into Day Shift into Night Shift
-- RR (Rest & Relax) = 1
-- DS (Day Shift) = 2
-- NS (Night Shift) = 3

;WITH roster AS
(
   SELECT @DateFrom AS RosterStart, 1 AS TeamA, 2 AS TeamB, 3 AS TeamC
   UNION ALL
   SELECT DATEADD(d, @IntervalDays, RosterStart),
          CASE TeamA WHEN 1 THEN 2 WHEN 2 THEN 3 WHEN 3 THEN 1 END AS TeamA,
          CASE TeamB WHEN 1 THEN 2 WHEN 2 THEN 3 WHEN 3 THEN 1 END AS TeamB,
          CASE TeamC WHEN 1 THEN 2 WHEN 2 THEN 3 WHEN 3 THEN 1 END AS TeamC
   FROM roster WHERE RosterStart < DATEADD(d, -@IntervalDays, @DateTo)
)

SELECT RosterStart,
       ISNULL(LEAD(RosterStart) OVER (ORDER BY RosterStart), RosterStart + @IntervalDays) AS RosterEnd,
       CASE TeamA WHEN 1 THEN 'RR' WHEN 2 THEN 'DS' WHEN 3 THEN 'NS' END AS TeamA,
       CASE TeamB WHEN 1 THEN 'RR' WHEN 2 THEN 'DS' WHEN 3 THEN 'NS' END AS TeamB,
       CASE TeamC WHEN 1 THEN 'RR' WHEN 2 THEN 'DS' WHEN 3 THEN 'NS' END AS TeamC
FROM roster

```

**Result**

I.e. For Week 1 TeamA is on R&R, TeamB is on Day Shift and TeamC is on Night Shift.

[<img src="http://i.stack.imgur.com/rm2xk.jpg" alt="enter image description here" />](http://i.stack.imgur.com/rm2xk.jpg)



## Oracle CONNECT BY functionality with recursive CTEs


Oracle's CONNECT BY functionality provides many useful and nontrivial features that are not built-in when using SQL standard recursive CTEs. This example replicates these features (with a few additions for sake of completeness), using SQL Server syntax. It is most useful for Oracle developers finding many features missing in their hierarchical queries on other databases, but it also serves to showcase what can be done with a hierarchical query in general.

```

 WITH tbl AS (
       SELECT id, name, parent_id
         FROM mytable)
     , tbl_hierarchy AS (
       /* Anchor */
       SELECT 1 AS "LEVEL"
            --, 1 AS CONNECT_BY_ISROOT
            --, 0 AS CONNECT_BY_ISBRANCH
            , CASE WHEN t.id IN (SELECT parent_id FROM tbl) THEN 0 ELSE 1 END AS CONNECT_BY_ISLEAF
            , 0 AS CONNECT_BY_ISCYCLE
            , '/' + CAST(t.id   AS VARCHAR(MAX)) + '/' AS SYS_CONNECT_BY_PATH_id
            , '/' + CAST(t.name AS VARCHAR(MAX)) + '/' AS SYS_CONNECT_BY_PATH_name
            , t.id AS root_id
            , t.*
         FROM tbl t
        WHERE t.parent_id IS NULL                            -- START WITH parent_id IS NULL
       UNION ALL
       /* Recursive */
       SELECT th."LEVEL" + 1 AS "LEVEL"
            --, 0 AS CONNECT_BY_ISROOT
            --, CASE WHEN t.id IN (SELECT parent_id FROM tbl) THEN 1 ELSE 0 END AS CONNECT_BY_ISBRANCH
            , CASE WHEN t.id IN (SELECT parent_id FROM tbl) THEN 0 ELSE 1 END AS CONNECT_BY_ISLEAF
            , CASE WHEN th.SYS_CONNECT_BY_PATH_id LIKE '%/' + CAST(t.id AS VARCHAR(MAX)) + '/%' THEN 1 ELSE 0 END AS CONNECT_BY_ISCYCLE
            , th.SYS_CONNECT_BY_PATH_id   + CAST(t.id   AS VARCHAR(MAX)) + '/' AS SYS_CONNECT_BY_PATH_id
            , th.SYS_CONNECT_BY_PATH_name + CAST(t.name AS VARCHAR(MAX)) + '/' AS SYS_CONNECT_BY_PATH_name
            , th.root_id
            , t.*
         FROM tbl t
              JOIN tbl_hierarchy th ON (th.id = t.parent_id) -- CONNECT BY PRIOR id = parent_id
        WHERE th.CONNECT_BY_ISCYCLE = 0)                     -- NOCYCLE
SELECT th.*
     --, REPLICATE(' ', (th."LEVEL" - 1) * 3) + th.name AS tbl_hierarchy
  FROM tbl_hierarchy th
       JOIN tbl CONNECT_BY_ROOT ON (CONNECT_BY_ROOT.id = th.root_id)
 ORDER BY th.SYS_CONNECT_BY_PATH_name;                       -- ORDER SIBLINGS BY name

```

CONNECT BY features demonstrated above, with explanations:

<li>Clauses
<ul>
- CONNECT BY: Specifies the relationship that defines the hierarchy.
- START WITH: Specifies the root nodes.
- ORDER SIBLINGS BY: Orders results properly.

- NOCYCLE: Stops processing a branch when a loop is detected. Valid hierarchies are Directed Acyclic Graphs, and circular references violate this construct.

- PRIOR: Obtains data from the node's parent.
- CONNECT_BY_ROOT: Obtains data from the node's root.

- LEVEL: Indicates the node's distance from its root.
- CONNECT_BY_ISLEAF: Indicates a node without children.
- CONNECT_BY_ISCYCLE: Indicates a node with a circular reference.

- SYS_CONNECT_BY_PATH: Returns a flattened/concatenated representation of the path to the node from its root.



## Refactoring a query to use Common Table Expressions


Suppose we want to get all product categories with total sales greater than 20.

Here is a query without Common Table Expressions:

```sql
SELECT category.description, sum(product.price) as total_sales
FROM sale
LEFT JOIN product on sale.product_id = product.id
LEFT JOIN category on product.category_id = category.id
GROUP BY category.id, category.description
HAVING sum(product.price) > 20

```

And an equivalent query using Common Table Expressions:

```sql
WITH all_sales AS (
  SELECT product.price, category.id as category_id, category.description as category_description
  FROM sale
  LEFT JOIN product on sale.product_id = product.id
  LEFT JOIN category on product.category_id = category.id
)
, sales_by_category AS (
  SELECT category_description, sum(price) as total_sales
  FROM all_sales
  GROUP BY category_id, category_description
)
SELECT * from sales_by_category WHERE total_sales > 20

```



## Example of a complex SQL with Common Table Expression


Suppose we want to query the "cheapest products" from the "top categories".

Here is an example of query using Common Table Expressions

```sql
-- all_sales: just a simple SELECT with all the needed JOINS
WITH all_sales AS (
  SELECT
  product.price as product_price,
  category.id as category_id,
  category.description as category_description
  FROM sale
  LEFT JOIN product on sale.product_id = product.id
  LEFT JOIN category on product.category_id = category.id
)
-- Group by category
, sales_by_category AS (
  SELECT category_id, category_description,
  sum(product_price) as total_sales
  FROM all_sales
  GROUP BY category_id, category_description
)
-- Filtering total_sales > 20
, top_categories AS (
  SELECT * from sales_by_category WHERE total_sales > 20
)
-- all_products: just a simple SELECT with all the needed JOINS
, all_products AS (
  SELECT
  product.id as product_id,
  product.description as product_description,
  product.price as product_price,
  category.id as category_id,
  category.description as category_description
  FROM product
  LEFT JOIN category on product.category_id = category.id
)
-- Order by product price
, cheapest_products AS (
  SELECT * from all_products
  ORDER by product_price ASC
)
-- Simple inner join 
, cheapest_products_from_top_categories AS (
  SELECT product_description, product_price
  FROM cheapest_products
  INNER JOIN top_categories ON cheapest_products.category_id = top_categories.category_id
)
--The main SELECT
SELECT * from cheapest_products_from_top_categories

```



#### Syntax


<li>
<p>WITH QueryName [(ColumnName, ...)] AS (<br />
  SELECT ...<br />
)<br />
SELECT ... FROM QueryName ...;</p>
</li>
<li>
<p>WITH RECURSIVE QueryName [(ColumnName, ...)] AS (<br />
  SELECT ...<br />
  UNION [ALL]<br />
  SELECT ... FROM QueryName ...<br />
)<br />
SELECT ... FROM QueryName ...;</p>
</li>



#### Remarks


Official documentation: [WITH clause](http://www.sqlite.org/lang_with.html)

A Common Table Expression is a temporary result set, and it can be result of complex sub query. It is defined by using WITH clause. CTE improves readability and it is created in memory rather than TempDB database where Temp Table and Table variable is created.

**Key concepts of Common Table Expressions:**

- Can be used to break up complex queries, especially complex joins and sub-queries.
- Is a way of encapsulating a query definition.
- Persist only until the next query is run.
- Correct use can lead to improvements in both code quality/maintainability and speed.
- Can be used to reference the resulting table multiple times in the same statement (eliminate duplication in SQL).
- Can be a substitute for a view when the general use of a view is not required; that is, you do not have to store the definition in metadata.
- Will be run when called, not when defined. If the CTE is used multiple times in a query it will be run multiple times (possibly with different results).

