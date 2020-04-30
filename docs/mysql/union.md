---
metaTitle: "UNION"
description: "Combining SELECT statements with UNION, ORDER BY, Pagination via OFFSET, Combining data with different columns, UNION ALL  and UNION, Combining and merging data on different MySQL tables with the same columns into unique rows and running query"
---

# UNION



## Combining SELECT statements with UNION


You can combine the results of two identically structured queries with the `UNION` keyword.

For example, if you wanted a list of all contact info from two separate tables, `authors` and `editors`, for instance, you could use the `UNION` keyword like so:

```sql
select name, email, phone_number 
from authors

union

select name, email, phone_number
from editors

```

Using `union` by itself will strip out duplicates. If you needed to keep duplicates in your query, you could use the `ALL` keyword like so: `UNION ALL`.



## ORDER BY


If you need to sort the results of a UNION, use this pattern:

```sql
( SELECT ... )
UNION
( SELECT ... )
ORDER BY

```

Without the parentheses, the final ORDER BY would belong to the last SELECT.



## Pagination via OFFSET


When adding a LIMIT to a UNION, this is the pattern to use:

```sql
( SELECT ... ORDER BY x  LIMIT 10 )
UNION
( SELECT ... ORDER BY x  LIMIT 10 )
ORDER BY x  LIMIT 10

```

Since you cannot predict which SELECT(s) will the "10" will come from, you need to get 10 from each, then further whittle down the list, repeating both the `ORDER BY` and `LIMIT`.

For the 4th page of 10 items, this pattern is needed:

```sql
( SELECT ... ORDER BY x  LIMIT 40 )
UNION
( SELECT ... ORDER BY x  LIMIT 40 )
ORDER BY x  LIMIT 30, 10

```

That is, collect 4 page's worth in each `SELECT`, then do the `OFFSET` in the `UNION`.



## Combining data with different columns


```sql
SELECT name, caption as title, year, pages FROM books 
UNION
SELECT name, title, year, 0 as pages FROM movies

```

When combining 2 record sets with different columns then emulate the missing ones with default values.



## UNION ALL  and UNION


SELECT 1,22,44
UNION
SELECT 2,33,55

[<img src="http://i.stack.imgur.com/rF9SA.png" alt="enter image description here" />](http://i.stack.imgur.com/rF9SA.png)

SELECT 1,22,44
UNION
SELECT 2,33,55
UNION
SELECT 2,33,55

**The result is the same as above.**

use UNION ALL

when

SELECT 1,22,44
UNION
SELECT 2,33,55
UNION ALL
SELECT 2,33,55

[<img src="http://i.stack.imgur.com/tD1Rz.png" alt="enter image description here" />](http://i.stack.imgur.com/tD1Rz.png)



## Combining and merging data on different MySQL tables with the same columns into unique rows and running query


This ****UNION ALL**** combines data from multiple tables and serve as a table name alias to use for your queries:

```sql
SELECT YEAR(date_time_column), MONTH(date_time_column), MIN(DATE(date_time_column)), MAX(DATE(date_time_column)), COUNT(DISTINCT (ip)), COUNT(ip), (COUNT(ip) / COUNT(DISTINCT (ip))) AS Ratio
FROM (
    (SELECT date_time_column, ip FROM server_log_1 WHERE state = 'action' AND log_id = 150) UNION ALL
    (SELECT date_time_column, ip FROM server_log_2 WHERE state = 'action' AND log_id = 150) UNION ALL
    (SELECT date_time_column, ip FROM server_log_3 WHERE state = 'action' AND log_id = 150) UNION ALL
    (SELECT date_time_column, ip FROM server_log WHERE state = 'action' AND log_id = 150)
) AS table_all
GROUP BY YEAR(date_time_column), MONTH(date_time_column);

```



#### Syntax


- UNION DISTINCT -- dedups after combining the SELECTs
- UNION ALL -- non dedup (faster)
- UNION -- the default is DISTINCT
- SELECT ... UNION SELECT ... -- is OK, but ambiguous on `ORDER BY`
- ( SELECT ... ) UNION ( SELECT ... ) ORDER BY ... -- resolves the ambiguity



#### Remarks


UNION does not use multiple CPUs.

UNION always* involves a temp table to collect the results. *As of 5.7.3 / MariaDB 10.1, some forms of UNION deliver the results without using a tmp table (hence, faster).

