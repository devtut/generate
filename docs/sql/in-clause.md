---
metaTitle: "SQL - IN clause"
description: "Simple IN clause, Using IN clause with a subquery"
---

# IN clause



## Simple IN clause


To get records having **any** of the given `id`s

```sql
select * 
from products
where id in (1,8,3)

```

The query above is equal to

```sql
select * 
from products
where id = 1
   or id = 8
   or id = 3

```



## Using IN clause with a subquery


```sql
SELECT * 
FROM customers 
WHERE id IN (
    SELECT DISTINCT customer_id 
    FROM orders
);

```

The above will give you all the customers that have orders in the system.

