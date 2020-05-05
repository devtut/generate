---
metaTitle: "SQL - SQL Group By vs Distinct"
description: "Difference between GROUP BY and DISTINCT"
---

# SQL Group By vs Distinct



## Difference between GROUP BY and DISTINCT


`GROUP BY` is used in combination with aggregation functions. Consider the following table:

|orderId|userId|storeName|orderValue|orderDate
|---|---|---|---|---|---|---|---|---
|1|43|Store A|25|20-03-2016
|2|57|Store B|50|22-03-2016
|3|43|Store A|30|25-03-2016
|4|82|Store C|10|26-03-2016
|5|21|Store A|45|29-03-2016

The query below uses `GROUP BY` to perform aggregated calculations.

```sql
SELECT
    storeName,
    COUNT(*) AS total_nr_orders,
    COUNT(DISTINCT userId) AS nr_unique_customers,
    AVG(orderValue) AS average_order_value,
    MIN(orderDate) AS first_order,
    MAX(orderDate) AS lastOrder
FROM
    orders
GROUP BY
    storeName;

```

and will return the following information

|storeName|total_nr_orders|nr_unique_customers|average_order_value|first_order|lastOrder
|---|---|---|---|---|---|---|---|---
|Store A|3|2|33.3|20-03-2016|29-03-2016
|Store B|1|1|50|22-03-2016|22-03-2016
|Store C|1|1|10|26-03-2016|26-03-2016

While `DISTINCT` is used to list a unique combination of distinct values for the specified columns.

```sql
SELECT DISTINCT
    storeName,
    userId
FROM
    orders;

```

|storeName|userId
|---|---|---|---|---|---|---|---|---
|Store A|43
|Store B|57
|Store C|82
|Store A|21

