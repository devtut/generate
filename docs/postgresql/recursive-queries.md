---
metaTitle: "Recursive queries"
description: "Sum of Integers"
---

# Recursive queries


There are no real recursive querys!



## Sum of Integers


```sql
WITH RECURSIVE t(n) AS (
    VALUES (1)
  UNION ALL
    SELECT n+1 FROM t WHERE n < 100
)
SELECT sum(n) FROM t;

```

[Link to Documentation](https://www.postgresql.org/docs/9.6/static/queries-with.html)

