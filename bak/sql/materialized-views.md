---
metaTitle: "SQL - Materialized Views"
description: "PostgreSQL example"
---

# Materialized Views




## PostgreSQL example


```sql
CREATE TABLE mytable (number INT);
INSERT INTO mytable VALUES (1);

CREATE MATERIALIZED VIEW myview AS SELECT * FROM mytable;

SELECT * FROM myview;
 number 
--------
      1
(1 row)

INSERT INTO mytable VALUES(2);

SELECT * FROM myview;
 number 
--------
      1
(1 row)

REFRESH MATERIALIZED VIEW myview;

SELECT * FROM myview;
 number 
--------
      1
      2
(2 rows)

```

