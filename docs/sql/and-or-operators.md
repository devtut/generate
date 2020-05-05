---
metaTitle: "SQL - AND & OR Operators"
description: "AND OR Example"
---

# AND & OR Operators



## AND OR Example


Have a table

|Name|Age|City
|---|---|---
|Bob|10|Paris
|Mat|20|Berlin
|Mary|24|Prague

```sql
select Name from table where Age>10 AND City='Prague'

```

Gives

|Name
|---
|Mary

```sql
select Name from table where Age=10 OR City='Prague'

```

Gives

|Name
|---
|Bob
|Mary



#### Syntax


<li>
SELECT * FROM table WHERE (condition1) AND (condition2);
</li>
<li>
SELECT * FROM table WHERE (condition1) OR (condition2);
</li>

