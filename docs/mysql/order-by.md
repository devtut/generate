---
metaTitle: "ORDER BY"
description: "Contexts, Basic, ASCending / DESCending, Some tricks"
---

# ORDER BY



## Contexts


The clauses in a `SELECT` have a specific order:

```sql
SELECT ... FROM ... WHERE ... GROUP BY ... HAVING ...
    ORDER BY ...  -- goes here
    LIMIT ... OFFSET ...;

( SELECT ... ) UNION ( SELECT ... ) ORDER BY ...  -- for ordering the result of the UNION.

SELECT ... GROUP_CONCAT(DISTINCT x ORDER BY ... SEPARATOR ...) ...

ALTER TABLE ... ORDER BY ... -- probably useful only for MyISAM; not for InnoDB

```



## Basic


ORDER BY x

`x` can be any datatype.

- `NULLs` precede non-NULLs.
- The default is `ASC` (lowest to highest)
- Strings (`VARCHAR`, etc) are ordered according the `COLLATION` of the declaration
- `ENUMs` are ordered by the declaration order of its strings.



## ASCending / DESCending


```sql
ORDER BY x ASC  -- same as default
ORDER BY x DESC  -- highest to lowest
ORDER BY lastname, firstname  -- typical name sorting; using two columns
ORDER BY submit_date DESC  -- latest first
ORDER BY submit_date DESC, id ASC  -- latest first, but fully specifying order.

```


- `ASC` = `ASCENDING`, `DESC` = `DESCENDING`
- `NULLs` come first even for `DESC`.
- In the above examples, `INDEX(x)`, `INDEX(lastname, firstname)`, `INDEX(submit_date)`  may significantly improve performance.

But... Mixing `ASC` and `DESC`, as in the last example, cannot use a composite index to benefit.  Nor will `INDEX(submit_date DESC, id ASC)` help -- "`DESC`" is recognized syntactically in the `INDEX` declaration, but ignored.



## Some tricks


```sql
ORDER BY FIND_IN_SET(card_type, "MASTER-CARD,VISA,DISCOVER") -- sort 'MASTER-CARD' first.
ORDER BY x IS NULL, x  -- order by `x`, but put `NULLs` last.

```

**Custom ordering**

```sql
SELECT * FROM some_table WHERE id IN (118, 17, 113, 23, 72) 
ORDER BY FIELD(id, 118, 17, 113, 23, 72);

```

Returns the result in the specified order of ids.

|id|...
|---|---|---|---
|118|...
|17|...
|113|...
|23|...
|72|...

Useful if the ids are already sorted and you just need to retrieve the rows.

