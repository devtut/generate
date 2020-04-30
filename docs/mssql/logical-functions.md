---
metaTitle: "Logical Functions"
description: "CHOOSE, IIF"
---

# Logical Functions




## CHOOSE


Returns the item at the specified index from a list of values. If `index` exceeds the bounds of `values` then `NULL` is returned.

Parameters:

1. `index`: integer, index to item in `values`. 1-based.
1. `values`: any type, comma separated list

```sql
SELECT CHOOSE (1, 'apples', 'pears', 'oranges', 'bananas') AS chosen_result

chosen_result
-------------
apples

```



## IIF


Returns one of two values, depending on whether a given Boolean expression evaluates to true or false.

Parameters:

1. `boolean_expression` evaluated to dtermine what value to return
1. `true_value` returned if `boolean_expression` evaluates to true
1. `false_value` returned if `boolean_expression` evaluates to false

```sql
SELECT IIF (42 > 23, 'I knew that!', 'That is not true.') AS iif_result

iif_result
------------
I knew that!

```

`IIF` may be replaced by a `CASE` statement. The above example my be written as

```sql
SELECT CASE WHEN 42 > 23 THEN 'I knew that!' ELSE 'That is not true.' END AS iif_result

iif_result
------------
I knew that!

```

