---
metaTitle: "Limit and Offset"
description: "Limit and Offset relationship"
---

# Limit and Offset




## Limit and Offset relationship


Considering the following `users` table:

|id|username
|---|---|---|---
|1|User1
|2|User2
|3|User3
|4|User4
|5|User5

In order to constrain the number of rows in the result set of a [`SELECT` query](http://dev.mysql.com/doc/refman/5.7/en/select.html), the `LIMIT` clause can be used together with one or two positive integers as arguments (zero included).

### `LIMIT` clause with one argument

When one argument is used, the result set will only be constrained to the number specified in the following manner:

```sql
SELECT * FROM users ORDER BY id ASC LIMIT 2

```

|id|username
|---|---|---|---
|1|User1
|2|User2

If the argument's value is `0`, the result set will be empty.

Also notice that the `ORDER BY` clause may be important in order to specify the first rows of the result set that will be presented (when ordering by another column).

### `LIMIT`clause with two arguments

When two arguments are used in a `LIMIT` clause:

- the **first** argument represents the row from which the result set rows will be presented – this number is often mentioned as an **offset**, since it represents the row previous to the initial row of the constrained result set. This allows the argument to receive `0` as value and thus taking into consideration the first row of the non-constrained result set.
- the **second** argument specifies the maximum number of rows to be returned in the result set (similarly to the one argument's example).

Therefore the query:

```sql
SELECT * FROM users ORDER BY id ASC LIMIT 2, 3

```

Presents the following result set:

|id|username
|---|---|---|---
|3|User3
|4|User4
|5|User5

Notice that when the **offset** argument is `0`, the result set will be equivalent to a one argument `LIMIT` clause. This means that the following 2 queries:

```sql
SELECT * FROM users ORDER BY id ASC LIMIT 0, 2

SELECT * FROM users ORDER BY id ASC LIMIT 2

```

Produce the same result set:

|id|username
|---|---|---|---
|1|User1
|2|User2

### `OFFSET` keyword: alternative syntax

An alternative syntax for the `LIMIT` clause with two arguments consists in the usage of the `OFFSET` keyword after the first argument in the following manner:

```sql
SELECT * FROM users ORDER BY id ASC LIMIT 2 OFFSET 3

```

This query would return the following result set:

|id|username
|---|---|---|---
|3|User3
|4|User4

Notice that in this alternative syntax the arguments have their positions switched:

<li>
the **first** argument represents the number of rows to be returned in the result set;
</li>
<li>
the **second** argument represents the offset.
</li>



#### Syntax


<li>SELECT column_1 [, column_2 ]<br />
FROM table_1<br />
ORDER BY order_column<br />
LIMIT row_count [OFFSET row_offset]</li>
<li>SELECT column_1 [, column_2 ]<br />
FROM table_1<br />
ORDER BY order_column<br />
LIMIT [row_offset,] row_count</li>



#### Remarks


"Limit" could mean "Max number of rows in a table".

"Offset" mean pick from `row` number (not to be confused by primary key value or any field data value)

