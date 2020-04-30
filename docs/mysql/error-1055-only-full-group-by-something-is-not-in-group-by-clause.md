---
metaTitle: "Error 1055: ONLY_FULL_GROUP_BY: something  is not in GROUP BY clause ..."
description: "Misusing GROUP BY to return unpredictable results: Murphy's Law, Misusing GROUP BY with SELECT *, and how to fix it., Using and misusing GROUP BY, ANY_VALUE()"
---

# Error 1055: ONLY_FULL_GROUP_BY: something  is not in GROUP BY clause ...




## Misusing GROUP BY to return unpredictable results: Murphy's Law


```sql
SELECT item.item_id, uses.category,   /* nonstandard */ 
       COUNT(*) number_of_uses 
  FROM item 
  JOIN uses ON item.item_id, uses.item_id
 GROUP BY item.item_id

```

will show the rows in a table called item, and show the count of related rows in a table called uses. It will also show the value of a column called `uses.category`.

This query works in MySQL (before the `ONLY_FULL_GROUP_BY` flag appeared). It uses [MySQL's nonstandard extension to `GROUP BY`](https://dev.mysql.com/doc/refman/5.7/en/group-by-handling.html).

But the query has a problem:  if several rows in the `uses` table match the `ON` condition in the `JOIN` clause, MySQL returns the `category` column from just one of those rows. Which row? The writer of the query, and the user of the application, doesn't get to know that in advance. Formally speaking, it's **unpredictable**: MySQL can return any value it wants.

**Unpredictable** is like **random,** with one significant difference. One might expect a **random** choice to change from time to time.  Therefore, if a choice were random, you might detect it during debugging or testing.  The **unpredictable** result is worse: MySQL returns the same result each time you use the query, **until it doesn't.**  Sometimes it's a new version of the MySQL server that causes a different result. Sometimes it's a growing table causing the problem. What can go wrong, will go wrong, and when you don't expect it. That's called [Murphy's Law](https://en.wikipedia.org/wiki/Murphy%27s_law).

The MySQL team has been working to make it harder for developers to make this mistake. Newer versions of MySQL in the 5.7 sequence have a `sql_mode` flag called `ONLY_FULL_GROUP_BY`. When that flag is set, the MySQL server returns the 1055 error and refuses to run this kind of query.



## Misusing GROUP BY with SELECT *, and how to fix it.


Sometimes a query looks like this, with a `*` in the `SELECT` clause.

```

SELECT item.*,     /* nonstandard */ 
        COUNT(*) number_of_uses
  FROM item 
  JOIN uses ON item.item_id, uses.item_id
 GROUP BY item.item_id

```

Such a query needs to be refactored to comply with the `ONLY_FULL_GROUP_BY` standard.

To do this, we need a subquery that uses `GROUP BY` correctly to return the `number_of_uses` value for each `item_id`.  This subquery is short and sweet, because it only needs to look at the `uses` table.

```

                             SELECT item_id, COUNT(*) number_of_uses
                                FROM  uses 
                               GROUP BY item_id

```

Then, we can join that subquery with the `item` table.

```

SELECT item.*, usecount.number_of_uses
   FROM item
   JOIN (
                              SELECT item_id, COUNT(*) number_of_uses
                                FROM  uses 
                               GROUP BY item_id
        ) usecount ON item.item_id = usecount.item_id

```

This allows the `GROUP BY` clause to be simple and correct, and also allows us to use the `*` specifier.

Note: nevertheless, wise developers avoid using the `*` specifier in any case. It's usually better to list the columns you want in a query.



## Using and misusing GROUP BY


```

SELECT item.item_id, item.name,     /* not SQL-92 */ 
        COUNT(*) number_of_uses
  FROM item 
  JOIN uses ON item.item_id, uses.item_id
 GROUP BY item.item_id

```

will show the rows in a table called `item`, and show the count of related rows in a table called `uses`.  This works well, but unfortunately it's not standard SQL-92.

Why not? because the `SELECT` clause (and the `ORDER BY` clause) in `GROUP BY` queries must contain columns that are

1. mentioned in the `GROUP BY` clause, or
1. aggregate functions such as `COUNT()`, `MIN()`, and the like.

This example's `SELECT` clause mentions `item.name`, a column that does not meet either of those criteria. MySQL 5.6 and earlier will reject this query if the SQL mode contains `ONLY_FULL_GROUP_BY`.

This example query can be made to comply with the SQL-92 standard by changing the `GROUP BY` clause, like this.

```sql
SELECT item.item_id, item.name, 
       COUNT(*) number_of_uses
  FROM item 
  JOIN uses ON item.item_id, uses.item_id
 GROUP BY item.item_id, item.name

```

The later SQL-99 standard allows a `SELECT` statement to omit unaggregated columns from the group key if the DBMS can prove a functional dependence between them and the group key columns. Because `item.name` is functionally dependent on `item.item_id`, the initial example is valid SQL-99. MySQL gained a [functional dependence prover](https://dev.mysql.com/doc/refman/5.7/en/group-by-functional-dependence.html) in version 5.7. The original example works under `ONLY_FULL_GROUP_BY`.



## ANY_VALUE()


```

SELECT item.item_id, ANY_VALUE(uses.tag) tag,   
        COUNT(*) number_of_uses
  FROM item 
  JOIN uses ON item.item_id, uses.item_id
 GROUP BY item.item_id

```

shows the rows in a table called `item`, the count of related rows, and one of the values in the related table called `uses`.

You can think of [this `ANY_VALUE()` function](http://dev.mysql.com/doc/refman/5.7/en/miscellaneous-functions.html#function_any-value) as a strange a kind of aggregate function. Instead of returning a count, sum, or maximum, it instructs the MySQL server to choose, arbitrarily, one value from the group in question. It's a way of working around Error 1055.

Be careful when using `ANY_VALUE()` in queries in production applications.

It really should be called `SURPRISE_ME()`. It returns the value of some row in the GROUP BY group. Which row it returns is indeterminate. That means it's entirely up to the MySQL server. Formally, it returns an unpredictable value.

The server doesn't choose a random value, it's worse than that. It returns the same value every time you run the query, until it doesn't. It can change, or not, when a table grows or shrinks, or when the server has more or less RAM, or when the server version changes, or when Mars is in retrograde (whatever that means), or for no reason at all.

You have been warned.



#### Remarks


For a long time now, MySQL has contained a notorious nonstandard extension to `GROUP BY`, which allows oddball behavior in the name of efficiency. This extension has allowed countless developers around the world to use `GROUP BY` in production code without completely understanding what they were doing.

In particular, it's a bad idea to use `SELECT *` in a `GROUP BY` query, because a standard `GROUP BY` clause requires enumerating the columns. Many developers have, unfortunately, done that.

Read this. [https://dev.mysql.com/doc/refman/5.7/en/group-by-handling.html](https://dev.mysql.com/doc/refman/5.7/en/group-by-handling.html)

The MySQL team has been trying to fix this misfeature without messing up production code. They added a `sql_mode` flag in 5.7.5 named [`ONLY_FULL_GROUP_BY`](http://dev.mysql.com/doc/refman/5.7/en/sql-mode.html) to compel standard behavior.  In a recent release, they turned on that flag by default. When you upgraded your local MySQL to 5.7.14, the flag got switched on and your production code, dependent on the old extension, stopped working.

If you've recently started getting 1055 errors, what are your choices?

1. fix the offending SQL queries, or get their authors to do that.
1. roll back to a version of MySQL compatible out-of-the-box with the application software you use.
1. change your server's [`sql_mode`](http://dev.mysql.com/doc/refman/5.7/en/sql-mode.html) to get rid of the newly set `ONLY_FULL_GROUP_BY` mode.

You can change the mode by doing a `SET` command.

```sql
SET  sql_mode = 'STRICT_TRANS_TABLES,NO_ZERO_IN_DATE,NO_ZERO_DATE,ERROR_FOR_DIVISION_BY_ZERO,NO_AUTO_CREATE_USER,NO_ENGINE_SUBSTITUTION'

```

should do the trick if you do it right after your application connects to MySQL.

Or, you can find [the init file in your MySQL installation](http://dev.mysql.com/doc/refman/5.7/en/server-configuration-defaults.html), locate the `sql_mode=` line, and change it to omit `ONLY_FULL_GROUP_BY`, and restart your server.

