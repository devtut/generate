---
metaTitle: "MySQL - Performance Tuning"
description: "Add the correct index, Don't hide in function, OR, Set the cache correctly, Negatives, Have an INDEX, Subqueries, JOIN + GROUP BY, Avoid inefficient constructs"
---

# Performance Tuning



## Add the correct index


This is a huge topic, but it is also the most important "performance" issue.

The main lesson for a novice is to learn of "composite" indexes.  Here's a quick example:

```sql
INDEX(last_name, first_name)

```

is excellent for these:

```sql
WHERE last_name = '...'
WHERE first_name = '...' AND last_name = '...'   -- (order in WHERE does not matter)

```

but not for

```sql
WHERE first_name = '...'   -- order in INDEX _does_ matter
WHERE last_name = '...' OR first_name = '...'   -- "OR" is a killer

```



## Don't hide in function


A common mistake is to hide an indexed column inside a function call.  For example, this can't be helped by an index:

```sql
WHERE DATE(dt) = '2000-01-01'

```

Instead, given `INDEX(dt)` then these may use the index:

```sql
WHERE dt = '2000-01-01'  -- if `dt` is datatype `DATE`

```

This works for `DATE`, `DATETIME`, `TIMESTAMP`, and even `DATETIME(6)` (microseconds):

```sql
WHERE dt >= '2000-01-01'
  AND dt  < '2000-01-01' + INTERVAL 1 DAY

```



## OR


In general `OR` kills optimization.

```sql
WHERE a = 12 OR b = 78

```

cannot use `INDEX(a,b)`, and may or may not use `INDEX(a), INDEX(b)` via "index merge".  Index merge is better than nothing, but only barely.

```sql
WHERE x = 3 OR x = 5

```

is turned into

```sql
WHERE x IN (3, 5)

```

which **may** use an index with `x` in it.



## Set the cache correctly


`innodb_buffer_pool_size` should be about 70% of available RAM.



## Negatives


Here are some things that are not likely to help performance.  They stem from out-of-date information and/or naivety.

- InnoDB has improved to the point where MyISAM is unlikely to be better.
- `PARTITIONing` rarely provides performance benefits; it can even hurt performance.
- Setting `query_cache_size` bigger than 100M will usually **hurt** performance.
- Increasing lots of values in `my.cnf` may lead to 'swapping', which is a **serious** performance problem.
- "Prefix indexes" (such as `INDEX(foo(20))`) are generally useless.
- `OPTIMIZE TABLE` is almost always useless.  (And it involves locking the table.)



## Have an INDEX


The most important thing for speeding up a query on any non-tiny table is to have a suitable index.

```sql
WHERE a = 12  --> INDEX(a)
WHERE a > 12  --> INDEX(a)

WHERE a = 12 AND b > 78  --> INDEX(a,b) is more useful than INDEX(b,a)
WHERE a > 12 AND b > 78  --> INDEX(a) or INDEX(b); no way to handle both ranges

ORDER BY x  --> INDEX(x)
ORDER BY x, y  --> INDEX(x,y) in that order
ORDER BY x DESC, y ASC  --> No index helps - because of mixing ASC and DESC

```



## Subqueries


Subqueries come in several flavors, and they have different optimization potential.  First, note that subqueries can be either "correlated" or "uncorrelated".  Correlated means that they depend on some value from outside the subquery.  This generally implies that the subquery **must** be re-evaluated for each outer value.

This correlated subquery is often pretty good.  Note: It must return at most 1 value.  It is often useful as an alternative to, though not necessarily faster than, a `LEFT JOIN`.

```sql
SELECT a, b, ( SELECT ... FROM t WHERE t.x = u.x ) AS c
    FROM u ...
SELECT a, b, ( SELECT MAX(x) ... ) AS c
    FROM u ...
SELECT a, b, ( SELECT x FROM t ORDER BY ... LIMIT 1 ) AS c
    FROM u ...

```

This is usually uncorrelated:

```sql
SELECT ...
    FROM ( SELECT ... ) AS a
    JOIN b ON ...

```

Notes on the `FROM-SELECT`:

- If it returns 1 row, great.
- A good paradigm (again "1 row") is for the subquery to be `( SELECT @n := 0 )`, thereby initializing an `@variable for use in the rest or the query.
- If it returns many rows **and** the `JOIN` also is `( SELECT ... )` with many rows, then efficiency can be terrible.  Pre-5.6, there was no index, so it became a `CROSS JOIN`; 5.6+ involves deducing the best index on the temp tables and then generating it, only to throw it away when finished with the `SELECT`.



## JOIN + GROUP BY


A common problem that leads to an inefficient query goes something like this:

```sql
SELECT ...
    FROM a
    JOIN b  ON ...
    WHERE ...
    GROUP BY a.id

```

First, the `JOIN` expands the number of rows; then the `GROUP BY` whittles it back down the the number of rows in `a`.

There may not be any good choices to solve this explode-implode problem.  One possible option is to turn the `JOIN` into a correlated subquery in the `SELECT`.  This also eliminates the `GROUP BY`.



## Avoid inefficient constructs


```sql
x IN ( SELECT ... )

```

turn into a `JOIN`

When possible, avoid `OR`.

Do not 'hide' an indexed column in a function, such as `WHERE DATE(x) = ...`; reformulate as `WHERE x = ...`

You can generally avoid `WHERE LCASE(name1) = LCASE(name2)` by having a suitable collation.

Do no use `OFFSET` for "pagination", instead 'remember where you left off'.

Avoid `SELECT * ...` (unless debugging).

**Note to Maria Deleva, Barranka, Batsu: This is a place holder; please make remove these items as you build full-scale examples.  After you have done the ones you can, I will move in to elaborate on the rest and/or toss them.**



#### Syntax


<li>
Don't use DISTINCT and GROUP BY in the same SELECT.
</li>
<li>
Don't paginate via OFFSET, "remember where you left off".
</li>
<li>
WHERE (a,b) = (22,33) does not optimize at all.
</li>
<li>
Explicitly say ALL or DISTINCT after UNION -- it reminds you pick between the faster ALL or the slower DISTINCT.
</li>
<li>
Don't use SELECT *, especially if you have TEXT or BLOB columns that you don't need. There is overhead in tmp tables and transmission.
</li>
<li>
It is faster when the GROUP BY and ORDER BY can have exactly the same list.
</li>
<li>
Don't use FORCE INDEX; it may help today, but will probably hurt tomorrow.
</li>



#### Remarks


See also discussions about ORDER BY, LIKE, REGEXP, etc. Note: this needs editing with links and more Topics.

[**Cookbook on building optimal indexes**](https://mariadb.com/kb/en/mariadb/building-the-best-index-for-a-given-select/).

