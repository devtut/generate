---
metaTitle: "PostgreSQL - Aggregate Functions"
description: "Simple statistics: min(), max(), avg(), string_agg(expression, delimiter), regr_slope(Y, X) : slope of the least-squares-fit linear equation determined by the (X, Y) pairs"
---

# Aggregate Functions



## Simple statistics: min(), max(), avg()


In order to determine some simple statistics of a value in a column of a table, you can use an aggregate function.

If your `individuals` table is:

|Name|Age
|---|---|---|---
|Allie|17
|Amanda|14
|Alana|20

You could write this statement to get the minimum, maximum and average value:

```sql
SELECT min(age), max(age), avg(age)
FROM individuals;

```

Result:

|min|max|avg
|---|---|---|---
|14|20|17



## string_agg(expression, delimiter)


You can concatenate strings separated by delimiter using the `string_agg()` function.

If your `individuals` table is:

|Name|Age|Country
|---|---|---|---
|Allie|15|USA
|Amanda|14|USA
|Alana|20|Russia

You could write `SELECT ... GROUP BY` statement to get names from each country:

```sql
SELECT string_agg(name, ', ') AS names, country 
FROM individuals 
GROUP BY country;

```

Note that you need to use a `GROUP BY` clause because `string_agg()` is an aggregate function.

**Result:**

|names|country
|---|---|---|---
|Allie, Amanda|USA
|Alana|Russia

[More PostgreSQL aggregate function described here](https://www.postgresql.org/docs/devel/static/functions-aggregate.html)



## regr_slope(Y, X) : slope of the least-squares-fit linear equation determined by the (X, Y) pairs


To illustrate how to use regr_slope(Y,X), I applied it to a real world problem. In Java, if you don't clean up memory properly, the garbage can get stuck and fill up the memory. You dump statistics every hour about memory utilization of different classes and load it into a postgres database for analysis.

All memory leak candidates will have a trend of consuming more memory as more time passes. If you plot this trend, you would imagine a line going up and to the left:

```

   ^
    |
s   |  Legend:
i   |  *  - data point
z   |  -- - trend
e   |
(   |
b   |                 *
y   |                     --
t   |                  --
e   |             * --    *
s   |           --
)   |       *--      *
    |     --    *
    |  -- *
   --------------------------------------->
                      time

```

Suppose you have a table containing heap dump histogram data (a mapping of classes to how much memory they consume):

```sql
CREATE TABLE heap_histogram (
    -- when the heap histogram was taken
    histwhen timestamp without time zone NOT NULL, 
    -- the object type bytes are referring to
    -- ex: java.util.String
    class character varying NOT NULL,
    -- the size in bytes used by the above class
    bytes integer NOT NULL
);

```

To compute the slope for each class, we group by over the class. The HAVING clause > 0 ensures that we get only candidates with a positive slop (a line going up and to the left). We sort by the slope descending so that we get the classes with the largest rate of memory increase at the top.

```sql
-- epoch returns seconds
SELECT class, REGR_SLOPE(bytes,extract(epoch from histwhen)) as slope
    FROM public.heap_histogram
    GROUP BY class
    HAVING REGR_SLOPE(bytes,extract(epoch from histwhen)) > 0
    ORDER BY slope DESC ;

```

Output:

```

        class             |        slope         
---------------------------+----------------------
 java.util.ArrayList       |     71.7993806279174
 java.util.HashMap         |     49.0324576155785
 java.lang.String          |     31.7770770326123
 joe.schmoe.BusinessObject |     23.2036817108056
 java.lang.ThreadLocal     |     20.9013528767851

```

From the output we see that java.util.ArrayList's memory consumption is increasing the fastest at 71.799 bytes per second and is potentially part of the memory leak.

