---
metaTitle: "Window Functions"
description: "generic example, column values vs dense_rank vs rank vs row_number"
---

# Window Functions



## generic example


Preparing data:

```sql
create table wf_example(i int, t text,ts timestamptz,b boolean);
insert into wf_example select 1,'a','1970.01.01',true;
insert into wf_example select 1,'a','1970.01.01',false;
insert into wf_example select 1,'b','1970.01.01',false;
insert into wf_example select 2,'b','1970.01.01',false;
insert into wf_example select 3,'b','1970.01.01',false;
insert into wf_example select 4,'b','1970.02.01',false;
insert into wf_example select 5,'b','1970.03.01',false;
insert into wf_example select 2,'c','1970.03.01',true;

```

Running:

```sql
select *
  , dense_rank() over (order by i) dist_by_i 
  , lag(t) over () prev_t 
  , nth_value(i, 6) over () nth
  , count(true) over (partition by i) num_by_i 
  , count(true) over () num_all
  , ntile(3) over() ntile
from wf_example
;

```

Result:

```

i | t |           ts           | b | dist_by_i | prev_t | nth | num_by_i | num_all | ntile
---+---+------------------------+---+-----------+--------+-----+----------+---------+-------
 1 | a | 1970-01-01 00:00:00+01 | f |         1 |        |   3 |        3 |       8 |     1
 1 | a | 1970-01-01 00:00:00+01 | t |         1 | a      |   3 |        3 |       8 |     1
 1 | b | 1970-01-01 00:00:00+01 | f |         1 | a      |   3 |        3 |       8 |     1
 2 | c | 1970-03-01 00:00:00+01 | t |         2 | b      |   3 |        2 |       8 |     2
 2 | b | 1970-01-01 00:00:00+01 | f |         2 | c      |   3 |        2 |       8 |     2
 3 | b | 1970-01-01 00:00:00+01 | f |         3 | b      |   3 |        1 |       8 |     2
 4 | b | 1970-02-01 00:00:00+01 | f |         4 | b      |   3 |        1 |       8 |     3
 5 | b | 1970-03-01 00:00:00+01 | f |         5 | b      |   3 |        1 |       8 |     3
(8 rows)

```

Explanation:

**dist_by_i**: `dense_rank() over (order by i)` is like a row_number per distinct values. Can be used for the number of distinct values of i (`count(DISTINCT i)` wold not work). Just use the maximum value.

**prev_t**: `lag(t) over ()` is a previous value of t over the whole window. mind that it is null for the first row.

**nth**: `nth_value(i, 6) over ()` is the value of sixth rows column i over the whole window

**num_by_i**: `count(true) over (partition by i)` is an amount of rows for each value of i

**num_all**: `count(true) over ()`  is an amount of rows over a whole window

**ntile**: `ntile(3) over()` splits the whole window to 3 (as much as possible) equal in quantity parts



## column values vs dense_rank vs rank vs row_number


[here](https://www.postgresql.org/docs/current/static/functions-window.html) you can find the functions.

With the table wf_example created in previous example, run:

```sql
select i
  , dense_rank() over (order by i)
  , row_number() over ()
  , rank() over (order by i)
from wf_example

```

The result is:

```

i | dense_rank | row_number | rank
---+------------+------------+------
 1 |          1 |          1 |    1
 1 |          1 |          2 |    1
 1 |          1 |          3 |    1
 2 |          2 |          4 |    4
 2 |          2 |          5 |    4
 3 |          3 |          6 |    6
 4 |          4 |          7 |    7
 5 |          5 |          8 |    8

```


<li>
**dense_rank** orders **VALUES** of **i** by appearance in window. `i=1` appears, so first row has dense_rank, next and third i value does not change, so it is `dense_rank` shows **1** - FIRST value not changed. fourth row `i=2`, it is second value of **i** met, so `dense_rank` shows 2, andso for the next row. Then it meets value `i=3` at 6th row, so it show 3. Same for the rest two values of **i**. So the last value of `dense_rank` is the number of distinct values of **i**.
</li>
<li>
**row_number** orders **ROWS** as they are listed.
</li>
<li>
**rank** Not to confuse with `dense_rank` this function orders **ROW NUMBER** of **i** values. So it starts same with three ones, but has next value 4, which means `i=2` (new value) was met at row 4. Same `i=3` was met at row 6. Etc..
</li>

