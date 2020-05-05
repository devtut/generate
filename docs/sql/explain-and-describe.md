---
metaTitle: "SQL - EXPLAIN and DESCRIBE"
description: "EXPLAIN Select query, DESCRIBE tablename;"
---

# EXPLAIN and DESCRIBE



## EXPLAIN Select query


An `Explain` infront of a `select` query shows you how the query will be executed. This way you to see if the query uses an index or if you could optimize your query by adding an index.

Example query:

```sql
explain select * from user join data on user.test = data.fk_user;

```

Example result:

```sql
id  select_type  table   type    possible_keys  key     key_len ref       rows  Extra
1   SIMPLE       user    index   test           test    5       (null)    1     Using where; Using index
1   SIMPLE       data    ref     fk_user        fk_user 5       user.test 1     (null)

```

on `type` you see if an index was used. In the column `possible_keys` you see if the execution plan can choose from different indexes of if none exists. `key` tells you the acutal used index. `key_len` shows you the size in bytes for one index item. The lower this value is the more index items fit into the same memory size an they can be faster processed. `rows` shows you the expected number of rows the query needs to scan, the lower the better.



## DESCRIBE tablename;


`DESCRIBE` and `EXPLAIN` are synonyms. `DESCRIBE` on a tablename returns the definition of the columns.

```sql
DESCRIBE tablename;

```

Exmple Result:

```sql
COLUMN_NAME     COLUMN_TYPE     IS_NULLABLE     COLUMN_KEY     COLUMN_DEFAULT    EXTRA
id              int(11)         NO              PRI            0                 auto_increment
test            varchar(255)    YES                            (null)     

```

Here you see the column names, followed by the columns type. It shows if `null` is allowed in the column and if the column uses an Index. the default value is also displayed and if the table contains any special behavior like an `auto_increment`.

