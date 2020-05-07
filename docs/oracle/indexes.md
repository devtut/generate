---
metaTitle: "Oracle Database - Indexes"
description: "b-tree index, Bitmap Index, Function Based Index"
---

# Indexes


Here I will explain different index using example, how index increase query performance, how index decrease DML performance etc



## b-tree index


```sql
CREATE INDEX ord_customer_ix ON orders (customer_id);

```

By default, if we do not mention anything, oracle creates an index as a b-tree index. But we should know when to use it.
B-tree index stores data as binary tree format. As we know that, index is a schema object which stores some sort of entry for each value for the indexed column. So, whenever any search happens on those columns, it checks in the index for the exact location of that record to access fast. Few points about indexing:

- To search for entry in the index, some sort of binary search algorithm used.
- When **data cardinality is high, b-tree** index is perfect to use.
- Index makes DML slow, as for each record, there should be one entry in the index for indexed column.
- So, if not necessary, we should avoid creating index.



## Bitmap Index


```sql
CREATE BITMAP INDEX 
emp_bitmap_idx
ON index_demo (gender);

```


- Bitmap index is used when **data cardinality is low.**
- Here, **Gender** has value with low cardinality. Values are may be Male, Female & others.
- So, if we create a binary tree for this 3 values while searching it will have unnecessary traverse.
- In bitmap structures, a two-dimensional array is created with one column for every row in the table being indexed. Each column represents a distinct value within the bitmapped index. This two-dimensional array represents each value within the index multiplied by the number of rows in the table.
- At row retrieval time, Oracle decompresses the bitmap into the RAM data buffers so it can be rapidly scanned for matching values. These matching values are delivered to Oracle in the form of a Row-ID list, and these Row-ID values may directly access the required information.



## Function Based Index


```sql
CREATE INDEX first_name_idx ON user_data (UPPER(first_name));


SELECT *
FROM   user_data
WHERE  UPPER(first_name) = 'JOHN2';

```


- Function based index means, creating index based on a function.
- If in search (where clause), frequently any function is used, it's better to create index based on that function.
- Here, in the example, for search, **Upper()** function is being used. So, it's better to create index using upper function.

