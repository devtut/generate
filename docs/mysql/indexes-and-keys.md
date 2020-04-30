---
metaTitle: "Indexes and Keys"
description: "Create index, Create unique index, Drop index, Create composite index, AUTO_INCREMENT key"
---

# Indexes and Keys



## Create index


```sql
-- Create an index for column 'name' in table 'my_table'
CREATE INDEX idx_name ON my_table(name);

```



## Create unique index


A unique index prevents the insertion of duplicated data in a table. `NULL` values can be inserted in the columns that form part of the unique index (since, by definition, a `NULL` value is different from any other value, including another `NULL` value)

```sql
-- Creates a unique index for column 'name' in table 'my_table'
CREATE UNIQUE INDEX idx_name ON my_table(name);

```



## Drop index


```sql
-- Drop an index for column 'name' in table 'my_table'
DROP INDEX idx_name ON my_table;

```



## Create composite index


This will create a composite index of both keys, `mystring` and `mydatetime` and speed up queries with both columns in the `WHERE` clause.

```sql
CREATE INDEX idx_mycol_myothercol ON my_table(mycol, myothercol)

```

**Note:** The order is important! If the search query does not include both columns in the `WHERE` clause, it can only use the leftmost index. In this case, a query with `mycol` in the `WHERE` will use the index, a query searching for `myothercol` **without** also searching for `mycol` will **not**. For more information [check out this blog post](https://www.percona.com/blog/2009/06/05/a-rule-of-thumb-for-choosing-column-order-in-indexes/).

**Note:** Due to the way BTREE's work, columns that are usually queried in ranges should go in the rightmost value. For example, `DATETIME` columns are usualy queried like `WHERE datecol > '2016-01-01 00:00:00'`. BTREE indexes handle ranges very efficiently but only if the column being queried as a range is the last one in the composite index.



## AUTO_INCREMENT key


```sql
CREATE TABLE (
    id INT UNSIGNED NOT NULL AUTO_INCREMENT,
    ...
    PRIMARY KEY(id),
    ...  );

```

Main notes:

- Starts with 1 and increments by 1 automatically when you fail to specify it on `INSERT`, or specify it as `NULL`.
- The ids are always distinct from each other, but...
- Do not make any assumptions (no gaps, consecutively generated, not reused, etc) about the values of the id other than being unique at any given instant.

Subtle notes:

- On restart of server, the 'next' value is 'computed' as `MAX(id)+1`.
- If the last operation before shutdown or crash was to delete the highest id, that id **may** be reused (this is engine-dependent).  So, **do not trust auto_increments to be permanently unique**; they are only unique at any moment.
- For multi-master or clustered solutions, see `auto_increment_offset` and `auto_increment_increment`.
- It is OK to have something else as the `PRIMARY KEY` and simply do `INDEX(id)`.  (This is an optimization in some situations.)
- Using the `AUTO_INCREMENT` as the "`PARTITION` key" is rarely beneficial; do something different.
- Various operations **may** "burn" values.  This happens when they pre-allocate value(s), then don't use them: `INSERT IGNORE` (with dup key), `REPLACE` (which is `DELETE` plus `INSERT`) and others.  `ROLLBACK` is another cause for gaps in ids.
- In Replication, you cannot trust ids to arrive at the slave(s) in ascending order.  Although ids are assigned in consecutive order, InnoDB statements are sent to slaves in `COMMIT` order.



#### Syntax


<li>
-- Create simple index
CREATE INDEX **index_name** ON **table_name**(**column_name1** [, **column_name2**, ...])
</li>
<li>
-- Create unique index
CREATE UNIQUE INDEX **index_name** ON **table_name**(**column_name1** [, **column_name2**, ...]
</li>
<li>
-- Drop index
<p>DROP INDEX **index_name** ON **tbl_name**
[**algorithm_option** | **lock_option**] ...</p>
<p>**algorithm_option:**
ALGORITHM [=] {DEFAULT|INPLACE|COPY}</p>
<p>**lock_option:**
LOCK [=] {DEFAULT|NONE|SHARED|EXCLUSIVE}</p>
</li>



#### Remarks


### Concepts

An index in a MySQL table works like an index in a book.

Let's say you have a book about databases and you want to find some information about, say, storage. Without an index (assuming no other aid, such as a table of contents) you'd have to go through the pages one by one, until you found the topic (that's a "full table scan"). On the other hand, an index has a list of keywords, so you'd consult the index and see that storage is mentioned on pages 113-120, 231, and 354. Then you could flip to those pages directly, without searching (that's a search with an index, somewhat faster).

Of course, the usefulness of the index depends on many things - a few examples, using the simile above:

- If you had a book on databases and indexed the word "database", you might see that it's mentioned on pages 1-59, 61-290, and 292-400. That's a lot of pages, and in such a case, the index is not much help and it might be faster to go through the pages one by one. (In a database, this is "poor selectivity".)
- For a 10-page book, it makes no sense to make an index, as you may end up with a 10-page book prefixed by a 5-page index, which is just silly - just scan the 10 pages and be done with it.
- The index also needs to be useful - there's generally no point to indexing, for example, the frequency of the letter "L" per page.

