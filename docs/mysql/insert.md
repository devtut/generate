---
metaTitle: "INSERT"
description: "INSERT, ON DUPLICATE KEY UPDATE, Inserting multiple rows, Basic Insert, INSERT with AUTO_INCREMENT + LAST_INSERT_ID(), INSERT SELECT (Inserting data from another Table), Lost AUTO_INCREMENT ids"
---

# INSERT




## INSERT, ON DUPLICATE KEY UPDATE


```sql
INSERT INTO `table_name` 
    (`index_field`, `other_field_1`, `other_field_2`) 
    VALUES 
    ('index_value', 'insert_value', 'other_value') 
    ON DUPLICATE KEY UPDATE 
        `other_field_1` = 'update_value',
        `other_field_2` = VALUES(`other_field_2`);

```

This will `INSERT` into `table_name` the specified values, but if the unique key already exists, it will update the `other_field_1` to have a new value.<br />
Sometimes, when updating on duplicate key it comes in handy to use [`VALUES()`](http://dev.mysql.com/doc/refman/5.7/en/miscellaneous-functions.html#function_values) in order to access the original value that was passed to the `INSERT` instead of setting the value directly. This way, you can set different values by using `INSERT` and  `UPDATE`. See  the example above where `other_field_1` is set to `insert_value` on `INSERT` or to `update_value` on `UPDATE` while `other_field_2` is always set to `other_value`.

Crucial for the Insert on Duplicate Key Update (IODKU) to work is the schema containing a unique key that will signal a duplicate clash. This unique key can be a Primary Key or not. It can be a unique key on a single column, or a multi-column (composite key).



## Inserting multiple rows


```sql
INSERT INTO `my_table` (`field_1`, `field_2`) VALUES 
    ('data_1', 'data_2'),
    ('data_1', 'data_3'),
    ('data_4', 'data_5'); 

```

This is an easy way to add several rows at once with one `INSERT` statement.

This kind of 'batch' insert is much faster than inserting rows one by one.  Typically, inserting 100 rows in a single batch insert this way is 10 times as fast as inserting them all individually.

### Ignoring existing rows

When importing large datasets, it may be preferable under certain circumstances to skip rows that would usually cause the query to fail due to a column restraint e.g. duplicate primary keys. This can be done using `INSERT IGNORE`.

Consider following example database:

```sql
SELECT * FROM `people`;
--- Produces:
+----+------+
| id | name |
+----+------+
|  1 | john |
|  2 | anna |
+----+------+


INSERT IGNORE INTO `people` (`id`, `name`) VALUES
    ('2', 'anna'), --- Without the IGNORE keyword, this record would produce an error
    ('3', 'mike');

SELECT * FROM `people`;
--- Produces:
+----+--------+
| id |  name  |
+----+--------+
|  1 |  john  |
|  2 |  anna  |
|  3 |  mike  |
+----+--------+

```

The important thing to remember is that **INSERT IGNORE** will also silently skip other errors too, here is what Mysql official documentations says:

> 
Data conversions that would trigger errors abort the statement if IGNORE is not > specified. With IGNORE, invalid values are adjusted to the closest values and >inserted; warnings are produced but the statement does not abort.


**Note :- The section below is added for the sake of completeness, but is not considered best practice  (this would fail, for example, if another column was added into the table).**

If you specify the value of the corresponding column for all columns in the table, you can ignore the column list in the `INSERT` statement as follows:

```sql
INSERT INTO `my_table` VALUES 
        ('data_1', 'data_2'),
        ('data_1', 'data_3'),
        ('data_4', 'data_5');

```



## Basic Insert


```sql
INSERT INTO `table_name` (`field_one`, `field_two`) VALUES ('value_one', 'value_two');

```

In this trivial example, `table_name` is where the data are to be added, `field_one` and `field_two` are fields to set data against, and `value_one` and `value_two` are the data to do against `field_one` and `field_two` respectively.

It's good practice to list the fields you are inserting data into within your code, as if the table changes and new columns are added, your insert would break should they not be there



## INSERT with AUTO_INCREMENT + LAST_INSERT_ID()


When a table has an `AUTO_INCREMENT` `PRIMARY KEY`, normally one does not insert into that column.  Instead, specify all the other columns, then ask what the new id was.

```sql
CREATE TABLE t (
    id SMALLINT UNSIGNED AUTO_INCREMENT NOT NULL,
    this ...,
    that ...,
    PRIMARY KEY(id) );

INSERT INTO t (this, that) VALUES (..., ...);
SELECT LAST_INSERT_ID() INTO @id;
INSERT INTO another_table (..., t_id, ...) VALUES (..., @id, ...);

```

Note that `LAST_INSERT_ID()` is tied to the session, so even if multiple connections are inserting into the same table, each with get its own id.

Your client API probably has an alternative way of getting the `LAST_INSERT_ID()` without actually performing a `SELECT` and handing the value back to the client instead of leaving it in an `@variable` inside MySQL.  Such is usually preferable.

**Longer, more detailed, example**

The "normal" usage of IODKU is to trigger "duplicate key" based on some `UNIQUE` key, not the `AUTO_INCREMENT PRIMARY KEY`.  The following demonstrates such.  Note that it does **not** supply the `id` in the INSERT.

Setup for examples to follow:

```sql
CREATE TABLE iodku (
    id INT AUTO_INCREMENT NOT NULL,
    name VARCHAR(99) NOT NULL,
    misc INT NOT NULL,
    PRIMARY KEY(id),
    UNIQUE(name)
) ENGINE=InnoDB;

INSERT INTO iodku (name, misc)
    VALUES
    ('Leslie', 123),
    ('Sally', 456);
Query OK, 2 rows affected (0.00 sec)
Records: 2  Duplicates: 0  Warnings: 0
+----+--------+------+
| id | name   | misc |
+----+--------+------+
|  1 | Leslie |  123 |
|  2 | Sally  |  456 |
+----+--------+------+

```

The case of IODKU performing an "update" and `LAST_INSERT_ID()` retrieving the relevant `id`:

```sql
INSERT INTO iodku (name, misc)
    VALUES
    ('Sally', 3333)            -- should update
    ON DUPLICATE KEY UPDATE    -- `name` will trigger "duplicate key"
    id = LAST_INSERT_ID(id),
    misc = VALUES(misc);
SELECT LAST_INSERT_ID();       -- picking up existing value
+------------------+
| LAST_INSERT_ID() |
+------------------+
|                2 |
+------------------+

```

The case where IODKU performs an "insert" and `LAST_INSERT_ID()` retrieves the new `id`:

```sql
INSERT INTO iodku (name, misc)
    VALUES
    ('Dana', 789)          -- Should insert
    ON DUPLICATE KEY UPDATE
    id = LAST_INSERT_ID(id),
    misc = VALUES(misc);
SELECT LAST_INSERT_ID();   -- picking up new value
+------------------+
| LAST_INSERT_ID() |
+------------------+
|                3 |
+------------------+

```

Resulting table contents:

```sql
SELECT * FROM iodku;
+----+--------+------+
| id | name   | misc |
+----+--------+------+
|  1 | Leslie |  123 |
|  2 | Sally  | 3333 |  -- IODKU changed this
|  3 | Dana   |  789 |  -- IODKU added this
+----+--------+------+

```



## INSERT SELECT (Inserting data from another Table)


This is the basic way to insert data from another table with the SELECT statement.

```sql
INSERT INTO `tableA` (`field_one`, `field_two`) 
   SELECT `tableB`.`field_one`, `tableB`.`field_two` 
   FROM `tableB` 
   WHERE `tableB`.clmn <> 'someValue'
   ORDER BY `tableB`.`sorting_clmn`;

```

You can `SELECT * FROM`, but then `tableA` and `tableB` **must** have matching column count and corresponding datatypes.

Columns with `AUTO_INCREMENT` are treated as in the `INSERT` with `VALUES` clause.

This syntax makes it easy to fill (temporary) tables with data from other tables, even more so when the data is to be filtered on the insert.



## Lost AUTO_INCREMENT ids


Several 'insert' functions can "burn" ids.  Here is an example, using InnoDB (other Engines may work differently):

```sql
CREATE TABLE Burn (
    id SMALLINT UNSIGNED AUTO_INCREMENT NOT NULL,
    name VARCHAR(99) NOT NULL,
    PRIMARY KEY(id),
    UNIQUE(name)
        ) ENGINE=InnoDB;

INSERT IGNORE INTO Burn (name) VALUES ('first'), ('second');
SELECT LAST_INSERT_ID();          -- 1
SELECT * FROM Burn ORDER BY id;
  +----+--------+
  |  1 | first  |
  |  2 | second |
  +----+--------+

INSERT IGNORE INTO Burn (name) VALUES ('second');  -- dup 'IGNOREd', but id=3 is burned
SELECT LAST_INSERT_ID();          -- Still "1" -- can't trust in this situation
SELECT * FROM Burn ORDER BY id;
  +----+--------+
  |  1 | first  |
  |  2 | second |
  +----+--------+

INSERT IGNORE INTO Burn (name) VALUES ('third');
SELECT LAST_INSERT_ID();           -- now "4"
SELECT * FROM Burn ORDER BY id;    -- note that id=3 was skipped over
  +----+--------+
  |  1 | first  |
  |  2 | second |
  |  4 | third  |    -- notice that id=3 has been 'burned'
  +----+--------+

```

Think of it (roughly) this way:  First the insert looks to see how many rows **might** be inserted.  Then grab that many values from the auto_increment for that table.  Finally, insert the rows, using ids as needed, and burning any left overs.

The only time the leftover are recoverable is if the system is shutdown and restarted.  On restart, effectively `MAX(id)` is performed.  This may reuse ids that were burned or that were freed up by `DELETEs` of the highest id(s).

Essentially any flavor of `INSERT` (including `REPLACE`, which is `DELETE` + `INSERT`) can burn ids.  In InnoDB, the global (not session!) variable [`innodb_autoinc_lock_mode`](https://dev.mysql.com/doc/refman/5.7/en/innodb-auto-increment-handling.html) can be used to control some of what is going on.

When "normalizing" long strings into an `AUTO INCREMENT id`, burning can easily happen.  This **could** lead to overflowing the size of the `INT` you chose.



#### Syntax


<li>
<p>INSERT [LOW_PRIORITY | DELAYED | HIGH_PRIORITY] [IGNORE]
[INTO] tbl_name
[PARTITION (partition_name,...)]
[(col_name,...)]
{VALUES | VALUE} ({expr | DEFAULT},...),(...),...
[ ON DUPLICATE KEY UPDATE
col_name=expr
[, col_name=expr] ... ]</p>
</li>
<li>
<p>INSERT [LOW_PRIORITY | DELAYED | HIGH_PRIORITY] [IGNORE]
[INTO] tbl_name
[PARTITION (partition_name,...)]
SET col_name={expr | DEFAULT}, ...
[ ON DUPLICATE KEY UPDATE
col_name=expr
[, col_name=expr] ... ]</p>
</li>
<li>
<p>INSERT [LOW_PRIORITY | HIGH_PRIORITY] [IGNORE]
[INTO] tbl_name
[PARTITION (partition_name,...)]
[(col_name,...)]
SELECT ...
[ ON DUPLICATE KEY UPDATE
col_name=expr
[, col_name=expr] ... ]</p>
</li>
<li>
<p>An expression expr can refer to any column that was set earlier in a value list. For example, you can do this because the value for col2 refers to col1, which has previously been assigned:<br />
INSERT INTO tbl_name (col1,col2) VALUES(15,col1*2);</p>
</li>
<li>
<p>INSERT statements that use VALUES syntax can insert multiple rows. To do this, include multiple lists of column values, each enclosed within parentheses and separated by commas. Example:<br />
INSERT INTO tbl_name (a,b,c) VALUES(1,2,3),(4,5,6),(7,8,9);</p>
</li>
<li>
<p>The values list for each row must be enclosed within parentheses. The following statement is illegal because the number of values in the list does not match the number of column names:<br />
INSERT INTO tbl_name (a,b,c) VALUES(1,2,3,4,5,6,7,8,9);</p>
</li>
<li>
<p>[INSERT ... SELECT Syntax](http://dev.mysql.com/doc/refman/5.7/en/insert-select.html)<br />
INSERT [LOW_PRIORITY | HIGH_PRIORITY] [IGNORE]
[INTO] tbl_name
[PARTITION (partition_name,...)]
[(col_name,...)]
SELECT ...
[ ON DUPLICATE KEY UPDATE col_name=expr, ... ]</p>
</li>
<li>
<p>With INSERT ... SELECT, you can quickly insert many rows into a table from one or many tables. For example:<br />
INSERT INTO tbl_temp2 (fld_id)
SELECT tbl_temp1.fld_order_id
FROM tbl_temp1 WHERE tbl_temp1.fld_order_id > 100;</p>
</li>



#### Remarks


[Official INSERT Syntax](http://dev.mysql.com/doc/refman/5.7/en/insert.html)

