---
metaTitle: "ALTER TABLE"
description: "Changing storage engine; rebuild table; change file_per_table, ALTER COLUMN OF TABLE, Change auto-increment value, Renaming a MySQL table, ALTER table add INDEX, Changing the type of a primary key column, Change column definition, Renaming a MySQL database, Swapping the names of two MySQL databases, Renaming a column in a MySQL table"
---

# ALTER TABLE



## Changing storage engine; rebuild table; change file_per_table


For example, if `t1` is currently not an InnoDB table, this statement changes its storage engine to InnoDB:

```sql
ALTER TABLE t1 ENGINE = InnoDB;

```

If the table is already InnoDB, this will rebuild the table and its indexes and have an effect similar to `OPTIMIZE TABLE`.  You may gain some disk space improvement.

If the value of `innodb_file_per_table` is currently different than the value in effect when `t1` was built, this will convert to (or from) file_per_table.



## ALTER COLUMN OF TABLE


```sql
CREATE DATABASE stackoverflow;

USE stackoverflow;

Create table stack(
    id_user int NOT NULL,
    username varchar(30) NOT NULL,
    password varchar(30) NOT NULL
);

ALTER TABLE stack ADD COLUMN submit date NOT NULL; -- add new column
ALTER TABLE stack DROP COLUMN submit; -- drop column
ALTER TABLE stack MODIFY submit DATETIME NOT NULL; -- modify type column
ALTER TABLE stack CHANGE submit submit_date DATETIME NOT NULL; -- change type and name of column
ALTER TABLE stack ADD COLUMN mod_id INT NOT NULL AFTER id_user; -- add new column after existing column

```



## Change auto-increment value


Changing an auto-increment value is useful when you don't want a gap in an AUTO_INCREMENT column after a massive deletion.

For example, you got a lot of unwanted (advertisement) rows posted in your table, you deleted them, and you want to fix the gap in auto-increment values. Assume the MAX value of AUTO_INCREMENT column is 100 now. You can use the following to fix the auto-increment value.

```sql
ALTER TABLE your_table_name AUTO_INCREMENT = 101;

```



## Renaming a MySQL table


Renaming a table can be done in a single command:

```sql
RENAME TABLE `<old name>` TO `<new name>`;

```

The following syntax does exactly the same:

```sql
ALTER TABLE `<old name>` RENAME TO `<new name>`;

```

If renaming a temporary table, the `ALTER TABLE` version of the syntax must be used.

**Steps:**

1. Replace `<old name>` and `<new name>` in the line above with the relevant values. **Note: If the table is being moved to a different database, the `dbname`.`tablename` syntax can be used for `<old name>` and/or `<new name>`.**
1. Execute it on the relevant database in the MySQL command line or a client such as MySQL Workbench. **Note: The user must have ALTER and DROP privileges on the old table and CREATE and INSERT on the new one.**



## ALTER table add INDEX


To improve performance one might want to add indexes to columns

```sql
ALTER TABLE TABLE_NAME ADD INDEX `index_name` (`column_name`)

```

altering to add composite (multiple column) indexes

```sql
ALTER TABLE TABLE_NAME ADD INDEX `index_name` (`col1`,`col2`)

```



## Changing the type of a primary key column


```sql
ALTER TABLE fish_data.fish DROP PRIMARY KEY;
ALTER TABLE fish_data.fish MODIFY COLUMN fish_id DECIMAL(20,0) NOT NULL PRIMARY KEY;

```

An attempt to modify the type of this column without first dropping the primary key would result in an error.



## Change column definition


The change the definition of a db column, the query below can be used for example, if we have this db schema

```sql
users (
    firstname varchar(20),
    lastname varchar(20),
    age char(2)
)

```

To change the type of `age` column from `char` to `int`, we use the query below:

```sql
ALTER TABLE users CHANGE age age tinyint UNSIGNED NOT NULL;

```

General format is:

```sql
ALTER TABLE table_name CHANGE column_name new_column_definition

```



## Renaming a MySQL database


There is no single command to rename a MySQL database but a simple workaround can be used to achieve this by backing up and restoring:

```sql
mysqladmin -uroot -p<password> create <new name>
mysqldump -uroot -p<password> --routines <old name> | mysql -uroot -pmypassword <new name>
mysqladmin -uroot -p<password> drop <old name>

```

**Steps:**

1. Copy the lines above into a text editor.
1. Replace all references to `<old name>`, `<new name>` and `<password>` (+ optionally `root` to use a different user) with the relevant values.
1. Execute one by one on the command line (assuming the MySQL "bin" folder is in the path and entering "y" when prompted).

**Alternative Steps:**

Rename (move) each table from one db to the other.  Do this for each table:

```sql
RENAME TABLE `<old db>`.`<name>` TO  `<new db>`.`<name>`;

```

You can create those statements by doing something like

```sql
SELECT CONCAT('RENAME TABLE old_db.', table_name, ' TO ',
                           'new_db.', table_name)
    FROM information_schema.TABLES
    WHERE table_schema = 'old_db';

```

Warning. Do not attempt to do any sort of table or database by simply moving files around on the filesystem.  This worked fine in the old days of just MyISAM, but in the new days of InnoDB and tablespaces, it won't work.  Especially when the "Data Dictionary" is moved from the filesystem into system InnoDB tables, probably in the next major release.  Moving (as opposed to just `DROPping`) a `PARTITION` of an InnoDB table requires using "transportable tablespaces".  In the near future, there won't even be a file to reach for.



## Swapping the names of two MySQL databases


The following commands can be used to swap the names of two MySQL databases (`<db1>` and `<db2>`):

```sql
mysqladmin -uroot -p<password> create swaptemp
mysqldump -uroot -p<password> --routines <db1> | mysql -uroot -p<password> swaptemp
mysqladmin -uroot -p<password> drop <db1>
mysqladmin -uroot -p<password> create <db1>
mysqldump -uroot -p<password> --routines <db2> | mysql -uroot -p<password> <db1>
mysqladmin -uroot -p<password> drop <db2>
mysqladmin -uroot -p<password> create <db2>
mysqldump -uroot -p<password> --routines swaptemp | mysql -uroot -p<password> <db2>
mysqladmin -uroot -p<password> drop swaptemp

```

**Steps:**

1. Copy the lines above into a text editor.
1. Replace all references to `<db1>`, `<db2>` and `<password>` (+ optionally `root` to use a different user) with the relevant values.
1. Execute one by one on the command line (assuming the MySQL "bin" folder is in the path and entering "y" when prompted).



## Renaming a column in a MySQL table


Renaming a column can be done in a single statement but as well as the new name, the "column definition" (i.e. its data type and other optional properties such as nullability, auto incrementing etc.) must also be specified.

```sql
ALTER TABLE `<table name>` CHANGE `<old name>` `<new name>` <column definition>;

```

**Steps:**

1. Open the MySQL command line or a client such as MySQL Workbench.
1. Run the following statement: `SHOW CREATE TABLE <table name>;` (replacing `<table name>` with the relevant value).
1. Make a note of the entire column definition for the column to be renamed **(i.e. everything that appears after the name of the column but before the comma separating it from the next column name)**.
1. Replace `<old name>`, `<new name>` and `<column definition>` in the line above with the relevant values and then execute it.



#### Syntax


- ALTER [IGNORE] TABLE tbl_name [**alter_specification** [, alter_specification] ...] [partition_options]



#### Remarks


```

   alter_specification: table_options
      | ADD [COLUMN] col_name column_definition [FIRST | AFTER col_name ]
      | ADD [COLUMN] (col_name column_definition,...)
      | ADD {INDEX|KEY} [index_name] [index_type] (index_col_name,...) [index_option] ...
      | ADD [CONSTRAINT [symbol]] PRIMARY KEY [index_type] (index_col_name,...) [index_option] ...
      | ADD [CONSTRAINT [symbol]] UNIQUE [INDEX|KEY] [index_name] [index_type] (index_col_name,...) [index_option] ...
      | ADD FULLTEXT [INDEX|KEY] [index_name] (index_col_name,...) [index_option] ...
      | ADD SPATIAL [INDEX|KEY] [index_name] (index_col_name,...) [index_option] ...
      | ADD [CONSTRAINT [symbol]] FOREIGN KEY [index_name] (index_col_name,...) reference_definition
      | ALGORITHM [=] {DEFAULT|INPLACE|COPY}
      | ALTER [COLUMN] col_name {SET DEFAULT literal | DROP DEFAULT}
      | CHANGE [COLUMN] old_col_name new_col_name column_definition [FIRST|AFTER col_name]
      | LOCK [=] {DEFAULT|NONE|SHARED|EXCLUSIVE}
      | MODIFY [COLUMN] col_name column_definition [FIRST | AFTER col_name]
      | DROP [COLUMN] col_name
      | DROP PRIMARY KEY
      | DROP {INDEX|KEY} index_name
      | DROP FOREIGN KEY fk_symbol
      | DISABLE KEYS
      | ENABLE KEYS
      | RENAME [TO|AS] new_tbl_name
      | RENAME {INDEX|KEY} old_index_name TO new_index_name
      | ORDER BY col_name [, col_name] ...
      | CONVERT TO CHARACTER SET charset_name [COLLATE collation_name]
      | [DEFAULT] CHARACTER SET [=] charset_name [COLLATE [=] collation_name]
      | DISCARD TABLESPACE
      | IMPORT TABLESPACE
      | FORCE
      | {WITHOUT|WITH} VALIDATION
      | ADD PARTITION (partition_definition)
      | DROP PARTITION partition_names
      | DISCARD PARTITION {partition_names | ALL} TABLESPACE
      | IMPORT PARTITION {partition_names | ALL} TABLESPACE
      | TRUNCATE PARTITION {partition_names | ALL}
      | COALESCE PARTITION number
      | REORGANIZE PARTITION partition_names INTO (partition_definitions)
      | EXCHANGE PARTITION partition_name WITH TABLE tbl_name [{WITH|WITHOUT} VALIDATION]
      | ANALYZE PARTITION {partition_names | ALL}
      | CHECK PARTITION {partition_names | ALL}
      | OPTIMIZE PARTITION {partition_names | ALL}
      | REBUILD PARTITION {partition_names | ALL}
      | REPAIR PARTITION {partition_names | ALL}
      | REMOVE PARTITIONING
      | UPGRADE PARTITIONING
   index_col_name: col_name [(length)] [ASC | DESC]
   index_type: USING {BTREE | HASH}
   index_option: KEY_BLOCK_SIZE [=] value
      | index_type
      | WITH PARSER parser_name
      | COMMENT 'string'

```

`table_options: table_option [[,] table_option] ... (see` [CREATE TABLE](http://dev.mysql.com/doc/refman/5.7/en/create-table.html) `options)`

`partition_options: (see` [CREATE TABLE](http://dev.mysql.com/doc/refman/5.7/en/create-table.html) `options)`

Ref: [MySQL 5.7 Reference Manual  /  ...  /  ALTER TABLE Syntax / 14.1.8 ALTER TABLE Syntax](http://dev.mysql.com/doc/refman/5.7/en/alter-table.html)

