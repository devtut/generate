---
metaTitle: "Drop Table"
description: "Drop Table, Drop tables from database"
---

# Drop Table



## Drop Table


Drop Table is used to delete the table from database.

**Creating Table:**

Creating a table named tbl and then deleting the created table

```sql
CREATE TABLE tbl(
    id INT NOT NULL AUTO_INCREMENT,
    title VARCHAR(100) NOT NULL,
    author VARCHAR(40) NOT NULL,
    submission_date DATE,
    PRIMARY KEY (id)
);

```

**Dropping Table:**

```sql
DROP TABLE tbl;

```

> 
**PLEASE NOTE**
<p>Dropping table will completely delete the table from the database and
all its information, and it will not be recovered.</p>




## Drop tables from database


DROP TABLE Database.table_name



#### Syntax


- DROP TABLE table_name;
- DROP TABLE IF EXISTS table_name;  -- to avoid pesky error in automated script
- DROP TABLE t1, t2, t3;   -- DROP multiple tables
- DROP TEMPORARY TABLE t;  -- DROP a table from CREATE TEMPORARY TABLE ...



#### Parameters


|Parameters|Details
|------
|TEMPORARY|Optional. It specifies that only temporary tables should be dropped by the DROP TABLE statement.
|IF EXISTS|Optional. If specified, the DROP TABLE statement will not raise an error if one of the tables does not exist.

