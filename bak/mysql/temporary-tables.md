---
metaTitle: "MySQL - Temporary Tables"
description: "Create Temporary Table, Drop Temporary Table"
---

# Temporary Tables



## Create Temporary Table


Temporary tables could be very useful to keep temporary data. Temporary tables option is available in MySQL version 3.23 and above.

Temporary table will be automatically destroyed when the session ends or connection is closed. The user can also drop temporary table.

Same temporary table name can be used in many connections at the same time, because the temporary table is only available and accessible by the client who creates that table.

The temporary table can be created in the following types

```sql
--->Basic temporary table creation
CREATE TEMPORARY TABLE tempTable1(
       id INT NOT NULL AUTO_INCREMENT,
       title VARCHAR(100) NOT NULL,
       PRIMARY KEY ( id )
    );

--->Temporary table creation from select query
CREATE TEMPORARY TABLE tempTable1
    SELECT ColumnName1,ColumnName2,... FROM table1;

```

You can add indexes as you build the table:

```sql
CREATE TEMPORARY TABLE tempTable1
        ( PRIMARY KEY(ColumnName2) )
    SELECT ColumnName1,ColumnName2,... FROM table1;

```

`IF NOT EXISTS` key word can be used as mentioned below to avoid **'table already exists'** error. But in that case table will not be created, if the table name which you are using already exists in your current session.

```sql
CREATE TEMPORARY TABLE IF NOT EXISTS tempTable1
    SELECT ColumnName1,ColumnName2,... FROM table1;

```



## Drop Temporary Table


Drop Temporary Table is used to delete the temporary table which you are created in your current session.

```sql
DROP TEMPORARY TABLE tempTable1

DROP TEMPORARY TABLE IF EXISTS tempTable1

```

Use `IF EXISTS` to prevent an error occurring for tables that may not exist

