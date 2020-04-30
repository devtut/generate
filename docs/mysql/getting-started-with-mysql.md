---
metaTitle: "Getting started with MySQL"
description: "Getting Started, Information Schema Examples"
---

# Getting started with MySQL



## Getting Started


**Creating a database in MySQL**

```sql
CREATE DATABASE mydb;

```

Return value:

> 
Query OK, 1 row affected (0.05 sec)


**Using the created database** `mydb`

```sql
USE mydb;

```

Return value:

> 
Database Changed


**Creating a table in MySQL**

```sql
CREATE TABLE mytable
(
  id              int unsigned NOT NULL auto_increment,
  username        varchar(100) NOT NULL,
  email           varchar(100) NOT NULL,
  PRIMARY KEY     (id)
);

```

`CREATE TABLE mytable` will create a new table called `mytable`.

`id int unsigned NOT NULL auto_increment` creates the `id` column, this type of field will assign a unique numeric ID to each record in the table (meaning that no two rows can have the same `id` in this case), MySQL will automatically assign a new, unique value to the record's `id` field (starting with 1).

Return value:

> 
Query OK, 0 rows affected (0.10 sec)


**Inserting a row into a MySQL table**

```sql
INSERT INTO mytable ( username, email )
VALUES ( "myuser", "myuser@example.com" );

```

Example return value:

> 
Query OK, 1 row affected (0.06 sec)


The `varchar` a.k.a `strings` can be also be inserted using single quotes:

```sql
INSERT INTO mytable ( username, email )
VALUES ( 'username', 'username@example.com' );

```

**Updating a row into a MySQL table**

```sql
UPDATE mytable SET username="myuser" WHERE id=8

```

Example return value:

> 
Query OK, 1 row affected (0.06 sec)


The `int` value can be inserted in a query without quotes. Strings and Dates must be enclosed in single quote `'` or double quotes `"`.

**Deleting a row into a MySQL table**

```sql
DELETE FROM mytable WHERE id=8

```

Example return value:

> 
Query OK, 1 row affected (0.06 sec)


This will delete the row having `id` is 8.

**Selecting rows based on conditions in MySQL**

```sql
SELECT * FROM mytable WHERE username = "myuser";

```

Return value:

```sql
+----+----------+---------------------+
| id | username | email               |
+----+----------+---------------------+
|  1 | myuser   | myuser@example.com  |
+----+----------+---------------------+

```

> 
1 row in set (0.00 sec)


**Show list of existing databases**

```sql
SHOW databases;

```

Return value:

```sql
+-------------------+
| Databases         |
+-------------------+
| information_schema|
| mydb              |
+-------------------+

```

> 
2 rows in set (0.00 sec)


You can think of "information_schema" as a "master database" that provides access to database metadata.

**Show tables in an existing database**

```sql
SHOW tables;

```

Return value:

```sql
+----------------+
| Tables_in_mydb |
+----------------+
| mytable        |
+----------------+

```

> 
1 row in set (0.00 sec)


**Show all the fields of a table**

```sql
DESCRIBE databaseName.tableName;

```

or, if already using a database:

```sql
DESCRIBE tableName;

```

Return value:

```sql
+-----------+----------------+--------+---------+-------------------+-------+
| Field     | Type           | Null   | Key     | Default           | Extra |
+-----------+----------------+--------+---------+-------------------+-------+
| fieldname | fieldvaluetype | NO/YES | keytype | defaultfieldvalue |       |
+-----------+----------------+--------+---------+-------------------+-------+

```

`Extra` may contain `auto_increment` for example.

`Key` refers to the type of key that may affect the field. Primary (PRI), Unique (UNI) ...

> 
n row in set (0.00 sec)


Where n is the number of fields in the table.

**Creating user**

First, you need to create a user and then give the user permissions on certain databases/tables. While creating the user, you also need to specify where this user can connect from.

```sql
CREATE USER 'user'@'localhost' IDENTIFIED BY 'some_password';

```

Will create a user that can only connect on the local machine where the database is hosted.

```sql
CREATE USER 'user'@'%' IDENTIFIED BY 'some_password';

```

Will create a user that can connect from anywhere (except the local machine).

Example return value:

> 
Query OK, 0 rows affected (0.00 sec)


**Adding privileges**

Grant common, basic privileges to the user for all tables of the specified database:

```sql
GRANT SELECT, INSERT, UPDATE ON databaseName.* TO 'userName'@'localhost';

```

Grant all privileges to the user for all tables on all databases (attention with this):

```sql
GRANT ALL ON *.* TO 'userName'@'localhost' WITH GRANT OPTION;

```

As demonstrated above, `*.*` targets all databases and tables, `databaseName.*` targets all tables of the specific database. It is also possible to specify database and table like so `databaseName.tableName`.

`WITH GRANT OPTION` should be left out if the user need not be able to grant other users privileges.

Privileges can be **either**

```sql
ALL

```

**or** a combination of the following, each separated by a comma (non-exhaustive list).

```sql
SELECT
INSERT
UPDATE
DELETE
CREATE
DROP

```

**Note**

Generally, you should try to avoid using column or table names containing spaces or using reserved words in SQL. For example, it's best to avoid names like `table` or `first name`.

If you must use such names, put them between back-tick ```sql` delimiters. For example:

```sql
CREATE TABLE `table`
(   
   `first name` VARCHAR(30)
);

```

A query containing the back-tick delimiters on this table might be:

```

SELECT `first name` FROM `table` WHERE `first name` LIKE 'a%';

```



## Information Schema Examples


### Processlist

```sql
SELECT * FROM information_schema.PROCESSLIST ORDER BY INFO DESC, TIME DESC;

```

This is a bit more detail on time-frames as it is in seconds by default

```sql
SELECT ID, USER, HOST, DB, COMMAND, 
TIME as time_seconds, 
ROUND(TIME / 60, 2) as time_minutes, 
ROUND(TIME / 60 / 60, 2) as time_hours, 
STATE, INFO
FROM information_schema.PROCESSLIST ORDER BY INFO DESC, TIME DESC;

```

### Stored Procedure Searching

Easily search thru all `Stored Procedures` for words and wildcards.

```sql
SELECT * FROM information_schema.ROUTINES WHERE ROUTINE_DEFINITION LIKE '%word%';

```



#### Remarks


<img src="https://i.stack.imgur.com/NWTjw.png" alt="MySQL Logo" />

[MySQL](http://www.mysql.com/) is an open-source Relational Database Management System (RDBMS) that is developed and supported by Oracle Corporation.

MySQL is [supported](http://www.mysql.com/support/supportedplatforms/database.html) on a large number of platforms, including Linux variants, OS X, and Windows. It also has [APIs](http://dev.mysql.com/doc/refman/5.7/en/connectors-apis.html) for a large number of languages, including C, C++, Java, Lua, .Net, Perl, PHP, Python, and Ruby.

[MariaDB](https://mariadb.com/) is a fork of MySQL with a [slightly different feature set](https://mariadb.com/kb/en/mariadb/mariadb-vs-mysql-features/). It is entirely compatible with MySQL for most applications.

