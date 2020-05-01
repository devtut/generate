---
metaTitle: "Creating databases"
description: "Create database, users, and grants, MyDatabase, System Databases, Creating and Selecting a Database"
---

# Creating databases



## Create database, users, and grants


Create a DATABASE. Note that the shortened word SCHEMA can be used as a synonym.

```sql
CREATE DATABASE Baseball; -- creates a database named Baseball

```

If the database already exists, Error 1007 is returned. To get around this error, try:

```sql
CREATE DATABASE IF NOT EXISTS Baseball;

```

Similarly,

```sql
DROP DATABASE IF EXISTS Baseball; -- Drops a database if it exists, avoids Error 1008
DROP DATABASE xyz; -- If xyz does not exist, ERROR 1008 will occur

```

Due to the above Error possibilities, DDL statements are often used with `IF EXISTS`.

One can create a database with a default CHARACTER SET and collation. For example:

```sql
CREATE DATABASE Baseball CHARACTER SET utf8 COLLATE utf8_general_ci;

SHOW CREATE DATABASE Baseball;
+----------+-------------------------------------------------------------------+
| Database | Create Database                                                   |
+----------+-------------------------------------------------------------------+
| Baseball | CREATE DATABASE `Baseball` /*!40100 DEFAULT CHARACTER SET utf8 */ |
+----------+-------------------------------------------------------------------+

```

See your current databases:

```sql
SHOW DATABASES;
+---------------------+
| Database            |
+---------------------+
| information_schema  |
| ajax_stuff          |
| Baseball            |
+---------------------+

```

Set the currently active database, and see some information:

```sql
USE Baseball; -- set it as the current database
SELECT @@character_set_database as cset,@@collation_database as col;
+------+-----------------+
| cset | col             |
+------+-----------------+
| utf8 | utf8_general_ci |
+------+-----------------+

```

The above shows the default CHARACTER SET and Collation for the database.

Create a user:

```sql
CREATE USER 'John123'@'%' IDENTIFIED BY 'OpenSesame';

```

The above creates a user John123, able to connect with any hostname due to the `%` wildcard. The Password for the user is set to 'OpenSesame' which is hashed.

And create another:

```sql
CREATE USER 'John456'@'%' IDENTIFIED BY 'somePassword';

```

Show that the users have been created by examining the special `mysql` database:

```sql
SELECT user,host,password from mysql.user where user in ('John123','John456');
+---------+------+-------------------------------------------+
| user    | host | password                                  |
+---------+------+-------------------------------------------+
| John123 | %    | *E6531C342ED87 ....................       |
| John456 | %    | *B04E11FAAAE9A ....................       |
+---------+------+-------------------------------------------+

```

Note that at this point, the users have been created, but without any permissions to use the Baseball database.

Work with permissions for users and databases. Grant rights to user John123 to have full privileges on the Baseball database, and just SELECT rights for the other user:

```sql
GRANT ALL ON Baseball.* TO 'John123'@'%';
GRANT SELECT ON Baseball.* TO 'John456'@'%';

```

Verify the above:

```sql
SHOW GRANTS FOR 'John123'@'%';
+--------------------------------------------------------------------------------------------------------+
| Grants for John123@%                                                                                   |
+--------------------------------------------------------------------------------------------------------+
| GRANT USAGE ON *.* TO 'John123'@'%' IDENTIFIED BY PASSWORD '*E6531C342ED87 ....................        |
| GRANT ALL PRIVILEGES ON `baseball`.* TO 'John123'@'%'                                                  |
+--------------------------------------------------------------------------------------------------------+

SHOW GRANTS FOR 'John456'@'%';
+--------------------------------------------------------------------------------------------------------+
| Grants for John456@%                                                                                   |
+--------------------------------------------------------------------------------------------------------+
| GRANT USAGE ON *.* TO 'John456'@'%' IDENTIFIED BY PASSWORD '*B04E11FAAAE9A ....................        |
| GRANT SELECT ON `baseball`.* TO 'John456'@'%'                                                          |
+--------------------------------------------------------------------------------------------------------+

```

Note that the `GRANT USAGE` that you will always see means simply that the user may login. That is all that that means.



## MyDatabase


You **must** create your own database, and not use write to any of the existing databases.  This is likely to be one of the very first things to do after getting connected the first time.

```sql
CREATE DATABASE my_db;
USE my_db;
CREATE TABLE some_table;
INSERT INTO some_table ...;

```

You can reference your table by qualifying with the database name:  `my_db.some_table`.



## System Databases


The following databases exist for MySQL's use.  You may read (`SELECT`) them, but you must not write (`INSERT`/`UPDATE`/`DELETE`) the tables in them.  (There are a few exceptions.)

- `mysql` -- repository for `GRANT` info and some other things.
- `information_schema` -- The tables here are 'virtual' in the sense that they are actually manifested by in-memory structures.  Their contents include the schema for all tables.
- `performance_schema` -- ?? [please accept, then edit]
- others??  (for MariaDB, Galera, TokuDB, etc)



## Creating and Selecting a Database


If the administrator creates your database for you when setting up your permissions, you can begin using it. Otherwise, you need to create it yourself:

```sql
mysql> CREATE DATABASE menagerie;

```

Under Unix, database names are case sensitive (unlike SQL keywords), so you must always refer to your database as menagerie, not as Menagerie, MENAGERIE, or some other variant. This is also true for table names. (Under Windows, this restriction does not apply, although you must refer to databases and tables using the same lettercase throughout a given query. However, for a variety of reasons, the recommended best practice is always to use the same lettercase that was used when the database was created.)

Creating a database does not select it for use; you must do that explicitly. To make menagerie the current database, use this statement:

```sql
mysql> USE menagerie
Database changed

```

Your database needs to be created only once, but you must select it for use each time you begin a mysql session. You can do this by issuing a USE statement as shown in the example. Alternatively, you can select the database on the command line when you invoke mysql. Just specify its name after any connection parameters that you might need to provide. For example:

```sql
shell> mysql -h host -u user -p menagerie
Enter password: ********

```



#### Syntax


- CREATE {DATABASE | SCHEMA} [IF NOT EXISTS] db_name [create_specification]  /// To create database
- DROP {DATABASE | SCHEMA} [IF EXISTS] db_name /// To drop database



#### Parameters


|Parameter|Details
|---|---|---|---
|CREATE DATABASE|Creates a database with the given name
|CREATE SCHEMA|This is a synonym for `CREATE DATABASE`
|IF NOT EXISTS|Used to avoid execution error, if specified database already exists
|create_specification|`create_specification` options specify database characteristics such as `CHARACTER SET` and `COLLATE`(database collation)

