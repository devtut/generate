---
metaTitle: "MySQL Admin"
description: "Atomic RENAME & Table Reload, Change root password, Drop database"
---

# MySQL Admin



## Atomic RENAME & Table Reload


```sql
RENAME TABLE t TO t_old, t_copy TO t;

```

No other sessions can access the tables involved while RENAME TABLE executes, so the rename operation is not subject to concurrency problems.

Atomic Rename is especially for completely reloading a table without waiting for `DELETE` and load to finish:

```sql
CREATE TABLE new LIKE real;
load `new` by whatever means - LOAD DATA, INSERT, whatever
RENAME TABLE real TO old, new TO real;
DROP TABLE old;

```



## Change root password


```sql
mysqladmin -u root -p'old-password' password 'new-password'

```



## Drop database


Useful for scripting to drop all tables and deletes the database:

```sql
mysqladmin -u[username] -p[password] drop [database]

```

Use with extreme caution.

To `DROP` database as a SQL Script (you will need DROP privilege on that database):

```sql
DROP DATABASE database_name

```

or

```sql
DROP SCHEMA database_name

```

