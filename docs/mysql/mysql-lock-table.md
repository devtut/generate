---
metaTitle: "MySQL LOCK TABLE"
description: "Mysql Locks, Row Level Locking"
---

# MySQL LOCK TABLE



## Mysql Locks


**Table locks can be an important tool for `ENGINE=MyISAM`, but are rarely useful for `ENGINE=InnoDB`.  If you are tempted to use table locks with InnoDB, you should rethink how you are working with transactions.**

MySQL enables client sessions to acquire table locks explicitly for the purpose of cooperating with other sessions for access to tables, or to prevent other sessions from modifying tables during periods when a session requires exclusive access to them. A session can acquire or release locks only for itself. One session cannot acquire locks for another session or release locks held by another session.

Locks may be used to emulate transactions or to get more speed when updating tables. This is explained in more detail later in this section.

Command:`LOCK TABLES table_name READ|WRITE;`

you can assign only lock type to a single table;

Example (READ LOCK):

```sql
LOCK TABLES table_name READ;

```

Example (WRITE LOCK):

```sql
LOCK TABLES table_name WRITE;

```

To see lock is applied or not, use following Command

```sql
SHOW OPEN TABLES;

```

To flush/remove all locks, use following command:

```sql
UNLOCK TABLES;

```

EXAMPLE:

```sql
LOCK TABLES products WRITE:  
INSERT INTO products(id,product_name) SELECT id,old_product_name FROM old_products;
UNLOCK TABLES;

```

Above example any external connection cannot write any data to products table until unlocking table product

EXAMPLE:

```sql
LOCK TABLES products READ:  
INSERT INTO products(id,product_name) SELECT id,old_product_name FROM old_products;
UNLOCK TABLES;

```

Above example any external connection cannot read any data from products table until unlocking table product



## Row Level Locking


If the tables use InnoDB, MySQL automatically uses row level locking so that multiple transactions can use same table simultaneously for read and write, without making each other wait.

If two transactions trying to modify the same row and both uses row level locking, one of the transactions waits for the other to complete.

Row level locking also can be obtained by using `SELECT ... FOR UPDATE` statement for each rows expected to be modified.

Consider two connections to explain Row level locking in detail

Connection 1

```sql
START TRANSACTION;
SELECT ledgerAmount FROM accDetails WHERE id = 1 FOR UPDATE;

```

In connection 1, row level lock obtained by `SELECT ... FOR UPDATE` statement.

Connection 2

```sql
UPDATE accDetails SET ledgerAmount = ledgerAmount + 500 WHERE id=1;

```

When some one try to update same row in connection 2, that will wait for connection 1 to finish transaction or error message will be displayed according to the `innodb_lock_wait_timeout` setting, which defaults to 50 seconds.

```sql
Error Code: 1205. Lock wait timeout exceeded; try restarting transaction

```

To view details about this lock, run `SHOW ENGINE INNODB STATUS`

```sql
---TRANSACTION 1973004, ACTIVE 7 sec updating
mysql tables in use 1, locked 1
LOCK WAIT 2 lock struct(s), heap size 360, 1 row lock(s)
MySQL thread id 4, OS thread handle 0x7f996beac700, query id 30 localhost root update
UPDATE accDetails SET ledgerAmount = ledgerAmount + 500 WHERE id=1
------- TRX HAS BEEN WAITING 7 SEC FOR THIS LOCK TO BE GRANTED:

```

Connection 2

```sql
UPDATE accDetails SET ledgerAmount = ledgerAmount + 250 WHERE id=2;
1 row(s) affected

```

But while updating some other row in connection 2 will be executed without any error.

Connection 1

```sql
UPDATE accDetails SET ledgerAmount = ledgerAmount + 750 WHERE id=1;
COMMIT;
1 row(s) affected

```

Now row lock is released, because transaction is commited in Connection 1.

Connection 2

```sql
UPDATE accDetails SET ledgerAmount = ledgerAmount + 500 WHERE id=1;
1 row(s) affected

```

The update is executed without any error in Connection 2 after Connection 1 released row lock by finishing the transaction.



#### Syntax


<li>
LOCK TABLES table_name [READ | WRITE]; // Lock Table
</li>
<li>
UNLOCK TABLES; // Unlock Tables
</li>



#### Remarks


Locking is used to solve concurrency problems.Locking is required only when running a transaction, that first read a value from a database and later write that value in to the database. Locks are never required for self-contained insert, update, or delete operations.

There are two kinds of locks available

READ LOCK - when a user is only reading from a table.

WRITE LOCK -  when a user is doing both reading and writing to a table.

When a user holds a `WRITE LOCK` on a table, no other users can read or write to that table. When a user holds a `READ LOCK` on a table, other users can also read or hold a `READ LOCK`, but no user can write or hold a `WRITE LOCK` on that table.

If default storage engine is InnoDB, MySQL automatically uses row level locking so that multiple transactions can use same table simultaneously for read and write, without making each other wait.

For all storage engines other than InnoDB, MySQL uses table locking.

For more details about table lock [See here](http://dev.mysql.com/doc/refman/5.7/en/lock-tables.html)

