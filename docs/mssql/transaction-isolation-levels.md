---
metaTitle: "Microsoft SQL Server - Transaction isolation levels"
description: "Read Uncommitted, Read Committed, What are dirty reads?, Repeatable Read, Snapshot, Serializable"
---

# Transaction isolation levels



## Read Uncommitted


```sql
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED

```

This is the most permissive isolation level, in that it does not cause any locks at all. It specifies that statements can read all rows, including rows that have been written in transactions but not yet committed (i.e., they are still in transaction). This isolation level can be subject to "dirty reads".



## Read Committed


```sql
SET TRANSACTION ISOLATION LEVEL READ COMMITTED

```

This isolation level is the 2nd most permissive. It prevents dirty reads. The behavior of `READ COMMITTED` depends on the setting of the `READ_COMMITTED_SNAPSHOT`:

<li>
If set to OFF (the default setting) the transaction uses shared locks to prevent other transactions from modifying rows used by the current transaction, as well as block the current transaction from reading rows modified by other transactions.
</li>
<li>
If set to ON, the `READCOMMITTEDLOCK` table hint can be used to request shared locking instead of row versioning for transactions running in `READ COMMITTED` mode.
</li>

Note: `READ COMMITTED` is the default SQL Server behavior.



## What are "dirty reads"?


Dirty reads (or uncommitted reads) are reads of rows which are being modified by an open transaction.

This behavior can be replicated by using 2 separate queries: one to open a transaction and write some data to a table without committing, the other to select the data to be written (but not yet committed) with this isolation level.

**Query 1** - Prepare a transaction but do not finish it:

```sql
CREATE TABLE dbo.demo (
    col1 INT,
    col2 VARCHAR(255)
);
GO
--This row will get committed normally:
BEGIN TRANSACTION;
    INSERT INTO dbo.demo(col1, col2) 
    VALUES (99, 'Normal transaction');
COMMIT TRANSACTION;
--This row will be "stuck" in an open transaction, causing a dirty read
BEGIN TRANSACTION;
    INSERT INTO dbo.demo(col1, col2) 
    VALUES (42, 'Dirty read');
--Do not COMMIT TRANSACTION or ROLLBACK TRANSACTION here

```

**Query 2** - Read the rows including the open transaction:

```sql
SET TRANSACTION ISOLATION LEVEL READ UNCOMMITTED;
SELECT * FROM dbo.demo;

```

Returns:

> 

```sql
col1        col2
----------- ---------------------------------------
99          Normal transaction
42          Dirty read

```




P.S.: Don't forget to clean up this demo data:

```sql
COMMIT TRANSACTION;
DROP TABLE dbo.demo;
GO

```



## Repeatable Read


```sql
SET TRANSACTION ISOLATION LEVEL REPEATABLE READ

```

This transaction isolation level is slightly less permissive than `READ COMMITTED`, in that shared locks are placed on all data read by each statement in the transaction and are held **until the transaction completes**, as opposed to being released after each statement.

Note: Use this option only when necessary, as it is more likely to cause database performance degradation as well as deadlocks than `READ COMMITTED`.



## Snapshot


```sql
SET TRANSACTION ISOLATION LEVEL SNAPSHOT

```

Specifies that data read by any statement in a transaction will be the transactionally consistent version of the data that existed at the start of the transaction, i.e., it will only read data that has been committed prior to the transaction starting.

`SNAPSHOT` transactions do not request or cause any locks on the data that is being read, as it is only reading the version (or snapshot) of the data that existed at the time the transaction began.

A transaction running in `SNAPSHOT` isolation level read only its own data changes while it is running. For example, a transaction could update some rows and then read the updated rows, but that change will only be visible to the current transaction until it is committed.

Note: The `ALLOW_SNAPSHOT_ISOLATION` database option must be set to ON before the `SNAPSHOT` isolation level can be used.



## Serializable


```sql
SET TRANSACTION ISOLATION LEVEL SERIALIZEABLE

```

This isolation level is the most restrictive. It requests **range locks** the range of key values that are read by each statement in the transaction. This also means that `INSERT` statements from other transactions will be blocked if the rows to be inserted are in the range locked by the current transaction.

This option has the same effect as setting `HOLDLOCK` on all tables in all `SELECT` statements in a transaction.

Note: This transaction isolation has the lowest concurrency and should only be used when necessary.



#### Syntax


<li>SET TRANSACTION ISOLATION LEVEL
{ READ UNCOMMITTED
| READ COMMITTED
| REPEATABLE READ
| SNAPSHOT
| SERIALIZABLE
}
[ ; ]</li>



#### Remarks


MSDN Reference: [SET TRANSACTION ISOLATION LEVEL](https://msdn.microsoft.com/en-us/library/ms173763.aspx)

