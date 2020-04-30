---
metaTitle: "Database Snapshots"
description: "Create a database snapshot, Restore a database snapshot, DELETE Snapshot"
---

# Database Snapshots



## Create a database snapshot


A database snapshot is a read-only, static view of a SQL Server database (the source database). It is similar to backup, but it is available as any other database so client can query snapshot database.

You can also create snapshot of database with multiple files:



## Restore a database snapshot


If data in a source database becomes damaged or some wrong data is written into database, in some cases, reverting the database to a database snapshot that predates the damage might be an appropriate alternative to restoring the database from a backup.

> 
**Warning:** This will **delete all changes** made to the source database since the snapshot was taken!




## DELETE Snapshot


You can delete existing snapshots of database using DELETE DATABASE statement:

```sql
DROP DATABASE Mydatabase_morning

```

In this statement you should reference name of the database snapshot.



#### Remarks


A database snapshot is a read-only, static view of a SQL Server database which is transactionally consistent with the source database as of the moment of the snapshot's creation.

A database snapshot always resides on the same server instance as its source database. As the source database is updated, the database snapshot is updated.

A snapshot differs from a backup since the process of snapshot creation is instantaneous and the snapshot occupies space only as changes in the source database are applied. A backup on the other hand stores a full copy of the data as on the time of backup creation.

Additionally, a snapshot gives an instant read only copy of the database, while a backup needs to be restored to a server in order to be readable (and once restored can be written to as well)

Database snapshots are only available in the Enterprise and Developer editions.

