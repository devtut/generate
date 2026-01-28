---
metaTitle: "Microsoft SQL Server - DBCC"
description: "DBCC maintenance commands, DBCC validation statements, DBCC informational statements, DBCC Trace commands, DBCC statement"
---

# DBCC



## DBCC maintenance commands


DBCC commands enable user to maintain space in database, clean caches, shrink databases and tables.

Examples are:

```sql
DBCC DROPCLEANBUFFERS 

```

Removes all clean buffers from the buffer pool, and columnstore objects from the columnstore object pool.

```sql
DBCC FREEPROCCACHE
-- or
DBCC FREEPROCCACHE (0x060006001ECA270EC0215D05000000000000000000000000);

```

Removes all SQL query in plan cache. Every new plan will be recompiled:
You can specify plan handle, query handle to clean plans for the specific query plan or SQL statement.

```sql
DBCC FREESYSTEMCACHE ('ALL', myresourcepool); 
-- or
DBCC FREESYSTEMCACHE;

```

Cleans all cached entries created by system. It can clean entries o=in all or some specified resource pool (**myresourcepool** in the example above)

```sql
DBCC FLUSHAUTHCACHE 

```

Empties the database authentication cache containing information about logins and firewall rules.

```sql
DBCC SHRINKDATABASE (MyDB [, 10]); 

```

Shrinks database MyDB to 10%. Second parameter is optional. You can use database id instead of name.

```sql
DBCC SHRINKFILE (DataFile1, 7); 

```

Shrinks data file named DataFile1 in the current database. Target size is 7 MB (tis parameter is optional).

```sql
DBCC CLEANTABLE (AdventureWorks2012,'Production.Document', 0) 

```

Reclaims a space from specified table



## DBCC validation statements


DBCC commands enable user to validate state of database.

```sql
ALTER TABLE Table1 WITH NOCHECK ADD CONSTRAINT chkTab1 CHECK (Col1 > 100);  
GO  
DBCC CHECKCONSTRAINTS(Table1);  
--OR
DBCC CHECKCONSTRAINTS ('Table1.chkTable1');  

```

Check constraint is added with nocheck options, so it will not be checked on existing data. DBCC will trigger constraint check.

Following DBCC commands check integrity of database, table or catalog:

```sql
DBCC CHECKTABLE tablename1 | tableid
DBCC CHECKDB databasename1 | dbid
DBCC CHECKFILEGROUP filegroup_name | filegroup_id | 0
DBCC CHECKCATALOG databasename1 | database_id1 | 0

```



## DBCC informational statements


DBCC commands can show information about database objects.

```sql
DBCC PROCCACHE

```

Displays information in a table format about the procedure cache.

```sql
DBCC OUTPUTBUFFER ( session_id [ , request_id ])  

```

Returns the current output buffer in hexadecimal and ASCII format for the specified session_id (and optional request_id).

```sql
DBCC INPUTBUFFER ( session_id [ , request_id ])  

```

Displays the last statement sent from a client to an instance of Microsoft SQL Server.

```sql
DBCC SHOW_STATISTICS ( table_or_indexed_view_name , column_statistic_or_index_name)

```



## DBCC Trace commands


Trace flags in SQL Server are used to modify behavior of SQL server, turn on/off some features. DBCC commands can control trace flags:

The following example switches on trace flag 3205 globally and 3206 for the current session:

```sql
DBCC TRACEON (3205, -1); 
DBCC TRACEON (3206);

```

The following example switches off trace flag 3205 globally and 3206 for the current session:

```sql
DBCC TRACEON (3205, -1); 
DBCC TRACEON (3206);

```

The following example displays the status of trace flags 2528 and 3205:

```sql
DBCC TRACESTATUS (2528, 3205);  

```



## DBCC statement


DBCC statements act as Database Console Commands for SQL Server.
To get the syntax information for the specified DBCC command use DBCC HELP (...) statement.

The following example returns all DBCC statements for which Help is available:

```sql
DBCC HELP ('?');  

```

The following example returns options for DBCC CHECKDB statement:

```sql
DBCC HELP ('CHECKDB'); 

```

