---
metaTitle: "Microsoft SQL Server - System database - TempDb"
description: "Identify TempDb usage, TempDB database details"
---

# System database - TempDb



## Identify TempDb usage


Following query will provide information about TempDb usage. Analyzing the counts you can identify which thing is impacting TempDb

```sql
SELECT
 SUM (user_object_reserved_page_count)*8 as usr_obj_kb,
 SUM (internal_object_reserved_page_count)*8 as internal_obj_kb,
 SUM (version_store_reserved_page_count)*8  as version_store_kb,
 SUM (unallocated_extent_page_count)*8 as freespace_kb,
 SUM (mixed_extent_page_count)*8 as mixedextent_kb
FROM sys.dm_db_file_space_usage

```

[<img src="http://i.stack.imgur.com/JsNnB.png" alt="enter image description here" />](http://i.stack.imgur.com/JsNnB.png)



## TempDB database details


Below query can be used to get TempDB database details:

```sql
USE [MASTER]
SELECT * FROM sys.databases WHERE database_id = 2

```

OR

```sql
USE [MASTER]
SELECT * FROM sys.master_files WHERE database_id = 2

```

With the help of below DMV, you can check how much TempDb space does your session is using. This query is quite helpful while debugging TempDb issues

```sql
SELECT * FROM sys.dm_db_session_space_usage WHERE session_id = @@SPID

```

