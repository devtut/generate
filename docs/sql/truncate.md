---
metaTitle: "TRUNCATE"
description: "Removing all rows from the Employee table"
---

# TRUNCATE


The TRUNCATE statement deletes all data from a table. This is similar to DELETE with no filter, but, depending on the database software, has certain restrictions and optimizations.



## Removing all rows from the Employee table


```sql
TRUNCATE TABLE Employee;

```

Using truncate table is often better then using DELETE TABLE as it ignores all the indexes and triggers and just removes everything.

Delete table is a row based operation this means that each row is deleted. Truncate table is a data page operation the entire data page is reallocated. If you have a table with a million rows it will be much faster to truncate the table than it would be to use a delete table statement.

Though we can delete specific Rows with DELETE, we cannot TRUNCATE specific rows, we can only TRUNCATE all the records at once. Deleting All rows and then inserting a new record will continue to add the Auto incremented Primary key value from the previously inserted value, where as in Truncate, the Auto Incremental primary key value will also get reset and starts from 1.

Note that when truncating table, **no foreign keys must be present**, otherwise you will get an error.



#### Syntax


- TRUNCATE TABLE  table_name;



#### Remarks


TRUNCATE is a DDL (Data Definition Language) command, and as such there are significant differences between it and DELETE (a Data Manipulation Language, DML, command). While TRUNCATE can be a means of quickly removing large volumes of records from a database, these differences should be understood in order to decide if using a TRUNCATE command is suitable in your particular situation.

- TRUNCATE is a data page operation. Therefore DML triggers (ON DELETE) associated with the table won't fire when you perform a TRUNCATE operation. While this will save a large amount of time for massive delete operations, however you may then need to manually delete the related data.
- TRUNCATE will release the disk space used by the deleted rows, DELETE will release space
- If the table to be truncated uses identity columns (MS SQL Server), then the seed is reset by the TRUNCATE command. This may result referential integrity problems
- Depending the security roles in place and the variant of SQL in use, you may not have the necessary permissions to perform a TRUNCATE command

