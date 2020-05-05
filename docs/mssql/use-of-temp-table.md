---
metaTitle: "Microsoft SQL Server - Use of TEMP Table"
description: "Dropping temp tables, Local Temp Table , Global Temp Table"
---

# Use of TEMP Table



## Dropping temp tables


Temp tables must have unique IDs (within the session, for local temp tables, or within the server, for global temp tables).  Trying to create a table using a name that already exists will return the following error:

```sql
There is already an object named '#tempTable' in the database.

```

If your query produces temp tables, and you want to run it more than once, you will need to drop the tables before trying to generate them again.  The basic syntax for this is:

```sql
drop table #tempTable

```

Trying to execute this syntax before the table exists (e.g. on the first run of your syntax) will cause another error:

```sql
Cannot drop the table '#tempTable', because it does not exist or you do not have permission.

```

To avoid this, you can check to see if the table already exists before dropping it, like so:

```sql
IF OBJECT_ID ('tempdb..#tempTable', 'U') is not null DROP TABLE #tempTable

```



## Local Temp Table 


<li>
Will be available till the current connection persists for the user.
Automatically deleted when the user disconnects.
The name should start with # (#temp)

```sql
 CREATE TABLE #LocalTempTable(
                StudentID      int,
                StudentName    varchar(50), 
                StudentAddress varchar(150))

```


</li>

```sql
insert into #LocalTempTable values ( 1, 'Ram','India');

select * from #LocalTempTable

```

After executing all these statements if we close the query window and open it again and try inserting and select it will show an error message

```sql
“Invalid object name #LocalTempTable”

```



## Global Temp Table


<li>
Will start with ## (##temp).
Will be deleted only if user disconnects all connections.
It behaves like a permanent table.

```sql
CREATE TABLE ##NewGlobalTempTable(
               StudentID      int,
               StudentName    varchar(50), 
               StudentAddress varchar(150))

Insert Into ##NewGlobalTempTable values ( 1,'Ram','India');
Select * from ##NewGlobalTempTable

```


</li>

Note: These are viewable by all users of the database, irrespective of permissions level.



#### Remarks


Temporary Tables are really very helpful.

The table can be created at runtime and can do all operations which are done in a normal table.

These tables are created in a tempdb database.

Used when ?

<li>
We have to do complex join operation.
</li>
<li>
We do large number of row manipulation in stored procedures.
</li>
<li>
Can replace the usage of cursor.
</li>

Thus increases the performance.

