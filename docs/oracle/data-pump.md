---
metaTitle: "Oracle Database - Data Pump"
description: "Monitor Datapump jobs, Step 3/6 : Create directory, Step 7 : Export Commands, Step 9 : Import Commands, 1. Datapump steps, Copy tables between different schemas and tablespaces"
---

# Data Pump


Following are the steps to create a data pump import/export:



## Monitor Datapump jobs


Datapump jobs can be monitored using

**1. data dictionary views:**

```

  select * from dba_datapump_jobs; 
   SELECT * FROM DBA_DATAPUMP_SESSIONS; 
   select username,opname,target_desc,sofar,totalwork,message from V$SESSION_LONGOPS where username = 'bkpadmin';

```

**2. Datapump status:**

- Note down the job name from the import/export logs or data dictionary name and
- Run **attach** command:
- type status in Import/Export prompt

```sql
impdp <bkpadmin>/<bkp123> attach=<SYS_IMPORT_SCHEMA_01>
Import> status

```

Press press **CTRL+C** to come out of Import/Export prompt



## Step 3/6 : Create directory


```sql
create or replace directory DATAPUMP_REMOTE_DIR as '/oracle/scripts/expimp';

```



## Step 7 : Export Commands


Commands:

```sql
expdp <bkpadmin>/<bkp123>  parfile=<exp.par>

```

*Please replace the data in <> with appropriate values as per your environment. You can add/modify parameters as per your requirements. In the above example all the remaining parameters are added in parameter files as stated below: *

- Export Type : **User Export**
- Export entire schema
- Parameter file details [say exp.par] :

```sql
schemas=<schema>
directory= DATAPUMP_REMOTE_DIR
dumpfile=<dbname>_<schema>.dmp
logfile=exp_<dbname>_<schema>.log

```


- Export Type : **User Export for large schema**
- Export entire schema for large datasets: Here the export dump files will be broken down and compressed. Parallelism is used here **(Note : Adding parallelism will increase the CPU load on server)**
- Parameter file details [say exp.par] :

```sql
schemas=<schema>
directory= DATAPUMP_REMOTE_DIR
dumpfile=<dbname>_<schema>_%U.dmp
logfile=exp_<dbname>_<schema>.log 
compression = all
parallel=5

```


- Export Type : **Table Export** [ Export set of tables]
- Parameter file details [say exp.par] :

```sql
tables= tname1, tname2, tname3
directory= DATAPUMP_REMOTE_DIR
dumpfile=<dbname>_<schema>.dmp
logfile=exp_<dbname>_<schema>.log

```



## Step 9 : Import Commands


Prerequisite:

- Prior to user import it is a good practice to drop the schema or table imported.

Commands:

```sql
impdp <bkpadmin>/<bkp123>  parfile=<imp.par>

```

*Please replace the data in <> with appropriate values as per your environment. You can add/modify parameters as per your requirements. In the above example all the remaining parameters are added in parameter files as stated below: *

- Import Type : **User Import**
- Import entire schema
- Parameter file details [say imp.par] :

```sql
schemas=<schema>
directory= DATAPUMP_REMOTE_DIR
dumpfile=<dbname>_<schema>.dmp
logfile=imp_<dbname>_<schema>.log

```


- Import Type : **User Import for large schema**
- Import entire schema for large datasets: Parallelism is used here **(Note : Adding parallelism will increase the CPU load on server)**
- Parameter file details [say imp.par] :

```sql
schemas=<schema>
directory= DATAPUMP_REMOTE_DIR
dumpfile=<dbname>_<schema>_%U.dmp
logfile=imp_<dbname>_<schema>.log 
parallel=5

```


- Import Type : **Table Import** [ Import set of tables]
- Parameter file details [say imp.par] :

```sql
tables= tname1, tname2, tname3
directory= DATAPUMP_REMOTE_DIR
dumpfile=<dbname>_<schema>.dmp
logfile=exp_<dbname>_<schema>.log
TABLE_EXISTS_ACTION= <APPEND /SKIP /TRUNCATE /REPLACE>

```



## 1. Datapump steps


|Source Server [Export Data]|Target Server [Import Data]
|---|---|---|---|---|---|---|---|---|---
|1. Create a datapump folder that will contain the export dump files|4. Create a datapump folder that will contain the import dump files
|2. Login to database schema that will perform the export.|5. Login to database schema that will perform the import.
|3. Create directory pointing to step 1.|6. Create directory pointing to step 4.
|7. Run Export Statements.|
|8. Copy/SCP the dump files to Target Server.|
||9. Run Import statements
||10. check data ,compile invalid objects and provide related grants



## Copy tables between different schemas and tablespaces


```

expdp <bkpadmin>/<bkp123> directory=DATAPUMP_REMOTE_DIR dumpfile=<customer.dmp>


 impdp <bkpadmin>/<bkp123> directory=DATAPUMP_REMOTE_DIR dumpfile=<customer.dmp> remap_schema=<source schema>:<target schema> remap_tablespace=<source tablespace>:<target tablespace>

```

