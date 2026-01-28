---
metaTitle: "Microsoft SQL Server - Managing Azure SQL Database"
description: "Find service tier information for Azure SQL Database, Change service tier of Azure SQL Database, Replication of Azure SQL Database, Create Azure SQL Database in Elastic pool"
---

# Managing Azure SQL Database



## Find service tier information for Azure SQL Database


Azure SQL Database has different editions and performance tiers.

You can find version, edition (basic, standard, or premium), and service objective (S0,S1,P4,P11, etc.) of SQL Database that is running as a service in Azure using the following statements:

```sql
select @@version
SELECT DATABASEPROPERTYEX('Wwi', 'EDITION')
SELECT DATABASEPROPERTYEX('Wwi', 'ServiceObjective')

```



## Change service tier of Azure SQL Database


You can scale-up or scale-down Azure SQL database using ALTER DATABASE statement:

```sql
ALTER DATABASE WWI
MODIFY (SERVICE_OBJECTIVE = 'P6')
-- or
ALTER DATABASE CURRENT
MODIFY (SERVICE_OBJECTIVE = 'P2')

```

If you try to change service level while changing service level of the current database is still in progress you wil get the following error:

> 
<p>Msg 40802, Level 16, State 1, Line 1 A service objective assignment on
server '......' and database '.......' is already in progress.
Please wait until the service objective assignment state for the
database is marked as 'Completed'.</p>


Re-run your ALTER DATABASE statement when transition period finishes.



## Replication of Azure SQL Database


You can create a secondary replica of database with the same name on another Azure SQL Server, making the local database primary, and begins asynchronously replicating data from the primary to the new secondary.

```sql
ALTER DATABASE <<mydb>>
ADD SECONDARY ON SERVER <<secondaryserver>>
WITH ( ALLOW_CONNECTIONS = ALL ) 

```

Target server may be in another data center (usable for geo-replication).
If a database with the same name already exists on the target server, the command will fail. The command is executed on the master database on the server hosting the local database that will become the primary.
When ALLOW_CONNECTIONS is set to ALL (it is set to NO by default), secondary replica will be a read-only database that will allow all logins with the appropriate permissions to connect.

Secondary database replica might be promoted to primary using the following command:

```sql
ALTER DATABASE mydb FAILOVER 

```

You can remove the secondary database on secondary server:

```sql
ALTER DATABASE <<mydb>>
REMOVE SECONDARY ON SERVER <<testsecondaryserver>>

```



## Create Azure SQL Database in Elastic pool


You can put your azure SQL Database in SQL elastic pool:

```sql
CREATE DATABASE wwi
( SERVICE_OBJECTIVE = ELASTIC_POOL ( name = mypool1 ) ) 

```

You can create copy of an existing database and place it in some elastic pool:

```sql
CREATE DATABASE wwi
AS COPY OF myserver.WideWorldImporters  
( SERVICE_OBJECTIVE = ELASTIC_POOL ( name = mypool1 ) ) 

```

