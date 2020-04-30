---
metaTitle: "DROP or DELETE Database"
description: "DROP Database"
---

# DROP or DELETE Database



## DROP Database


Dropping the database is a simple one-liner statement. Drop database will delete the database, hence always ensure to have a backup of the database if required.

Below is the command to drop Employees Database

```sql
DROP DATABASE [dbo].[Employees]

```



#### Syntax


- MSSQL Syntax:
- DROP DATABASE [ IF EXISTS ] { database_name | database_snapshot_name } [ ,...n ] [;]
- MySQL Syntax:
- DROP {DATABASE | SCHEMA} [IF EXISTS] db_name



#### Remarks


`DROP DATABASE` is used for dropping a database from SQL. Be sure to create a backup of your database before dropping it to prevent accidental loss of information.

