---
metaTitle: "Schemas"
description: "Purpose, Creating a Schema, Alter Schema, Dropping Schemas"
---

# Schemas



## Purpose


Schema refers to a specific database tables and how they are related to each other. It provides an organisational blueprint of how the database is constructed. Additional benefits of implementing database schemas is that schemas can be used as a method restricting / granting access to specific tables within a database.



## Creating a Schema


```sql
CREATE SCHEMA dvr AUTHORIZATION Owner
    CREATE TABLE sat_Sales (source int, cost int, partid int)
    GRANT SELECT ON SCHEMA :: dvr TO  User1
    DENY SELECT ON SCHEMA :: dvr to User 2
GO

```



## Alter Schema


```sql
ALTER SCHEMA dvr
    TRANSFER dbo.tbl_Staging;
GO

```

This would transfer the tbl_Staging table from the dbo schema to the dvr schema



## Dropping Schemas


```sql
DROP SCHEMA dvr

```

