---
metaTitle: "Privileges or Permissions"
description: "Simple rules"
---

# Privileges or Permissions



## Simple rules


**Granting permission to create tables**

```sql
USE AdventureWorks;  
GRANT CREATE TABLE TO MelanieK;  
GO 

```

**Granting SHOWPLAN permission to an application role**

```sql
USE AdventureWorks2012;  
GRANT SHOWPLAN TO AuditMonitor;  
GO  

```

**Granting CREATE VIEW with GRANT OPTION**

```sql
USE AdventureWorks2012;  
GRANT CREATE VIEW TO CarmineEs WITH GRANT OPTION;  
GO 

```

**Granting all rights to a user on a specific database**

```sql
use YourDatabase
go
exec sp_addrolemember 'db_owner', 'UserName'
go

```

