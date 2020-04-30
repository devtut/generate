---
metaTitle: "Advanced options"
description: "Enable and show advanced options, Enable backup compression default, Enable cmd permission , Set default fill factor percent , Set system recovery interval, Set max server memory size, Set number of checkpoint tasks"
---

# Advanced options



## Enable and show advanced options


```sql
Exec sp_configure 'show advanced options' ,1
RECONFIGURE
GO
-- Show all configure
sp_configure

```



## Enable backup compression default


```sql
Exec sp_configure 'backup compression default',1
GO  
RECONFIGURE;

```



## Enable cmd permission 


```sql
EXEC sp_configure 'xp_cmdshell', 1
GO
RECONFIGURE

```



## Set default fill factor percent 


```sql
sp_configure 'fill factor', 100;  
GO  
RECONFIGURE;  

```

The server must be restarted before the change can take effect.



## Set system recovery interval


```sql
USE master;  
GO 
-- Set recovery every 3 min
EXEC sp_configure 'recovery interval', '3';  
RECONFIGURE WITH OVERRIDE;  

```



## Set max server memory size


```sql
USE master
EXEC sp_configure 'max server memory (MB)', 64
RECONFIGURE WITH OVERRIDE

```



## Set number of checkpoint tasks


```sql
EXEC sp_configure "number of checkpoint tasks", 4

```

