---
metaTitle: "Backup and Restore Database"
description: "Basic Backup to disk with no options, Basic Restore from disk with no options, RESTORE Database with REPLACE"
---

# Backup and Restore Database



## Basic Backup to disk with no options


The following command backs up the **'Users'** database to **'D:\DB_Backup'** file. Its better to not give an extension.

`BACKUP DATABASE Users TO DISK = 'D:\DB_Backup'`



## Basic Restore from disk with no options


The following command restores the **'Users'** database from **'D:\DB_Backup'** file.

`RESTORE DATABASE Users FROM DISK = 'D:\DB_Backup'`



## RESTORE Database with REPLACE


When you try to restore database from another server you might get the following error:

> 
<p>Error 3154: The backup set holds a backup of a database other than the
existing database.</p>


In that case you should use WITH REPLACE option to replace database with the database from backup:

```sql
RESTORE DATABASE WWIDW
FROM DISK = 'C:\Backup\WideWorldImportersDW-Full.bak'
WITH REPLACE

```

Even in this case you might get the errors saying that files cannot be located on some path:

> 
<p>Msg 3156, Level 16, State 3, Line 1 File 'WWI_Primary' cannot be
restored to 'D:\Data\WideWorldImportersDW.mdf'. Use WITH MOVE to
identify a valid location for the file.</p>


This error happens probably because your files were not placed on the same folder path that exist on new server. In that case you should move individual database files to new location:

```sql
RESTORE DATABASE WWIDW
FROM DISK = 'C:\Backup\WideWorldImportersDW-Full.bak'
WITH REPLACE,
MOVE 'WWI_Primary' to 'C:\Data\WideWorldImportersDW.mdf',
MOVE 'WWI_UserData' to 'C:\Data\WideWorldImportersDW_UserData.ndf',
MOVE 'WWI_Log' to 'C:\Data\WideWorldImportersDW.ldf',
MOVE 'WWIDW_InMemory_Data_1' to 'C:\Data\WideWorldImportersDW_InMemory_Data_1'

```

With this statement you can replace database with all database files moved to new location.



#### Syntax


- BACKUP DATABASE **database** TO **backup_device [ ,...n ]** WITH **with_options [ ,...o ]**
- RESTORE DATABASE **database** FROM **backup_device [ ,...n ]** WITH **with_options [ ,...o ]**



#### Parameters


|Parameter|Details
|------
|**database**|The name of the database to backup or restore
|**backup_device**|The device to backup or restore the database from, Like {DISK or TAPE}. Can be separated by commas ( , )
|**with_options**|Various options which can be used while performing the operation. Like formatting the disk where the backup is to be placed or restoring the database with replace option.

