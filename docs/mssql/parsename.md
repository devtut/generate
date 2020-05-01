---
metaTitle: "Parsename"
description: "PARSENAME"
---

# Parsename



## PARSENAME


```sql
Declare @ObjectName nVarChar(1000) 
Set @ObjectName = 'HeadOfficeSQL1.Northwind.dbo.Authors' 

SELECT
 PARSENAME(@ObjectName, 4) as Server
,PARSENAME(@ObjectName, 3) as DB
,PARSENAME(@ObjectName, 2) as Owner
,PARSENAME(@ObjectName, 1) as Object 

```

Returns:

|Server|DB
|---|---|---|---
|HeadofficeSQL1|Northwind

|Owner|Object
|---|---|---|---
|dbo|Authors



#### Syntax


- PARSENAME ( 'object_name' , object_piece )



#### Parameters


|'object_name'|object_piece
|---|---|---|---
|Is the name of the object for which to retrieve the specified object part. object_name is sysname. This parameter is an optionally-qualified object name. If all parts of the object name are qualified, this name can have four parts: the server name, the database name, the owner name, and the object name.|Is the object part to return. object_piece is of type int, and can have these values:1 = Object name 2 = Schema name 3 = Database name 4 = Server name

