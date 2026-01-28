---
metaTitle: "Microsoft SQL Server - SCOPE_IDENTITY()"
description: "Introduction with Simple Example"
---

# SCOPE_IDENTITY()



## Introduction with Simple Example


SCOPE_IDENTITY() returns the last identity value inserted into an identity column in the same scope. A scope is a module: a stored procedure, trigger, function, or batch. Therefore, two statements are in the same scope if they are in the same stored procedure, function, or batch.

INSERT INTO  ([column1],[column2]) VALUES (8,9);<br />
GO<br />
SELECT SCOPE_IDENTITY() AS [SCOPE_IDENTITY];<br />
GO



#### Syntax


- SELECT SCOPE_IDENTITY();
- SELECT SCOPE_IDENTITY() AS [SCOPE_IDENTITY];
- SCOPE_IDENTITY()

