---
metaTitle: "Permissions and Security"
description: "Assign Object Permissions to a user"
---

# Permissions and Security




## Assign Object Permissions to a user


In Production its good practice to secure your data and only allow operations on it to be undertaken via Stored Procedures. This means your application can't directly run CRUD operations on your data and potentially cause problems. Assigning permissions is a time-consuming, fiddly and generally onerous task. For this reason its often easier to harness some of the (considerable) power contained in the [INFORMATION_SCHEMA](https://msdn.microsoft.com/en-GB/library/ms186778.aspx) er schema which is contained in every SQL Server database.

Instead individually assigning permissions to a user on a piece-meal basis, just run the script below, copy the output and then run it in a Query window.

```sql
SELECT 'GRANT EXEC ON core.' + r.ROUTINE_NAME + ' TO ' + <MyDatabaseUsername>
FROM INFORMATION_SCHEMA.ROUTINES r 
WHERE r.ROUTINE_CATALOG = '<MyDataBaseName>'

```

