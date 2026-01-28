---
metaTitle: "SQL - GRANT and REVOKE"
description: "Grant/revoke privileges"
---

# GRANT and REVOKE



## Grant/revoke privileges


```sql
GRANT SELECT, UPDATE
ON Employees
TO User1, User2;

```

Grant `User1` and `User2` permission to perform `SELECT` and `UPDATE` operations on table `Employees`.

```sql
REVOKE SELECT, UPDATE
ON Employees
FROM User1, User2;

```

Revoke from `User1` and `User2` the permission to perform `SELECT` and `UPDATE` operations on table Employees.



#### Syntax


- GRANT [privilege1] [, [privilege2] ... ] ON [table] TO [grantee1] [, [grantee2] ... ] [ WITH GRANT OPTION ]
- REVOKE [privilege1] [, [privilege2] ... ] ON [table] FROM [grantee1] [, [grantee2] ... ]



#### Remarks


Grant permissions to users. If the `WITH GRANT OPTION` is specified, the grantee additionally gains the privilege to grant the given permission or revoke previously granted permissions.

