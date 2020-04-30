---
metaTitle: "Database permissions"
description: "Changing permissions, CREATE USER, CREATE ROLE, Changing role membership"
---

# Database permissions



## Changing permissions


```sql
GRANT SELECT ON [dbo].[someTable] TO [aUser];

REVOKE SELECT ON [dbo].[someTable] TO [aUser];
--REVOKE SELECT [dbo].[someTable] FROM [aUser]; is equivalent

DENY SELECT ON [dbo].[someTable] TO [aUser];

```



## CREATE USER


```sql
--implicitly map this user to a login of the same name as the user
CREATE USER [aUser];

--explicitly mapping what login the user should be associated with
CREATE USER [aUser] FOR LOGIN [aUser];

```



## CREATE ROLE


```sql
CREATE ROLE [myRole];

```



## Changing role membership


```sql
-- SQL 2005+
exec sp_addrolemember @rolename = 'myRole', @membername = 'aUser';
exec sp_droprolemember @rolename = 'myRole', @membername = 'aUser';

-- SQL 2008+
ALTER ROLE [myRole] ADD MEMBER [aUser];
ALTER ROLE [myRole] DROP MEMBER [aUser];

```

Note: role members can be any database-level principal. That is, you can add a role as a member in another role. Also, adding/dropping role members is idempotent. That is, attempting to add/drop will result in their presence/absence (respectively) in the role regardless of the current state of their role membership.



#### Remarks


Basic Syntax:

```sql
{GRANT| REVOKE | DENY} {PERMISSION_NAME} [ON {SECURABLE}] TO {PRINCIPAL};

```


<li>{GRANT| REVOKE | DENY} - What you're trying to accomplish
<ul>
- Grant: "Give this permission to the stated principal"
- Revoke: "Take this permission away from the stated principal"
- Deny: "Make sure the stated principal never has this permission (i.e. "`DENY SELECT`" means that regardless of any other permissions, `SELECT` will fail for this principal)

