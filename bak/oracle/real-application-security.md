---
metaTitle: "Oracle Database - Real Application Security"
description: "Application"
---

# Real Application Security


Oracle Real Application Security was introduced in Oracle 12c. It summarize many Security Topics like User-Role-Model, Access Control, Application vs. Database, End-User-Security or Row- and Column Level Security



## Application


To associate an Application with something in the Database there are three main parts:

**Application Privilege:**
An Application Privilege describes Privileges like `SELECT`, `INSERT`, `UPDATE`, `DELETE`, ... Application Privileges can be summarized as an Aggregate Privilege.

```sql
XS$PRIVILEGE(
    name=>'privilege_name'
    [, implied_priv_list=>XS$NAME_LIST('"SELECT"', '"INSERT"', '"UPDATE"', '"DELETE"')]
)

XS$PRIVILEGE_LIST(
    XS$PRIVILEGE(...),
    XS$PRIVILEGE(...),
    ...
);

```

**Application User:**

Simple Application User:

```sql
BEGIN 
    SYS.XS_PRINCIPAL.CREATE_USER('user_name'); 
END;

```

Direct Login Application User:

```sql
BEGIN 
    SYS.XS_PRINCIPAL.CREATE_USER(name => 'user_name', schema => 'schema_name');
END;

BEGIN 
    SYS.XS_PRINCIPAL.SET_PASSWORD('user_name', 'password'); 
END;
CREATE PROFILE prof LIMIT 
    PASSWORD_REUSE_TIME 1/4440 
    PASSWORD_REUSE_MAX 3 
    PASSWORD_VERIFY_FUNCTION Verify_Pass;

BEGIN 
    SYS.XS_PRINCIPAL.SET_PROFILE('user_name', 'prof'); 
END;

BEGIN 
    SYS.XS_PRINCIPAL.GRANT_ROLES('user_name', 'XSONNCENT');
END;

```

(optional:)

```sql
BEGIN 
    SYS.XS_PRINCIPAL.SET_VERIFIER('user_name', '6DFF060084ECE67F', XS_PRINCIPAL.XS_SHA512“); 
END;

```

**Application Role:**

Regular Application Role:

```sql
DECLARE
    st_date TIMESTAMP WITH TIME ZONE;
    ed_date TIMESTAMP WITH TIME ZONE;
BEGIN
    st_date := SYSTIMESTAMP;
    ed_date := TO_TIMESTAMP_TZ('2013-06-18 11:00:00 -5:00','YYYY-MM-DD HH:MI:SS');
    SYS.XS_PRINCIPAL.CREATE_ROLE
        (name => 'app_regular_role',
        enabled => TRUE,
        start_date => st_date,
        end_date => ed_date);
END;

```

Dynamic Application Role: (gets enabled dynamical based on the authenatication state)

```sql
BEGIN
    SYS.XS_PRINCIPAL.CREATE_DYNAMIC_ROLE
        (name => 'app_dynamic_role',
        duration => 40,
        scope => XS_PRINCIPAL.SESSION_SCOPE);
END;

```

Predefined Application Roles:

Regular:

- `XSPUBLIC`
- `XSBYPASS`
- `XSSESSIONADMIN`
- `XSNAMESPACEADMIN`
- `XSPROVISIONER`
- `XSCACHEADMIN`
- `XSDISPATCHER`

Dynamic: (depended on the authentication state of application user)

- `DBMS_AUTH`: (direct-logon or other database authentication method)
- `EXTERNAL_DBMS_AUTH`: (direct-logon or other database authentication method and user is external)
- `DBMS_PASSWD`: (direct-logon with password)
- `MIDTIER_AUTH`: (authentication through middle tier application)
- `XSAUTHENTICATED`: (direct or middle tier application)
- `XSSWITCH`: (user switched from proxy user to application user)

