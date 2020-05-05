---
metaTitle: "PostgreSQL - Role Management"
description: "Create a user with a password, Create Role and matching database, Grant and Revoke Privileges., Alter default search_path of user, Grant access privileges on objects created in the future., Create Read Only User"
---

# Role Management



## Create a user with a password


Generally you should avoid using the default database role (often `postgres`) in your application. You should instead create a user with lower levels of privileges. Here we make one called `niceusername` and give it a password `very-strong-password`

```sql
CREATE ROLE niceusername with PASSWORD 'very-strong-password' LOGIN;

```

The problem with that is that queries typed into the `psql` console get saved in a history file `.psql_history` in the user's home directory and may as well be logged to the PostgreSQL database server log, thus exposing the password.

To avoid this, use the `\password` command to set the user password. If the user issuing the command is a superuser, the current password will not be asked. (Must be superuser to alter passwords of superusers)

```sql
CREATE ROLE niceusername with LOGIN;
\password niceusername

```



## Create Role and matching database


To support a given application, you often create a new role and database to match.

The shell commands to run would be these:

```sql
$ createuser -P blogger
Enter password for the new role: ********
Enter it again: ********

$ createdb -O blogger blogger

```

This assumes that `pg_hba.conf` has been properly configured, which probably looks like this:

```sql
# TYPE  DATABASE        USER            ADDRESS                 METHOD
host    sameuser        all             localhost               md5
local   sameuser        all                                     md5

```



## Grant and Revoke Privileges.


Suppose, that we have three users :

1. The Administrator of the database > admin
1. The application with a full access for her data > read_write
1. The read only access > read_only

```sql
--ACCESS DB
REVOKE CONNECT ON DATABASE nova FROM PUBLIC;
GRANT  CONNECT ON DATABASE nova  TO user;

```

With the above queries, untrusted users can no longer connect to the database.

```sql
--ACCESS SCHEMA
REVOKE ALL     ON SCHEMA public FROM PUBLIC;
GRANT  USAGE   ON SCHEMA public  TO user;

```

The next set of queries revoke all privileges from unauthenticated users and provide limited set of privileges for the `read_write` user.

```sql
--ACCESS TABLES
REVOKE ALL ON ALL TABLES IN SCHEMA public FROM PUBLIC ;
GRANT SELECT                         ON ALL TABLES IN SCHEMA public TO read_only ;
GRANT SELECT, INSERT, UPDATE, DELETE ON ALL TABLES IN SCHEMA public TO read_write ;
GRANT ALL                            ON ALL TABLES IN SCHEMA public TO admin ;

--ACCESS SEQUENCES
REVOKE ALL   ON ALL SEQUENCES IN SCHEMA public FROM PUBLIC;
GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO read_only; -- allows the use of CURRVAL
GRANT UPDATE ON ALL SEQUENCES IN SCHEMA public TO read_write; -- allows the use of NEXTVAL and SETVAL
GRANT USAGE  ON ALL SEQUENCES IN SCHEMA public TO read_write; -- allows the use of CURRVAL and NEXTVAL
GRANT ALL    ON ALL SEQUENCES IN SCHEMA public TO admin;

```



## Alter default search_path of user


With the below commands, user's default search_path can be set.

1. Check search path before set default schema.

```sql
postgres=# \c postgres user1
You are now connected to database "postgres" as user "user1".
postgres=> show search_path;
  search_path   
----------------
 "$user",public
(1 row)

```


1. Set `search_path` with `alter user` command to append a new schema `my_schema`

```sql
postgres=> \c postgres postgres
You are now connected to database "postgres" as user "postgres".
postgres=# alter user user1 set search_path='my_schema, "$user", public';
ALTER ROLE

```


1. Check result after execution.

```sql
postgres=# \c postgres user1
Password for user user1: 
You are now connected to database "postgres" as user "user1".
postgres=> show search_path;
 search_path 
-------------
 my_schema, "$user", public
(1 row)

```

Alternative:

```sql
postgres=# set role user1;
postgres=# show search_path;
 search_path 
-------------
 my_schema, "$user", public
(1 row)

```



## Grant access privileges on objects created in the future.


Suppose, that we have `three users` :

1. The Administrator of the database > `admin`
1. The application with a full access for her data > `read_write`
1. The read only access > `read_only`

With below queries, you can set access privileges on objects created in the future in specified schema.

```sql
ALTER DEFAULT PRIVILEGES IN SCHEMA myschema GRANT SELECT                      ON TABLES TO read_only;
ALTER DEFAULT PRIVILEGES IN SCHEMA myschema GRANT SELECT,INSERT,DELETE,UPDATE ON TABLES TO read_write;
ALTER DEFAULT PRIVILEGES IN SCHEMA myschema GRANT ALL                         ON TABLES TO admin;

```

Or, you can set access privileges on objects created in the future by specified user.

```sql
ALTER DEFAULT PRIVILEGES FOR ROLE admin GRANT SELECT  ON TABLES TO read_only;

```



## Create Read Only User


```sql
CREATE USER readonly WITH ENCRYPTED PASSWORD 'yourpassword';
GRANT CONNECT ON DATABASE <database_name> to readonly;

GRANT USAGE ON SCHEMA public to readonly;
GRANT SELECT ON ALL SEQUENCES IN SCHEMA public TO readonly;
GRANT SELECT ON ALL TABLES IN SCHEMA public TO readonly;

```



#### Syntax


<li>
`CREATE ROLE name [ [ WITH ] option [ ... ] ]`
</li>
<li>
`CREATE USER name [ [ WITH ] option [ ... ] ]`
</li>
<li>
`where option can be: SUPERUSER | NOSUPERUSER | CREATEDB | NOCREATEDB | CREATEROLE | NOCREATEROLE | CREATEUSER | NOCREATEUSER | INHERIT | NOINHERIT | LOGIN | NOLOGIN | CONNECTION LIMIT connlimit | [ ENCRYPTED | UNENCRYPTED ] PASSWORD 'password' | VALID UNTIL 'timestamp' | IN ROLE role_name [, ...] | IN GROUP role_name [, ...] | ROLE role_name [, ...] | ADMIN role_name [, ...] | USER role_name [, ...] | SYSID uid`
</li>

