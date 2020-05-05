---
metaTitle: "PostgreSQL - EXTENSION dblink and postgres_fdw"
description: "Extention FDW, Foreign Data Wrapper, Extention  dblink"
---

# EXTENSION dblink and postgres_fdw



## Extention FDW


FDW is an implimentation of dblink it is more helpful, so to use it:

1-Create an extention:

```sql
CREATE EXTENSION postgres_fdw;

```

2-Create SERVER:

```sql
CREATE SERVER name_srv FOREIGN DATA WRAPPER postgres_fdw OPTIONS (host 'hostname', 
dbname 'bd_name', port '5432');

```

3-Create user mapping for postgres server

```sql
CREATE USER MAPPING FOR postgres SERVER name_srv OPTIONS(user 'postgres', password 'password');

```

4-Create foreign table:

```sql
CREATE FOREIGN TABLE table_foreign (id INTEGER, code character varying) 
SERVER name_srv OPTIONS(schema_name 'schema', table_name 'table');

```

5-use this foreign table like it is in your database:

```sql
SELECT * FROM table_foreign;

```



## Foreign Data Wrapper


To access complete schema of server db instead of single table. Follow below steps:

1. Create EXTENSION :

1. Create SERVER :

1. Create USER MAPPING:

1. Create new schema to access schema of server DB:

1. Import server schema:

1. Access any table of server schema:

This can be used to access multiple schema of remote DB.



## Extention  dblink


dblink EXTENSION is a technique to connect another database and make operation of this database so to do that you need:

1-Create a dblink extention:

```sql
CREATE EXTENSION dblink;

```

2-Make your operation:

For exemple Select some attribute from another table in another database:

```sql
SELECT * FROM 
dblink ('dbname = bd_distance port = 5432 host = 10.6.6.6 user = username 
password = passw@rd', 'SELECT id, code FROM schema.table') 
AS newTable(id INTEGER, code character varying);

```



#### Syntax


<li>
<p>dblink ('dbname = name_db_distance port = PortOfDB host = HostOfDB user = usernameDB
password = passwordDB', 'MY QUESRY')</p>
</li>
<li>
dbname = name of the database
</li>
<li>
port = Port Of the database
</li>
<li>
host = Host Of the database
</li>
<li>
user = username of the database
</li>
<li>
password = password of the database',
</li>
<li>
MY QUESRY = this can be any operation i want to do SELECT, INSERT, ...
</li>

