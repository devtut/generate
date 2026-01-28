---
metaTitle: "Oracle Database - Database Links"
description: "Creating a database link, Create Database Link"
---

# Database Links



## Creating a database link


```sql
CREATE DATABASE LINK dblink_name 
CONNECT TO remote_username 
IDENTIFIED BY remote_password 
USING 'tns_service_name';

```

The remote DB will then be accessible in the following way:

```sql
SELECT * FROM MY_TABLE@dblink_name;

```

To test a database link connection without needing to know any of the object names in the linked database, use the following query:

```sql
SELECT * FROM DUAL@dblink_name;

```

To explicitly specify a domain for the linked database service, the domain name is added to the `USING` statement. For example:

```sql
USING 'tns_service_name.WORLD'

```

If no domain name is explicitly specified, Oracle uses the domain of the database in which the link is being created.

Oracle documentation for database link creation:

- 10g: [https://docs.oracle.com/cd/B19306_01/server.102/b14200/statements_5005.htm](https://docs.oracle.com/cd/B19306_01/server.102/b14200/statements_5005.htm)
- 11g: [https://docs.oracle.com/cd/B28359_01/server.111/b28310/ds_concepts002.htm](https://docs.oracle.com/cd/B28359_01/server.111/b28310/ds_concepts002.htm)
- 12g: [https://docs.oracle.com/database/121/SQLRF/statements_5006.htm#SQLRF01205](https://docs.oracle.com/database/121/SQLRF/statements_5006.htm#SQLRF01205)



## Create Database Link


Let we assume we have two databases "ORA1" and "ORA2". We can access the objects of "ORA2" from database "ORA1" using a database link.

Prerequisites:
For creating a private Database link you need a `CREATE DATABASE LINK` privilege.
For creating a private Database link you need a `CREATE PUBLIC DATABASE LINK` privilege.

*[Oracle Net](https://docs.oracle.com/cd/B19306_01/gateways.102/b16223/net.htm#i1153556) must be present on both the instances.

How to create a database link:

From ORA1:

```sql
SQL> create <public> database link ora2 connect to user1 identified by pass1 using <tns name of ora2>;

```

Database link created.

Now that we have the DB link set up, we can prove that by running the following from ORA1:

```sql
SQL> Select name from V$DATABASE@ORA2; -- should return ORA2

```

You can also access the DB Objects of "ORA2" from "ORA1", given the user `user1` has the `SELECT` privilege on those objects on ORA2 (such as TABLE1 below):

```

 SELECT COUNT(*) FROM TABLE1@ORA2;

```

Pre-requistes:

- Both databases must be up and running (opened).
- Both database listeners must be up and running.
- TNS must be configured correctly.
- User user1 must be present in ORA2 database, password must be checked and verified.
- User user1 must have at least the `SELECT` privilege, or any other required to access the objects on ORA2.

