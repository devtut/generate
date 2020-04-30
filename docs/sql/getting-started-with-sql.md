---
metaTitle: "Getting started with SQL"
description: "Overview"
---

# Getting started with SQL



## Overview


Structured Query Language (SQL) is a special-purpose programming language designed for managing data held in a Relational Database Management System (RDBMS).
SQL-like languages can also be used in Relational Data Stream Management Systems (RDSMS), or in "not-only SQL" (NoSQL) databases.

SQL comprises of 3 major sub-languages:

1. Data Definition Language (DDL): to create and modify the structure of the database;
1. Data Manipulation Language (DML): to perform Read, Insert, Update and Delete operations on the data of the database;
1. Data Control Language (DCL): to control the access of the data stored in the database.

[SQL article on Wikipedia](https://en.wikipedia.org/wiki/SQL)

The core DML operations are Create, Read, Update and Delete (CRUD for short) which are performed by the statements `INSERT`, `SELECT`, `UPDATE` and `DELETE`.<br />
There is also a (recently added) `MERGE` statement which can perform all 3 write operations (INSERT, UPDATE, DELETE).

[CRUD article on Wikipedia](https://en.wikipedia.org/wiki/Create,_read,_update_and_delete)

Many SQL databases are implemented as client/server systems; the term "SQL server" describes such a database.<br />
At the same time, Microsoft makes a database that is named "SQL Server". While that database speaks a dialect of SQL, information specific to that database is not on topic in this tag but belongs into the [SQL Server documentation](http://stackoverflow.com/documentation/sql-server).



#### Remarks


SQL is Structured Query Language used to manage data in a relational database system.
Different vendors have improved upon the language and have variety of flavors for the language.

NB: This tag refers explicitly to the **ISO/ANSI SQL standard**; not to any specific implementation of that standard.

