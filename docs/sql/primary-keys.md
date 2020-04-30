---
metaTitle: "Primary Keys"
description: "Creating a Primary Key, Using Auto Increment"
---

# Primary Keys




## Creating a Primary Key


```sql
CREATE TABLE Employees (
    Id int NOT NULL,
    PRIMARY KEY (Id),
    ...
);

```

This will create the Employees table with 'Id' as its primary key. The primary key can be used to uniquely identify the rows of a table. Only one primary key is allowed per table.

A key can also be composed by one or more fields, so called composite key, with the following syntax:

```sql
CREATE TABLE EMPLOYEE (
    e1_id INT,
    e2_id INT,
    PRIMARY KEY (e1_id, e2_id)
) 

```



## Using Auto Increment


Many databases allow to make the primary key value automatically increment when a new key is added. This ensures that every key is different.

[**MySQL**](https://dev.mysql.com/doc/refman/5.7/en/create-table.html#create-table-types-attributes)

```sql
CREATE TABLE Employees (
    Id int NOT NULL AUTO_INCREMENT,
    PRIMARY KEY (Id)
);

```

[**PostgreSQL**](https://www.postgresql.org/docs/current/static/datatype-numeric.html#DATATYPE-SERIAL)

```sql
CREATE TABLE Employees (
    Id SERIAL PRIMARY KEY
);

```

[**SQL Server**](https://msdn.microsoft.com/en-us/library/ms186775.aspx)

```sql
CREATE TABLE Employees (
    Id int NOT NULL IDENTITY,
    PRIMARY KEY (Id)
);

```

[**SQLite**](http://www.sqlite.org/autoinc.html)

```sql
CREATE TABLE Employees (
    Id INTEGER PRIMARY KEY    
);

```



#### Syntax


<li>
<p>MySQL:
CREATE TABLE Employees (
Id int NOT NULL,
PRIMARY KEY (Id),
...
);</p>
</li>
<li>
<p>Others: CREATE TABLE Employees (
Id int NOT NULL PRIMARY KEY,
...
);</p>
</li>

