---
metaTitle: "User Defined Table Types"
description: "creating a UDT with a single int column that is also a primary key, Creating a UDT with multiple columns, Creating a UDT with a unique constraint:, Creating a UDT with a primary key and a column with a default value:"
---

# User Defined Table Types




## creating a UDT with a single int column that is also a primary key


```sql
CREATE TYPE dbo.Ids as TABLE
(
    Id int PRIMARY KEY
)

```



## Creating a UDT with multiple columns


```sql
CREATE TYPE MyComplexType as TABLE
(
    Id int,
    Name varchar(10)
)

```



## Creating a UDT with a unique constraint:


```sql
CREATE TYPE MyUniqueNamesType as TABLE
(
    FirstName varchar(10),
    LastName varchar(10),
    UNIQUE (FirstName,LastName)
)

```

Note: constraints in user defined table types can not be named.



## Creating a UDT with a primary key and a column with a default value:


```sql
CREATE TYPE MyUniqueNamesType as TABLE
(
    FirstName varchar(10),
    LastName varchar(10),
    CreateDate datetime default GETDATE()
    PRIMARY KEY (FirstName,LastName)
)

```



#### Remarks


UDTs have following restrictions -

- can not be used as a column in a table or a field in a structured user-defined types
- a non-clustered index cannot be created in a UDT unless the index is the result of creating a PRIMARY KEY or UNIQUE constraint on the UDT
- UDT definition CANNOT be modified after it is created

