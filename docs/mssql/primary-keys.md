---
metaTitle: "Primary Keys"
description: "Create table w/ identity column as primary key, Create table w/ GUID primary key, Create table w/ natural key, Create table w/ composite key, Add primary key to existing table, Delete primary key"
---

# Primary Keys



## Create table w/ identity column as primary key


```

-- Identity primary key - unique arbitrary increment number
 create table person (
 id int identity(1,1) primary key not null,
 firstName varchar(100) not null,
 lastName varchar(100) not null,
 dob DateTime not null,
 ssn varchar(9) not null
 )

```



## Create table w/ GUID primary key


```

-- GUID primary key - arbitrary unique value for table
 create table person (
 id uniqueIdentifier default (newId()) primary key,
 firstName varchar(100) not null,
 lastName varchar(100) not null,
 dob DateTime not null,
 ssn varchar(9) not null
 )

```



## Create table w/ natural key


```

-- natural primary key - using an existing piece of data within the table that uniquely identifies the record
 create table person (
 firstName varchar(100) not null,
 lastName varchar(100) not null,
 dob DateTime not null,
 ssn varchar(9) primary key not null
 )

```



## Create table w/ composite key


```

-- composite key - using two or more existing columns within a table to create a primary key
 create table person (
 firstName varchar(100) not null,
 lastName varchar(100) not null,
 dob DateTime not null,
 ssn varchar(9) not null,
 primary key (firstName, lastName, dob)
 )

```



## Add primary key to existing table


```sql
ALTER TABLE person
 ADD CONSTRAINT pk_PersonSSN PRIMARY KEY (ssn)

```

Note, if the primary key column (in this case `ssn`) has more than one row with the same candidate key, the above statement will fail, as primary key values must be unique.



## Delete primary key


```sql
ALTER TABLE Person
 DROP CONSTRAINT pk_PersonSSN

```



#### Remarks


Primary keys are used to uniquely identify a record in a table. A table may only have a single primary key (though the primary key can consist of multiple columns), and a primary key is required for certain types of replication.

Primary keys are often used as (but don't have to be) the [clustered index](http://stackoverflow.com/questions/1251636/what-do-clustered-and-non-clustered-index-actually-mean) on a table.

