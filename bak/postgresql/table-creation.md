---
metaTitle: "PostgreSQL - Table Creation"
description: "Table creation with Primary Key, Show table definition, Create table from select, Create unlogged table, Create a table that references other table."
---

# Table Creation




## Table creation with Primary Key


```sql
CREATE TABLE person (
    person_id BIGINT NOT NULL,
    last_name VARCHAR(255) NOT NULL,
    first_name VARCHAR(255),
    address VARCHAR(255),
    city VARCHAR(255),
    PRIMARY KEY (person_id)
);

```

Alternatively, you can place the `PRIMARY KEY` constraint directly in the column definition:

```sql
CREATE TABLE person (
    person_id BIGINT NOT NULL PRIMARY KEY,
    last_name VARCHAR(255) NOT NULL,
    first_name VARCHAR(255),
    address VARCHAR(255),
    city VARCHAR(255)
);

```

It is recommended that you use lower case names for the table and as well as all the columns. If you use upper case names such as `Person` you would have to wrap that name in double quotes (`"Person"`) in each and every query because PostgreSQL enforces case folding.



## Show table definition


Open the `psql` command line tool connected to the database where your table is. Then type the following command:

```sql
\d tablename

```

To get extended information type

```sql
\d+ tablename

```

If you have forgotten the name of the table, just type \d into psql to obtain a list of tables and views in the current database.



## Create table from select


Let's say you have a table called person:

```sql
CREATE TABLE person (
    person_id BIGINT NOT NULL,
    last_name VARCHAR(255) NOT NULL,
    first_name VARCHAR(255),
    age INT NOT NULL,
    PRIMARY KEY (person_id)
);     

```

You can create a new table of people over 30 like this:

```sql
CREATE TABLE people_over_30 AS SELECT * FROM person WHERE age > 30;

```



## Create unlogged table


You can create unlogged tables so that you can make the tables considerably faster. Unlogged table skips writing `write-ahead` log which means it's not crash-safe and unable to replicate.

```sql
CREATE UNLOGGED TABLE person (
    person_id BIGINT NOT NULL PRIMARY KEY,
    last_name VARCHAR(255) NOT NULL,
    first_name VARCHAR(255),
    address VARCHAR(255),
    city VARCHAR(255)
);

```



## Create a table that references other table.


In this example, User Table will have a column that references the Agency table.

```sql
CREATE TABLE agencies ( -- first create the agency table
  id SERIAL PRIMARY KEY,
  name TEXT NOT NULL
)

CREATE TABLE users (
  id SERIAL PRIMARY KEY,
  agency_id NOT NULL INTEGER REFERENCES agencies(id) DEFERRABLE INITIALLY DEFERRED -- this is going to references your agency table.
)

```

