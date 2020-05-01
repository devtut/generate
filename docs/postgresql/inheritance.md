---
metaTitle: "Inheritance"
description: "Creating children tables, Altering tables"
---

# Inheritance



## Creating children tables


```sql
CREATE TABLE users (username text, email text);
CREATE TABLE simple_users () INHERITS (users);
CREATE TABLE users_with_password (password text) INHERITS (users);

```

Our three tables look like this:

### users

|Column|Type
|---|---|---|---
|username|text
|email|text

### simple_users

|Column|Type
|---|---|---|---
|username|text
|email|text

### users_with_password

|Column|Type
|---|---|---|---
|username|text
|email|text
|password|text



## Altering tables


Let's create two simple tables:

```sql
CREATE TABLE users (username text, email text);
CREATE TABLE simple_users () INHERITS (users);

```

### Adding columns

```sql
ALTER TABLE simple_users ADD COLUMN password text;

```

### simple_users

|Column|Type
|---|---|---|---
|username|text
|email|text
|password|text

Adding the same column to the parent table will merge the definition of both columns:

```sql
ALTER TABLE users ADD COLUMN password text;

```

> 
NOTICE:  merging definition of column "password" for child "simple_users"


### Dropping columns

Using our altered tables:

```sql
ALTER TABLE users DROP COLUMN password;

```

### users

|Column|Type
|---|---|---|---
|username|text
|email|text

### simple_users

|Column|Type
|---|---|---|---
|username|text
|email|text
|password|text

Since we first added the column to `simple_users`, PostgreSQL makes sure this column isn't dropped.

Now if we had another child table, its `password` column would, of course, have been dropped.



#### Remarks


An explanation as to why you would want to use inheritance in PostgreSQL is available here: [http://stackoverflow.com/a/3075248/653378](http://stackoverflow.com/a/3075248/653378)

