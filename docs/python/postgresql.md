---
metaTitle: "PostgreSQL"
description: "Getting Started"
---

# PostgreSQL




## Getting Started


PostgreSQL is an actively developed and mature open source database. Using the `psycopg2` module, we can execute queries on the database.

### Installation using pip

`pip install psycopg2`

### Basic usage

Lets assume we have a table `my_table` in the database `my_database` defined as follows.

|id|first_name|last_name
|------
|1|John|Doe

We can use the `psycopg2` module to run queries on the database in the following fashion.

```py
import psycopg2

# Establish a connection to the existing database 'my_database' using
# the user 'my_user' with password 'my_password'
con = psycopg2.connect("host=localhost dbname=my_database user=my_user password=my_password")

# Create a cursor
cur = con.cursor()

# Insert a record into 'my_table'
cur.execute("INSERT INTO my_table(id, first_name, last_name) VALUES (2, 'Jane', 'Doe');")

# Commit the current transaction
con.commit()

# Retrieve all records from 'my_table'
cur.execute("SELECT * FROM my_table;")
results = cur.fetchall()

# Close the database connection
con.close()

# Print the results
print(results)

# OUTPUT: [(1, 'John', 'Doe'), (2, 'Jane', 'Doe')]

```

