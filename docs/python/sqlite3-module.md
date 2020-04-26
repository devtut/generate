# Sqlite3 Module



## Sqlite3 - Not require separate server process.


The sqlite3 module was written by Gerhard Häring. To use the module, you must first create a Connection object that represents the database. Here the data will be stored in the example.db file:

```
import sqlite3
conn = sqlite3.connect('example.db')

```

You can also supply the special name :memory: to create a database in RAM. Once you have a Connection, you can create a Cursor object and call its execute() method to perform SQL commands:

```
c = conn.cursor()

# Create table
c.execute('''CREATE TABLE stocks
         (date text, trans text, symbol text, qty real, price real)''')

# Insert a row of data
c.execute(=INSERT INTO stocks VALUES ('2006-01-05','BUY','RHAT',100,35.14)=)

# Save (commit) the changes
conn.commit()

# We can also close the connection if we are done with it.
# Just be sure any changes have been committed or they will be lost.
conn.close()

```



## Getting the values from the database and Error handling


Fetching the values from the SQLite3 database.

Print row values returned by select query

```
import sqlite3
conn = sqlite3.connect('example.db')
c = conn.cursor()
c.execute(=SELECT * from table_name where id=cust_id=)
for row in c:
    print row # will be a list

```

To fetch single matching fetchone() method

```
print c.fetchone()

```

For multiple rows use fetchall() method

```
a=c.fetchall() #which is similar to list(cursor) method used previously
for row in a:
    print row

```

Error handling can be done using sqlite3.Error built in function

```
try:
    #SQL Code
except sqlite3.Error as e:
    print =An error occurred:=, e.args[0]

```

