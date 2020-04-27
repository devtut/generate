---
metaTitle Database Access
description SQLite, Accessing MySQL database using MySQLdb, Connection, PostgreSQL Database access using psycopg2, Oracle database, Using sqlalchemy
---

# Database Access




## SQLite


SQLite is a lightweight, disk-based database. Since it does not require a separate database server, it is often used for prototyping or for small applications that are often used by a single user or by one user at a given time.

```
import sqlite3

conn = sqlite3.connect("users.db")
c = conn.cursor()

c.execute("CREATE TABLE user (name text, age integer)")

c.execute("INSERT INTO user VALUES ('User A', 42)")
c.execute("INSERT INTO user VALUES ('User B', 43)")

conn.commit()

c.execute("SELECT * FROM user")
print(c.fetchall())

conn.close()

```

The code above connects to the database stored in the file named `users.db`, creating the file first if it doesn't already exist. You can interact with the database via SQL statements.

The result of this example should be:

```
[(u'User A', 42), (u'User B', 43)]

```

### The SQLite Syntax: An in-depth analysis

### Getting started

<li>
Import the sqlite module using
<pre><code>>>> import sqlite3
</code></pre>
</li>
<li>
To use the module, you must first create a Connection object that represents the database. Here the data will be stored in the example.db file:
<pre><code>>>> conn = sqlite3.connect('users.db')
</code></pre>
Alternatively, you can also supply the special name `:memory:` to create a temporary database in RAM, as follows:
<pre><code>>>> conn = sqlite3.connect(':memory:')
</code></pre>
</li>
<li>
Once you have a `Connection`, you can create a `Cursor` object and call its `execute()` method to perform SQL commands:
<h3></h3>
<pre><code>c = conn.cursor()

# Create table
c.execute('''CREATE TABLE stocks
            (date text, trans text, symbol text, qty real, price real)''')

# Insert a row of data
c.execute("INSERT INTO stocks VALUES ('2006-01-05','BUY','RHAT',100,35.14)")

# Save (commit) the changes
conn.commit()

# We can also close the connection if we are done with it.
# Just be sure any changes have been committed or they will be lost.
conn.close()
</code></pre>
</li>

### Important Attributes and Functions of `Connection`

<li>
`isolation_level`
It is an attribute used to get or set the current isolation level. None for autocommit mode or one of `DEFERRED`, `IMMEDIATE` or `EXCLUSIVE`.
</li>

<li>
`cursor`
The cursor object is used to execute SQL commands and queries.
</li>

<li>
`commit()`
Commits the current transaction.
</li>

<li>
`rollback()`
Rolls back any changes made since the previous call to `commit()`
</li>

<li>
`close()`
Closes the database connection. It does not call `commit()` automatically. If `close()` is called without first calling `commit()` (assuming you are not in autocommit mode) then all changes made will be lost.
</li>

<li>
`total_changes`
An attribute that logs the total number of rows modified, deleted or inserted since the database was opened.
</li>
<li>
`execute`, `executemany`, and `executescript`
These functions perform the same way as those of the cursor object. This is a shortcut since calling these functions through the connection object results in the creation of an intermediate cursor object and calls the corresponding method of the cursor object
</li>

<li>
`row_factory`
You can change this attribute to a callable that accepts the cursor and the original row as a tuple and will return the real result row.
<pre><code>def dict_factory(cursor, row):
    d = {}
    for i, col in enumerate(cursor.description):
        d[col[0]] = row[i]
    return d

conn = sqlite3.connect(":memory:")
conn.row_factory = dict_factory
</code></pre>
</li>

### Important Functions of `Cursor`

<li>
`execute(sql[, parameters])`
<p>Executes a **single** SQL statement. The SQL statement may be parametrized (i. e. placeholders instead of SQL literals).
The sqlite3 module supports two kinds of placeholders: question marks `?` (“qmark style”) and named placeholders `:name` (“named style”).</p>
<pre><code>import sqlite3
conn = sqlite3.connect(":memory:")
cur = conn.cursor()
cur.execute("create table people (name, age)")

who = "Sophia"
age = 37
# This is the qmark style:
cur.execute("insert into people values (?, ?)",
            (who, age))

# And this is the named style:
cur.execute("select * from people where name=:who and age=:age",
            {"who": who, "age": age})  # the keys correspond to the placeholders in SQL

print(cur.fetchone())
</code></pre>
</li>

> 
<p>Beware: don't use `%s` for inserting strings into SQL commands as it
can make your program vulnerable to an SQL injection attack (see
[SQL Injection](https://stackoverflow.com/documentation/sql/3517/sql-injection) ).</p>


<li>
`executemany(sql, seq_of_parameters)`
Executes an SQL command against all parameter sequences or mappings found in the sequence sql. The sqlite3 module also allows using an iterator yielding parameters instead of a sequence.
<pre><code>L = [(1, 'abcd', 'dfj', 300),    # A list of tuples to be inserted into the database
     (2, 'cfgd', 'dyfj', 400),
     (3, 'sdd', 'dfjh', 300.50)]                           

conn = sqlite3.connect("test1.db")
conn.execute("create table if not exists book (id int, name text, author text, price real)")
conn.executemany("insert into book values (?, ?, ?, ?)", L)

for row in conn.execute("select * from book"):
    print(row)
</code></pre>
You can also pass iterator objects as a parameter to executemany, and the function will iterate over the each tuple of values that the iterator returns. The iterator must return a tuple of values.
<pre><code>import sqlite3

class IterChars:
    def __init__(self):
        self.count = ord('a')

    def __iter__(self):
        return self

    def __next__(self):            # (use next(self) for Python 2)
        if self.count > ord('z'):
            raise StopIteration
        self.count += 1
        return (chr(self.count - 1),) 

conn = sqlite3.connect("abc.db")
cur = conn.cursor()
cur.execute("create table characters(c)")

theIter = IterChars()
cur.executemany("insert into characters(c) values (?)", theIter)

rows = cur.execute("select c from characters")
for row in rows:
    print(row[0]),
</code></pre>
</li>
<li>
`executescript(sql_script)`
This is a nonstandard convenience method for executing multiple SQL statements at once. It issues a `COMMIT` statement first, then executes the SQL script it gets as a parameter.
`sql_script` can be an instance of `str` or `bytes`.
<pre><code>import sqlite3
conn = sqlite3.connect(":memory:")
cur = conn.cursor()
cur.executescript("""
     create table person(
         firstname,
         lastname,
         age
     );

     create table book(
         title,
         author,
         published
     );

     insert into book(title, author, published)
     values (
         'Dirk Gently''s Holistic Detective Agency',
         'Douglas Adams',
         1987
     );
     """)
</code></pre>
The next set of functions are used in conjunction with `SELECT` statements in SQL. To retrieve data after executing a `SELECT` statement, you can either treat the cursor as an iterator, call the cursor’s `fetchone()` method to retrieve a single matching row, or call `fetchall()` to get a list of the matching rows.
Example of the iterator form:
<pre><code>import sqlite3
stocks = [('2006-01-05', 'BUY', 'RHAT', 100, 35.14),
          ('2006-03-28', 'BUY', 'IBM', 1000, 45.0),
          ('2006-04-06', 'SELL', 'IBM', 500, 53.0),
          ('2006-04-05', 'BUY', 'MSFT', 1000, 72.0)]
conn = sqlite3.connect(":memory:")
conn.execute("create table stocks (date text, buysell text, symb text, amount int, price real)")
conn.executemany("insert into stocks values (?, ?, ?, ?, ?)", stocks)    
cur = conn.cursor()

for row in cur.execute('SELECT * FROM stocks ORDER BY price'):
    print(row)

# Output:
# ('2006-01-05', 'BUY', 'RHAT', 100, 35.14)
# ('2006-03-28', 'BUY', 'IBM', 1000, 45.0)
# ('2006-04-06', 'SELL', 'IBM', 500, 53.0)
# ('2006-04-05', 'BUY', 'MSFT', 1000, 72.0)
</code></pre>
</li>
<li>
`fetchone()`
Fetches the next row of a query result set, returning a single sequence, or None when no more data is available.
<pre><code>cur.execute('SELECT * FROM stocks ORDER BY price')
i = cur.fetchone()
while(i): 
    print(i)
    i = cur.fetchone()

# Output:
# ('2006-01-05', 'BUY', 'RHAT', 100, 35.14)
# ('2006-03-28', 'BUY', 'IBM', 1000, 45.0)
# ('2006-04-06', 'SELL', 'IBM', 500, 53.0)
# ('2006-04-05', 'BUY', 'MSFT', 1000, 72.0)
</code></pre>
</li>
<li>
`fetchmany(size=cursor.arraysize)`
Fetches the next set of rows of a query result (specified by size), returning a list. If size is omitted, fetchmany returns a single row. An empty list is returned when no more rows are available.
<pre><code>cur.execute('SELECT * FROM stocks ORDER BY price')
print(cur.fetchmany(2))

# Output:    
# [('2006-01-05', 'BUY', 'RHAT', 100, 35.14), ('2006-03-28', 'BUY', 'IBM', 1000, 45.0)]
</code></pre>
</li>
<li>
`fetchall()`
Fetches all (remaining) rows of a query result, returning a list.
<pre><code>cur.execute('SELECT * FROM stocks ORDER BY price')
print(cur.fetchall())

# Output:
# [('2006-01-05', 'BUY', 'RHAT', 100, 35.14), ('2006-03-28', 'BUY', 'IBM', 1000, 45.0), ('2006-04-06', 'SELL', 'IBM', 500, 53.0), ('2006-04-05', 'BUY', 'MSFT', 1000, 72.0)]
</code></pre>
</li>

### SQLite and Python data types

SQLite natively supports the following types: NULL, INTEGER, REAL, TEXT, BLOB.

This is how the data types are converted when moving from SQL to Python or vice versa.

```
                None     <->     NULL
                int      <->     INTEGER/INT
                float    <->     REAL/FLOAT
                str      <->     TEXT/VARCHAR(n)
                bytes    <->     BLOB

```



## Accessing MySQL database using MySQLdb


The first thing you need to do is create a connection to the database using the connect method. After that, you will need a cursor that will operate with that connection.

Use the execute method of the cursor to interact with the database, and every once in a while, commit the changes using the commit method of the connection object.

Once everything is done, don't forget to close the cursor and the connection.

Here is a Dbconnect class with everything you'll need.

```
import MySQLdb

class Dbconnect(object):

    def __init__(self):

        self.dbconection = MySQLdb.connect(host='host_example',
                                           port=int('port_example'),
                                           user='user_example',
                                           passwd='pass_example',
                                           db='schema_example')
        self.dbcursor = self.dbconection.cursor()

    def commit_db(self):
        self.dbconection.commit()

    def close_db(self):
        self.dbcursor.close()
        self.dbconection.close()

```

Interacting with the database is simple. After creating the object, just use the execute method.

```
db = Dbconnect()
db.dbcursor.execute('SELECT * FROM %s' % 'table_example')

```

If you want to call a stored procedure, use the following syntax. Note that the parameters list is optional.

```
db = Dbconnect()
db.callproc('stored_procedure_name', [parameters] )

```

After the query is done, you can access the results multiple ways. The cursor object is a generator that can fetch all the results or be looped.

```
results = db.dbcursor.fetchall()
for individual_row in results:
    first_field = individual_row[0]

```

If you want a loop using directly the generator:

```
for individual_row in db.dbcursor:
    first_field = individual_row[0]

```

If you want to commit changes to the database:

```
db.commit_db()

```

If you want to close the cursor and the connection:

```
db.close_db()

```



## Connection


**Creating a connection**

According to PEP 249, the connection to a database should be established using a `connect()` constructor, which returns a `Connection` object. The arguments for this constructor are database dependent. Refer to the database specific topics for the relevant arguments.

```
import MyDBAPI

con = MyDBAPI.connect(*database_dependent_args)

```

This connection object has four methods:

**1: close**

```
con.close()

```

Closes the connection instantly. Note that the connection is automatically closed if the `Connection.__del___` method is called. Any pending transactions will implicitely be rolled back.

**2: commit**

```
con.commit()

```

Commits any pending transaction the to database.

**3: rollback**

```
con.rollback()

```

Rolls back to the start of any pending transaction. In other words: this cancels any non-committed transaction to the database.

**4: cursor**

```
cur = con.cursor()

```

Returns a `Cursor` object. This is used to do transactions on the database.



## PostgreSQL Database access using psycopg2


****psycopg2**** is the most popular PostgreSQL database adapter that is both lightweight and efficient. It is the current implementation of the PostgreSQL adapter.

> 
<p>Its main features are the complete implementation of the Python DB API
2.0 specification and the thread safety (several threads can share the same connection)</p>


### Establishing a connection to the database and creating a table

```
import psycopg2

# Establish a connection to the database.
# Replace parameter values with database credentials.
conn = psycopg2.connect(database="testpython", 
                        user="postgres",
                        host="localhost",
                        password="abc123",
                        port="5432") 

# Create a cursor. The cursor allows you to execute database queries. 
cur = conn.cursor()

# Create a table. Initialise the table name, the column names and data type. 
cur.execute("""CREATE TABLE FRUITS (
                    id          INT ,
                    fruit_name  TEXT,
                    color       TEXT,
                    price       REAL
            )""")
conn.commit()
conn.close()

```

### Inserting data into the table:

```
# After creating the table as shown above, insert values into it.
cur.execute("""INSERT INTO FRUITS (id, fruit_name, color, price)
               VALUES (1, 'Apples', 'green', 1.00)""")

cur.execute("""INSERT INTO FRUITS (id, fruit_name, color, price)
               VALUES (1, 'Bananas', 'yellow', 0.80)""")

```

### Retrieving table data:

```
# Set up a query and execute it 
cur.execute("""SELECT id, fruit_name, color, price 
             FROM fruits""")

# Fetch the data 
rows = cur.fetchall()

# Do stuff with the data
for row in rows:
    print "ID = {} ".format(row[0])
    print "FRUIT NAME = {}".format(row[1])
    print("COLOR = {}".format(row[2]))
    print("PRICE = {}".format(row[3]))

```

The output of the above would be:

```
ID = 1 
NAME = Apples
COLOR = green
PRICE = 1.0

ID = 2 
NAME = Bananas
COLOR = yellow
PRICE = 0.8

```

And so, there you go, you now know half of
all you need to know about ****psycopg2****! :)



## Oracle database


**Pre-requisites:**

- cx_Oracle package - See [here](https://pypi.python.org/pypi/cx_Oracle/5.2.1) for all versions
- Oracle instant client - For [Windows x64](http://www.oracle.com/technetwork/topics/winx64soft-089540.html), [Linux x64](http://www.oracle.com/technetwork/topics/linuxx86-64soft-092277.html)

**Setup:**

<li>
Install the cx_Oracle package as:
`sudo rpm -i <YOUR_PACKAGE_FILENAME>`
</li>
<li>
Extract the Oracle instant client and set environment variables as:
</li>

```
ORACLE_HOME=<PATH_TO_INSTANTCLIENT>
PATH=$ORACLE_HOME:$PATH
LD_LIBRARY_PATH=<PATH_TO_INSTANTCLIENT>:$LD_LIBRARY_PATH

```

**Creating a connection:**

```
import cx_Oracle

class OraExec(object):
    _db_connection = None
    _db_cur = None

    def __init__(self):
        self._db_connection = 
            cx_Oracle.connect('<USERNAME>/<PASSWORD>@<HOSTNAME>:<PORT>/<SERVICE_NAME>')
        self._db_cur = self._db_connection.cursor()

```

**Get database version:**

```
ver = con.version.split(".")
print ver

```

Sample Output:
['12', '1', '0', '2', '0']

**Execute query: SELECT**

```
_db_cur.execute("select * from employees order by emp_id")
for result in _db_cur:
    print result

```

Output will be in Python tuples:

(10, 'SYSADMIN', 'IT-INFRA', 7)

(23, 'HR ASSOCIATE', 'HUMAN RESOURCES', 6)

**Execute query: INSERT**

```
_db_cur.execute("insert into employees(emp_id, title, dept, grade) 
                values (31, 'MTS', 'ENGINEERING', 7)
_db_connection.commit()

```

When you perform insert/update/delete operations in an Oracle Database, the changes are only available within your session until `commit` is issued. When the updated data is committed to the database, it is then available to other users and sessions.

**Execute query: INSERT using Bind variables**

[Reference](http://www.oracle.com/technetwork/articles/dsl/python-091105.html)

Bind variables enable you to re-execute statements with new values, without the overhead of re-parsing the statement. Bind variables improve code re-usability, and can reduce the risk of SQL Injection attacks.

```
rows = [ (1, "First" ),
     (2, "Second" ),
     (3, "Third" ) ]
_db_cur.bindarraysize = 3
_db_cur.setinputsizes(int, 10)
_db_cur.executemany("insert into mytab(id, data) values (:1, :2)", rows)
_db_connection.commit()

```

**Close connection:**

```
_db_connection.close()

```

The close() method closes the connection. Any connections not explicitly closed will be automatically released when the script ends.



## Using sqlalchemy


To use sqlalchemy for database:

```
from sqlalchemy import create_engine
from sqlalchemy.engine.url import URL


url = URL(drivername='mysql',
          username='user',
          password='passwd',
          host='host',
          database='db')

engine = create_engine(url)  # sqlalchemy engine

```

Now this engine can be used:
e.g. with pandas to fetch dataframes directly from mysql

```
import pandas as pd

con = engine.connect()
dataframe = pd.read_sql(sql=query, con=con)

```



#### Remarks


Python can handle many different types of databases. For each of these types a different API exists. So encourage similarity between those different API's, PEP 249 has been introduced.

> 
This API has been defined to encourage similarity between the Python modules that are used to access databases. By doing this, we hope to achieve a consistency leading to more easily understood modules, code that is generally more portable across databases, and a broader reach of database connectivity from Python. [PEP-249](https://www.python.org/dev/peps/pep-0249/)


