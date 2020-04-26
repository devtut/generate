# Connecting Python to SQL Server




## Connect to Server, Create Table, Query Data


Install the package:

`$ pip install pymssql`

```
import pymssql

SERVER = &quot;servername&quot;
USER = &quot;username&quot;
PASSWORD = &quot;password&quot;
DATABASE = &quot;dbname&quot;

connection = pymssql.connect(server=SERVER, user=USER, 
                password=PASSWORD, database=DATABASE)

cursor = connection.cursor() # to access field as dictionary use cursor(as_dict=True)
cursor.execute(&quot;SELECT TOP 1 * FROM TableName&quot;)
row = cursor.fetchone()

######## CREATE TABLE ########
cursor.execute(&quot;&quot;&quot;
CREATE TABLE posts (
    post_id INT PRIMARY KEY NOT NULL,
    message TEXT,
    publish_date DATETIME
)
&quot;&quot;&quot;)

######## INSERT DATA IN TABLE ########
cursor.execute(&quot;&quot;&quot;
    INSERT INTO posts VALUES(1, &quot;Hey There&quot;, &quot;11.23.2016&quot;)
&quot;&quot;&quot;)
# commit your work to database
connection.commit()

######## ITERATE THROUGH RESULTS  ########
cursor.execute(&quot;SELECT TOP 10 * FROM posts ORDER BY publish_date DESC&quot;)
for row in cursor:
    print(&quot;Message: &quot; + row[1] + &quot; | &quot; + &quot;Date: &quot; + row[2])
    # if you pass as_dict=True to cursor
    # print(row[&quot;message&quot;])

connection.close()


```

You can do anything if your work is related with SQL expressions, just pass this expressions to the execute method(CRUD operations).

For with statement, calling stored procedure, error handling or more example check: [pymssql.org](http://pymssql.org)

