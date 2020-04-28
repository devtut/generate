---
metaTitle: "SQLite3"
description: "Querying a database, Retrieving only one result, SQLite3 Quickstart Tutorial"
---

# SQLite3



## Querying a database


```
<?php
//Create a new SQLite3 object from a database file on the server.
$database = new SQLite3('mysqlitedb.db');

//Query the database with SQL
$results = $database->query('SELECT bar FROM foo');

//Iterate through all of the results, var_dumping them onto the page
while ($row = $results->fetchArray()) {
    var_dump($row);
}
?>

```

See also [http://stackoverflow.com/documentation/sql/topics](http://stackoverflow.com/documentation/sql/topics)



## Retrieving only one result


In addition to using LIMIT SQL statements you can also use the SQLite3 function `querySingle` to retrieve a single row, or the first column.

```
<?php
$database = new SQLite3('mysqlitedb.db');

//Without the optional second parameter set to true, this query would return just
//the first column of the first row of results and be of the same type as columnName
$database->querySingle('SELECT column1Name FROM table WHERE column2Name=1');

//With the optional entire_row parameter, this query would return an array of the
//entire first row of query results.
$database->querySingle('SELECT column1Name, column2Name FROM user WHERE column3Name=1', true);
?>

```



## SQLite3 Quickstart Tutorial


This is a complete example of all the commonly used SQLite related APIs. The aim is to get you up and running really fast. You can also get a [runnable PHP file](https://gist.github.com/bladeSk/6294d3266370868601a7d2e50285dbf5) of of this tutorial.

### Creating/opening a database

Let's create a new database first. Create it only if the file doesn't exist and open it for reading/writing.
The extension of the file is up to you, but `.sqlite` is pretty common and self-explanatory.

```
$db = new SQLite3('analytics.sqlite', SQLITE3_OPEN_CREATE | SQLITE3_OPEN_READWRITE);

```

### Creating a table

```
$db->query('CREATE TABLE IF NOT EXISTS "visits" (
    "id" INTEGER PRIMARY KEY AUTOINCREMENT NOT NULL,
    "user_id" INTEGER,
    "url" VARCHAR,
    "time" DATETIME
)');

```

### Inserting sample data.

It's advisable to wrap related queries in a transaction (with keywords `BEGIN` and `COMMIT`),
even if you don't care about atomicity. If you don't do this, SQLite automatically wraps every single query in a transaction, which slows down everything immensely. If you're new to SQLite, you may be surprised why the [INSERTs are so slow ](http://stackoverflow.com/a/3852082/388994).

```
$db->exec('BEGIN');
$db->query('INSERT INTO "visits" ("user_id", "url", "time")
    VALUES (42, "/test", "2017-01-14 10:11:23")');
$db->query('INSERT INTO "visits" ("user_id", "url", "time")
    VALUES (42, "/test2", "2017-01-14 10:11:44")');
$db->exec('COMMIT');

```

Insert potentially unsafe data with a prepared statement.
You can do this with **named parameters**:

```
$statement = $db->prepare('INSERT INTO "visits" ("user_id", "url", "time")
    VALUES (:uid, :url, :time)');
$statement->bindValue(':uid', 1337);
$statement->bindValue(':url', '/test');
$statement->bindValue(':time', date('Y-m-d H:i:s'));
$statement->execute(); you can reuse the statement with different values

```

### Fetching data

Let's fetch today's visits of user #42.
We'll use a prepared statement again, but with **numbered parameters** this time, which are more concise:

```
$statement = $db->prepare('SELECT * FROM "visits" WHERE "user_id" = ? AND "time" >= ?');
$statement->bindValue(1, 42);
$statement->bindValue(2, '2017-01-14');
$result = $statement->execute();

echo "Get the 1st row as an associative array:\n";
print_r($result->fetchArray(SQLITE3_ASSOC));
echo "\n";

echo "Get the next row as a numeric array:\n";
print_r($result->fetchArray(SQLITE3_NUM));
echo "\n";

```

> 
Note: If there are no more rows, fetchArray() returns `false`. You can take advantage of this in a `while` loop.


Free the memory - this in **not** done automatically, while your script is running

```
$result->finalize();

```

### Shorthands

Here's a useful shorthand for fetching a single row as an associative array.
The second parameter means we want all the selected columns.

Watch out, this shorthand doesn't support parameter binding, but you can
escape the strings instead.
Always put the values in SINGLE quotes! Double quotes are used for table
and column names (similar to backticks in MySQL).

```
$query = 'SELECT * FROM "visits" WHERE "url" = \'' .
    SQLite3::escapeString('/test') .
    '\' ORDER BY "id" DESC LIMIT 1';

$lastVisit = $db->querySingle($query, true);

echo "Last visit of '/test':\n";
print_r($lastVisit);
echo "\n";

```

Another useful shorthand for retrieving just one value.

```
$userCount = $db->querySingle('SELECT COUNT(DISTINCT "user_id") FROM "visits"');

echo "User count: $userCount\n";
echo "\n";

```

### Cleaning up

Finally, close the database.
This is done automatically when the script finishes, though.

```
$db->close();

```

