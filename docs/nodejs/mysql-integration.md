---
metaTitle: "MySQL integration"
description: "Connect to MySQL, Using a connection pool, Query a connection object with parameters, Query a connection object without parameters, Run a number of queries with a single connection from a pool, Export Connection Pool, Return the query when an error occurs"
---

# MySQL integration


In this topic you will learn how to integrate with Node.js using MYSQL database management tool. You will learn various ways to connect and interact with data residing in mysql using a nodejs program and script.



## Connect to MySQL


One of the easiest ways to connect to MySQL is by using [`mysql`](https://github.com/mysqljs/mysql) module. This module handles the connection between Node.js app and MySQL server.
You can install it like any other module:

```js
npm install --save mysql

```

Now you have to create a mysql connection, which you can later query.

```js
const mysql      = require('mysql');
const connection = mysql.createConnection({
  host     : 'localhost',
  user     : 'me',
  password : 'secret',
  database : 'database_schema'
});

connection.connect();

// Execute some query statements
// I.e. SELECT * FROM FOO

connection.end();

```

In the next example you will learn how to query the `connection` object.



## Using a connection pool


### a. Running multiple queries at same time

All queries in MySQL connection are done one after another. It means that if you want to do 10 queries and each query takes 2 seconds then it will take 20 seconds to complete whole execution. The solution is to create 10 connection and run each query in a different connection. This can be done automatically using connection pool

```js
var pool  = mysql.createPool({
  connectionLimit : 10,
  host            : 'example.org',
  user            : 'bobby',
  password        : 'pass',
  database        : 'schema'
});

for(var i=0;i<10;i++){
  pool.query('SELECT ` as example', function(err, rows, fields) {
    if (err) throw err;
    console.log(rows[0].example); //Show 1
  });
 }

```

It will run all the 10 queries in parallel.

When you use `pool` you don't need the connection anymore. You can query directly the pool. MySQL module will search for the next free connection to execute your query.

### b. Achieving multi-tenancy on database server with different databases hosted on it.

Multitenancy is a common requirement of enterprise application nowadays and creating connection pool for each database in database server is not recommended. so, what we can do instead is create connection pool with database server and then switch them between databases hosted on database server on demand.

Suppose our application has different databases for each firm hosted on database server.
We will connect to respective firm database when user hits the application.
Here is the example on how to do that:-

```js
var pool  = mysql.createPool({
      connectionLimit : 10,
      host            : 'example.org',
      user            : 'bobby',
      password        : 'pass'
    });
    
pool.getConnection(function(err, connection){
    if(err){
        return cb(err);
    }
    connection.changeUser({database : "firm1"});
    connection.query("SELECT * from history", function(err, data){
        connection.release();
        cb(err, data);
    });
});

```

Let me break down the example:-

When defining pool configuration i did not gave the database name but only gave database server i.e

```js
{
  connectionLimit : 10,
  host            : 'example.org',
  user            : 'bobby',
  password        : 'pass'
}

```

so when we want to use the specific database on database server, we ask the connection to hit database by using:-

```

   connection.changeUser({database : "firm1"});

```

you can refer the official documentation [here](https://github.com/mysqljs/mysql#switching-users-and-altering-connection-state)



## Query a connection object with parameters


When you want to use user generated content in the SQL, it with done with parameters. For example for searching user with the name `aminadav` you should do:

```js
var username = 'aminadav';
var querystring = 'SELECT name, email from users where name = ?'; 
connection.query(querystring, [username], function(err, rows, fields) {
  if (err) throw err;
  if (rows.length) {
    rows.forEach(function(row) {
      console.log(row.name, 'email address is', row.email);
    });
  } else {
    console.log('There were no results.');
  }
});

```



## Query a connection object without parameters


You send the query as a string and in response callback with the answer is received.
The callback gives you `error`, array of `rows` and fields.
Each row contains all the column of the returned table. Here is a snippet for the following explanation.

```js
connection.query('SELECT name,email from users', function(err, rows, fields) {
  if (err) throw err;

  console.log('There are:', rows.length,' users');
  console.log('First user name is:',rows[0].name)
});

```



## Run a number of queries with a single connection from a pool


There may be situations where you have setup a pool of MySQL connections, but you have a number of queries you would like to run in sequence:

You **could** just run then using `pool.query` [as seen elsewhere](http://stackoverflow.com/documentation/node.js/1406/mysql-integration/4587/using-a-connection-pool), however if you only have one free connection in the pool you must wait until a connection becomes available before you can run the second query.

You can, however, retain an active connection from the pool and run as many queries as you would like using a single connection using `pool.getConnection`:

**Note:** You must remember to `release` the connection, otherwise there is one less MySQL connection available to the rest of the pool!

For more information on pooling MySQL connections [check out the MySQL docs](https://www.npmjs.com/package/mysql#pooling-connections).



## Export Connection Pool


```js
// db.js

const mysql = require('mysql');

const pool = mysql.createPool({
  connectionLimit : 10,
  host            : 'example.org',
  user            : 'bob',
  password        : 'secret',
  database        : 'my_db'
});

module.export = {
  getConnection: (callback) => {
    return pool.getConnection(callback);
  } 
}

```

```js
// app.js

const db = require('./db');

db.getConnection((err, conn) => {
  conn.query('SELECT something from sometable', (error, results, fields) => {
    // get the results
    conn.release();
  });
});

```



## Return the query when an error occurs


You can attach the query executed to your `err` object when an error occurs:

