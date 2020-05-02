---
metaTitle: "Mysql Connection Pool"
description: "Using a connection pool without database"
---

# Mysql Connection Pool



## Using a connection pool without database


Achieving multitenancy on database server with multiple databases hosted on it.

Multitenancy is common requirement of enterprise application nowadays and creating connection pool for each database in database server is not recommended. so, what we can do instead is create connection pool with database server and than switch between databases hosted on database server on demand.

Suppose our application has different databases for each firm hosted on database server.
We will connect to respective firm database when user hits the application.
here is the example on how to do that:-

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

