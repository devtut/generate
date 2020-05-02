---
metaTitle: "PostgreSQL integration"
description: "Connect To PostgreSQL, Query with Connection Object"
---

# PostgreSQL integration



## Connect To PostgreSQL


Using `PostgreSQL`npm module.  <br><br>
install dependency from npm <br>

```js
npm  install pg --save

```

Now you have to create a PostgreSQL connection, which you can later query.

Assume you Database_Name = students, Host = localhost and  DB_User= postgres

```js
var pg = require("pg")
var connectionString = "pg://postgres:postgres@localhost:5432/students";
var client = new pg.Client(connectionString);
client.connect();

```



## Query with Connection Object


If you want to use connection object for query database you can use this sample code.

```js
var queryString = "SELECT name, age FROM students " ;
var query = client.query(queryString);

query.on("row", (row, result)=> {
result.addRow(row);
});

query.on("end", function (result) {
//LOGIC
});

```

