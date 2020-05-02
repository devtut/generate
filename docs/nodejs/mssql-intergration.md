---
metaTitle: "MSSQL Intergration"
description: "Connecting with SQL via. mssql npm module"
---

# MSSQL Intergration


To integrate any database with nodejs you need a driver package or you can call it a npm module which will provide you with basic API to connect with the database and perform interactions .
Same is true with mssql database , here we will integrate mssql with nodejs and perform some basic queries on SQL tabels.



## Connecting with SQL via. mssql npm module


We will start with creating a simple node application with a basic structure and then connecting with local sql server database and performing some queries on that database.

**Step 1:** Create a directory/folder by the name of project which you intent to create. Initialize a node application using **npm init** command which will create a package.json in current directory .

```js
mkdir mySqlApp
//folder created 
cd mwSqlApp
//change to newly created directory
npm init
//answer all the question ..
npm install
//This will complete quickly since we have not added any packages to our app.

```

**Step 2:** Now we will create a App.js file in this directory and install some packages which we are going to need to connect to sql db.

```js
sudo gedit App.js
//This will create App.js file , you can use your fav. text editor :)
npm install --save mssql
//This will install the mssql package to you app

```

**Step 3:** Now we will add a basic configuration variable to our application which will be used by mssql module to establish a connection .

```js
console.log("Hello world, This is an app to connect to sql server.");
var config = {
        "user": "myusername", //default is sa
        "password": "yourStrong(!)Password",
        "server": "localhost", // for local machine
        "database": "staging", // name of database
        "options": {
            "encrypt": true
        }
      }

sql.connect(config, err => { 
    if(err){
        throw err ;
    }
    console.log("Connection Successful !");

    new sql.Request().query('select 1 as number', (err, result) => {
        //handle err
        console.dir(result)
        // This example uses callbacks strategy for getting results.
    })
        
});

sql.on('error', err => {
    // ... error handler 
    console.log("Sql database connection error " ,err);
})

```

**Step 4:** This is the easiest step ,where we start the application and the application will connect to the sql server and print out some simple results .

```js
node App.js
// Output : 
// Hello world, This is an app to connect to sql server.
// Connection Successful !
// 1

```

> 
To use promises or async for query execution refer the official documents of the mssql package :
<ul>
- [Promises](https://www.npmjs.com/package/mssql#promises)
- [Async/Await](https://www.npmjs.com/package/mssql#async-await)
</ul>




#### Remarks


We have assumed that we will have a local instance of mssql database server running on local machine . You can refer [this document](https://docs.microsoft.com/en-us/sql/linux/sql-server-linux-setup-ubuntu) to do the same .

Also make sure you appropriate user created with privileges added as well.

