---
metaTitle: "Connect to Mongodb"
description: "Simple example to Connect mongoDB from Node.JS, Simple way to Connect mongoDB with core Node.JS"
---

# Connect to Mongodb


MongoDB is a free and open-source cross-platform document-oriented database program. Classified as a NoSQL database program, MongoDB uses JSON-like documents with schemas.

For more details go to [https://www.mongodb.com/](https://www.mongodb.com/)



## Simple example to Connect mongoDB from Node.JS


```js
MongoClient.connect('mongodb://localhost:27017/myNewDB',function (err,db) {
    if(err) 
        console.log("Unable to connect DB. Error: " + err)
    else 
        console.log('Connected to DB');

    db.close();
});

```

myNewDB is DB name, if it does not exists in database then it will create automatically with this call.



## Simple way to Connect mongoDB with core Node.JS


```

 var MongoClient = require('mongodb').MongoClient;
    
    //connection with mongoDB
    MongoClient.connect("mongodb://localhost:27017/MyDb", function (err, db) {
          //check the connection
 if(err){
           console.log("connection failed.");      
}else{
                    console.log("successfully connected to mongoDB.");
    });

```



#### Syntax


- MongoClient.connect('mongodb://127.0.0.1:27017/crud',function (err,db) {//do womething here});

