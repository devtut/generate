---
metaTitle: "Mongodb integration"
description: "Connect to MongoDB, Insert a document, Read a collection, Update a document, Delete a document, Delete multiple documents, Simple connect, Simple connect, using promises"
---

# Mongodb integration



## Connect to MongoDB


Connect to MongoDB, print 'Connected!' and close the connection.

```js
const MongoClient = require('mongodb').MongoClient;

var url = 'mongodb://localhost:27017/test';

MongoClient.connect(url, function(err, db) { // MongoClient method 'connect'
    if (err) throw new Error(err);
    console.log("Connected!");
    db.close(); // Don't forget to close the connection when you are done
});

```

### MongoClient method `Connect()`

> 
MongoClient.connect(**url**, **options**, **callback**)


|Argument|Type|Description
|---|---|---
|`url`|string|A string specifying the server ip/hostname, port and database
|`options`|object|**(optional)** Optional settings **(default: null)**
|`callback`|Function|Function to be called when the connection attempt is done

The `callback` function takes two arguments

- `err` : Error - If an error occurs the `err` argument will be defined
- `db` : object -  The MongoDB instance



## Insert a document


Insert a document called 'myFirstDocument' and set **2** properties, `greetings` and `farewell`

```js
const MongoClient = require('mongodb').MongoClient;

const url = 'mongodb://localhost:27017/test';

MongoClient.connect(url, function (err, db) {
  if (err) throw new Error(err);
  db.collection('myCollection').insertOne({ // Insert method 'insertOne'
    "myFirstDocument": {
      "greetings": "Hellu",
      "farewell": "Bye"
    }
  }, function (err, result) {
    if (err) throw new Error(err);
    console.log("Inserted a document into the myCollection collection!");
    db.close(); // Don't forget to close the connection when you are done
  });
});

```

### Collection method `insertOne()`

> 
db.collection(**collection**).insertOne(**document**, **options**, **callback**)


|Argument|Type|Description
|---|---|---
|`collection`|string|A string specifying the collection
|`document`|object|The document to be inserted into the collection
|`options`|object|**(optional)** Optional settings **(default: null)**
|`callback`|Function|Function to be called when the insert operation is done

The `callback` function takes two arguments

- `err` : Error - If an error occurs the `err` argument will be defined
- `result` : object -  An object containing details about the insert operation



## Read a collection


Get all documents in the collection 'myCollection' and print them to the console.

```js
const MongoClient = require('mongodb').MongoClient;

const url = 'mongodb://localhost:27017/test';

MongoClient.connect(url, function (err, db) {
  if (err) throw new Error(err);
  var cursor = db.collection('myCollection').find(); // Read method 'find'
  cursor.each(function (err, doc) {
    if (err) throw new Error(err);
    if (doc != null) {
      console.log(doc); // Print all documents
    } else {
      db.close(); // Don't forget to close the connection when you are done
    }
  });
});

```

### Collection method `find()`

> 
db.collection(**collection**).find()


|Argument|Type|Description
|---|---|---
|`collection`|string|A string specifying the collection



## Update a document


Find a document with the property `{ greetings: 'Hellu' }` and change it to `{ greetings: 'Whut?' }`

```js
const MongoClient = require('mongodb').MongoClient;

const url = 'mongodb://localhost:27017/test';

MongoClient.connect(url, function (err, db) {
    if (err) throw new Error(err);
    db.collection('myCollection').updateOne({ // Update method 'updateOne'
        greetings: "Hellu" }, 
        { $set: { greetings: "Whut?" }},
        function (err, result) {
            if (err) throw new Error(err);
            db.close(); // Don't forget to close the connection when you are done
        });
});

```

### Collection method `updateOne()`

> 
db.collection(**collection**).updateOne(**filter**, **update**, **options**. **callback**)


|Parameter|Type|Description
|---|---|---
|`filter`|object|Specifies the selection critera
|`update`|object|Specifies the modifications to apply
|`options`|object|**(optional)** Optional settings **(default: null)**
|`callback`|Function|Function to be called when the operation is done

The `callback` function takes two arguments

- `err` : Error - If an error occurs the err argument will be defined
- `db` : object - The MongoDB instance



## Delete a document


Delete a document with the property `{ greetings: 'Whut?' }`

```js
const MongoClient = require('mongodb').MongoClient;

const url = 'mongodb://localhost:27017/test';

MongoClient.connect(url, function (err, db) {
    if (err) throw new Error(err);
    db.collection('myCollection').deleteOne(// Delete method 'deleteOne'
        { greetings: "Whut?" },
        function (err, result) {
            if (err) throw new Error(err);
            db.close(); // Don't forget to close the connection when you are done
    });
});

```

### Collection method `deleteOne()`

> 
db.collection(**collection**).deleteOne(**filter**, **options**, **callback**)


|Parameter|Type|Description
|---|---|---
|`filter`|object|A document specifying the selection critera
|`options`|object|**(optional)** Optional settings **(default: null)**
|`callback`|Function|Function to be called when the operation is done

The `callback` function takes two arguments

- `err` : Error - If an error occurs the err argument will be defined
- `db` : object - The MongoDB instance



## Delete multiple documents


Delete ALL documents with a 'farewell' property set to 'okay'.

```js
const MongoClient = require('mongodb').MongoClient;

const url = 'mongodb://localhost:27017/test';

MongoClient.connect(url, function (err, db) {
    if (err) throw new Error(err);
    db.collection('myCollection').deleteMany(// MongoDB delete method 'deleteMany'
        { farewell: "okay" }, // Delete ALL documents with the property 'farewell: okay'
        function (err, result) {
            if (err) throw new Error(err);
            db.close(); // Don't forget to close the connection when you are done
    });
});

```

### Collection method `deleteMany()`

> 
db.collection(**collection**).deleteMany(**filter**, **options**, **callback**)


|Parameter|Type|Description
|---|---|---
|`filter`|document|A document specifying the selection critera
|`options`|object|**(optional)** Optional settings  **(default: null)**
|`callback`|function|Function to be called when the operation is done

The `callback` function takes two arguments

- `err` : Error - If an error occurs the err argument will be defined
- `db` : object - The MongoDB instance



## Simple connect




## Simple connect, using promises


```js
const MongoDB = require('mongodb');

MongoDB.connect('mongodb://localhost:27017/databaseName')
    .then(function(database) {
        const collection = database.collection('collectionName');
        return collection.insert({key: 'value'});
    })    
    .then(function(result) {
        console.log(result);
    });
    ```js

```



#### Syntax


- db.**collection**.insertOne(**document**, **options(w, wtimeout, j, serializeFuntions, forceServerObjectId, bypassDocumentValidation)**, **callback**)
- db.**collection**.insertMany(**[documents]**, **options(w, wtimeout, j, serializeFuntions, forceServerObjectId, bypassDocumentValidation)**, **callback**)
- db.**collection**.find(**query**)
- db.**collection**.updateOne(**filter**, **update**, **options(upsert, w, wtimeout, j, bypassDocumentValidation)**, **callback**)
- db.**collection**.updateMany(**filter**, **update**, **options(upsert, w, wtimeout, j)**, **callback**)
- db.**collection**.deleteOne(**filter**, **options(upsert, w, wtimeout, j)**, **callback**)
- db.**collection**.deleteMany(**filter**, **options(upsert, w, wtimeout, j)**, **callback**)



#### Parameters


|Parameter|Details
|---|---|---
|document|A javascript object representing a document
|documents|An array of documents
|query|An object defining a search query
|filter|An object defining a search query
|callback|Function to be called when the operation is done
|options|**(optional)** Optional settings  **(default: null)**
|w|**(optional)** The write concern
|wtimeout|**(optional)** The write concern timeout. **(default: null)**
|j|**(optional)** Specify a journal write concern **(default: false)**
|upsert|**(optional)** Update operation **(default: false)**
|multi|**(optional)** Update one/all documents **(default: false)**
|serializeFunctions|**(optional)** Serialize functions on any object **(default: false)**
|forceServerObjectId|**(optional)** Force server to assign _id values instead of driver  **(default: false)**
|bypassDocumentValidation|**(optional)** Allow driver to bypass schema validation in MongoDB 3.2 or higher **(default: false)**

