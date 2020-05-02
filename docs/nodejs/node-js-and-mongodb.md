---
metaTitle: "Node.JS and MongoDB."
description: "Connecting To a Database, Creating New Collection, Inserting Documents, Reading, Updating, Deleting"
---

# Node.JS and MongoDB.



## Connecting To a Database


To connect to a mongo database from node application we require mongoose.

Installing Mongoose
Go to the toot of your application and install mongoose by

```js
npm install mongoose

```

Next we connect to the database.

```js
var mongoose = require('mongoose');

//connect to the test database running on default mongod port of localhost  
mongoose.connect('mongodb://localhost/test');



//Connecting with custom credentials
mongoose.connect('mongodb://USER:PASSWORD@HOST:PORT/DATABASE');


//Using Pool Size to define the number of connections opening
//Also you can use a call back function for error handling
mongoose.connect('mongodb://localhost:27017/consumers', 
                 {server: { poolSize: 50 }}, 
                 function(err) {
                    if(err) {
                        console.log('error in this')
                        console.log(err);
                        // Do whatever to handle the error 
                    } else {
                        console.log('Connected to the database');
                    }
                });  

```



## Creating New Collection


With Mongoose, everything is derived from a Schema. Lets create a schema.

```js
var mongoose = require('mongoose');
        
var Schema = mongoose.Schema;

var AutoSchema = new Schema({
    name : String,
    countOf: Number,
});
// defining the document structure 

// by default the collection created in the db would be the first parameter we use (or the plural of it)    
module.exports = mongoose.model('Auto', AutoSchema); 

// we can over write it and define the collection name by specifying that in the third parameters. 
module.exports = mongoose.model('Auto', AutoSchema, 'collectionName'); 


// We can also define methods in the models. 
AutoSchema.methods.speak = function () {
  var greeting = this.name
    ? "Hello this is  " + this.name+ " and I have counts of "+ this.countOf
    : "I don't have a name";
  console.log(greeting);
}
mongoose.model('Auto', AutoSchema, 'collectionName'); 

```

Remember methods must be added to the schema before compiling it with mongoose.model() like done above ..



## Inserting Documents


For inserting a new document in the collection, we create a object of the schema.

```js
var Auto = require('models/auto')
var autoObj = new Auto({
    name: "NewName", 
    countOf: 10
});

```

We save it like the following

```js
autoObj.save(function(err, insertedAuto) {
    if (err) return console.error(err);
    insertedAuto.speak();
    // output: Hello this is NewName and I have counts of 10
});

```

This will insert a new document in the collection



## Reading


Reading Data from the collection is very easy. Getting all data of the collection.

```js
var Auto = require('models/auto')
Auto.find({}, function (err, autos) {
      if (err) return console.error(err);
       // will return a json array of all the documents in the collection
      console.log(autos); 
})

```

Reading data with a condition

```js
Auto.find({countOf: {$gte: 5}}, function (err, autos) {
      if (err) return console.error(err);
       // will return a json array of all the documents in the collection whose count is greater than 5
      console.log(autos); 
})

```

You can also specify the second parameter as object of what all fields you need

```js
Auto.find({},{name:1}, function (err, autos) {
      if (err) return console.error(err);
       // will return a json array of name field of all the documents in the collection
      console.log(autos); 
})

```

Finding one document in a collection.

```js
Auto.findOne({name:"newName"}, function (err, auto) {
      if (err) return console.error(err);
     //will return the first object of the document whose name is "newName"
      console.log(auto); 
})

```

Finding one document in a collection by id .

```js
Auto.findById(123, function (err, auto) {
      if (err) return console.error(err);
     //will return the first json object of the document whose id is 123
      console.log(auto); 
})

```



## Updating


For updating collections and documents we can use any of these methods:

### Methods

- update()
- updateOne()
- updateMany()
- replaceOne()

### Update()

**The update() method modifies one or many documents (update parameters)**

```js
db.lights.update(
   { room: "Bedroom" },
   { status: "On" }
)

```

This operation searches the 'lights' collection for a document where `room` is **Bedroom** **(1st parameter)**. It then updates the matching documents `status` property to **On** **(2nd parameter)** and returns a  WriteResult object that looks like this:

```js
{ "nMatched" : 1, "nUpserted" : 0, "nModified" : 1 }

```

### UpdateOne

**The UpdateOne() method modifies ONE document (update parameters)**

```js
db.countries.update(
   { country: "Sweden" },
   { capital: "Stockholm" }
)

```

This operation searches the 'countries' collection for a document where `country` is **Sweden** **(1st parameter)**. It then updates the matching documents property `capital`  to **Stockholm** **(2nd parameter)** and returns a  WriteResult object that looks like this:

```js
{ "acknowledged" : true, "matchedCount" : 1, "modifiedCount" : 1 }

```

### UpdateMany

**The UpdateMany() method modifies multible documents (update parameters)**

```js
db.food.updateMany(
   { sold: { $lt: 10 } },
   { $set: { sold: 55 } }
)

```

This operation updates all documents **(in a 'food' collection)** where `sold` is **lesser than 10** *(1st parameter) by setting `sold` to **55**. It then returns a  WriteResult object that looks like this:

```js
{ "acknowledged" : true, "matchedCount" : a, "modifiedCount" : b }

```

a = Number of matched documents<br />
b = Number of modified documents

### ReplaceOne

**Replaces the first matching document (replacement document)**

This example collection called **countries** contains 3 documents:

```js
{ "_id" : 1, "country" : "Sweden" }
{ "_id" : 2, "country" : "Norway" }
{ "_id" : 3, "country" : "Spain" }

```

The following operation replaces the document `{ country: "Spain" }` with document `{ country: "Finland" }`

```js
db.countries.replaceOne(
   { country: "Spain" },
   { country: "Finland" }
)

```

And returns:

```js
{ "acknowledged" : true, "matchedCount" : 1, "modifiedCount" : 1 }

```

The example collection **countries** now contains:

```js
{ "_id" : 1, "country" : "Sweden" }
{ "_id" : 2, "country" : "Norway" }
{ "_id" : 3, "country" : "Finland" }

```



## Deleting


Deleting documents from a collection in mongoose is done in the following manner.

```js
Auto.remove({_id:123}, function(err, result){
    if (err) return console.error(err);
    console.log(result); // this will specify the mongo default delete result.
});

```



#### Remarks


These are the basic CRUD operations for using mongo db with nodejs.

Question: Are there other ways you can do what is done here ??

Answer :  Yes, there are numerous way to do this.

Question: Is using mongoose necessary ??

Answer :  No. There are other packages available which can help you.

Question: Where can I get full documentation of mongoose ??

Answer: [Click Here](http://mongoosejs.com/docs/api.html)

