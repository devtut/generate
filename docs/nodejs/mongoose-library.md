---
metaTitle: "Mongoose Library"
description: "Connect to MongoDB Using Mongoose, Find Data in MongoDB Using Mongoose, Express.js Routes and $text Operator, Save Data to MongoDB using Mongoose and Express.js Routes, Find Data in MongoDB Using Mongoose and Express.js Routes, Indexes in models., Useful Mongoose functions, find data in mongodb using promises"
---

# Mongoose Library



## Connect to MongoDB Using Mongoose


First, install Mongoose with:

```js
npm install mongoose

```

Then, add it to `server.js` as dependencies:

```js
var mongoose = require('mongoose');
var Schema = mongoose.Schema;

```

Next, create the database schema and the name of the collection:

```js
var schemaName = new Schema({
    request: String,
    time: Number
}, {
    collection: 'collectionName'
});

```

Create a model and connect to the database:

```js
var Model = mongoose.model('Model', schemaName);
mongoose.connect('mongodb://localhost:27017/dbName');

```

Next, start MongoDB and run `server.js` using `node server.js`

To check if we have successfully connected to the database, we can use the events `open`, `error` from the `mongoose.connection` object.

```js
var db = mongoose.connection;
db.on('error', console.error.bind(console, 'connection error:'));
db.once('open', function() {
  // we're connected!
});

```



## Find Data in MongoDB Using Mongoose, Express.js Routes and $text Operator


### Setup

First, install the necessary packages with:

```js
npm install express cors mongoose

```

### Code

Then, add dependencies to `server.js`, create the database schema and the name of the collection, create an Express.js server, and connect to MongoDB:

```js
var express = require('express');
var cors = require('cors'); // We will use CORS to enable cross origin domain requests.
var mongoose = require('mongoose');
var Schema = mongoose.Schema;

var app = express();

var schemaName = new Schema({
    request: String,
    time: Number
}, {
    collection: 'collectionName'
});

var Model = mongoose.model('Model', schemaName);
mongoose.connect('mongodb://localhost:27017/dbName');

var port = process.env.PORT || 8080;
app.listen(port, function() {
    console.log('Node.js listening on port ' + port);
});

```

Now add Express.js routes that we will use to query the data:

```js
app.get('/find/:query', cors(), function(req, res) {
    var query = req.params.query;

    Model.find({
        'request': query
    }, function(err, result) {
        if (err) throw err;
        if (result) {
            res.json(result)
        } else {
            res.send(JSON.stringify({
                error : 'Error'
            }))
        }
    })
})

```

Assume that the following documents are in the collection in the model:

```js
{
        "_id" : ObjectId("578abe97522ad414b8eeb55a"),
        "request" : "JavaScript is Awesome",
        "time" : 1468710551
}
{
        "_id" : ObjectId("578abe9b522ad414b8eeb55b"),
        "request" : "JavaScript is Awesome",
        "time" : 1468710555
}
{
        "_id" : ObjectId("578abea0522ad414b8eeb55c"),
        "request" : "JavaScript is Awesome",
        "time" : 1468710560
}

```

And that the goal is to find and display all the documents containing only `"JavaScript"` word under the `"request"` key.

To do this, first create a **text index** for `"request"` in the collection. For this, add the following code to `server.js`:

```js
schemaName.index({ request: 'text' });

```

And replace:

```

   Model.find({
        'request': query
    }, function(err, result) {

```

With:

```js
Model.find({
    $text: {
        $search: query
    }
}, function(err, result) {

```

Here, we are using `$text` and `$search` MongoDB operators for find all documents in collection `collectionName` which contains at least one word from the specified find query.

### Usage

To use this to find data, go to the following URL in a browser:

```js
http://localhost:8080/find/<query>

```

Where `<query>` is the search query.

Example:

```js
http://localhost:8080/find/JavaScript

```

Output:

```js
[{
    _id: "578abe97522ad414b8eeb55a",
    request: "JavaScript is Awesome",
    time: 1468710551,
    __v: 0
},
{
    _id: "578abe9b522ad414b8eeb55b",
    request: "JavaScript is Awesome",
    time: 1468710555,
    __v: 0
},
{
    _id: "578abea0522ad414b8eeb55c",
    request: "JavaScript is Awesome",
    time: 1468710560,
    __v: 0
}]

```



## Save Data to MongoDB using Mongoose and Express.js Routes


### Setup

First, install the necessary packages with:

```js
npm install express cors mongoose

```

### Code

Then, add dependencies to your `server.js` file, create the database schema and the name of the collection, create an Express.js server, and connect to MongoDB:

```js
var express = require('express');
var cors = require('cors'); // We will use CORS to enable cross origin domain requests.
var mongoose = require('mongoose');
var Schema = mongoose.Schema;

var app = express();

var schemaName = new Schema({
    request: String,
    time: Number
}, {
    collection: 'collectionName'
});

var Model = mongoose.model('Model', schemaName);
mongoose.connect('mongodb://localhost:27017/dbName');

var port = process.env.PORT || 8080;
app.listen(port, function() {
    console.log('Node.js listening on port ' + port);
});

```

Now add Express.js routes that we will use to write the data:

```js
app.get('/save/:query', cors(), function(req, res) {
    var query = req.params.query;

    var savedata = new Model({
        'request': query,
        'time': Math.floor(Date.now() / 1000) // Time of save the data in unix timestamp format
    }).save(function(err, result) {
        if (err) throw err;

        if(result) {
            res.json(result)
        }
    })
})

```

Here the `query` variable will be the `<query>` parameter from the incoming HTTP request, which will be saved to MongoDB:

```js
var savedata = new Model({
    'request': query,
    //...

```

If an error occurs while trying to write to MongoDB, you will receive an error message on the console. If all is successful, you will see the saved data in JSON format on the page.

```js
//...
}).save(function(err, result) {
    if (err) throw err;

    if(result) {
        res.json(result)
    }
})
//...

```

Now, you need to start MongoDB and run your `server.js` file using `node server.js`.

### Usage

To use this to save data, go to the following URL in your browser:

```js
http://localhost:8080/save/<query>

```

Where `<query>` is the new request you wish to save.

Example:

```js
http://localhost:8080/save/JavaScript%20is%20Awesome

```

Output in JSON format:

```js
{
    __v: 0,
    request: "JavaScript is Awesome",
    time: 1469411348,
    _id: "57957014b93bc8640f2c78c4"
}

```



## Find Data in MongoDB Using Mongoose and Express.js Routes


### Setup

First, install the necessary packages with:

```js
npm install express cors mongoose

```

### Code

Then, add dependencies to `server.js`, create the database schema and the name of the collection, create an Express.js server, and connect to MongoDB:

```js
var express = require('express');
var cors = require('cors'); // We will use CORS to enable cross origin domain requests.
var mongoose = require('mongoose');
var Schema = mongoose.Schema;

var app = express();

var schemaName = new Schema({
    request: String,
    time: Number
}, {
    collection: 'collectionName'
});

var Model = mongoose.model('Model', schemaName);
mongoose.connect('mongodb://localhost:27017/dbName');

var port = process.env.PORT || 8080;
app.listen(port, function() {
    console.log('Node.js listening on port ' + port);
});

```

Now add Express.js routes that we will use to query the data:

```js
app.get('/find/:query', cors(), function(req, res) {
    var query = req.params.query;

    Model.find({
        'request': query
    }, function(err, result) {
        if (err) throw err;
        if (result) {
            res.json(result)
        } else {
            res.send(JSON.stringify({
                error : 'Error'
            }))
        }
    })
})

```

Assume that the following documents are in the collection in the model:

```js
{
        "_id" : ObjectId("578abe97522ad414b8eeb55a"),
        "request" : "JavaScript is Awesome",
        "time" : 1468710551
}
{
        "_id" : ObjectId("578abe9b522ad414b8eeb55b"),
        "request" : "JavaScript is Awesome",
        "time" : 1468710555
}
{
        "_id" : ObjectId("578abea0522ad414b8eeb55c"),
        "request" : "JavaScript is Awesome",
        "time" : 1468710560
}

```

And the goal is to find and display all the documents containing `"JavaScript is Awesome"` under the `"request"` key.

For this, start MongoDB and run `server.js` with `node server.js`:

### Usage

To use this to find data, go to the following URL in a browser:

```js
http://localhost:8080/find/<query>

```

Where `<query>` is the search query.

Example:

```js
http://localhost:8080/find/JavaScript%20is%20Awesome

```

Output:

```js
[{
    _id: "578abe97522ad414b8eeb55a",
    request: "JavaScript is Awesome",
    time: 1468710551,
    __v: 0
},
{
    _id: "578abe9b522ad414b8eeb55b",
    request: "JavaScript is Awesome",
    time: 1468710555,
    __v: 0
},
{
    _id: "578abea0522ad414b8eeb55c",
    request: "JavaScript is Awesome",
    time: 1468710560,
    __v: 0
}]

```



## Indexes in models.


MongoDB supports secondary indexes. In Mongoose, we define these indexes within our schema. Defining indexes at schema level is necessary when we need to create compound indexes.

**Mongoose Connection**

```js
var strConnection = 'mongodb://localhost:27017/dbName';
var db = mongoose.createConnection(strConnection)

```

**Creating a basic schema**

```js
var Schema = require('mongoose').Schema;
var usersSchema = new Schema({
    username: {
        type: String,
        required: true,
        unique: true
    },
    email: {
        type: String,
        required: true
    },
    password: {
        type: String,
        required: true
    },
    created: {
        type: Date,
        default: Date.now
    }
});

var usersModel = db.model('users', usersSchema);
module.exports = usersModel;

```

By default, mongoose adds two new fields into our model, even when those are not defined in the model. Those fields are:

**_id**

Mongoose assigns each of your schemas an _id field by default if one is not passed into the Schema constructor. The type assigned is an ObjectId to coincide with MongoDB's default behavior. If you don't want an _id added to your schema at all, you may disable it using this option.

```js
var usersSchema = new Schema({
    username: {
        type: String,
        required: true,
        unique: true
    }, {
        _id: false 
});

```

**__v or versionKey**

The versionKey is a property set on each document when first created by Mongoose. This keys value contains the internal revision of the document. The name of this document property is configurable.

You can easy disable this field in the model configuration:

```js
var usersSchema = new Schema({
    username: {
        type: String,
        required: true,
        unique: true
    }, {
    versionKey: false 
});

```

**Compound indexes**

We can create another indexes besides those Mongoose creates.

```js
usersSchema.index({username: 1 });
usersSchema.index({email: 1 });

```

In these case our model have two more indexes, one for the field username and another for email field. But we can create compound indexes.

```js
usersSchema.index({username: 1, email: 1 });

```

**Index performance impact**

By default, mongoose always call the ensureIndex for each index sequentially and emit an 'index' event on the model when all the ensureIndex calls succeeded or when there was an error.

In MongoDB ensureIndex is deprecated since 3.0.0 version, now is an alias for createIndex.

Is recommended disable the behavior by setting the autoIndex option of your schema to false, or globally on the connection by setting the option config.autoIndex to false.

```js
usersSchema.set('autoIndex', false);

```



## Useful Mongoose functions


Mongoose contains some built in functions that build on the standard `find()`.

```js
doc.find({'some.value':5},function(err,docs){
    //returns array docs
});

doc.findOne({'some.value':5},function(err,doc){
    //returns document doc
});

doc.findById(obj._id,function(err,doc){
    //returns document doc
});

```



## find data in mongodb using promises


### Setup

First, install the necessary packages with:

```js
npm install express cors mongoose

```

### Code

Then, add dependencies to `server.js`, create the database schema and the name of the collection, create an Express.js server, and connect to MongoDB:

```js
var express = require('express');
var cors = require('cors'); // We will use CORS to enable cross origin domain requests.
var mongoose = require('mongoose');
var Schema = mongoose.Schema;

var app = express();

var schemaName = new Schema({
    request: String,
    time: Number
}, {
    collection: 'collectionName'
});

var Model = mongoose.model('Model', schemaName);
mongoose.connect('mongodb://localhost:27017/dbName');

var port = process.env.PORT || 8080;
app.listen(port, function() {
    console.log('Node.js listening on port ' + port);
});

app.use(function(err, req, res, next) {
  console.error(err.stack);
  res.status(500).send('Something broke!');
});

app.use(function(req, res, next) {
  res.status(404).send('Sorry cant find that!');
});

```

Now add Express.js routes that we will use to query the data:

```js
app.get('/find/:query', cors(), function(req, res, next) {
    var query = req.params.query;

    Model.find({
        'request': query
    })
    .exec() //remember to add exec, queries have a .then attribute but aren't promises
    .then(function(result) {
        if (result) {
            res.json(result)
        } else {
            next() //pass to 404 handler
        }
    })
    .catch(next) //pass to error handler
})

```

Assume that the following documents are in the collection in the model:

```js
{
        "_id" : ObjectId("578abe97522ad414b8eeb55a"),
        "request" : "JavaScript is Awesome",
        "time" : 1468710551
}
{
        "_id" : ObjectId("578abe9b522ad414b8eeb55b"),
        "request" : "JavaScript is Awesome",
        "time" : 1468710555
}
{
        "_id" : ObjectId("578abea0522ad414b8eeb55c"),
        "request" : "JavaScript is Awesome",
        "time" : 1468710560
}

```

And the goal is to find and display all the documents containing `"JavaScript is Awesome"` under the `"request"` key.

For this, start MongoDB and run `server.js` with `node server.js`:

### Usage

To use this to find data, go to the following URL in a browser:

```js
http://localhost:8080/find/<query>

```

Where `<query>` is the search query.

Example:

```js
http://localhost:8080/find/JavaScript%20is%20Awesome

```

Output:

```js
[{
    _id: "578abe97522ad414b8eeb55a",
    request: "JavaScript is Awesome",
    time: 1468710551,
    __v: 0
},
{
    _id: "578abe9b522ad414b8eeb55b",
    request: "JavaScript is Awesome",
    time: 1468710555,
    __v: 0
},
{
    _id: "578abea0522ad414b8eeb55c",
    request: "JavaScript is Awesome",
    time: 1468710560,
    __v: 0
}]

```

