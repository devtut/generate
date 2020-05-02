---
metaTitle: "MongoDB Integration for Node.js/Express.js"
description: "Installing MongoDB, Creating a Mongoose Model, Querying your Mongo Database"
---

# MongoDB Integration for Node.js/Express.js


MongoDB is one of the most popular NoSQL databases, thanks to the help of the MEAN stack. Interfacing with a Mongo database from an Express app is quick and easy, once you understand the kinda-wonky query syntax. We'll use Mongoose to help us out.



## Installing MongoDB


```js
npm install --save mongodb
npm install --save mongoose //A simple wrapper for ease of development

```

In your server file (normally named  index.js or server.js)

```js
const express = require('express');
const mongodb = require('mongodb');
const mongoose = require('mongoose');
const mongoConnectString = 'http://localhost/database name';

mongoose.connect(mongoConnectString, (err) => {
  if (err) {
    console.log('Could not connect to the database');
  }
});

```



## Creating a Mongoose Model


```js
const Schema = mongoose.Schema;
const ObjectId = Schema.Types.ObjectId;

const Article = new Schema({
  title: {
    type: String,
    unique: true,
    required: [true, 'Article must have title']
  },
  author: {
    type: ObjectId,
    ref: 'User'
  }
});

module.exports = mongoose.model('Article, Article);

```

Let's dissect this. MongoDB and Mongoose use JSON(actually BSON, but that's irrelevant here) as the data format. At the top, I've set a few variables to reduce typing.

I create a `new Schema` and assign it to a constant. It's simple JSON, and each attribute is another Object with properties that help enforce a more consistent schema. Unique forces new instances being inserted in the database to, obviously, be unique. This is great for preventing a user creating multiple accounts on a service.

Required is another, declared as an array. The first element is the boolean value, and the second the error message should the value being inserted or updated fail to exist.

ObjectIds are used for relationships between Models. Examples might be 'Users have many Comments`. Other attributes can be used instead of ObjectId. Strings like a username is one example.

Lastly, exporting the model for use with your API routes provides access to your schema.



## Querying your Mongo Database


A simple GET request. Let's assume the Model from the example above is in the file `./db/models/Article.js`.

```js
const express = require('express');
const Articles = require('./db/models/Article');

module.exports = function (app) {
  const routes = express.Router();
  
  routes.get('/articles', (req, res) => {
    Articles.find().limit(5).lean().exec((err, doc) => {
      if (doc.length > 0) {
        res.send({ data: doc });
      } else {
        res.send({ success: false, message: 'No documents retrieved' });
      }
    });
  });

app.use('/api', routes);
};

```

We can now get the data from our database by sending an HTTP request to this endpoint. A few key things, though:

1. Limit does exactly what it looks like. I'm only getting 5 documents back.
1. Lean strips away some stuff from the raw BSON, reducing complexity and overhead. Not required. But useful.
1. When using `find` instead of `findOne`, confirm that the `doc.length` is greater than 0. This is because `find` always returns an array, so an empty array will not handle your error unless it is checked for length
1. I personally like to send the error message in that format. Change it to suit your needs. Same thing for the returned document.
1. The code in this example is written under the assumption that you have placed it in another file and not directly on the express server. To call this in the server, include these lines in your server code:

```js
const app = express();
require('./path/to/this/file')(app) // 

```



#### Remarks


More information can be found here: [http://mongoosejs.com/docs/guide.html](http://mongoosejs.com/docs/guide.html)

