---
metaTitle: "Database (MongoDB with Mongoose)"
description: "Mongoose connection, Model, Insert data, Read data"
---

# Database (MongoDB with Mongoose)



## Mongoose connection


Make sure to have mongodb running first!
`mongod --dbpath data/`

package.json

```js
"dependencies": {
    "mongoose": "^4.5.5",
}

```

server.js (ECMA 6)

```js
import mongoose from 'mongoose';

mongoose.connect('mongodb://localhost:27017/stackoverflow-example');
const db = mongoose.connection;
db.on('error', console.error.bind(console, 'DB connection error!'));

```

server.js (ECMA 5.1)

```js
var mongoose = require('mongoose');

mongoose.connect('mongodb://localhost:27017/stackoverflow-example');
var db = mongoose.connection;
db.on('error', console.error.bind(console, 'DB connection error!'));

```



## Model


Define your model(s):

app/models/user.js (ECMA 6)

```js
import mongoose from 'mongoose';

const userSchema = new mongoose.Schema({
    name: String,
    password: String
});

const User = mongoose.model('User', userSchema);

export default User;

```

app/model/user.js (ECMA 5.1)

```js
var mongoose = require('mongoose');

var userSchema = new mongoose.Schema({
    name: String,
    password: String
});

var User = mongoose.model('User', userSchema);

module.exports = User

```



## Insert data


ECMA 6:

```js
const user = new User({
   name: 'Stack',
   password: 'Overflow',
   }) ;

user.save((err) => {
    if (err) throw err;

    console.log('User saved!');
});

```

ECMA5.1:

```js
var user = new User({
   name: 'Stack',
   password: 'Overflow',
   }) ;

user.save(function (err) {
    if (err) throw err;

    console.log('User saved!');
});

```



## Read data


ECMA6:

```js
User.findOne({
    name: 'stack'
}, (err, user) => {
    if (err) throw err;

    if (!user) {
        console.log('No user was found');
    } else {
        console.log('User was found');
    }
});

```

ECMA5.1:

```js
User.findOne({
    name: 'stack'
}, function (err, user) {
    if (err) throw err;

    if (!user) {
        console.log('No user was found');
    } else {
        console.log('User was found');
    }
});

```

