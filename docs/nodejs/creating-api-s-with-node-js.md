---
metaTitle: "Node.js - Creating API's with Node.js"
description: "GET api using Express, POST api using Express"
---

# Creating API's with Node.js




## GET api using Express


`Node.js` apis can be easily constructed in `Express` web framework.

Following example creates a simple `GET` api for listing all users.

**Example**

```js
var express = require('express');   
var app = express();

var users =[{
        id: 1,
        name: "John Doe",
        age : 23,
        email: "john@doe.com"
    }];

// GET /api/users
app.get('/api/users', function(req, res){
    return res.json(users);    //return response as JSON
});

app.listen('3000', function(){
    console.log('Server listening on port 3000');
});

```



## POST api using Express


Following example create `POST` api using `Express`. This example is similar to `GET` example except the use of `body-parser` that parses the post data and add it to `req.body`.

**Example**

```js
var express = require('express');
var app = express();
// for parsing the body in POST request
var bodyParser = require('body-parser');

var users =[{
    id: 1,
    name: "John Doe",
    age : 23,
    email: "john@doe.com"
}];

app.use(bodyParser.urlencoded({ extended: false }));
app.use(bodyParser.json());

// GET /api/users
app.get('/api/users', function(req, res){
    return res.json(users);    
});


/* POST /api/users
    {
        "user": {
           "id": 3,
            "name": "Test User",
            "age" : 20,
            "email": "test@test.com"
        }
    }
*/
app.post('/api/users', function (req, res) {
    var user = req.body.user;
    users.push(user);

    return res.send('User has been added successfully');
});

app.listen('3000', function(){
    console.log('Server listening on port 3000');
});

```

