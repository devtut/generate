---
metaTitle: "Deliver HTML or any other sort of file"
description: "Deliver HTML at specified path"
---

# Deliver HTML or any other sort of file



## Deliver HTML at specified path


Here's how to create an Express server and serve `index.html` by default (empty path `/`), and `page1.html` for `/page1` path.

### Folder structure

```js
project root
|    server.js
|____views
     |    index.html
     |    page1.html


```

### server.js

```js
var express = require('express');
var path = require('path');
var app = express();

// deliver index.html if no file is requested
app.get("/", function (request, response) {
  response.sendFile(path.join(__dirname, 'views/index.html'));
});

// deliver page1.html if page1 is requested
app.get('/page1', function(request, response) {
    response.sendFile(path.join(__dirname, 'views', 'page1.html', function(error) {
        if (error) {
            // do something in case of error
            console.log(err);
            response.end(JSON.stringify({error:"page not found"}));
        }
    });
});

app.listen(8080);

```

Note that `sendFile()` just streams a static file as response, offering no opportunity to modify it. If you are serving an HTML file and want to include dynamic data with it, then you will need to use a **template engine** such as Pug, Mustache, or EJS.



#### Syntax


- response.sendFile(fileName, options, function (err) {});

