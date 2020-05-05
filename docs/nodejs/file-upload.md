---
metaTitle: "Node.js - File upload"
description: "Single File Upload using multer, Using formidable module"
---

# File upload



## Single File Upload using multer


Remember to

- create folder for upload (`uploads` in example).
- install multer `npm i -S multer`

**`server.js`**:

```js
var express =   require("express");
var multer  =   require('multer');
var app         =   express();
var fs = require('fs');

app.get('/',function(req,res){
      res.sendFile(__dirname + "/index.html");
});

var storage =   multer.diskStorage({
  destination: function (req, file, callback) {
    fs.mkdir('./uploads', function(err) {
        if(err) {
            console.log(err.stack)
        } else {
            callback(null, './uploads');
        }
    })
  },
  filename: function (req, file, callback) {
    callback(null, file.fieldname + '-' + Date.now());
  }
});

app.post('/api/file',function(req,res){
    var upload = multer({ storage : storage}).single('userFile');
    upload(req,res,function(err) {
        if(err) {
            return res.end("Error uploading file.");
        }
        res.end("File is uploaded");
    });
});

app.listen(3000,function(){
    console.log("Working on port 3000");
});

```

**`index.html`**:

```js
<form id        =  "uploadForm"
     enctype   =  "multipart/form-data"
     action    =  "/api/file"
     method    =  "post"
>
<input type="file" name="userFile" />
<input type="submit" value="Upload File" name="submit">
</form>

```

### Note:

To upload file with extension you can use Node.js [path](https://nodejs.org/api/path.html#path_path) built-in library

For that just require `path` to `server.js` file:

```js
var path = require('path');

```

and change:

```js
callback(null, file.fieldname + '-' + Date.now());

```

adding a file extension in the following way:

```js
callback(null, file.fieldname + '-' + Date.now() + path.extname(file.originalname));

```

### How to filter upload by extension:

In this example, view how to upload files to allow only certain extensions.

For example only images extensions. Just add to `var upload = multer({ storage : storage}).single('userFile');` fileFilter condition

```js
var upload = multer({
    storage: storage,
    fileFilter: function (req, file, callback) {
        var ext = path.extname(file.originalname);
        if(ext !== '.png' && ext !== '.jpg' && ext !== '.gif' && ext !== '.jpeg') {
            return callback(new Error('Only images are allowed'))
        }
        callback(null, true)
    }
}).single('userFile');

```

Now you can upload only image files with `png`, `jpg`, `gif` or `jpeg` extensions



## Using formidable module


Install module and read [**docs**](https://github.com/felixge/node-formidable)

```js
npm i formidable@latest

```

Example of server on 8080 port

```js
var formidable = require('formidable'),
    http = require('http'),
    util = require('util');

http.createServer(function(req, res) {
  if (req.url == '/upload' && req.method.toLowerCase() == 'post') {
    // parse a file upload
    var form = new formidable.IncomingForm();

    form.parse(req, function(err, fields, files) {
      if (err)
          do-smth; // process error

      // Copy file from temporary place
      // var fs = require('fs');
      // fs.rename(file.path, <targetPath>, function (err) { ... });         

      // Send result on client
      res.writeHead(200, {'content-type': 'text/plain'});
      res.write('received upload:\n\n');
      res.end(util.inspect({fields: fields, files: files}));
    });

    return;
  }

  // show a file upload form
  res.writeHead(200, {'content-type': 'text/html'});
  res.end(
    '<form action="/upload" enctype="multipart/form-data" method="post">'+
    '<input type="text" name="title"><br>'+
    '<input type="file" name="upload" multiple="multiple"><br>'+
    '<input type="submit" value="Upload">'+
    '</form>'
  );
}).listen(8080);

```

