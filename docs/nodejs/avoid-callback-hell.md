---
metaTitle: "Avoid callback hell"
description: "Async module, Async Module"
---

# Avoid callback hell




## Async module


The source is available for download from GitHub. Alternatively, you can install using npm:

$ npm install --save async

As well as using Bower:

$ bower install async

Example:

```js
var async = require("async");
async.parallel([
    function(callback) { ... },
    function(callback) { ... }
], function(err, results) {
    // optional callback
});

```



## Async Module


Thankfully, libraries like Async.js exist to try and curb the problem. Async adds a thin layer of functions on top of your code, but can greatly reduce the complexity by avoiding callback nesting.

Many helper methods exist in Async that can be used in different situations, like series, parallel, waterfall, etc. Each function has a specific use-case, so take some time to learn which one will help in which situations.

As good as Async is, like anything, its not perfect. Its very easy to get carried away by combining series, parallel, forever, etc, at which point you're right back to where you started with messy code. Be careful not to prematurely optimize. Just because a few async tasks can be run in parallel doesn't always mean they should. In reality, since Node is only single-threaded, running tasks in parallel on using Async has little to no performance gain.

The source is available for download from [https://github.com/caolan/async](https://github.com/caolan/async) . Alternatively, you can install using npm:

$ npm install --save async

As well as using Bower:

$ bower install async

Async's waterfall Example:

```js
var fs = require('fs');  
var async = require('async');

var myFile = '/tmp/test';

async.waterfall([  
    function(callback) {
        fs.readFile(myFile, 'utf8', callback);
    },
    function(txt, callback) {
        txt = txt + '\nAppended something!';
        fs.writeFile(myFile, txt, callback);
    }
], function (err, result) {
    if(err) return console.log(err);
    console.log('Appended text!');
});

```

