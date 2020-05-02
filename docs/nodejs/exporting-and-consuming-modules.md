---
metaTitle: "Exporting and Consuming Modules"
description: "Creating a hello-world.js module, Loading and using a module, Every module injected only once, Folder as a module, Invalidating the module cache, Building your own modules, Module loading from node_modules"
---

# Exporting and Consuming Modules



## Creating a hello-world.js module


Node provides the `module.exports` interface to expose functions and variables to other files. The most simple way to do so is to export only one object (function or variable), as shown in the first example.

**hello-world.js**

```js
module.exports = function(subject) {
    console.log('Hello ' + subject);
};

```

If we don't want the entire export to be a single object, we can export functions and variables as properties of the `exports` object. The three following examples all demonstrate this in slightly different ways :

- hello-venus.js : the function definition is done separately then added as a property of `module.exports`
- hello-jupiter.js : the functions definitions are directly put as the value of properties of `module.exports`
- hello-mars.js : the function definition is directly declared as a property of `exports` which is a short version of `module.exports`

**hello-venus.js**

```js
function hello(subject) {
    console.log('Venus says Hello ' + subject);
}

module.exports = {
    hello: hello
};

```

**hello-jupiter.js**

```js
module.exports = {
    hello: function(subject) {
      console.log('Jupiter says hello ' + subject);
    },

    bye: function(subject) {
      console.log('Jupiter says goodbye ' + subject);
    }
};

```

**hello-mars.js**

```js
exports.hello = function(subject) {
    console.log('Mars says Hello ' + subject);
};

```

**Loading module with directory name**

We have a directory named `hello` which includes the following files:

**index.js**

```js
// hello/index.js
module.exports = function(){
    console.log('Hej');
};

```

**main.js**

```js
// hello/main.js
// We can include the other files we've defined by using the `require()` method
var hw = require('./hello-world.js'),
    hm = require('./hello-mars.js'),
    hv = require('./hello-venus.js'),
    hj = require('./hello-jupiter.js'),
    hu = require('./index.js');

// Because we assigned our function to the entire `module.exports` object, we
// can use it directly
hw('World!'); // outputs "Hello World!"

// In this case, we assigned our function to the `hello` property of exports, so we must
// use that here too
hm.hello('Solar System!'); // outputs "Mars says Hello Solar System!"

// The result of assigning module.exports at once is the same as in hello-world.js
hv.hello('Milky Way!'); // outputs "Venus says Hello Milky Way!"

hj.hello('Universe!'); //  outputs "Jupiter says hello Universe!"
hj.bye('Universe!'); // outputs "Jupiter says goodbye Universe!"

hu(); //output 'hej'

```



## Loading and using a module


A module can be "imported", or otherwise "required" by the `require()` function. For example, to load the `http` module that ships with Node.js, the following can be used:

```js
const http = require('http');

```

Aside from modules that are shipped with the runtime, you can also require modules that you have installed from npm, such as express. If you had already installed express on your system via `npm install express`, you could simply write:

```js
const express = require('express');

```

You can also include modules that you have written yourself as part of your application. In this case, to include a file named `lib.js` in the same directory as current file:

```js
const mylib = require('./lib');

```

Note that you can omit the extension, and `.js` will be assumed. Once you load a module, the variable is populated with an object that contains the methods and properties published from the required file. A full example:

```js
const http = require('http');

// The `http` module has the property `STATUS_CODES`
console.log(http.STATUS_CODES[404]); // outputs 'Not Found'

// Also contains `createServer()`
http.createServer(function(req, res) {
  res.writeHead(200, {'Content-Type': 'text/html'});
  res.write('<html><body>Module Test</body></html>');
  res.end();
}).listen(80);

```



## Every module injected only once


NodeJS executes the module only the first time you require it.
Any further require functions will re-use the same Object, thus not executing the code in the module another time. Also Node caches the modules first time they are loaded using require. This reduces the number of file reads and helps to speed up the application.

**`myModule.js`**

```js
console.log(123) ; 
exports.var1 = 4 ; 

```

**index.js**

```js
var a=require('./myModule') ; // Output 123
var b=require('./myModule') ; // No output
console.log(a.var1) ; // Output 4
console.log(b.var1) ; // Output 4
a.var2 = 5 ; 
console.log(b.var2) ; // Output 5

```



## Folder as a module


Modules can be split across many .js files in the same folder. An example in a **my_module** folder:

**function_one.js**

```js
module.exports = function() {
  return 1;
}

```

**function_two.js**

```js
module.exports = function() {
  return 2;
}

```

**index.js**

```js
exports.f_one = require('./function_one.js');
exports.f_two = require('./function_two.js');

```

A module like this one is used by referring to it by the folder name:

```js
var split_module = require('./my_module');

```

Please note that if you required it by omitting `./` or any indication of a path to a folder from the require function argument, Node will try to load a module from the **node_modules** folder.

Alternatively you can create in the same folder a `package.json` file with these contents:

```js
{
    "name": "my_module",
    "main": "./your_main_entry_point.js"
}

```

This way you are not required to name the main module file "index".



## Invalidating the module cache


In development, you may find that using `require()` on the same module multiple times always returns the same module, even if you have made changes to that file. This is because modules are cached the first time they are loaded, and any subsequent module loads will load from the cache.

To get around this issue, you will have to `delete` the entry in the cache. For example, if you loaded a module:

```js
var a = require('./a');

```

You could then delete the cache entry:

```js
var rpath = require.resolve('./a.js');
delete require.cache[rpath];

```

And then require the module again:

```js
var a = require('./a');

```

Do note that this is not recommended in production because the `delete` will only delete the reference to the loaded module, not the loaded data itself. The module is not garbage collected, so improper use of this feature could lead to leaking memory.



## Building your own modules


You can also reference an object to publicly export and continuously append methods to that object:

```js
const auth = module.exports = {}
const config = require('../config')
const request = require('request')

auth.email = function (data, callback) {
  // Authenticate with an email address
}

auth.facebook = function (data, callback) {
  // Authenticate with a Facebook account
}

auth.twitter = function (data, callback) {
  // Authenticate with a Twitter account
}

auth.slack = function (data, callback) {
  // Authenticate with a Slack account
}

auth.stack_overflow = function (data, callback) {
  // Authenticate with a Stack Overflow account
}

```

To use any of these, just require the module as you normally would:

```js
const auth = require('./auth')

module.exports = function (req, res, next) {
  auth.facebook(req.body, function (err, user) {
    if (err) return next(err)

    req.user = user
    next()
  })
}

```



## Module loading from node_modules


Modules can be `require`d without using relative paths by putting them in a special directory called `node_modules`.

For example, to `require` a module called `foo` from a file `index.js`, you can use the following directory structure:

```js
index.js
 \- node_modules
  \- foo
   |- foo.js
   \- package.json

```

Modules should be placed inside a directory, along with a `package.json` file. The `main` field of the `package.json` file should point to the entry point for your module--this is the file that is imported when users do `require('your-module')`. `main` defaults to `index.js` if not provided. Alternatively, you can refer to files relative to your module simply by appending the relative path to the `require` call: `require('your-module/path/to/file')`.

Modules can also be `require`d from `node_modules` directories up the file system hierarchy. If we have the following directory structure:

```js
my-project
\- node_modules
 |- foo   // the foo module
  \- ...
 \- baz   // the baz module
  \- node_modules
   \- bar   // the bar module

```

we will be able to `require` the module `foo` from any file within `bar` using `require('foo')`.

Note that node will only match the module that is closest to the file in the filesystem hierarchy, starting from (the file's current directory/node_modules). Node matches directories this way up to the file system root.

You can either install new modules from the npm registry or other npm registries, or make your own.



#### Remarks


While everything in Node.js is generally done asynchronously, `require()` is not one of those things. Since modules in practice only need to be loaded once, it is a blocking operation and should be used properly.

Modules are cached after the first time they are loaded. Should you be editing a module in development, you will need to delete its entry in the module cache in order to use new changes. That being said, even if a module is cleared out of the module cache, the module itself is not garbage collected, so care should be taken for its use in production environments.

