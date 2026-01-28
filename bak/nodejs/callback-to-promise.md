---
metaTitle: "Node.js - Callback to Promise"
description: "Promisifying a callback, Manually promisifying a callback, setTimeout promisified"
---

# Callback to Promise



## Promisifying a callback


Callback-based:

```js
db.notification.email.find({subject: 'promisify callback'}, (error, result) => {
   if (error) {
       console.log(error);
   }

   // normal code here
});

```

This uses bluebird's promisifyAll method to promisify what is conventionally callback-based code like above. bluebird will make a promise version of all the methods in the object, those promise-based methods names has Async appended to them:

```js
let email = bluebird.promisifyAll(db.notification.email);

email.findAsync({subject: 'promisify callback'}).then(result => {

    // normal code here
})
.catch(console.error);

```

If only specific methods need to be promisified, just use its promisify:

```js
let find = bluebird.promisify(db.notification.email.find);

find({locationId: 168}).then(result => {
    
    // normal code here
});
.catch(console.error);

```

There are some libraries (e.g., MassiveJS) that can't be promisified if the immediate object of the method is not passed on second parameter. In that case, just pass the immediate object of the method that need to be promisified on second parameter and enclosed it in context property.

```js
let find = bluebird.promisify(db.notification.email.find, { context: db.notification.email });

find({locationId: 168}).then(result => {

    // normal code here
});
.catch(console.error);

```



## Manually promisifying a callback


Sometimes it might be necessary to manually promisify a callback function. This could be for a case where the callback does not follow the standard [error-first format](http://fredkschott.com/post/2014/03/understanding-error-first-callbacks-in-node-js/) or if additional logic is needed to promisify:

Example with [fs.exists(path, callback)](https://nodejs.org/api/fs.html#fs_fs_exists_path_callback):

```js
var fs = require('fs');

var existsAsync = function(path) {
  return new Promise(function(resolve, reject) {
    fs.exists(path, function(exists) {
      // exists is a boolean
      if (exists) {
        // Resolve successfully
        resolve();
      } else {
        // Reject with error
        reject(new Error('path does not exist'));
      }
    });
});

// Use as a promise now
existsAsync('/path/to/some/file').then(function() {
  console.log('file exists!');
}).catch(function(err) {
  // file does not exist
  console.error(err);
});

```



## setTimeout promisified


```js
function wait(ms) {
    return new Promise(function (resolve, reject) {
        setTimeout(resolve, ms)
    })
}

```

