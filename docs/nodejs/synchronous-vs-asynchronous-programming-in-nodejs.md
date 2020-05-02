---
metaTitle: "Synchronous vs Asynchronous programming in nodejs"
description: "Using async"
---

# Synchronous vs Asynchronous programming in nodejs




## Using async


The [async package](https://www.npmjs.com/package/async) provides functions for asynchronous code.

Using the [auto](http://caolan.github.io/async/docs.html#auto) function you can define asynchronous relations between two or more functions:

```js
var async = require('async');

async.auto({
    get_data: function(callback) {
        console.log('in get_data');
        // async code to get some data
        callback(null, 'data', 'converted to array');
    },
    make_folder: function(callback) {
        console.log('in make_folder');
        // async code to create a directory to store a file in
        // this is run at the same time as getting the data
        callback(null, 'folder');
    },
    write_file: ['get_data', 'make_folder', function(results, callback) {
        console.log('in write_file', JSON.stringify(results));
        // once there is some data and the directory exists,
        // write the data to a file in the directory
        callback(null, 'filename');
    }],
    email_link: ['write_file', function(results, callback) {
        console.log('in email_link', JSON.stringify(results));
        // once the file is written let's email a link to it...
        // results.write_file contains the filename returned by write_file.
        callback(null, {'file':results.write_file, 'email':'user@example.com'});
    }]
}, function(err, results) {
    console.log('err = ', err);
    console.log('results = ', results);
});

```

This code could have been made synchronously, by just calling the `get_data`, `make_folder`, `write_file` and `email_link` in the correct order. Async keeps track of the results for you, and if an error occurred (first parameter of `callback` unequal to `null`) it stops the execution of the other functions.

