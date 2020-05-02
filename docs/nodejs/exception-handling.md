---
metaTitle: "Exception handling"
description: "Handling Exception In Node.Js, Unhanded Exception Management, Errors and Promises"
---

# Exception handling



## Handling Exception In Node.Js


Node.js has 3 basic ways to handle exceptions/errors:

1. **try**-**catch** block
1. **error** as the first argument to a `callback`
1. `emit` an **error** event using eventEmitter

**try-catch** is used to catch the exceptions thrown from the synchronous code execution. If the caller (or the caller's caller, ...) used try/catch, then they can catch the error. If none of the callers had try-catch than the program crashes.<br />
If using try-catch on an async operation and exception was thrown from callback of async method than it will not get caught by try-catch. To catch an exception from async operation callback, it is preferred to use **promises**.<br />
Example to understand it better

```js
// ** Example - 1  **
function doSomeSynchronousOperation(req, res) {
    if(req.body.username === ''){
        throw new Error('User Name cannot be empty');
    }  
    return true;  
}

// calling the method above
try {
    // synchronous code   
    doSomeSynchronousOperation(req, res)    
catch(e) {
    //exception handled here   
    console.log(e.message);  
} 

// ** Example - 2 **
function doSomeAsynchronousOperation(req, res, cb) {
    // imitating async operation
    return setTimeout(function(){
        cb(null, []);
    },1000);
}
 
try {
    // asynchronous code   
    doSomeAsynchronousOperation(req, res, function(err, rs){
        throw new Error("async operation exception");
    })   
} catch(e) {
     // Exception will not get handled here
     console.log(e.message);  
}
// The exception is unhandled and hence will cause application to break

```

**callbacks** are mostly used in Node.js as callback delivers an event asynchronously. The user passes you a function (the callback), and you invoke it sometime later when the asynchronous operation completes.<br />
The usual pattern is that the callback is invoked as a **callback(err, result)**, where only one of err and result is non-null, depending on whether the operation succeeded or failed.

```js
function doSomeAsynchronousOperation(req, res, callback) {
   setTimeout(function(){
        return callback(new Error('User Name cannot be empty'));    
   }, 1000);  
   return true;
}

doSomeAsynchronousOperation(req, res, function(err, result) {
   if (err) {
       //exception handled here 
       console.log(err.message);
   }
   
   //do some stuff with valid data
});

```

**emit** For more complicated cases, instead of using a callback, the function itself can return an EventEmitter object, and the caller would be expected to listen for error events on the emitter.

```js
const EventEmitter = require('events');

function doSomeAsynchronousOperation(req, res) {
    let myEvent = new EventEmitter();

    // runs asynchronously
    setTimeout(function(){
        myEvent.emit('error', new Error('User Name cannot be empty'));
    }, 1000);

    return myEvent;
}

// Invoke the function
let event = doSomeAsynchronousOperation(req, res);

event.on('error', function(err) {
    console.log(err);
});

event.on('done', function(result) {
    console.log(result); // true
});

```



## Unhanded Exception Management


Because Node.js runs on a single process uncaught exceptions are an issue to be aware of when developing applications.

### Silently Handling Exceptions

Most of the people let node.js server(s) silently swallow up the errors.

- Silently handling the exception

```js
process.on('uncaughtException', function (err) {
  console.log(err);
});

```

**This is bad**, it will work but:

<li>
Root cause will remains unknown, as such will not contribute to resolution of what caused the Exception ( Error ).
</li>
<li>
In case of database connection ( pool ) gets closed for some reason this will result in constant propagation of errors, meaning that server will be running but it will not reconnect to db.
</li>

### Returning to Initial state

In case of an " uncaughtException " it is good to restart the server and return it to its **initial state**, where we know it will work. Exception is logged, application is terminated but since it will be running in a container that will make sure that the server is running we will achieve restarting of the server ( returning to the initial working state ) .

- Installing the forever ( or other CLI tool to make sure that node server runs continuously )

```js
npm install forever -g

```


- Starting the server in forever

```js
forever start app.js

```

> 
<p>Reason why is it started and why we use forever is after the server is
**terminated** forever process will start the server again.</p>


- Restarting the server

```js
process.on('uncaughtException', function (err) {
    console.log(err);

    // some logging mechanisam
    // ....        

    process.exit(1); // terminates process
});

```

On a side note there was a way also to handle exceptions with **Clusters and Domains**.

Domains are deprecated more information [here](https://nodejs.org/api/domain.html).



## Errors and Promises


Promises handle errors differently to synchronous or callback-driven code.

```js
const p = new Promise(function (resolve, reject) {
    reject(new Error('Oops'));
});

// anything that is `reject`ed inside a promise will be available through catch
// while a promise is rejected, `.then` will not be called
p
    .then(() => {
        console.log("won't be called");
    })
    .catch(e => {
        console.log(e.message); // output: Oops
    })
    // once the error is caught, execution flow resumes
    .then(() => {
        console.log('hello!'); // output: hello!
    });

```

currently, errors thrown in a promise that are not caught results in the error being swallowed, which can make it difficult to track down the error. This can be [solved](https://www.npmjs.com/package/eslint-plugin-promise) using linting tools like [eslint](http://eslint.org/) or by ensuring you always have a `catch` clause.

This behaviour is deprecated [in node 8](https://nodejs.org/dist/latest-v8.x/docs/api/deprecations.html#deprecations_dep0018_unhandled_promise_rejections) in favour of terminating the node process.

