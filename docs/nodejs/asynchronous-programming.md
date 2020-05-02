---
metaTitle: "Asynchronous programming"
description: "Callback functions, Callback hell, Native Promises, Code example, Async error handling"
---

# Asynchronous programming


Node is a programming language where everything could run on an asynchronous way. Below you could find some examples and the typical things of asynchronous working.



## Callback functions


### Callback functions in JavaScript

Callback functions are common in JavaScript. Callback functions are possible in JavaScript because [functions are first-class citizens](https://en.wikipedia.org/wiki/First-class_function).

### Synchronous callbacks.

Callback functions can be synchronous or asynchronous. Since Asynchronous callback functions may be more complex here is a simple example of a synchronous callback function.

```js
// a function that uses a callback named `cb` as a parameter
function getSyncMessage(cb) {
    cb("Hello World!");
}

console.log("Before getSyncMessage call");
// calling a function and sending in a callback function as an argument.
getSyncMessage(function(message) {
    console.log(message);
});
console.log("After getSyncMessage call");

```

The output for the above code is:

```js
> Before getSyncMessage call
> Hello World!
> After getSyncMessage call

```

First we will step through how the above code is executed. This is more for those who do not already understand the concept of callbacks if you do already understand it feel free to skip this paragraph. First the code is parsed and then the first interesting thing to happen is line 6 is executed which outputs `Before getSyncMessage call` to the console. Then line 8 is executed which calls the function `getSyncMessage` sending in an anonymous function as an argument for the parameter named `cb` in the `getSyncMessage` function. Execution is now done inside the `getSyncMessage` function on line 3 which executes the function `cb` which was just passed in, this call sends an argument string "Hello World" for the param named `message` in the passed in anonymous function. Execution then goes to line 9 which logs `Hello World!` to the console. Then the execution goes through the process of exiting the [callstack](https://developer.mozilla.org/en-US/docs/Glossary/Call_Stack) ([see also](https://developer.mozilla.org/en-US/docs/Web/JavaScript/EventLoop)) hitting line 10 then line 4 then finally back to line 11.

Some information to know about callbacks in general:

- The function you send in to a function as a callback may be called zero times, once, or multiple times. It all depends on implementation.
- The callback function may be called synchronously or asynchronously and possibly both synchronously and asynchronously.
- Just like normal functions the names you give parameters to your function are not important but the order is. So for example on line 8 the parameter `message` could have been named `statement`, `msg`, or if you're being nonsensical something like `jellybean`. So you should know what parameters are sent into your callback so you can get them in the right order with proper names.

### Asynchronous callbacks.

One thing to note about JavaScript is it is synchronous by default, but there are APIs given in the environment (browser, Node.js, etc.) that could make it asynchronous (there's more about that [here](https://stackoverflow.com/a/13806828/2066736)).

Some common things that are asynchronous in JavaScript environments that accept callbacks:

- Events
- setTimeout
- setInterval
- the fetch API
- Promises

Also any function that uses one of the above functions may be wrapped with a function that takes a callback and the callback would then be an asynchronous callback (although wrapping a promises with a function that takes a callback would likely be considered an anti-pattern as there are more preferred ways to handle promises).

So given that information we can construct an asynchronous function similar to the above synchronous one.

```js
// a function that uses a callback named `cb` as a parameter
function getAsyncMessage(cb) {
    setTimeout(function () { cb("Hello World!") }, 1000);
}

console.log("Before getSyncMessage call");
// calling a function and sending in a callback function as an argument.
getAsyncMessage(function(message) {
    console.log(message);
});
console.log("After getSyncMessage call");

```

Which prints the following to the console:

```js
> Before getSyncMessage call
> After getSyncMessage call
// pauses for 1000 ms with no output
> Hello World!

```

Line execution goes to line 6 logs "Before getSyncMessage call". Then execution goes to line 8 calling getAsyncMessage with a callback for the param `cb`. Line 3 is then executed which calls setTimeout with a callback as the first argument and the number 300 as the second argument. `setTimeout` does whatever it does and holds on to that callback so that it can call it later in 1000 milliseconds, but following setting up the timeout and before it pauses the 1000 milliseconds it hands execution back to where it left off so it goes to line 4, then line 11, and then pauses for 1 second and setTimeout then calls its callback function which takes execution back to line 3 where `getAsyncMessages` callback is called with value "Hello World" for its parameter `message` which is then logged to the console on line 9.

### Callback functions in Node.js

NodeJS has asynchronous callbacks and commonly supplies two parameters to your functions sometimes conventionally called `err` and `data`. An example with reading a file text.

```js
const fs = require("fs");

fs.readFile("./test.txt", "utf8", function(err, data) {
    if(err) {
        // handle the error 
    } else {
        // process the file text given with data
    }
});

```

This is an example of a callback that is called a single time.

It's good practice to handle the error somehow even if your just logging it or throwing it. The else is not necessary if you throw or return and can be removed to decrease indentation so long as you stop execution of the current function in the if by doing something like throwing or returning.

Though it may be common to see `err`, `data` it may not always be the case that your callbacks will use that pattern it's best to look at documentation.

Another example callback comes from the express library (express 4.x):

```js
// this code snippet was on http://expressjs.com/en/4x/api.html
const express = require('express');
const app = express();

// this app.get method takes a url route to watch for and a callback
// to call whenever that route is requested by a user.
app.get('/', function(req, res){
  res.send('hello world');
});

app.listen(3000);

```

This example shows a callback that is called multiple times. The callback is provided with two objects as params named here as `req` and `res` these names correspond to request and response respectively, and they provide ways to view the request coming in and set up the response that will be sent to the user.

As you can see there are various ways a callback can be used to execute sync and async code in JavaScript and callbacks are very ubiquitous throughout JavaScript.



## Callback hell


Callback hell (also a pyramid of doom or boomerang effect) arises when you nest too many callback functions inside a callback function. Here is an example to read a file (in ES6).

```js
const fs = require('fs');
let filename = `${__dirname}/myfile.txt`;

fs.exists(filename, exists => {
    if (exists) {
        fs.stat(filename, (err, stats) => {
            if (err) {
                throw err;
            }
            if (stats.isFile()) {
                fs.readFile(filename, null, (err, data) => {
                    if (err) {
                        throw err;
                    }
                    console.log(data);
                });
            }
            else {
                throw new Error("This location contains not a file");
            }
        });
    }
    else {
        throw new Error("404: file not found");
    }
}); 

```

**How to avoid "Callback Hell"**

It is recommended to nest no more than 2 callback functions. This will help you maintain code readability and will me much easier to maintain in the future. If you have a need to nest more than 2 callbacks, try to make use of [distributed events](https://nodejs.org/api/events.html) instead.

There also exists a library called [async](https://caolan.github.io/async/) that helps manage callbacks and their execution available on npm. It increases the readability of callback code and gives you more control over your callback code flow, including allowing you to run them in parallel or in series.



## Native Promises


Promises are a tool for async programming. In JavaScript promises are known for their `then` methods. Promises have two main states 'pending' and 'settled'. Once a promise is 'settled' it cannot go back to 'pending'. This means that promises are mostly good for events that only occur once. The 'settled' state has two states as well 'resolved' and 'rejected'. You can create a new promise using the `new` keyword and passing a function into the constructor `new Promise(function (resolve, reject) {})`.

The function passed into the Promise constructor always receives a first and second parameter usually named `resolve` and `reject` respectively. The naming of these two parameters is convention, but they will put the promise into either the 'resolved' state or the 'rejected' state. When either one of these is called the promise goes from being 'pending' to 'settled'. `resolve` is called when the desired action, which is often asynchronous, has been performed and `reject` is used if the action has errored.

In the below timeout is a function that returns a Promise.

```js
function timeout (ms) {
  return new Promise(function (resolve, reject) {
    setTimeout(function () {
      resolve("It was resolved!");
    }, ms)
  });
}

timeout(1000).then(function (dataFromPromise) {
  // logs "It was resolved!"
  console.log(dataFromPromise);
})

console.log("waiting...");

```

console output

```js
waiting...
// << pauses for one second>>
It was resolved!

```

When timeout is called the function passed to the Promise constructor is executed without delay. Then the setTimeout method is executed and its callback is set to fire in the next `ms` milliseconds, in this case `ms=1000`. Since the callback to the setTimeout isn't fired yet the timeout function returns control to the calling scope. The chain of `then` methods are then stored to be called later when/if the Promise has resolved. If there were `catch` methods here they would be stored as well, but would be fired when/if the promise 'rejects'.

The script then prints 'waiting...'. One second later the setTimeout calls its callback which calls the resolve function with the string "It was resolved!". That string is then passed into the `then` method's callback and is then logged to the user.

In the same sense you can wrap the asynchronous setTimeout function which requires a callback you can wrap any singular asynchronous action with a promise.

Read more about promises in the JavaScript documentation [Promises](http://stackoverflow.com/documentation/javascript/231/promises#t=201704200048290479802).



## Code example


**Question:** What is the output of code below and why?

```js
setTimeout(function() {
    console.log("A");
}, 1000);

setTimeout(function() {
    console.log("B");
}, 0);

getDataFromDatabase(function(err, data) {
    console.log("C");
    setTimeout(function() {
        console.log("D");
    }, 1000);
});

console.log("E");

```

**Output:** This is known for sure: `EBAD`. `C` is unknown when it will be logged.

**Explanation:** The compiler will not stop on the `setTimeout` and the `getDataFromDatabase` methodes. So the first line he will log is `E`. The callback functions **(first argument of `setTimeout`)** will run after the set timeout on a asynchronous way!

**More details:**

1. `E` has no `setTimeout`
1. `B` has a set timeout of 0 milliseconds
1. `A` has a set timeout of 1000 milliseconds
1. `D` must request a database, after it must `D` wait 1000 milliseconds so it comes after `A`.
1. `C` is unknown because it is unknown when the data of the database is requested. It could be before or after `A`.



## Async error handling


### Try catch

Errors must always be handled. If you are using synchronous programming you could use a `try catch`. But this does not work if you work asynchronous! Example:

```js
try {
    setTimeout(function() {
        throw new Error("I'm an uncaught error and will stop the server!");
    }, 100); 
}
catch (ex) {
    console.error("This error will not be work in an asynchronous situation: " + ex);
}

```

Async errors will only be handled inside the callback function!

### Working possibilities

### Event handlers

The first versions of Node.JS got an event handler.

```js
process.on("UncaughtException", function(err, data) { 
    if (err) {
        // error handling
    }
});

```

### Domains

Inside a domain, the errors are release via the event emitters. By using this are all errors, timers, callback methodes implicitly only registrated inside the domain. By an error, be an error event send and didn't crash the application.

```js
var domain = require("domain");
var d1 = domain.create();
var d2 = domain.create();

d1.run(function() {
    d2.add(setTimeout(function() {
        throw new Error("error on the timer of domain 2");
    }, 0));
});

d1.on("error", function(err) {
    console.log("error at domain 1: " + err);
});

d2.on("error", function(err) {
    console.log("error at domain 2: " + err);
});

```



#### Syntax


- doSomething([args], function([argsCB]) { /* do something when done */});
- doSomething([args], ([argsCB]) => { /* do something when done */ });

