---
metaTitle: "Promises"
description: "Introduction, Promise chaining, Waiting for multiple concurrent promises, Reduce an array to chained promises, Waiting for the first of multiple concurrent promises, Promisifying functions with callbacks, Error Handling, Reconciling synchronous and asynchronous operations, Delay function call, Promisifying values, Using ES2017 async/await, forEach with promises, Performing cleanup with finally(), Asynchronous API request"
---

# Promises




## Introduction


A [`Promise`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) object represents an operation which **has produced or will eventually produce** a value. Promises provide a robust way to wrap the (possibly pending) result of asynchronous work, mitigating the problem of deeply nested callbacks (known as "[callback hell](http://callbackhell.com/)").

### States and control flow

A promise can be in one of three states:

- **pending** — The underlying operation has not yet completed, and the promise is **pending** fulfillment.
- **fulfilled** — The operation has finished, and the promise is **fulfilled** with a **value**. This is analogous to returning a value from a synchronous function.
- **rejected** — An error has occurred during the operation, and the promise is **rejected** with a **reason**. This is analogous to throwing an error in a synchronous function.

A promise is said to be **settled** (or **resolved**) when it is either fulfilled or rejected. Once a promise is settled, it becomes immutable, and its state cannot change. The [`then`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) and [`catch`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch) methods of a promise can be used to attach callbacks that execute when it is settled. These callbacks are invoked with the fulfillment value and rejection reason, respectively.

[<img src="https://i.stack.imgur.com/idOX8.png" alt="Promise flow diagram" />](https://i.stack.imgur.com/idOX8.png)

### Example

```js
const promise = new Promise((resolve, reject) => {
    // Perform some work (possibly asynchronous)
    // ...

    if (/* Work has successfully finished and produced "value" */) {
        resolve(value);
    } else {
        // Something went wrong because of "reason"
        // The reason is traditionally an Error object, although
        // this is not required or enforced.
        let reason = new Error(message);
        reject(reason);

        // Throwing an error also rejects the promise.
        throw reason;
    }
});

```

The [`then`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) and [`catch`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch) methods can be used to attach fulfillment and rejection callbacks:

```js
promise.then(value => {
    // Work has completed successfully,
    // promise has been fulfilled with "value"
}).catch(reason => {
    // Something went wrong,
    // promise has been rejected with "reason"
});

```

**Note:** Calling `promise.then(...)` and `promise.catch(...)` on the same promise might result in an `Uncaught exception in Promise` if an error occurs, either while executing the promise or inside one of the callbacks, so the preferred way would be to attach the next listener on the promise returned by the previous `then` / `catch`.

Alternatively, both callbacks can be attached in a single call to [`then`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then):

```js
promise.then(onFulfilled, onRejected);

```

Attaching callbacks to a promise that has already been settled will immediately place them in the [microtask queue](http://stackoverflow.com/a/25933985/5931915), and they will be invoked "as soon as possible" (i.e. immediately after the currently executing script). It is not necessary to check the state of the promise before attaching callbacks, unlike with many other event-emitting implementations.

[Live demo](https://jsfiddle.net/SO_AMK/sy8s7a3a/)



## Promise chaining


The [`then`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) method of a promise returns a new promise.

```js
const promise = new Promise(resolve => setTimeout(resolve, 5000));

promise
    // 5 seconds later
    .then(() => 2)
    // returning a value from a then callback will cause
    // the new promise to resolve with this value
    .then(value => { /* value === 2 */ });

```

Returning a [`Promise`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise) from a [`then`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) callback will append it to the promise chain.

```js
function wait(millis) {
    return new Promise(resolve => setTimeout(resolve, millis));
}

const p = wait(5000).then(() => wait(4000)).then(() => wait(1000));
p.then(() => { /* 10 seconds have passed */ });

```

A [`catch`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch) allows a rejected promise to recover, similar to how `catch` in a `try`/`catch` statement works. Any chained [`then`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) after a [`catch`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch) will execute its resolve handler using the value resolved from the [`catch`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch).

```js
const p = new Promise(resolve => {throw 'oh no'});
p.catch(() => 'oh yes').then(console.log.bind(console));  // outputs "oh yes"

```

If there are no [`catch`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch) or `reject` handlers in the middle of the chain, a [`catch`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch) at the end will capture any rejection in the chain:

```js
p.catch(() => Promise.reject('oh yes'))
  .then(console.log.bind(console))      // won't be called
  .catch(console.error.bind(console));  // outputs "oh yes"

```

On certain occasions, you may want to "branch" the execution of the functions. You can do it by returning different promises from a function depending on the condition. Later in the code, you can merge all of these branches into one to call other functions on them and/or to handle all errors in one place.

```js
promise
    .then(result => {          
        if (result.condition) {
            return handlerFn1() 
                .then(handlerFn2);
        } else if (result.condition2) {
            return handlerFn3()
                .then(handlerFn4);
        } else {
            throw new Error("Invalid result");
        }
    })
    .then(handlerFn5)
    .catch(err => {
        console.error(err);
    });

```

Thus, the execution order of the functions looks like:

```js
promise --> handlerFn1 -> handlerFn2 --> handlerFn5 ~~> .catch()
         |                            ^
         V                            |
         -> handlerFn3 -> handlerFn4 -^            

```

The single `catch` will get the error on whichever branch it may occur.



## Waiting for multiple concurrent promises


The [`Promise.all()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/all) static method accepts an iterable (e.g. an `Array`) of promises and returns a new promise, which resolves when **all** promises in the iterable have resolved, or rejects if **at least one** of the promises in the iterable have rejected.

```js
// wait "millis" ms, then resolve with "value"
function resolve(value, milliseconds) {
    return new Promise(resolve => setTimeout(() => resolve(value), milliseconds));
}

// wait "millis" ms, then reject with "reason"
function reject(reason, milliseconds) {
    return new Promise((_, reject) => setTimeout(() => reject(reason), milliseconds));
}

Promise.all([
    resolve(1, 5000),
    resolve(2, 6000),
    resolve(3, 7000)    
]).then(values => console.log(values)); // outputs "[1, 2, 3]" after 7 seconds.

Promise.all([
    resolve(1, 5000),
    reject('Error!', 6000),
    resolve(2, 7000)
]).then(values => console.log(values)) // does not output anything
.catch(reason => console.log(reason)); // outputs "Error!" after 6 seconds.

```

Non-promise values in the iterable are ["promisified"](http://stackoverflow.com/documentation/javascript/231/promises/1850/promisifying-values).

```js
Promise.all([
    resolve(1, 5000),
    resolve(2, 6000),
    { hello: 3 }
])
.then(values => console.log(values)); // outputs "[1, 2, { hello: 3 }]" after 6 seconds

```

Destructuring assignment can help to retrieve results from multiple promises.

```js
Promise.all([
    resolve(1, 5000),
    resolve(2, 6000),
    resolve(3, 7000)
])
.then(([result1, result2, result3]) => {
    console.log(result1);
    console.log(result2);
    console.log(result3);
});

```



## Reduce an array to chained promises


This design pattern is useful for generating a sequence of asynchronous actions from a list of elements.

There are two variants :

- the "then" reduction, which builds a chain that continues as long as the chain experiences success.
- the "catch" reduction, which builds a chain that continues as long as the chain experiences error.

**The "then" reduction**

This variant of the pattern builds a [`.then()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) chain, and might be used for chaining animations, or making a sequence of dependent HTTP requests.

```js
[1, 3, 5, 7, 9].reduce((seq, n) => {
    return seq.then(() => {
        console.log(n);
        return new Promise(res => setTimeout(res, 1000));
    });
}, Promise.resolve()).then(
    () => console.log('done'),
    (e) => console.log(e)
);
// will log 1, 3, 5, 7, 9, 'done' in 1s intervals

```

Explanation:

1. We call [`.reduce()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce) on a source array, and provide [`Promise.resolve()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/resolve) as an initial value.
1. Every element reduced will add a [`.then()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) to the initial value.
1. [`reduce()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce)'s product will be Promise.resolve().then(...).then(...).
1. We manually append a [`.then(successHandler, errorHandler)`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) after the reduce, to execute `successHandler` once all the previous steps have resolved. If any step was to fail, then `errorHandler` would execute.

Note: The "then" reduction is a sequential counterpart of `Promise.all()`.

**The "catch" reduction**

This variant of the pattern builds a [`.catch()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch) chain and might be used for sequentially probing a set of web servers for some mirrored resource until a working server is found.

```js
var working_resource = 5; // one of the values from the source array
[1, 3, 5, 7, 9].reduce((seq, n) => {
    return seq.catch(() => {
        console.log(n);
        if(n === working_resource) { // 5 is working
            return new Promise((resolve, reject) => setTimeout(() => resolve(n), 1000));
        } else { // all other values are not working
            return new Promise((resolve, reject) => setTimeout(reject, 1000));
        }
    });
}, Promise.reject()).then(
    (n) => console.log('success at: ' + n),
    () => console.log('total failure')
);
// will log 1, 3, 5, 'success at 5' at 1s intervals

```

Explanation:

1. We call [`.reduce()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce) on a source array, and provide [`Promise.reject()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/reject) as an initial value.
1. Every element reduced will add a [`.catch()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/catch) to the initial value.
1. [`reduce()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce)'s product will be `Promise.reject().catch(...).catch(...)`.
1. We manually append [`.then(successHandler, errorHandler)`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/then) after the reduce, to execute `successHandler` once any of the previous steps has resolved. If all steps were to fail, then `errorHandler` would execute.

Note: The "catch" reduction is a sequential counterpart of `Promise.any()` (as implemented in `bluebird.js`, but not currently in native ECMAScript).



## Waiting for the first of multiple concurrent promises


The [`Promise.race()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/race) static method accepts an iterable of Promises and returns a new Promise which resolves or rejects as soon as the **first** of the promises in the iterable has resolved or rejected.

```js
// wait "milliseconds" milliseconds, then resolve with "value"
function resolve(value, milliseconds) {
    return new Promise(resolve => setTimeout(() => resolve(value), milliseconds));
}

// wait "milliseconds" milliseconds, then reject with "reason"
function reject(reason, milliseconds) {
    return new Promise((_, reject) => setTimeout(() => reject(reason), milliseconds));
}

Promise.race([
    resolve(1, 5000),
    resolve(2, 3000),
    resolve(3, 1000)
])
.then(value => console.log(value)); // outputs "3" after 1 second.


Promise.race([
    reject(new Error('bad things!'), 1000),
    resolve(2, 2000)
])
.then(value => console.log(value)) // does not output anything
.catch(error => console.log(error.message)); // outputs "bad things!" after 1 second

```



## "Promisifying" functions with callbacks


Given a function that accepts a Node-style callback,

```js
fooFn(options, function callback(err, result) { ... });

```

you can promisify it **(convert it to a promise-based function)** like this:

```js
function promiseFooFn(options) {
    return new Promise((resolve, reject) =>
        fooFn(options, (err, result) =>
            // If there's an error, reject; otherwise resolve
            err ? reject(err) : resolve(result)
        )
    );
}

```

This function can then be used as follows:

```js
promiseFooFn(options).then(result => {
    // success!
}).catch(err => {
    // error!
});

```

In a more generic way, here's how to promisify any given callback-style function:

```js
function promisify(func) {
    return function(...args) {
        return new Promise((resolve, reject) => {
            func(...args, (err, result) => err ? reject(err) : resolve(result));
        });
    }
}

```

This can be used like this:

```js
const fs = require('fs');
const promisedStat = promisify(fs.stat.bind(fs));

promisedStat('/foo/bar')
    .then(stat => console.log('STATE', stat))
    .catch(err => console.log('ERROR', err));

```



## Error Handling


Errors thrown from promises are handled by the second parameter (`reject`) passed to `then` or by the handler passed to `catch`:

```js
throwErrorAsync()
  .then(null, error => { /* handle error here */ });
// or
throwErrorAsync()
  .catch(error => { /* handle error here */ });

```

### Chaining

If you have a promise chain then an error will cause `resolve` handlers to be skipped:

```js
throwErrorAsync()
  .then(() => { /* never called */ })
  .catch(error => { /* handle error here */ });

```

The same applies to your `then` functions. If a `resolve` handler throws an exception then the next `reject` handler will be invoked:

```js
doSomethingAsync()
  .then(result => { throwErrorSync(); })
  .then(() => { /* never called */ })
  .catch(error => { /* handle error from throwErrorSync() */ });

```

An error handler returns a new promise, allowing you to continue a promise chain. The promise returned by the error handler is resolved with the value returned by the handler:

```js
throwErrorAsync()
  .catch(error => { /* handle error here */; return result; })
  .then(result => { /* handle result here */ });

```

You can let an error cascade down a promise chain by re-throwing the error:

```js
throwErrorAsync()
  .catch(error => {
      /* handle error from throwErrorAsync() */
      throw error;
  })
  .then(() => { /* will not be called if there's an error */ })
  .catch(error => { /* will get called with the same error */ });

```

It is possible to throw an exception that is not handled by the promise by wrapping the `throw` statement inside a `setTimeout` callback:

```js
new Promise((resolve, reject) => {
  setTimeout(() => { throw new Error(); });
});

```

This works because promises cannot handle exceptions thrown asynchronously.

### Unhandled rejections

An error will be silently ignored if a promise doesn't have a `catch` block or `reject` handler:

```js
throwErrorAsync()
  .then(() => { /* will not be called */ });
// error silently ignored

```

To prevent this, always use a `catch` block:

```js
throwErrorAsync()
  .then(() => { /* will not be called */ })
  .catch(error => { /* handle error*/ });
// or
throwErrorAsync()
  .then(() => { /* will not be called */ }, error => { /* handle error*/ });

```

Alternatively, subscribe to the [`unhandledrejection`](https://developer.mozilla.org/en-US/docs/Web/Events/unhandledrejection) event to catch any unhandled rejected promises:

```js
window.addEventListener('unhandledrejection', event => {});

```

Some promises can handle their rejection later than their creation time. The [`rejectionhandled`](https://developer.mozilla.org/en-US/docs/Web/Events/rejectionhandled) event gets fired whenever such a promise is handled:

```js
window.addEventListener('unhandledrejection', event => console.log('unhandled'));
window.addEventListener('rejectionhandled', event => console.log('handled'));
var p = Promise.reject('test');

setTimeout(() => p.catch(console.log), 1000);

// Will print 'unhandled', and after one second 'test' and 'handled'

```

The `event` argument contains information about the rejection. `event.reason` is the error object and `event.promise` is the promise object that caused the event.

In Nodejs the `rejectionhandled` and `unhandledrejection` events are called [`rejectionHandled`](https://nodejs.org/api/process.html#process_event_rejectionhandled) and [`unhandledRejection`](https://nodejs.org/api/process.html#process_event_unhandledrejection) on `process`, respectively, and have a different signature:

```js
process.on('rejectionHandled', (reason, promise) => {});
process.on('unhandledRejection', (reason, promise) => {});

```

The `reason` argument is the error object and the `promise` argument is a reference to the promise object that caused the event to fire.

Usage of these `unhandledrejection` and `rejectionhandled` events should be considered for debugging purposes only. Typically, all promises should handle their rejections.

**Note:** Currently, only Chrome 49+ and Node.js support `unhandledrejection` and `rejectionhandled` events.

### Caveats

### Chaining with `fulfill` and `reject`

The `then(fulfill, reject)` function (with both parameters not `null`) has unique and complex behavior, and shouldn't be used unless you know exactly how it works.

The function works as expected if given `null` for one of the inputs:

```js
// the following calls are equivalent
promise.then(fulfill, null) 
promise.then(fulfill)

// the following calls are also equivalent
promise.then(null, reject) 
promise.catch(reject)

```

However, it adopts unique behavior when both inputs are given:

```js
// the following calls are not equivalent!
promise.then(fulfill, reject)
promise.then(fulfill).catch(reject)

// the following calls are not equivalent!
promise.then(fulfill, reject)
promise.catch(reject).then(fulfill)

```

The `then(fulfill, reject)` function looks like it is a shortcut for `then(fulfill).catch(reject)`, but it is not, and will cause problems if used interchangeably. One such problem is that the `reject` handler does not handle errors from the `fulfill` handler. Here is what will happen:

```js
Promise.resolve() // previous promise is fulfilled
    .then(() => { throw new Error(); }, // error in the fulfill handler
        error => { /* this is not called! */ });

```

The above code will result in a rejected promise because the error is propagated. Compare it to the following code, which results in a fulfilled promise:

```js
Promise.resolve() // previous promise is fulfilled
    .then(() => { throw new Error(); }) // error in the fulfill handler
    .catch(error => { /* handle error */ });

```

A similar problem exists when using `then(fulfill, reject)` interchangeably with `catch(reject).then(fulfill)`, except with propagating fulfilled promises instead of rejected promises.

### Synchronously throwing from function that should return a promise

Imagine a function like this:

```js
function foo(arg) {
  if (arg === 'unexepectedValue') {
    throw new Error('UnexpectedValue')
  }

  return new Promise(resolve => 
    setTimeout(() => resolve(arg), 1000)
  )
}

```

If such function is used in the **middle** of a promise chain, then apparently there is no problem:

```js
makeSomethingAsync().
  .then(() => foo('unexpectedValue'))
  .catch(err => console.log(err)) // <-- Error: UnexpectedValue will be caught here

```

However, if the same function is called outside of a promise chain, then the error will not be handled by it and will be thrown to the application:

```js
foo('unexpectedValue') // <-- error will be thrown, so the application will crash
  .then(makeSomethingAsync) // <-- will not run
  .catch(err => console.log(err)) // <-- will not catch

```

There are 2 possible workarounds:

### Return a rejected promise with the error

Instead of throwing, do as follows:

```js
function foo(arg) {
  if (arg === 'unexepectedValue') {
    return Promise.reject(new Error('UnexpectedValue'))
  }

  return new Promise(resolve => 
    setTimeout(() => resolve(arg), 1000)
  )
}

```

### Wrap your function into a promise chain

Your `throw` statement will be properly caught when it is already inside a promise chain:

```js
function foo(arg) {
  return Promise.resolve()
    .then(() => {
      if (arg === 'unexepectedValue') {
        throw new Error('UnexpectedValue')
      }

      return new Promise(resolve => 
        setTimeout(() => resolve(arg), 1000)
      )
    })
}

```



## Reconciling synchronous and asynchronous operations


In some cases you may want to wrap a synchronous operation inside a promise to prevent repetition in code branches. Take this example:

```js
if (result) { // if we already have a result
  processResult(result); // process it
} else {
  fetchResult().then(processResult);
}

```

The synchronous and asynchronous branches of the above code can be reconciled by redundantly wrapping the synchronous operation inside a promise:

```js
var fetch = result
  ? Promise.resolve(result)
  : fetchResult();

fetch.then(processResult);

```

When caching the result of an asynchronous call, it is preferable to cache the promise rather than the result
itself. This ensures that only one asynchronous operation is required to resolve multiple parallel requests.

Care should be taken to invalidate cached values when error conditions are encountered.

```js
// A resource that is not expected to change frequently
var planets = 'http://swapi.co/api/planets/';
// The cached promise, or null
var cachedPromise;

function fetchResult() {
    if (!cachedPromise) {
        cachedPromise = fetch(planets)
            .catch(function (e) {
                // Invalidate the current result to retry on the next fetch
                cachedPromise = null;
                // re-raise the error to propagate it to callers
                throw e;
            });
    }
    return cachedPromise;
}

```



## Delay function call


The [`setTimeout()`](https://developer.mozilla.org/en-US/docs/Web/API/WindowTimers/setTimeout) method calls a function or evaluates an expression after a specified number of milliseconds. It is also a trivial way to achieve an asynchronous operation.

In this example calling the `wait` function resolves the promise after the time specified as first argument:

```js
function wait(ms) {
    return new Promise(resolve => setTimeout(resolve, ms));  
}

wait(5000).then(() => { 
    console.log('5 seconds have passed...');
});

```



## "Promisifying" values


The [`Promise.resolve`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/resolve) static method can be used to wrap values into promises.

```js
let resolved = Promise.resolve(2);
resolved.then(value => {
    // immediately invoked
    // value === 2
});

```

If `value` is already a promise, [`Promise.resolve`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/resolve) simply recasts it.

```js
let one = new Promise(resolve => setTimeout(() => resolve(2), 1000));
let two = Promise.resolve(one);
two.then(value => {
    // 1 second has passed
    // value === 2
});

```

In fact, `value` can be any "thenable" (object defining a `then` method that works sufficiently like a spec-compliant promise). This allows [`Promise.resolve`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/resolve) to convert untrusted 3rd-party objects into trusted 1st-party Promises.

```js
let resolved = Promise.resolve({
    then(onResolved) {
        onResolved(2);
    }
});
resolved.then(value => {
    // immediately invoked
    // value === 2
});

```

The [`Promise.reject`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Promise/reject) static method returns a promise which immediately rejects with the given `reason`.

```js
let rejected = Promise.reject("Oops!");
rejected.catch(reason => {
    // immediately invoked
    // reason === "Oops!"
});

```



## Using ES2017 async/await


The same example above, [Image loading](http://stackoverflow.com/documentation/javascript/231/promises/848/image-loading), can be written using [async functions](http://stackoverflow.com/documentation/javascript/925/async-functions-async-await). This also allows using the common `try/catch` method for exception handling.

Note: [as of April 2017, the current releases of all browsers but Internet Explorer supports async functions](http://caniuse.com/#feat=async-functions).

```js
function loadImage(url) {
    return new Promise((resolve, reject) => {
        const img = new Image();
        img.addEventListener('load', () => resolve(img));
        img.addEventListener('error', () => {
            reject(new Error(`Failed to load ${url}`));
        });
        img.src = url;
    });
}

(async () => {

    // load /image.png and append to #image-holder, otherwise throw error
    try {
        let img = await loadImage('http://example.com/image.png');
        document.getElementById('image-holder').appendChild(img);
    }
    catch (error) {
        console.error(error);
    }

})();

```



## forEach with promises


It is possible to effectively apply a function (`cb`) which returns a promise to each element of an array, with each element waiting to be processed until the previous element is processed.

```js
function promiseForEach(arr, cb) {
  var i = 0;

  var nextPromise = function () {
    if (i >= arr.length) {
      // Processing finished.
      return;
    }

    // Process next function. Wrap in `Promise.resolve` in case
    // the function does not return a promise
    var newPromise = Promise.resolve(cb(arr[i], i));
    i++;
    // Chain to finish processing.
    return newPromise.then(nextPromise);
  };

  // Kick off the chain.
  return Promise.resolve().then(nextPromise);
};

```

This can be helpful if you need to efficiently process thousands of items, one at a time. Using a regular `for` loop to create the promises will create them all at once and take up a significant amount of RAM.



## Performing cleanup with finally()


There is currently a [proposal](https://github.com/tc39/proposal-promise-finally) (not yet part of the ECMAScript standard) to add a `finally` callback to promises that will be executed regardless of whether the promise is fulfilled or rejected. Semantically, this is similar to the [`finally` clause of the `try` block](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/try...catch#The_finally_clause).

You would usually use this functionality for cleanup:

```js
var loadingData = true;

fetch('/data')
    .then(result => processData(result.data))
    .catch(error => console.error(error))
    .finally(() => {
        loadingData = false;
    });

```

It is important to note that the `finally` callback doesn't affect the state of the promise. It doesn't matter what value it returns, the promise stays in the fulfilled/rejected state that it had before. So in the example above the promise
will be resolved with the return value of `processData(result.data)` even though the `finally` callback returned `undefined`.

With the standardization process still being in progress, your promises implementation most likely won't support `finally` callbacks out of the box. For synchronous callbacks you can add this functionality with a polyfill however:

```js
if (!Promise.prototype.finally) {
    Promise.prototype.finally = function(callback) {
        return this.then(result => {
            callback();
            return result;
        }, error => {
            callback();
            throw error;
        });
    };
}

```



## Asynchronous API request


This is an example of a simple `GET` API call wrapped in a promise to take advantage of its asynchronous functionality.

```js
var get = function(path) {
  return new Promise(function(resolve, reject) {
    let request = new XMLHttpRequest();
    request.open('GET', path);
    request.onload = resolve;
    request.onerror = reject;
    request.send();
  });
};

```

More robust error handling can be done using the following [`onload`](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequestEventTarget/onload) and [`onerror`](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequestEventTarget/onerror) functions.

```js
request.onload = function() {
  if (this.status >= 200 && this.status < 300) {
    if(request.response) {
      // Assuming a successful call returns JSON
      resolve(JSON.parse(request.response));
    } else {
      resolve();
  } else {
    reject({
      'status': this.status,
      'message': request.statusText
    });
  }
};

request.onerror = function() {
  reject({
    'status': this.status,
    'message': request.statusText
  });
};

```



#### Syntax


- new Promise( /* executor function: */ function(resolve, reject) { })
- promise.then(onFulfilled[, onRejected])
- promise.catch(onRejected)
- Promise.resolve(resolution)
- Promise.reject(reason)
- Promise.all(iterable)
- Promise.race(iterable)



#### Remarks


Promises are part of the ECMAScript 2015 specification and [browser support](http://caniuse.com/#feat=promises) is limited, with 88% of browsers worldwide supporting it as of July 2017. The following table gives an overview of the earliest browser versions that provide support for promises.

|Chrome|Edge|Firefox|Internet Explorer|Opera|Opera Mini|Safari|iOS Safari</th>
|---|---|---|---|---|---|---|---|---|---
|32|12|27|x|19|x|7.1|8</td>

In environments which do not support them, `Promise` can be polyfilled. Third-party libraries may also provide extended functionalities, such as automated "promisification" of callback functions or additional methods like `progress`—also known as `notify`.

The Promises/A+ standard website provides a [list of 1.0 and 1.1 compliant implementations](https://promisesaplus.com/implementations). Promise callbacks based on the A+ standard are always executed asynchronously as [microtasks in the event loop](https://html.spec.whatwg.org/multipage/webappapis.html#task-queue).

