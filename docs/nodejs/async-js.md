---
metaTitle: "Node.js - async.js"
description: "Parallel : multi-tasking, Series : independent mono-tasking, Waterfall : dependent mono-tasking, async.each(To handle array of data efficiently), async.times(To handle for loop in better way), async.series(To handle events  one by one)"
---

# async.js




## Parallel : multi-tasking


**[async.parallel(tasks, afterTasksCallback)](http://caolan.github.io/async/docs.html#.parallel)** will execute a set of tasks in parallel and **wait the end of all tasks** (reported by the call of **callback** function).

When tasks are finished, **async** call the main callback with all errors and all results of tasks.

```js
function shortTimeFunction(callback) {
  setTimeout(function() {
    callback(null, 'resultOfShortTime');
  }, 200);
}

function mediumTimeFunction(callback) {
  setTimeout(function() {
    callback(null, 'resultOfMediumTime');
  }, 500);
}

function longTimeFunction(callback) {
  setTimeout(function() {
    callback(null, 'resultOfLongTime');
  }, 1000);
}

async.parallel([
    shortTimeFunction,
    mediumTimeFunction,
    longTimeFunction
  ],
  function(err, results) {
    if (err) {
      return console.error(err);
    }

    console.log(results);
  });

```

**Result :** `["resultOfShortTime", "resultOfMediumTime", "resultOfLongTime"]`.

### Call `async.parallel()` with an object

You can replace the **tasks** array parameter by an object. In this case, results will be also an object **with the same keys than tasks**.

It's very useful to compute some tasks and find easily each result.

```js
async.parallel({
    short: shortTimeFunction,
    medium: mediumTimeFunction,
    long: longTimeFunction
  },
  function(err, results) {
    if (err) {
      return console.error(err);
    }

    console.log(results);
  });

```

**Result :** `{short: "resultOfShortTime", medium: "resultOfMediumTime", long: "resultOfLongTime"}`.

### Resolving multiple values

Each parallel function is passed a callback. This callback can either return an error as the first argument or success values after that. If a callback is passed several success values, these results are returned as an array.

```js
async.parallel({
    short: function shortTimeFunction(callback) {
      setTimeout(function() {
        callback(null, 'resultOfShortTime1', 'resultOfShortTime2');
      }, 200);
    },
    medium: function mediumTimeFunction(callback) {
      setTimeout(function() {
        callback(null, 'resultOfMediumTime1', 'resultOfMeiumTime2');
      }, 500);
    }
  },
  function(err, results) {
    if (err) {
      return console.error(err);
    }

    console.log(results);
  });

```

**Result :**

```js
{
    short: ["resultOfShortTime1", "resultOfShortTime2"], 
    medium: ["resultOfMediumTime1", "resultOfMediumTime2"]
}

```

.



## Series : independent mono-tasking


**[async.series(tasks, afterTasksCallback)](http://caolan.github.io/async/docs.html#.series)** will execute a set of tasks. Each task are executed **after another**. **If a task fails, **async** stops immediately the execution and jump into the main callback**.

When tasks are finished successfully, **async** call the "master" callback with all errors and all results of tasks.

```js
function shortTimeFunction(callback) {
  setTimeout(function() {
    callback(null, 'resultOfShortTime');
  }, 200);
}

function mediumTimeFunction(callback) {
  setTimeout(function() {
    callback(null, 'resultOfMediumTime');
  }, 500);
}

function longTimeFunction(callback) {
  setTimeout(function() {
    callback(null, 'resultOfLongTime');
  }, 1000);
}

async.series([
    mediumTimeFunction,
    shortTimeFunction,
    longTimeFunction
  ],
  function(err, results) {
    if (err) {
      return console.error(err);
    }

    console.log(results);
  });

```

**Result :** `["resultOfMediumTime", "resultOfShortTime", "resultOfLongTime"]`.

### Call `async.series()` with an object

You can replace the **tasks** array parameter by an object. In this case, results will be also an object **with the same keys than tasks**.

It's very useful to compute some tasks and find easily each result.

```js
async.series({
    short: shortTimeFunction,
    medium: mediumTimeFunction,
    long: longTimeFunction
  },
  function(err, results) {
    if (err) {
      return console.error(err);
    }

    console.log(results);
  });

```

**Result :** `{short: "resultOfShortTime", medium: "resultOfMediumTime", long: "resultOfLongTime"}`.



## Waterfall : dependent mono-tasking


**[async.waterfall(tasks, afterTasksCallback)](http://caolan.github.io/async/docs.html#.waterfall)** will execute a set of tasks. Each task are executed **after another, and the result of a task is passed to the next task**. As **async.series()**, if a task fails, **async** stop the execution and call immediately the main callback.

When tasks are finished successfully, **async** call the "master" callback with all errors and all results of tasks.

```js
function getUserRequest(callback) {
  // We simulate the request with a timeout
  setTimeout(function() {
    var userResult = {
      name : 'Aamu'
    };

    callback(null, userResult);
  }, 500);
}

function getUserFriendsRequest(user, callback) {
  // Another request simulate with a timeout
  setTimeout(function() {
    var friendsResult = [];

    if (user.name === "Aamu"){
        friendsResult = [{
          name : 'Alice'
        }, {
          name: 'Bob'
        }];
    }
    
    callback(null, friendsResult);
  }, 500);
}

async.waterfall([
    getUserRequest,
    getUserFriendsRequest
  ],
  function(err, results) {
    if (err) {
      return console.error(err);
    }

    console.log(JSON.stringify(results));
  });

```

**Result:** `results` contains the second callback parameter of the last function of the waterfall, which is `friendsResult` in that case.



## async.each(To handle array of data efficiently)


When we want to handle array of data, its better to use **async.each**. When we want to perform something with all data & want to get the final callback once everything is done, then this method will be useful. This is handled in parallel way.

```js
function createUser(userName, callback)
{
    //create user in db
    callback(null)//or error based on creation
}

var arrayOfData = ['Ritu', 'Sid', 'Tom'];
async.each(arrayOfData, function(eachUserName, callback) {

    // Perform operation on each user.
    console.log('Creating user '+eachUserName);
    //Returning callback is must. Else it wont get the final callback, even if we miss to return one callback
    createUser(eachUserName, callback);
  
}, function(err) {
    //If any of the user creation failed may throw error.
    if( err ) {
      // One of the iterations produced an error.
      // All processing will now stop.
      console.log('unable to create user');
    } else {
      console.log('All user created successfully');
    }
});

```

To do one at a time can use **async.eachSeries**



## async.times(To handle for loop in better way)


To execute a function within a loop in node.js, it's fine to use a `for` loop for short loops. But the loop is long, using `for` loop will increase the time of processing which might cause the node process to hang. In such scenarios, you can use: **asycn.times**

```js
function recursiveAction(n, callback)
{
    //do whatever want to do repeatedly
    callback(err, result);
}
async.times(5, function(n, next) {
    recursiveAction(n, function(err, result) {
        next(err, result);
    });
}, function(err, results) {
    // we should now have 5 result
});

```

This is called in parallel. When we want to call it one at a time, use:
**async.timesSeries**



## async.series(To handle events  one by one)


/**In async.series,all the functions are executed in series and the consolidated outputs of each function is passed to the final callback. e.g**/

var async = require('async');
async.series([
function (callback) {
console.log('First Execute..');
callback(null, 'userPersonalData');
},
function (callback) {
console.log('Second Execute.. ');
callback(null, 'userDependentData');
}
],
function (err, result) {
console.log(result);
});

//Output:

First Execute..
Second Execute..
['userPersonalData','userDependentData'] //result



#### Syntax


<li>
**Each callback must be written with this syntax:**
</li>
<li>
function callback(err, result [, arg1[, ...]])
</li>
<li>
**This way, you are forced to return the error first, and can't ignore handling them later on. `null` is the convention in absence of errors**
</li>
<li>
callback(null, myResult);
</li>
<li>
**Your callbacks can contain more arguments than **err** and **result**, but it's useful only for a specific set of functions (waterfall, seq, ...)**
</li>
<li>
callback(null, myResult, myCustomArgument);
</li>
<li>
**And, of course, send errors. You must do it, and handle errors (or at least log them).**
</li>
<li>
callback(err);
</li>

