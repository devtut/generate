---
metaTitle: "AngularJS - Angular promises with $q service"
description: "Wrap simple value into a promise using $q.when(), Using angular promises with $q service, Using the $q constructor to create promises, Using $q.all to handle multiple promises, Avoid the $q Deferred Anti-Pattern, Deferring operations using $q.defer"
---

# Angular promises with $q service




## Wrap simple value into a promise using $q.when()


If all you need is to wrap the value into a promise, you don't need to use the long syntax like here:

```js
//OVERLY VERBOSE
var defer;
defer = $q.defer();
defer.resolve(['one', 'two']);
return defer.promise;

```

In this case you can just write:

```js
//BETTER
return $q.when(['one', 'two']);

```

> 
<h3>$q.when and its alias $q.resolve</h3>
Wraps an object that might be a value or a (3rd party) then-able promise into a $q promise. This is useful when you are dealing with an object that might or might not be a promise, or if the promise comes from a source that can't be trusted.
[— AngularJS $q Service API Reference - $q.when](https://docs.angularjs.org/api/ng/service/$q#when)


With the release of AngularJS v1.4.1

You can also use an ES6-consistent alias `resolve`

```js
//ABSOLUTELY THE SAME AS when
return $q.resolve(['one', 'two'])

```



## Using angular promises with $q service


`$q` is a  built-in service which helps in executing asynchronous functions and using their return values(or exception) when they are finished with processing.

`$q` is integrated with the `$rootScope.Scope` model observation mechanism, which means faster propagation of resolution or rejection into your models and avoiding unnecessary browser repaints, which would result in flickering UI.

In our example, we call our factory `getMyData`, which return a promise object. If the object is `resolved`, it returns a random number. If it is `rejected`, it return a rejection with an error message after 2 seconds.

In Angular factory

```js
function getMyData($timeout, $q) {
  return function() {
    // simulated async function
    var promise = $timeout(function() {
      if(Math.round(Math.random())) {
        return 'data received!'
      } else {
        return $q.reject('oh no an error! try again')
      }
    }, 2000);
    return promise;
  }
}

```

### Using Promises on call

```js
angular.module('app', [])
.factory('getMyData', getMyData)
.run(function(getData) {
  var promise = getData()
    .then(function(string) {
      console.log(string)
    }, function(error) {
      console.error(error)
    })
    .finally(function() {
      console.log('Finished at:', new Date())
    })
})

```

To use promises, inject `$q` as dependency. Here we injected `$q` in `getMyData` factory.

```js
var defer = $q.defer();

```

A new instance of deferred is constructed by calling `$q.defer()`

A deferred object is simply an object that exposes a promise as well as the associated methods for resolving that promise. It is constructed using the `$q.deferred()` function and exposes three main methods: `resolve()`, `reject()`, and `notify()`.

- `resolve(value)` – resolves the derived promise with the value.
- `reject(reason)` – rejects the derived promise with the reason.
- `notify(value)` - provides updates on the status of the promise's execution. This may be called multiple times before the promise is either resolved or rejected.

### Properties

The associated promise object is accessed via the promise property.
`promise` – {Promise} – promise object associated with this deferred.

A new promise instance is created when a deferred instance is created and can be retrieved by calling `deferred.promise`.

The purpose of the `promise` object is to allow for interested parties to get access to the result of the deferred task when it completes.

Promise Methods -

<li>
`then(successCallback, [errorCallback], [notifyCallback])` – Regardless of when the promise was or will be resolved or rejected, then calls one of the success or error callbacks asynchronously as soon as the result is available. The callbacks are called with a single argument: the result or rejection reason. Additionally, the notify callback may be called zero or more times to provide a progress indication, before the promise is resolved or rejected.
</li>
<li>
`catch(errorCallback)` – shorthand for promise.then(null, errorCallback)
</li>
<li>
`finally(callback, notifyCallback)` – allows you to observe either the fulfillment or rejection of a promise, but to do so without modifying the final value.
</li>

One of the most powerful features of promises is the ability to chain them together. This allows the data to flow through the chain and be manipulated and mutated at each step. This is demonstrated with the following example:

**Example 1:**

```js
// Creates a promise that when resolved, returns 4.
function getNumbers() {

  var promise = $timeout(function() {
    return 4;
  }, 1000);
  
  return promise;
}

// Resolve getNumbers() and chain subsequent then() calls to decrement
// initial number from 4 to 0 and then output a string.
getNumbers()
  .then(function(num) {
      // 4
      console.log(num);
      return --num;
  })
  .then(function (num) {
      // 3
      console.log(num);
      return --num;
  })
   .then(function (num) {
      // 2
      console.log(num);
      return --num;
  })
  .then(function (num) {
      // 1
      console.log(num);
      return --num;
  })
  .then(function (num) {
      // 0
      console.log(num);
      return 'And we are done!';
  })
  .then(function (text) {
      // "And we are done!"
      console.log(text);        
  });

```



## Using the $q constructor to create promises


The `$q` constructor function is used to create promises from asynchronous APIs that use callbacks to return results.

> 
$q(function(resolve, reject) {...})


The constructor function receives a function that is invoked with two arguments, `resolve` and `reject` that are functions which are used to either resolve or reject the promise.

**Example 1:**

```js
function $timeout(fn, delay) {
    return = $q(function(resolve, reject) {
         setTimeout(function() {
              try {
                 let r = fn();
                 resolve(r);
              } 
              catch (e) {
                 reject(e);
              }
         }, delay);
     };
}
    

```

The above example creates a promise from the [WindowTimers.setTimeout API](https://developer.mozilla.org/en-US/docs/Web/API/WindowTimers/setTimeout). The AngularJS framework provides a more elaborate version of this function. For usage, see the [AngularJS $timeout Service API Reference](https://docs.angularjs.org/api/ng/service/$timeout).

**Example 2:**

```js
$scope.divide = function(a, b) {
    return $q(function(resolve, reject) {
      if (b===0) {
        return reject("Cannot devide by 0")
      } else {
        return resolve(a/b);
      }
    });
}

```

The above code showing a promisified division function, it will return a promise with the result or reject with a reason if the calculation is impossible.

You can then call and use `.then`

```js
$scope.divide(7, 2).then(function(result) {
    // will return 3.5
}, function(err) {
    // will not run
})

$scope.divide(2, 0).then(function(result) {
    // will not run as the calculation will fail on a divide by 0
}, function(err) {
    // will return the error string.
})

```



## Using $q.all to handle multiple promises


You can use the `$q.all` function to call a `.then` method after an array of promises has been successfully resolved and fetch the data they resolved with.

Example:

**JS:**

```

$scope.data = []

 $q.all([
    $http.get("data.json"),
    $http.get("more-data.json"),
    ]).then(function(responses) {
      $scope.data = responses.map((resp) => resp.data);
    });

```

The above code runs `$http.get` 2 times for data in local json files, when both `get` method complete they resolve their associated promises, when all the promises in the array are resolved, the `.then` method starts with both promises data inside the `responses` array argument.

The data is then mapped so it could be shown on the template, we can then show

**HTML:**

```js
<ul>
   <li ng-repeat="d in data">
      <ul>
         <li ng-repeat="item in d">{ {item.name} }: { {item.occupation} }</li>
      </ul>
   </li>
</ul>

```

**JSON:**

```js
[{
  "name": "alice",
  "occupation": "manager"
}, {
  "name": "bob",
  "occupation": "developer"
}]

```



## Avoid the $q Deferred Anti-Pattern


> 
<h3>Avoid this Anti-Pattern</h3>

```js
var myDeferred = $q.defer();

$http(config).then(function(res) {  
   myDeferred.resolve(res);
}, function(error) {
   myDeferred.reject(error);
});

return myDeferred.promise;

```




There is no need to manufacture a promise with `$q.defer` as the $http service already returns a promise.

```js
//INSTEAD
return $http(config);

```

Simply return the promise created by the $http service.



## Deferring operations using $q.defer


We can use `$q` to defer operations to the future while having a pending promise object at the present, by using `$q.defer` we create a promise that will either resolve or reject in the future.

This method is not equivalent of using the `$q` constructor, as we use `$q.defer` to promisify an existing routine that may or may not return (or had ever returned) a promise at all.

**Example:**

```js
var runAnimation = function(animation, duration) {
    var deferred = $q.defer();
    try {
        ...
        // run some animation for a given duration
        deferred.resolve("done");
    } catch (err) {
        // in case of error we would want to run the error hander of .then
        deferred.reject(err);
    }
    return deferred.promise;
}

// and then
runAnimation.then(function(status) {}, function(error) {})


```


<li>
Be sure you always return a the `deferred.promise` object or risk an error when invoking `.then`
</li>
<li>
Make sure you always resolve or reject your deferred object or `.then` may not run and you risk a memory leak
</li>

