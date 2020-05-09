---
metaTitle: "AngularJS - $http request"
description: "Using $http inside a controller, Using $http request in a service, Timing of an $http request"
---

# $http request



## Using $http inside a controller


The `$http` service is a function which generates an HTTP request and returns a promise.

**General Usage**

```js
// Simple GET request example:
$http({
  method: 'GET',
  url: '/someUrl'
}).then(function successCallback(response) {
    // this callback will be called asynchronously
    // when the response is available
  }, function errorCallback(response) {
    // called asynchronously if an error occurs
    // or server returns response with an error status.
  });

```

**Usage inside controller**

```js
appName.controller('controllerName',
    ['$http', function($http){

    // Simple GET request example:
    $http({
        method: 'GET',
        url: '/someUrl'
    }).then(function successCallback(response) {
        // this callback will be called asynchronously
        // when the response is available
    }, function errorCallback(response) {
        // called asynchronously if an error occurs
        // or server returns response with an error status.
    });
}])

```

**Shortcut Methods**

`$http` service also has shortcut methods. Read about [http methods here](https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html)

**Syntax**

```js
$http.get('/someUrl', config).then(successCallback, errorCallback);
$http.post('/someUrl', data, config).then(successCallback, errorCallback);

```

**Shortcut Methods**

- $http.get
- $http.head
- $http.post
- $http.put
- $http.delete
- $http.jsonp
- $http.patch



## Using $http request in a service


HTTP requests are widely used repeatedly across every web app, so it is wise to write a method for each common request, and then use it in multiple places throughout the app.

Create a `httpRequestsService.js`

**httpRequestsService.js**

```js
appName.service('httpRequestsService', function($q, $http){

    return {
        // function that performs a basic get request
        getName: function(){
            // make sure $http is injected
            return $http.get("/someAPI/names")
                .then(function(response) {
                    // return the result as a promise
                    return response;
                }, function(response) {
                    // defer the promise
                    return $q.reject(response.data);
                });
        },

        // add functions for other requests made by your app
        addName: function(){
            // some code...
        }
    }
})

```

The service above will perform a get request inside the service. This will be available to any controller where the service has been injected.

**Sample usage**

```js
appName.controller('controllerName',
    ['httpRequestsService', function(httpRequestsService){

        // we injected httpRequestsService service on this controller
        // that made the getName() function available to use.
        httpRequestsService.getName()
            .then(function(response){
                // success
            }, function(error){
                // do something with the error
            })
    }])

```

Using this approach we can now use **httpRequestsService.js** anytime and in any controller.



## Timing of an $http request


The $http requests require time which varies depending on the server, some may take a few milliseconds, and some may take up to a few seconds. Often the time required to retrieve the data from a request is critical. Assuming the response value is an array of names,
consider the following example:

**Incorrect**

```js
$scope.names = [];

$http({
    method: 'GET',
    url: '/someURL'
}).then(function successCallback(response) {
            $scope.names = response.data;
        },
        function errorCallback(response) {
            alert(response.status);
        });

alert("The first name is: " + $scope.names[0]);

```

Accessing `$scope.names[0]` right below the $http request will often throw an error - this line of code executes before the response is received from the server.

**Correct**

```js
$scope.names = [];

$scope.$watch('names', function(newVal, oldVal) {
    if(!(newVal.length == 0)) {
        alert("The first name is: " + $scope.names[0]);
    }
});

$http({
    method: 'GET',
    url: '/someURL'
}).then(function successCallback(response) {
            $scope.names = response.data;
        },
        function errorCallback(response) {
            alert(response.status);
        });

```

Using the [$watch](http://stackoverflow.com/documentation/angularjs/3156/digest-loop-walkthrough/10758/digest-and-watch#t=201610092006226200235) service we access the `$scope.names` array only when the response is received. During initialization, the function is called even though `$scope.names` was initialized before, therefore checking if the `newVal.length` is different than 0 is necessary. Be aware - any changes made to `$scope.names` will trigger the watch function.

