---
metaTitle: "AngularJS - Services"
description: "Creating a service using angular.factory, How to create a Service, How to use a service, $sce - sanitize and render content and resources in templates, Difference between Service and Factory, How to create a Service with dependencies using 'array syntax', Registering a Service"
---

# Services



## Creating a service using angular.factory


First define the service (in this case it uses the factory pattern):

```js
.factory('dataService', function() {
    var dataObject = {};
    var service = {
        // define the getter method
        get data() {
            return dataObject;
        },
        // define the setter method
        set data(value) {
            dataObject = value || {};
        }
    };
    // return the "service" object to expose the getter/setter
    return service;
})

```

Now you can use the service to share data between controllers:

```js
.controller('controllerOne', function(dataService) {
    // create a local reference to the dataService
    this.dataService = dataService;
    // create an object to store
    var someObject = {
        name: 'SomeObject',
        value: 1
    };
    // store the object
    this.dataService.data = someObject;
})

.controller('controllerTwo', function(dataService) {
    // create a local reference to the dataService
    this.dataService = dataService;
    // this will automatically update with any changes to the shared data object
    this.objectFromControllerOne = this.dataService.data;
})

```



## How to create a Service


```js
angular.module("app")
  .service("counterService", function(){

    var service = {
        number: 0
    };
    
    return service;
   });

```



## How to use a service


```

   angular.module("app")
        
        // Custom services are injected just like Angular's built-in services
        .controller("step1Controller", ['counterService', '$scope', function(counterService, $scope) {
            counterService.number++;
            // bind to object (by reference), not to value, for automatic sync
            $scope.counter = counterService;
        })

```

In the template using this controller you'd then write:

```js
// editable
<input ng-model="counter.number" />

```

or

```js
// read-only
<span ng-bind="counter.number"></span>

```

Of course, in real code you would interact with the service using methods on the controller, which  in turn delegate to the service. The example above simply increments the counter value each time the controller is used in a template.

Services in Angularjs are singletons:

Services are singleton objects that are instantiated only once per app (by the $injector) and lazy loaded (created only when necessary).

> 
<p>A singleton is a class which only allows one instance of itself to be
created - and gives simple, easy access to said instance.
[As stated here](http://stackoverflow.com/questions/2155688/what-is-a-singleton-in-c)</p>




## $sce - sanitize and render content and resources in templates


> 
$sce [("Strict Contextual Escaping")](https://docs.angularjs.org/api/ng/service/$sce) is a built-in angular service that automatically sanitize content and internal sources in templates.


injecting **external** sources and **raw HTML**  into the template requires manual wrapping of`$sce`.

**In this example we'll create a simple $sce sanitation filter :**`.

[Demo](http://plnkr.co/edit/9tpXY7RF3QWN4eIXPdEU?p=preview)

```js
.filter('sanitizer', ['$sce', [function($sce) {
     return function(content) {
          return $sce.trustAsResourceUrl(content);
      };
}]);

```

**Usage in template**

```js
<div ng-repeat="item in items">
    
    // Sanitize external sources
    <ifrmae ng-src="{ {item.youtube_url | sanitizer} }">
    
    // Sanitaize and render HTML 
    <div ng-bind-html="{ {item.raw_html_content| sanitizer} }"></div>

</div>

```



## Difference between Service and Factory


**1) Services**

A service is a `constructor` function that is invoked once at runtime with `new`, just like what we would do with plain javascript with only
difference that `AngularJs` is calling the `new` behind the scenes.

There is one thumb rule to remember in case of services

1. Services are constructors which are called with `new`

Lets see a simple example where we would register a service which uses `$http` service to fetch student details, and use it in the controller

```js
function StudentDetailsService($http) {
  this.getStudentDetails = function getStudentDetails() {
    return $http.get('/details');
  };
}

angular.module('myapp').service('StudentDetailsService', StudentDetailsService);

```

We just inject this service into the controller

```js
function StudentController(StudentDetailsService) {
  StudentDetailsService.getStudentDetails().then(function (response) {
      // handle response
    });
}
angular.module('app').controller('StudentController', StudentController);

```

When to use?

Use `.service()` wherever you want to use a constructor. It is usually used to create public API's just like `getStudentDetails()`. But if you
don't want to use a constructor and wish to use a simple API pattern instead, then there isn't much flexibility in `.service()`.

**2) Factory**

Even though we can achieve all the things using `.factory()` which we would, using `.services()`, it doesn't make `.factory()` "same as" `.service()`.
It is much more powerful and flexible than `.service()`

A `.factory()` is a design pattern which is used to return a value.

There are two thumb rules to remember in case of factories

1. Factories return values
<li>Factories (can) create objects (Any
object)</li>

Lets see some examples on what we can do using `.factory()`

****Returning Objects Literals****

Lets see an example where factory is used to return an object using a basic Revealing module pattern

```js
function StudentDetailsService($http) {
  function getStudentDetails() {
    return $http.get('/details');
  }
  return {
    getStudentDetails: getStudentDetails
  };
}

angular.module('myapp').factory('StudentDetailsService', StudentDetailsService);

```

Usage inside a controller

```js
function StudentController(StudentDetailsService) {
  StudentDetailsService.getStudentDetails().then(function (response) {
      // handle response
    });
}
angular.module('app').controller('StudentController', StudentController);

```

****Returning Closures****

**What is a closure?**

Closures are functions that refer to variables that are used locally, BUT defined in an enclosing scope.

Following is an example of a closure

```js
function closureFunction(name) {
  function innerClosureFunction(age) { // innerClosureFunction() is the inner function, a closure
    // Here you can manipulate 'age' AND 'name' variables both
  };
};

```

The **"wonderful"** part is that it can access the `name` which is in the parent scope.

Lets use the above closure example inside `.factory()`

```js
function StudentDetailsService($http) {
  function closureFunction(name) {
  function innerClosureFunction(age) {
    // Here you can manipulate 'age' AND 'name' variables
    };
  };
};

angular.module('myapp').factory('StudentDetailsService', StudentDetailsService);

```

Usage inside a controller

```js
function StudentController(StudentDetailsService) {
  var myClosure = StudentDetailsService('Student Name'); // This now HAS the innerClosureFunction()
  var callMyClosure = myClosure(24); // This calls the innerClosureFunction()
};

angular.module('app').controller('StudentController', StudentController);

```

****Creating Constructors/instances****

`.service()` creates constructors with a call to `new` as seen above. `.factory()` can also create constructors with a call to `new`

Lets see an example on how to achieve this

```js
function StudentDetailsService($http) {
  function Student() {
    this.age = function () {
        return 'This is my age';
    };
  }
  Student.prototype.address = function () {
        return 'This is my address';
  };
  return Student;
};

angular.module('myapp').factory('StudentDetailsService', StudentDetailsService);

```

Usage inside a controller

```js
function StudentController(StudentDetailsService) {
  var newStudent = new StudentDetailsService();
  
  //Now the instance has been created. Its properties can be accessed.

 newStudent.age();
 newStudent.address();
  
};

angular.module('app').controller('StudentController', StudentController);

```



## How to create a Service with dependencies using 'array syntax'


```js
angular.module("app")
  .service("counterService", ["fooService", "barService", function(anotherService, barService){

    var service = {
        number: 0,
        foo: function () {
            return fooService.bazMethod(); // Use of 'fooService'
        },
        bar: function () {
            return barService.bazMethod(); // Use of 'barService'
        }
    };
    
    return service;
   }]);

```



## Registering a Service


The most common and flexible way to create a service uses the angular.module API factory:

```js
angular.module('myApp.services', []).factory('githubService', function() {       
   var serviceInstance = {};
   // Our first service
   return serviceInstance;
});

```

The service factory function can be either a function or an array, just like the way we create
controllers:

```js
// Creating the factory through using the
// bracket notation
angular.module('myApp.services', [])
.factory('githubService', [function($http) {
}]);

```

To expose a method on our service, we can place it as an attribute on the service object.

```js
angular.module('myApp.services', [])
    .factory('githubService', function($http) {
        var githubUrl = 'https://api.github.com';
        var runUserRequest = function(username, path) {
        // Return the promise from the $http service
        // that calls the Github API using JSONP
        return $http({
            method: 'JSONP',
            url: githubUrl + '/users/' +
            username + '/' +
            path + '?callback=JSON_CALLBACK'
        });
    }
// Return the service object with a single function
// events
return {
    events: function(username) {
    return runUserRequest(username, 'events');
    }
};

```

