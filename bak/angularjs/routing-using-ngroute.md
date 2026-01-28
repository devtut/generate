---
metaTitle: "AngularJS - Routing using ngRoute"
description: "Basic example, Defining custom behavior for individual routes, Route parameters example"
---

# Routing using ngRoute



## Basic example


This example shows setting up a small application with 3 routes, each with it's own view and controller, using the `controllerAs` syntax.

We configure our router at the angular `.config` function

1. We inject `$routeProvider` into `.config`
1. We define our route names at the `.when` method with a route definition object.
1. We supply the `.when` method with an object specifying our `template` or `templateUrl`, `controller` and `controllerAs`

**app.js**

```js
angular.module('myApp', ['ngRoute'])
  .controller('controllerOne', function() {
    this.message = 'Hello world from Controller One!';
  })
  .controller('controllerTwo', function() {
    this.message = 'Hello world from Controller Two!';
  })
  .controller('controllerThree', function() {
    this.message = 'Hello world from Controller Three!';
  })
  .config(function($routeProvider) {
    $routeProvider
    .when('/one', {
      templateUrl: 'view-one.html',
      controller: 'controllerOne',
      controllerAs: 'ctrlOne'
    })
    .when('/two', {
      templateUrl: 'view-two.html',
      controller: 'controllerTwo',
      controllerAs: 'ctrlTwo'
    })
    .when('/three', {
      templateUrl: 'view-three.html',
      controller: 'controllerThree',
      controllerAs: 'ctrlThree'
    })
    // redirect to here if no other routes match
    .otherwise({
      redirectTo: '/one'
    });
  });

```

Then in our HTML we define our navigation using `<a>` elements with `href`, for a route name of `helloRoute` we will route as `<a href="#/helloRoute">My route</a>`

We also provide our view with a container and the directive `ng-view` to inject our routes.

**index.html**

```js
<div ng-app="myApp">
  <nav>
    <!-- links to switch routes -->
    <a href="#/one">View One</a>
    <a href="#/two">View Two</a>
    <a href="#/three">View Three</a>
  </nav>
  <!-- views will be injected here -->
  <div ng-view></div>
  <!-- templates can live in normal html files -->
  <script type="text/ng-template" id="view-one.html">
    <h1>{ {ctrlOne.message} }</h1>
  </script>

  <script type="text/ng-template" id="view-two.html">
    <h1>{ {ctrlTwo.message} }</h1>
  </script>

  <script type="text/ng-template" id="view-three.html">
    <h1>{ {ctrlThree.message} }</h1>
  </script>
</div>

```



## Defining custom behavior for individual routes


The simplest manner of defining custom behavior for individual routes would be fairly easy.

In this example we use it to authenticate a user :

**1) `routes.js`: create a new property (like `requireAuth`) for any desired route**

```js
angular.module('yourApp').config(['$routeProvider', function($routeProvider) {
    $routeProvider
        .when('/home', {
            templateUrl: 'templates/home.html',
            requireAuth: true
        })
        .when('/login', {
            templateUrl: 'templates/login.html',
        })
        .otherwise({
            redirectTo: '/home'
        });
}])

```

**2)  In a top-tier controller that isn't bound to an element inside the `ng-view` (to avoid conflict with angular `$routeProvider`), check if the `newUrl` has the `requireAuth` property and act accordingly**

```js
angular.module('YourApp').controller('YourController', ['$scope', 'session', '$location',
    function($scope, session, $location) {

        $scope.$on('$routeChangeStart', function(angularEvent, newUrl) {
            
            if (newUrl.requireAuth && !session.user) {
                // User isn’t authenticated
                $location.path("/login");
            }
            
        });
    }
]);

```



## Route parameters example


This example extends the basic example passing parameters in the route in order to use them in the controller

To do so we need to:

1. Configure the parameter position and name in the route name
1. Inject `$routeParams` service in our Controller

**app.js**

```js
angular.module('myApp', ['ngRoute'])
  .controller('controllerOne', function() {
    this.message = 'Hello world from Controller One!';
  })
  .controller('controllerTwo', function() {
    this.message = 'Hello world from Controller Two!';
  })
  .controller('controllerThree', ['$routeParams', function($routeParams) {
    var routeParam = $routeParams.paramName

    if ($routeParams.message) {
        // If a param called 'message' exists, we show it's value as the message
        this.message = $routeParams.message;
    } else {
        // If it doesn't exist, we show a default message
        this.message = 'Hello world from Controller Three!';
    }
  }])
  .config(function($routeProvider) {
    $routeProvider
    .when('/one', {
      templateUrl: 'view-one.html',
      controller: 'controllerOne',
      controllerAs: 'ctrlOne'
    })
    .when('/two', {
      templateUrl: 'view-two.html',
      controller: 'controllerTwo',
      controllerAs: 'ctrlTwo'
    })
    .when('/three', {
      templateUrl: 'view-three.html',
      controller: 'controllerThree',
      controllerAs: 'ctrlThree'
    })
    .when('/three/:message', { // We will pass a param called 'message' with this route
      templateUrl: 'view-three.html',
      controller: 'controllerThree',
      controllerAs: 'ctrlThree'
    })
    // redirect to here if no other routes match
    .otherwise({
      redirectTo: '/one'
    });
  });

```

Then, withoud making any changes in our templates, only adding a new link with custom message, we can see the new custom message in our view.

**index.html**

```js
<div ng-app="myApp">
  <nav>
    <!-- links to switch routes -->
    <a href="#/one">View One</a>
    <a href="#/two">View Two</a>
    <a href="#/three">View Three</a>
    <!-- New link with custom message -->
    <a href="#/three/This-is-a-message">View Three with "This-is-a-message" custom message</a>
  </nav>
  <!-- views will be injected here -->
  <div ng-view></div>
  <!-- templates can live in normal html files -->
  <script type="text/ng-template" id="view-one.html">
    <h1>{ {ctrlOne.message} }</h1>
  </script>

  <script type="text/ng-template" id="view-two.html">
    <h1>{ {ctrlTwo.message} }</h1>
  </script>

  <script type="text/ng-template" id="view-three.html">
    <h1>{ {ctrlThree.message} }</h1>
  </script>
</div>

```



#### Remarks


The `ngRoute` is a build-in module provides routing and deeplinking services and directives for angular apps.

Full documentation about `ngRoute` is avalable on  [https://docs.angularjs.org/api/ngRoute](https://docs.angularjs.org/api/ngRoute)

