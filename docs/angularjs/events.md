---
metaTitle: "AngularJS - Events"
description: "Using angular event system, Always deregister $rootScope.$on listeners on the scope $destory event, Uses and significance"
---

# Events



## Using angular event system


### $scope.$emit

Using `$scope.$emit` will fire an event name upwards through the scope hierarchy and notify to the `$scope`.The event life cycle starts at the scope on which `$emit` was called.

**Working wireframe :**

[<img src="http://i.stack.imgur.com/Xa0aM.png" alt="enter image description here" />](http://i.stack.imgur.com/Xa0aM.png)

### $scope.$broadcast

Using `$scope.$broadcast` will fire an event down the `$scope`. We can listen of these events using `$scope.$on`

**Working wireframe :**

[<img src="http://i.stack.imgur.com/Yd6vf.png" alt="enter image description here" />](http://i.stack.imgur.com/Yd6vf.png)

### Syntax :

```js
// firing an event upwards
$scope.$emit('myCustomEvent', 'Data to send');

// firing an event downwards
$scope.$broadcast('myCustomEvent', {
  someProp: 'some value'
});

// listen for the event in the relevant $scope
$scope.$on('myCustomEvent', function (event, data) {
  console.log(data); // 'Data from the event'
});

```

Instead of `$scope` you can use `$rootScope`, in that case your event will be available in all the controllers regardless of that controllers scope

### Clean registered event in AngularJS

The reason to clean the registered events because even the controller has been destroyed the handling of registered event are still alive. So the code will run as unexpected for sure.

```js
// firing an event upwards
$rootScope.$emit('myEvent', 'Data to send');

// listening an event
var listenerEventHandler = $rootScope.$on('myEvent', function(){
    //handle code
});

$scope.$on('$destroy', function() {
    listenerEventHandler();
});

```



## Always deregister $rootScope.$on listeners on the scope $destory event


$rootScope.$on listeners will remain in memory if you navigate to another controller. This will create a memory leak if the controller falls out of scope.

**Don't**

```js
angular.module('app').controller('badExampleController', badExample);

badExample.$inject = ['$scope', '$rootScope'];
function badExample($scope, $rootScope) {

    $rootScope.$on('post:created', function postCreated(event, data) {});

}

```

**Do**

```js
angular.module('app').controller('goodExampleController', goodExample);

goodExample.$inject = ['$scope', '$rootScope'];
function goodExample($scope, $rootScope) {

    var deregister = $rootScope.$on('post:created', function postCreated(event, data) {});

    $scope.$on('$destroy', function destroyScope() {
        deregister();
    });

}

```



## Uses and significance


These events can be used to communicate between 2 or more controllers.

`$emit` dispatches an event upwards through the scope hierarchy, while `$broadcast` dispatches an event downwards to all child scopes.This has been beautifully explained [here](http://stackoverflow.com/questions/26752030/rootscope-broadcast-vs-scope-emit/#answer-28156845).

There can be basically two types of scenario while communicating among controllers:

1. When controllers have Parent-Child relationship. (we can mostly use `$scope` in such scenarios)

1. When controllers are not independent to each other and are needed to be informed about each others activity. (we can use `$rootScope` in such scenarios)

**eg:** For any ecommerce website, suppose we have `ProductListController`(which controls the product listing page when any product brand is clicked ) and `CartController` (to manage cart items) . Now, when we click on **Add to Cart** button , it has to be informed to `CartController` as well, so that it can reflect new cart item count/details in the navigation bar of the website. This can be achieved using `$rootScope`.

With `$scope.$emit`

```js
<html>
  <head>
    <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.4.4/angular.js"></script>
    <script>
        var app = angular.module('app', []);

        app.controller("FirstController", function ($scope) {
            $scope.$on('eventName', function (event, args) {
                $scope.message = args.message;
            });
        });

        app.controller("SecondController", function ($scope) {
            $scope.handleClick = function (msg) {
                $scope.$emit('eventName', {message: msg});
            };
        });

    </script>
  </head>
  <body ng-app="app">
    <div ng-controller="FirstController" style="border:2px ;padding:5px;">
        <h1>Parent Controller</h1>
        <p>Emit Message : { {message} }</p>
        <br />
        <div ng-controller="SecondController" style="border:2px;padding:5px;">
            <h1>Child Controller</h1>
            <input ng-model="msg">
            <button ng-click="handleClick(msg);">Emit</button>
        </div>
    </div>
  </body>
</html>

```

With `$scope.$broadcast`:

```js
<html>
  <head>
    <title>Broadcasting</title>
    <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.4.4/angular.js"></script>
    <script>
        var app = angular.module('app', []);

        app.controller("FirstController", function ($scope) {
            $scope.handleClick = function (msg) {
                $scope.$broadcast('eventName', {message: msg});
            };

        });

        app.controller("SecondController", function ($scope) {
            $scope.$on('eventName', function (event, args) {
                $scope.message = args.message;
            });
        });

    </script>
  </head>
  <body ng-app="app">
    <div ng-controller="FirstController" style="border:2px solid ; padding:5px;">
        <h1>Parent Controller</h1>
        <input ng-model="msg">
        <button ng-click="handleClick(msg);">Broadcast</button>
        <br /><br />
        <div ng-controller="SecondController" style="border:2px solid ;padding:5px;">
            <h1>Child Controller</h1>
            <p>Broadcast Message : { {message} }</p>
        </div>
    </div>
  </body>
</html>

```



#### Parameters


|Parameters|Values types
|---|---|---|---|---|---|---|---|---|---
|event|Object {name: "eventName", targetScope: Scope, defaultPrevented: false, currentScope: ChildScope}
|args|data that has been passed along with event execution

