---
metaTitle: "AngularJS - Controllers"
description: "Your First Controller, Creating Controllers, Minification safe, Using ControllerAs in Angular JS, Creating Minification-Safe Angular Controllers, Creating Controllers, Nested Controllers"
---

# Controllers

## Your First Controller

A controller is a basic structure used in Angular to preserve scope and handle certain actions within a page. Each controller is coupled with an HTML view.

Below is a basic boilerplate for an Angular app:

```js
<!DOCTYPE html>

<html lang="en" ng-app='MyFirstApp'>
    <head>
        <title>My First App</title>

        <!-- angular source -->
        <script src="https://code.angularjs.org/1.5.3/angular.min.js"></script>

        <!-- Your custom controller code -->
        <script src="js/controllers.js"></script>
    </head>
    <body>
        <div ng-controller="MyController as mc">
            <h1>{ { mc.title } }</h1>
            <p>{ { mc.description } }</p>
            <button ng-click="mc.clicked()">
                Click Me!
            </button>
        </div>
    </body>
</html>

```

There are a few things to note here:

```js
<html ng-app='MyFirstApp'>

```

Setting the app name with `ng-app` lets you access the application in an external Javascript file, which will be covered below.

```js
<script src="js/controllers.js"></script>
```

We'll need a Javascript file where you define your controllers and their actions/data.

```js
<div ng-controller="MyController as mc">

```

The `ng-controller` attribute sets the controller for that DOM element and all elements that are children (recursively) below it.

You can have multiple of the same controller (in this case, `MyController`) by saying `... as mc`, we're giving this instance of the controller an alias.

```js
<h1>{ { mc.title } }</h1>

```

The `{ { ... } }` notation is an Angular expression. In this case, this will set the inner text of that `<h1>` element to whatever the value of `mc.title` is.

**Note:** Angular employs dual-way data binding, meaning that regardless of how you update the `mc.title` value, it will be reflected in both the controller and the page.

Also note that Angular expressions do **not** have to reference a controller. An Angular expression can be as simple as `{ { 1 + 2 } }` or `{ { "Hello " + "World" } }`.

```js
<button ng-click="mc.clicked()">

```

`ng-click` is an Angular directive, in this case binding the click event for the button to trigger the `clicked()` function of the `MyController` instance.

With those things in mind, let's write an implementation of the `MyController` controller. With the example above, you would write this code in `js/controller.js`.

First, you'll need to instantiate the Angular app in your Javascript.

```js
var app = angular.module("MyFirstApp", []);
```

Note that the name we pass here is the same as the name you set in your HTML with the `ng-app` directive.

Now that we have the app object, we can use that to create controllers.

```js
app.controller("MyController", function() {
  var ctrl = this;

  ctrl.title = "My First Angular App";
  ctrl.description = "This is my first Angular app!";

  ctrl.clicked = function() {
    alert("MyController.clicked()");
  };
});
```

**Note:** For anything that we want to be a part of the controller instance, we use the `this` keyword.

This is all that is required to build a simple controller.

## Creating Controllers, Minification safe

There are a couple different ways to protect your controller creation from minification.

The first is called inline array annotation. It looks like the following:

```js
var app = angular.module("app");
app.controller("sampleController", [
  "$scope",
  "$http",
  function(a, b) {
    //logic here
  },
]);
```

The second parameter of the controller method can accept an array of dependencies. As you can see I've defined `$scope` and `$http` which should correspond to the parameters of the controller function in which `a` will be the `$scope`, and `b` would be `$http`. Take note that the last item in the array should be your controller function.

The second option is using the `$inject` property. It looks like the following:

```js
var app = angular.module("app");
app.controller("sampleController", sampleController);
sampleController.$inject = ["$scope", "$http"];
function sampleController(a, b) {
  //logic here
}
```

This does the same thing as inline array annotation but provides a different styling for those that prefer one option over the other.

### **The order of injected dependencies is important**

When injecting dependencies using the array form, be sure that the list of the dependencies match its corresponding list of arguments passed to the controller function.

Note that in the following example, `$scope` and `$http` are reversed. This will cause a problem in the code.

```js
// Intentional Bug: injected dependencies are reversed which will cause a problem
app.controller("sampleController", [
  "$scope",
  "$http",
  function($http, $scope) {
    $http.get("sample.json");
  },
]);
```

## Using ControllerAs in Angular JS

In Angular `$scope` is the glue between the Controller and the View that helps with all of our data binding needs. Controller As is another way of binding controller and view and is mostly recommended to use. Basically these are the two controller constructs in Angular (i.e \$scope and Controller As).

Different ways of using Controller As are -

**controllerAs View Syntax**

```js
<div ng-controller="CustomerController as customer">
    { { customer.name } }
</div>

```

**controllerAs Controller Syntax**

```js
function CustomerController() {
  this.name = {};
  this.sendMessage = function() {};
}
```

**controllerAs with vm**

```js
function CustomerController() {
  /*jshint validthis: true */
  var vm = this;
  vm.name = {};
  vm.sendMessage = function() {};
}
```

`controllerAs` is syntactic sugar over `$scope`. You can still bind to the View and still access `$scope` methods. Using `controllerAs`, is one of the best practices suggested by the angular core team. There are many reason for this, few of them are -

<li>
`$scope` is exposing the members from the controller to the view via an intermediary object. By setting `this.*`, we can expose just what we want to expose from the controller to the view. It also follow the standard JavaScript way of using this.
</li>
<li>
using `controllerAs` syntax, we have more readable code and the parent property can be accessed using the alias name of the parent controller instead of using the `$parent` syntax.
</li>
<li>
It promotes the use of binding to a "dotted" object in the View (e.g. customer.name instead of name), which is more contextual, easier to read, and avoids any reference issues that may occur without "dotting".
</li>
<li>
Helps avoid using `$parent` calls in Views with nested controllers.
</li>
<li>
Use a capture variable for this when using the `controllerAs` syntax. Choose a consistent variable name such as `vm`, which stands for ViewModel. Because, `this` keyword is contextual and when used within a function inside a controller may change its context. Capturing the context of this avoids encountering this problem.
</li>

**NOTE:** using `controllerAs` syntax add to current scope reference to current controller, so it available as field

```js
<div ng-controller="Controller as vm>...</div>

```

`vm` is available as `$scope.vm`.

## Creating Minification-Safe Angular Controllers

To create minification-safe angular controllers, you will change the `controller` function parameters.

The second argument in the `module.controller` function should be passed an **array**, where the **last parameter** is the **controller function**, and every parameter before that is the **name** of each injected value.

This is different from the normal paradigm; that takes the **controller function** with the injected arguments.

Given:

```js
var app = angular.module("myApp");
```

The controller should look like this:

```js
app.controller("ctrlInject", [
  /* Injected Parameters */
  "$Injectable1",
  "$Injectable2",
  /* Controller Function */
  function($injectable1Instance, $injectable2Instance) {
    /* Controller Content */
  },
]);
```

Note: **The names of injected parameters are not required to match, but they will be bound in order.**

This will minify to something similar to this:

```js
var a = angular.module("myApp");
a.controller("ctrlInject", [
  "$Injectable1",
  "$Injectable2",
  function(b, c) {
    /* Controller Content */
  },
]);
```

The minification process will replace every instance of `app` with `a`, every instance of `$Injectable1Instance` with `b`, and every instance of `$Injectable2Instance` with `c`.

## Creating Controllers

```js
angular.module("app").controller("SampleController", SampleController);

SampleController.$inject = ["$log", "$scope"];
function SampleController($log, $scope) {
  $log.debug("*****SampleController******");

  /* Your code below */
}
```

Note: The `.$inject` will make sure your dependencies doesn't get scrambled after minification. Also, make sure it's in order with the named function.

## Nested Controllers

Nesting controllers chains the `$scope` as well. Changing a `$scope` variable in the nested controller changes the same `$scope` variable in the parent controller.

```js
.controller('parentController', function ($scope) {
    $scope.parentVariable = "I'm the parent";
});

.controller('childController', function ($scope) {
    $scope.childVariable = "I'm the child";

    $scope.childFunction = function () {
        $scope.parentVariable = "I'm overriding you";
    };
});

```

Now let's try to handle both of them, nested.

```js
<body ng-controller="parentController">
  What controller am I? {{ parentVariable }}
  <div ng-controller="childController">
    What controller am I? {{ childVariable }}
    <button ng-click="childFunction()"> Click me to override! </button>
  </div>
</body>
```

Nesting controllers may have it's benefits, but one thing must be kept in mind when doing so. Calling the `ngController` directive creates a new instance of the controller - which can often create confusion and unexpected results.

#### Syntax

- `<htmlElement ng-controller="controllerName"> ... </htmlElement>`
- `app.controller('controllerName', controllerFunction);`
