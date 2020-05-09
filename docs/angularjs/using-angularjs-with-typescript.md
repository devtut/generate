---
metaTitle: "AngularJS - Using AngularJS with TypeScript"
description: "Using Bundling / Minification, Angular Controllers in Typescript, Using the Controller with ControllerAs Syntax, Why ControllerAs Syntax ?"
---

# Using AngularJS with TypeScript



## Using Bundling / Minification


The way the $scope is injected in the controller's constructor functions is a way to demonstrate and use the basic option of [angular dependency injection](https://docs.angularjs.org/guide/di)  but is not production ready as it cannot be minified.
Thats because the minification system changes the variable names and anguar's dependency injection uses the parameter names to know what has to be injected. So for an example the ExampleController's constructor function is minified to the following code.

```js
function n(n){this.setUpWatches(n)

```

and `$scope` is changed to `n`!<br>
to overcome this we can add an $inject array(`string[]`). So that angular's DI knows what to inject at what position is the controllers constructor function.<br>
So the above typescript changes to

```js
module App.Controllers {
    class Address {
        line1: string;
        line2: string;
        city: string;
        state: string;
    }
    export class SampleController {
        firstName: string;
        lastName: string;
        age: number;
        address: Address;
        setUpWatches($scope: ng.IScope): void {
            $scope.$watch(() => this.firstName, (n, o) => {
                //n is string and so is o
            });
        };
        static $inject : string[] = ['$scope'];
        constructor($scope: ng.IScope) {
            this.setUpWatches($scope);
        }
    }
}

```



## Angular Controllers in Typescript


As defined in the AngularJS [Documentation](https://docs.angularjs.org/guide/controller)

> 
<p>When a Controller is attached to the DOM via the ng-controller
directive, Angular will instantiate a new Controller object, using the
specified Controller's constructor function. A new child scope will be
created and made available as an injectable parameter to the
Controller's constructor function as $scope.</p>


Controllers can be very easily made using the typescript classes.

```js
module App.Controllers {
    class Address {
        line1: string;
        line2: string;
        city: string;
        state: string;
    }
    export class SampleController {
        firstName: string;
        lastName: string;
        age: number;
        address: Address;
        setUpWatches($scope: ng.IScope): void {
            $scope.$watch(() => this.firstName, (n, o) => {
                //n is string and so is o
            });
        };
        constructor($scope: ng.IScope) {
            this.setUpWatches($scope);
        }
    }
}

```

The Resulting Javascript is

```js
var App;
(function (App) {
    var Controllers;
    (function (Controllers) {
        var Address = (function () {
            function Address() {
            }
            return Address;
        }());
        var SampleController = (function () {
            function SampleController($scope) {
                this.setUpWatches($scope);
            }
            SampleController.prototype.setUpWatches = function ($scope) {
                var _this = this;
                $scope.$watch(function () { return _this.firstName; }, function (n, o) {
                    //n is string and so is o
                });
            };
            ;
            return SampleController;
        }());
        Controllers.SampleController = SampleController;
    })(Controllers = App.Controllers || (App.Controllers = {}));
})(App || (App = {}));
//# sourceMappingURL=ExampleController.js.map

```

After making the controller class let the angular js module about the controller can be done simple by using the class

```js
app
 .module('app')
 .controller('exampleController', App.Controller.SampleController)

```



## Using the Controller with ControllerAs Syntax


The Controller we have made can be instantiated and used using `controller as` Syntax. That's because we have put variable directly on the controller class and not on the `$scope`.

Using `controller as someName` is to seperate the controller from `$scope` itself.So, there is no need of injecting $scope as the dependency in the controller.

**Traditional way :**

```js
// we are using $scope object.
app.controller('MyCtrl', function ($scope) {
  $scope.name = 'John';
});

<div ng-controller="MyCtrl">
  { {name} }
</div>

```

**Now, with** **`controller as`** **Syntax** **:**

```js
// we are using the "this" Object instead of "$scope"
app.controller('MyCtrl', function() {
  this.name = 'John';
});

<div ng-controller="MyCtrl as info">
  { {info.name} }
</div>

```

**If you instantiate a "class" in JavaScript, you might do this :**

```js
var jsClass = function () {
  this.name = 'John';
}
var jsObj = new jsClass();

```

So, now we can use **`jsObj`** instance to access any method or property of **`jsClass`**.

In angular, we do same type of thing.we use controller as syntax for instantiation.



## Why ControllerAs Syntax ?


### Controller Function

Controller function is nothing but just a JavaScript constructor function. Hence, when a view loads the `function context`(**`this`**) is set to the controller object.

**Case 1 :**

```js
this.constFunction = function() { ... }

```

It is created in the `controller object`, not on `$scope`. views can not access the functions defined on controller object.

**Example :**

```js
<a href="#123" ng-click="constFunction()"></a> // It will not work

```

**Case 2 :**

```js
$scope.scopeFunction = function() { ... }

```

It is created in the `$scope object`, not on `controller object`. views can only access the functions defined on `$scope` object.

**Example :**

```js
<a href="#123" ng-click="scopeFunction()"></a> // It will work

```

### Why ControllerAs ?

<li>
**`ControllerAs`** syntax makes it much clearer where objects are being manipulated.Having `oneCtrl.name` and `anotherCtrl.name` makes it much easier to identify that you have an `name` assigned by multiple different controllers for different purposes but if both used same `$scope.name` and having two different HTML elements on a page which both are bound to  **`{ {name} }`** then it is difficult to identify which one is from which controller.
</li>
<li>
Hiding the `$scope` and exposing the members from the controller to the view via an `intermediary object`. By setting `this.*`, we can expose just what we want to expose from the controller to the view.

```js
  <div ng-controller="FirstCtrl">
      { { name } }
      <div ng-controller="SecondCtrl">
          { { name } }
          <div ng-controller="ThirdCtrl">
              { { name } }
          </div>
      </div>
  </div>

```


</li>

Here, in above case `{ { name } }` will be very confusing to use and We also don't know which one related to which controller.

```js
<div ng-controller="FirstCtrl as first">
    { { first.name } }
    <div ng-controller="SecondCtrl as second">
        { { second.name } }
        <div ng-controller="ThirdCtrl as third">
            { { third.name } }
        </div>
    </div>
</div>

```

### Why $scope ?

- Use `$scope` when you need to access one or more methods of $scope such as `$watch`, `$digest`, `$emit`, `$http` etc.
- limit which properties and/or methods are exposed to `$scope`, then explicitly passing them to `$scope` as needed.



#### Syntax


<li>[$scope : ng.IScope](http://DefinitelyTyped/angularjs/angular.d.ts) - this is way in typescript to define type
for a particular variable.</li>

