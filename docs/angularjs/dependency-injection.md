---
metaTitle: "AngularJS - Dependency Injection"
description: "Dynamic Injections, Dynamically load AngularJS service in vanilla JavaScript, Injections, $inject Property Annotation"
---

# Dependency Injection




## Dynamic Injections


There is also an option to dynamically request components. You can do it using the `$injector` service:

```js
myModule.controller('myController', ['$injector', function($injector) {
    var myService = $injector.get('myService');
}]);

```

Note: while this method could be used to prevent the circular dependency issue that might break your app, it is not considered best practice to bypass the problem by using it. Circular dependency usually indicates there is a flaw in your application's architecture, and you should address that instead.



## Dynamically load AngularJS service in vanilla JavaScript


You can load AngularJS services in vanilla JavaScript using AngularJS `injector()` method.
Every jqLite element retrieved calling `angular.element()` has a method `injector()` that can be used to retrieve the injector.

```js
var service;
var serviceName = 'myService';

var ngAppElement = angular.element(document.querySelector('[ng-app],[data-ng-app]') || document);
var injector = ngAppElement.injector();

if(injector && injector.has(serviceNameToInject)) {
    service = injector.get(serviceNameToInject);
}

```

In the above example we try to retrieve the jqLite element containing the root of the AngularJS application (`ngAppElement`). To do that, we use `angular.element()` method, searching for a DOM element containing `ng-app` or `data-ng-app` attribute or, if it does not exists, we fall back to `document` element.
We use `ngAppElement` to retrieve injector instance (with `ngAppElement.injector()`). The injector instance is used to check if the service to inject exists (with `injector.has()`) and then to load the service (with `injector.get()`) inside `service` variable.



## Injections


The simplest example of an injection in an Angular app - injecting `$scope` to an Angular `Controller`:

```js
angular.module('myModule', [])
.controller('myController', ['$scope', function($scope) {
    $scope.members = ['Alice', 'Bob'];
    ...
}])

```

The above illustrates an injection of a `$scope` into a `controller`, but it is the same whether you inject any module into any other. The process is the same.

Angular's system is in charge of resolving dependencies for you. If you create a service for instance, you can list it like in the example above and it will be available for you.

You can use DI - Dependency Injection - wherever you are defining a component.

Note that in the above example we use what is called "Inline Array Annotation". Meaning, we explicitly write as strings the names of our dependencies. We do it to prevent the application from breaking when the code is minified for Production. Code minification changes the names of the variables (but not strings), which breaks the injection. By using strings, Angular knows which dependencies we want.

**Very important - the order of string names must be the same as the parameters in the function.**

There are tools that automate this process and take care of this for you.



## $inject Property Annotation


Equivalently, we can use the `$inject` property annotation to achieve the same as above:

```js
var MyController = function($scope) {
  // ...
}
MyController.$inject = ['$scope'];
myModule.controller('MyController', MyController);

```



#### Syntax


<li>
myApp.controller('MyController', function($scope) { ... }); // non-minified code
</li>
<li>
myApp.controller('MyController', ['$scope', function($scope) { ... }]); //support minification
</li>
<li>
function MyController(){}
MyController.$inject = ['$scope'];
myApp.controller('MyController', MyController); //$inject annotation
</li>
<li>
$injector.get('injectable'); //dynamic/runtime injection
</li>



#### Remarks


Providers cannot be injected into `run` blocks.

Services or Values cannot be injected into `config` blocks.

Make sure to annotate your injections so your code will not break on minification.

