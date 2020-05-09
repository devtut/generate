---
metaTitle: "AngularJS - Getting started with AngularJS"
description: "Getting Started, Showcasing all common Angular constructs, The importance of scope, Minification in Angular, AngularJS Getting Started Video Tutorials, The Simplest Possible Angular Hello World."
---

# Getting started with AngularJS



## Getting Started


Create a new HTML file and paste the following content:

```js
<!DOCTYPE html>
<html ng-app>
<head>
  <title>Hello, Angular</title>
  <script src="https://code.angularjs.org/1.5.8/angular.min.js"></script>
</head>
<body ng-init="name='World'">
  <label>Name</label>
  <input ng-model="name" />
  <span>Hello, { { name } }!</span>
  <p ng-bind="name"></p>
</body>
</html>

```

[Live demo](http://jsfiddle.net/U3pVM/26397/)

When you open the file with a browser, you will see an input field followed by the text `Hello, World!`. Editing the value in the input will update the text in real-time, without the need to refresh the whole page.

Explanation:

<li>
Load the Angular framework from a Content Delivery Network.

```js
<script src="https://code.angularjs.org/1.5.8/angular.min.js"></script>

```


</li>
<li>
Define the HTML document as an Angular application with the `ng-app` directive

```js
<html ng-app>

```


</li>
<li>
Initialize the `name` variable using `ng-init`

```js
<body ng-init=" name = 'World' ">

```


**Note that ng-init should be used for demonstrative and testing purposes only. When building an actual application, controllers should initialize the data.**
</li>
<li>
Bind data from the model to the view on HTML controls. Bind an `<input>` to the `name` property with `ng-model`

```js
<input ng-model="name" />

```


</li>
<li>
Display  content from the model using double braces `{ { } }`

```js
<span>Hello, { { name } }</span>

```


</li>
<li>
Another way of binding the `name` property is using `ng-bind` instead of handlebars`"{ { } }"`

```js
 <span ng-bind="name"></span>

```


</li>

The last three steps establish the [**two way data-binding**](https://docs.angularjs.org/guide/databinding). Changes made to the input update the **model**, which is reflected in the **view**.

There is a difference between using handlebars and `ng-bind`. If you use handlebars, you might see the actual `Hello, { {name} }` as the page loads before the expression is resolved (before the data is loaded) whereas if you use `ng-bind`, it will only show the data when the name is resolved. As an alternative the directive `ng-cloak` can be used to prevent handlebars to display before it is compiled.



## Showcasing all common Angular constructs


The following example shows common AngularJS constructs in one file:

```js
<!DOCTYPE html>
<html ng-app="myDemoApp">
  <head>
    <style>.started { background: gold; }</style>
    <script src="https://code.angularjs.org/1.5.8/angular.min.js"></script>
    <script>
      function MyDataService() {
        return {
          getWorlds: function getWorlds() {
            return ["this world", "another world"];
          }
        };
      }

      function DemoController(worldsService) {
        var vm = this;
        vm.messages = worldsService.getWorlds().map(function(w) {
          return "Hello, " + w + "!";
        });
      }

      function startup($rootScope, $window) {
        $window.alert("Hello, user! Loading worlds...");
        $rootScope.hasStarted = true;
      }
      
      angular.module("myDemoApp", [/* module dependencies go here */])
        .service("worldsService", [MyDataService])
        .controller("demoController", ["worldsService", DemoController])
        .config(function() {
          console.log('configuring application');
        })
        .run(["$rootScope", "$window", startup]);
    </script>
  </head>
  <body ng-class="{ 'started': hasStarted }" ng-cloak>
    <div ng-controller="demoController as vm">
      <ul>
        <li ng-repeat="msg in vm.messages">{ { msg } }</li>
      </ul>
    </div>
  </body>
</html>

```

Every line of the file is explained below:

[Live Demo](https://jsfiddle.net/15vspt5t/)

1. `ng-app="myDemoApp"`, [the ngApp directive](https://docs.angularjs.org/api/ng/directive/ngApp) that bootstraps the application and tells angular that a DOM element is controlled by a specific `angular.module` named `"myDemoApp"`;
1. `<script src="angular.min.js">` is the first step in [bootstrapping the AngularJS library](https://docs.angularjs.org/guide/bootstrap#angular-script-tag);

Three functions (`MyDataService`, `DemoController`, and `startup`) are declared, which are used (and explained) below.

<li>
[`angular.module(...)`](https://docs.angularjs.org/api/ng/function/angular.module) used with an array as the second argument creates a new module. This array is used to supply a list of module dependencies. In this example we chain calls on the result of the `module(...)` function;
</li>
<li>
`.service(...)` creates an [Angular Service](https://docs.angularjs.org/guide/services) and returns the module for chaining;
</li>
<li>
`.controller(...)` creates an [Angular Controller](https://docs.angularjs.org/guide/controller) and returns the module for chaining;
</li>
<li>
`.config(...)` Use this method to register work which needs to be performed on module loading.
</li>
<li>
`.run(...)` makes sure code is [run at startup time](http://stackoverflow.com/q/19276095/419956) and takes an array of items as a parameter. Use this method to register work which should be performed when the injector is done loading all modules.
<ul>
1. the first item is letting Angular know that the `startup` function requires [the built-in `$rootScope` service](https://docs.angularjs.org/api/ng/service/$rootScope) to be injected as an argument;
1. the second item is letting Angular know that the `startup` function requires [the built-in `$window` service](https://docs.angularjs.org/api/ng/service/$window) to be injected as an argument;
1. the **last** item in the array, `startup`, is the actual function to run on startup;
</ul>
</li>
<li>
`ng-class` is [the ngClass directive](https://docs.angularjs.org/api/ng/directive/ngClass) to set a dynamic `class`, and in this example utilizes `hasStarted` on the `$rootScope` dynamically
</li>
<li>
`ng-cloak` is [a directive](https://docs.angularjs.org/api/ng/directive/ngCloak) to prevent the unrendered Angular html template (e.g. "`{ { msg } }`") to be briefly shown before Angular has fully loaded the application.
</li>
<li>
`ng-controller` is [the directive](https://docs.angularjs.org/api/ng/directive/ngController) that asks Angular to instantiate a new controller of specific name to orchestrate that part of the DOM;
</li>
<li>
`ng-repeat` is [the directive](https://docs.angularjs.org/api/ng/directive/ngRepeat) to make Angular iterate over a collection and clone a DOM template for each item;
</li>
<li>
`{ { msg } }` showcases [interpolation](https://docs.angularjs.org/guide/interpolation): on-the-spot rendering of a part of the scope or controller;
</li>



## The importance of scope


As Angular uses HTML to extend a web page and plain Javascript to add logic, it makes it easy to create a web page using **[ng-app](https://docs.angularjs.org/api/ng/directive/ngApp)**, **[ng-controller](https://docs.angularjs.org/api/ng/directive/ngController)** and some built-in directives such as **[ng-if](https://docs.angularjs.org/api/ng/directive/ngIf)**, **[ng-repeat](https://docs.angularjs.org/api/ng/directive/ngRepeat)**, etc. With the new **controllerAs** syntax, newcomers to Angular users can attach functions and data to their controller instead of using `$scope`.

However, sooner or later, it is important to understand what exactly this `$scope` thing is. It will keep showing up in examples so it is important to have some understanding.

The good news is that it is a simple yet powerful concept.

When you create the following:

```js
<div ng-app="myApp">
 <h1>Hello { { name } }</h1>
</div>

```

Where does **name** live?

The answer is that Angular creates a `$rootScope` object. This is simply a regular Javascript object and so **name** is a property on the `$rootScope` object:

```js
angular.module("myApp", [])
  .run(function($rootScope) {
    $rootScope.name = "World!";
  });

```

And just as with global scope in Javascript, it's usually not such a good idea to add items to the global scope or `$rootScope`.

Of course, most of the time, we create a controller and put our required functionality into that controller. But when we create a controller, Angular does it's magic and creates a `$scope` object for that controller. This is sometimes referred to as the **local scope**.

So, creating the following controller:

```js
<div ng-app="myApp">
  <div ng-controller="MyController">
    <h1>Hello { { name } }</h1>
  </div>
</div>

```

would allow the local scope to be accessible via the `$scope` parameter.

```js
angular.module("myApp", [])
  .controller("MyController", function($scope) {
    $scope.name = "Mr Local!";
  });

```

A controller without a `$scope` parameter may simply not need it for some reason. But it is important to realize that, **even with controllerAs syntax**, the local scope exists.

As `$scope` is a JavaScript object, Angular magically sets it up to prototypically inherit from `$rootScope`. And as you can imagine, there can be a chain of scopes. For example, you could create a model in a parent controller and attach to it to the parent controller's scope as `$scope.model`.

Then via the prototype chain, a child controller could access that same model locally with `$scope.model`.

None of this is initially evident, as it's just Angular doing its magic in the background. But understanding `$scope` is an important step in getting to know how Angular works.



## Minification in Angular


**What is Minification ?**

It is the process of removing all unnecessary characters from source code without changing its functionality.

**Normal Syntax**

If we use normal angular syntax for writing a controller then after minifiying our files it going to break our functionality.

Controller (Before minification) :

```js
var app = angular.module('mainApp', []);    
app.controller('FirstController', function($scope) {
    $scope.name= 'Hello World !';  
});

```

After using minification tool, It will be minified as like below.

```js
var app=angular.module("mainApp",[]);app.controller("FirstController",function(e){e.name= 'Hello World !'})

```

Here, minification removed unnecessary spaces and the $scope variable from code.
So when we use this minified code then its not going to print anything on view. Because $scope is a crucial part between controller and view, which is now replaced by the small 'e' variable. So when you run the application it is going to give Unknown Provider 'e' dependency error.

There are two ways of annotating your code with service name information which are minification safe:

**Inline Annotation Syntax**

```js
var app = angular.module('mainApp', []);    
app.controller('FirstController', ['$scope', function($scope) {
    $scope.message = 'Hello World !'; 
}]);

```

**$inject Property Annotation Syntax**

```js
FirstController.$inject = ['$scope'];
var FirstController = function($scope) {
    $scope.message = 'Hello World !'; 
}

var app = angular.module('mainApp', []);
app.controller('FirstController', FirstController);

```

After minification, this code will be

```js
var app=angular.module("mainApp",[]);app.controller("FirstController",["$scope",function(a){a.message="Hello World !"}]);

```

Here, angular will consider variable 'a' to be treated as $scope, and It will display output as 'Hello World !'.



## AngularJS Getting Started Video Tutorials


There are a lot of good video tutorials for the AngularJS framework on [egghead.io](https://egghead.io)

[<img src="https://i.stack.imgur.com/JxQ0P.png" alt="enter image description here" />](https://i.stack.imgur.com/JxQ0P.png)

- [https://egghead.io/courses/angularjs-app-from-scratch-getting-started](https://egghead.io/courses/angularjs-app-from-scratch-getting-started)
- [https://egghead.io/courses/angularjs-application-architecture](https://egghead.io/courses/angularjs-application-architecture)
- [https://egghead.io/courses/angular-material-introduction](https://egghead.io/courses/angular-material-introduction)
- [https://egghead.io/courses/building-an-angular-1-x-ionic-application](https://egghead.io/courses/building-an-angular-1-x-ionic-application)
- [https://egghead.io/courses/angular-and-webpack-for-modular-applications](https://egghead.io/courses/angular-and-webpack-for-modular-applications)
- [https://egghead.io/courses/angularjs-authentication-with-jwt](https://egghead.io/courses/angularjs-authentication-with-jwt)
- [https://egghead.io/courses/angularjs-data-modeling](https://egghead.io/courses/angularjs-data-modeling)
- [https://egghead.io/courses/angular-automation-with-gulp](https://egghead.io/courses/angular-automation-with-gulp)
- [https://egghead.io/courses/learn-protractor-testing-for-angularjs](https://egghead.io/courses/learn-protractor-testing-for-angularjs)
- [https://egghead.io/courses/ionic-quickstart-for-windows](https://egghead.io/courses/ionic-quickstart-for-windows)
- [https://egghead.io/courses/build-angular-1-x-apps-with-redux](https://egghead.io/courses/build-angular-1-x-apps-with-redux)
- [https://egghead.io/courses/using-angular-2-patterns-in-angular-1-x-apps](https://egghead.io/courses/using-angular-2-patterns-in-angular-1-x-apps)



## The Simplest Possible Angular Hello World.


Angular 1 is at heart a DOM compiler. We can pass it HTML, either as a template or just as a regular web page, and then have it compile an app.

We can tell Angular to treat a region of the page as an **expression** using the `{ { } }` handlebars style syntax. Anything between the curly braces will be compiled, like so:

```js
{ { 'Hello' + 'World' } }

```

This will output:

```js
HelloWorld

```

### **ng-app**

We tell Angular which portion of our DOM to treat as the master template using the `ng-app` **directive**. A directive is a custom attribute or element that the Angular template compiler knows how to deal with. Let's add an ng-app directive now:

```js
<html>
  <head>
    <script src="/angular.js"></script>
  </head>
  <body ng-app>
    { { 'Hello' + 'World' } }
  </body>
</html>

```

I've now told the body element to be the root template. Anything in it will be compiled.

### **Directives**

Directives are compiler directives. They extend the capabilities of the Angular DOM compiler. This is why **Misko**, the creator of Angular, describes Angular as:

> 
"What a web browser would have been had it been built for web applications.


We literally create new HTML attributes and elements, and have Angular compile them into an app. `ng-app` is a directive that simply turns on the compiler. Other directives include:

- `ng-click`, which adds a click handler,
- `ng-hide`, which conditionally hides an element, and
- `<form>`, which adds additional behaviour to a standard HTML form element.

Angular comes with around 100 built-in directives which allow you to accomplish most common tasks. We can also write our own, and these will be treated in the same way as the built in directives.

We build an Angular app out of a series of directives, wired together with HTML.



#### Remarks


AngularJS is a web application framework designed to simplify rich client-side application development. This documentation is for [Angular 1.x](https://angularjs.org/), the predecessor of the more modern [Angular 2](https://angular.io/) or see the [Stack Overflow documentation for Angular 2](http://stackoverflow.com/documentation/angular2/topics).

