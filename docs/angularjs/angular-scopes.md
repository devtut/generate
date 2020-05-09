---
metaTitle: "AngularJS - Angular $scopes"
description: "A function available in the entire app, Avoid inheriting primitive values, Basic Example of $scope inheritance, How can you limit the scope on a directive and why would you do this?, Creating custom $scope events, Using $scope functions"
---

# Angular $scopes



## A function available in the entire app


Be careful, this approach might be considered as a bad design for angular apps, since it requires programmers to remember both where functions are placed in the scope tree, and to be aware of scope inheritance. In many cases it would be preferred to inject a service  ([Angular practice - using scope inheritance vs injection](http://stackoverflow.com/questions/23659315/angular-practice-using-scope-inheritance-vs-injection)).

This example only show how scope inheritance could be used for our needs, and the how you could take advantage of it, and not the best practices of designing an entire app.

In some cases, we could take advantage of scope inheritance, and set a function as a property of the rootScope.
This way - all of the scopes in the app (except for isolated scopes) will inherit this function, and it could be called from anywhere in the app.

```js
angular.module('app', [])
.run(['$rootScope', function($rootScope){
    var messages = []
    $rootScope.addMessage = function(msg){
        messages.push(msg);
    }
}]);

<div ng-app="app">
    <a ng-click="addMessage('hello world!')">it could be accsessed from here</a>
    <div ng-include="inner.html"></div>
</div>

```

inner.html:

```js
<div>
    <button ng-click="addMessage('page')">and from here to!</button>
</div>

```



## Avoid inheriting primitive values


In javascript, assigning a non-[primitive](https://developer.mozilla.org/en-US/docs/Glossary/Primitive) value (Such as Object, Array, Function, and [many](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference) more), keeps a reference (an address in the memory) to the assigned value.

Assigning a primitive value (String, Number, Boolean, or Symbol) to two different variables, and changing one, won't change both:

```js
var x = 5;
var y = x;
y = 6;
console.log(y === x, x, y); //false, 5, 6

```

But with a non-primitive value, since both variables are simply keeping references to the same object, changing one variable **will** change the other:

```js
var x = { name : 'John Doe' };
var y = x;
y.name = 'Jhon';
console.log(x.name === y.name, x.name, y.name); //true, John, John

```

In angular, when a scope is created, it is assigned all of its parent's properties However, changing properties afterwards will only affect the parent scope if it is a non-primitive value:

```js
angular.module('app', [])
.controller('myController', ['$scope', function($scope){
    $scope.person = { name: 'John Doe' }; //non-primitive
    $scope.name = 'Jhon Doe'; //primitive
}])
.controller('myController1', ['$scope', function($scope){}]);

<div ng-app="app" ng-controller="myController">
    binding to input works: { {person.name} }<br/>
    binding to input does not work: { {name} }<br/>
    <div ng-controller="myController1">
        <input ng-model="person.name" />
        <input ng-model="name" />
    </div>
</div>

```

Remember: in Angular scopes can be created in many ways (such as built-in or custom directives, or the `$scope.$new()` function), and keeping track of the scope tree is probably impossible.

Using only non-primitive values as scope properties will keep you on the safe side (unless you need a property to not inherit, or other cases where you are aware of scope inheritance).



## Basic Example of $scope inheritance


```js
angular.module('app', [])
.controller('myController', ['$scope', function($scope){
    $scope.person = { name: 'John Doe' };
}]);

<div ng-app="app" ng-conroller="myController">
    <input ng-model="person.name" />
    <div ng-repeat="number in [0,1,2,3]">
        { {person.name} } { {number} }
    </div>
</div>

```

In this example, the ng-repeat directive creates a new scope for each of its newly created children.

These created scopes are children of their parent scope (in this case the scope created by myController), and therfore, they inherit all of its proporties, such as person.



## How can you limit the scope on a directive and why would you do this?


Scope is used as the "glue" that we use to communicate between the parent controller, the directive, and the directive template. Whenever the AngularJS application is bootstrapped, a rootScope object is created. Each scope created by controllers, directives and services are prototypically inherited from rootScope.

Yes, we can limit the scope on a directive . We can do so by creating an isolated scope for directive.

> 
There are 3 types of directive scopes:
<ol>
- Scope : False ( Directive uses its parent scope )
- Scope : True ( Directive gets a new scope )
- Scope : { } ( Directive gets a new isolated scope )
</ol>


**Directives with the new isolated scope:**  When we create a new isolated scope then it will not be inherited from the parent scope. This new scope is called Isolated scope because it is completely detached from its parent scope.
Why? should we use isolated scope:  We should use isolated scope when we want to create a custom directive because it will make sure that our directive is generic, and placed anywhere inside the application. Parent scope is not going to interfere with the directive scope.

Example of isolated scope:

```js
var app = angular.module("test",[]);

app.controller("Ctrl1",function($scope){
    $scope.name = "Prateek";
    $scope.reverseName = function(){
        $scope.name = $scope.name.split('').reverse().join('');
    };
});
app.directive("myDirective", function(){
    return {
        restrict: "EA",
        scope: {},
        template: "<div>Your name is : { {name} }</div>"+
        "Change your name : <input type='text' ng-model='name'/>"
    };
});

```

There’re 3 types of prefixes AngularJS provides for isolated scope these are :

1. "@"   (  Text binding / one-way binding )
1. "="   ( Direct model binding / two-way binding )
1. "&"   ( Behaviour binding / Method binding  )

All these prefixes receives data from the attributes of the directive element like :

```js
<div my-directive 
  class="directive"
  name="{ {name} }" 
  reverse="reverseName()" 
  color="color" >
</div>

```



## Creating custom $scope events


Like normal HTML elements, it is possible for $scopes to have their own events. $scope events can be subscribed to using the following manner:

```

$scope.$on('my-event', function(event, args) {
    console.log(args); // { custom: 'data' }
});

```

If you need unregister an event listener, the **$on** function will return an unbinding function. To continue with  the above example:

```js
var unregisterMyEvent = $scope.$on('my-event', function(event, args) {
    console.log(args); // { custom: 'data' }
    unregisterMyEvent();
});

```

There are two ways of triggering your own custom $scope event **$broadcast** and **$emit**.
To notify the parent(s) of a scope of a specific event, use **$emit**

```js
$scope.$emit('my-event', { custom: 'data' }); 

```

The above example will trigger any event listeners for `my-event` on the parent scope and will continue up the scope tree to **$rootScope** unless a listener calls `stopPropagation` on the event. Only events triggered with **$emit** may call `stopPropagation`

The reverse of **$emit** is **$broadcast**, which will trigger any event listeners on all child scopes in the scope tree that are children of the scope that called **$broadcast**.

```js
$scope.$broadcast('my-event', { custom: 'data' });

```

Events triggered with **$broadcast** cannot be canceled.



## Using $scope functions


While declaring a function in the $rootscope has it's advantages, we can also declare a $scope function any part of the code that is injected by the $scope service. Controller, for instance.

**Controller**

```js
myApp.controller('myController', ['$scope', function($scope){
    $scope.myFunction = function () {
        alert("You are in myFunction!");
    };
}]);

```

Now you can call your function from the controller using:

```js
$scope.myfunction();

```

Or via HTML that is under that specific controller:

```js
<div ng-controller="myController">
    <button ng-click="myFunction()"> Click me! </button>
</div>

```

**Directive**

An [angular directive](https://stackoverflow.com/documentation/angularjs/965/custom-directives/6349/directive-definition-object-template#t=201610091611432589758) is another place you can use your scope:

```js
myApp.directive('triggerFunction', function() {
    return {
        scope: {
            triggerFunction: '&'
        },
        link: function(scope, element) {
            element.bind('mouseover', function() {
                scope.triggerFunction();
            });
        }
    };
});

```

And in your HTML code under the same controller:

```js
<div ng-controller="myController">
    <button trigger-function="myFunction()"> Hover over me! </button>
</div>

```

Of course, you can use ngMouseover for the same thing, but what's special about directives is that you can customize them the way you want. And now you know how to use your $scope functions inside them, be creative!



#### Remarks


Angular uses a **tree** of scopes to bind the logic (from controllers, directives, etc) to the view and are the primary mechanism behind change detection in AngularJS. A more detailed reference for scopes can be found at [docs.angularjs.org](https://docs.angularjs.org/guide/scope)

The root of the tree is accessible as via inject-able service **$rootScope**. All child $scopes inherit the methods and properties of their parent $scope, allowing children access to methods without the use of Angular Services.

