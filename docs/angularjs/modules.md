---
metaTitle: "AngularJS - Modules"
description: "Modules, Modules"
---

# Modules



## Modules


Module serves as a container of different parts of your app such as controllers, services, filters, directives, etc. Modules can be referenced by other modules through Angular's dependency injection mechanism.

**Creating a module:**

```js
angular
    .module('app', []);

```

Array `[]` passed in above example is the **list of modules** `app` depends on, if there are no dependencies then we pass Empty Array i.e. `[]`.

**Injecting a module as a dependency of another module:**

```js
angular.module('app', [
    'app.auth',
    'app.dashboard'
]);

```

**Referencing a module:**

```js
angular
    .module('app');

```



## Modules


Module is a container for various parts of your applications - controller, services,  filters, directive, etc.

**Why to use Modules**<br />
Most applications have a main method that instantiates and wires together the different parts of the application.<br />
Angular apps don't have main method.<br />
But in AngularJs the declarative process is easy to understand and one can package code as reusable modules.<br />
Modules can be loaded in any order because modules delay execution.

**declare a module**

```js
var app = angular.module('myApp', []);
// Empty array is list of modules myApp is depends on.
// if there are any required dependancies, 
// then you can add in module, Like ['ngAnimate']

app.controller('myController', function() {

  // write your business logic here
});

```

**Module Loading and Dependencies**

<li>
Configuration Blocks:- get executed during provider and configurtation phase.

```js
angular.module('myModule', []).
config(function(injectables) {
  // here you can only inject providers in to config blocks.
});

```


</li>
<li>
Run Blocks:- get executed after the injector is created and are used to start the application.

```js
angular.module('myModule', []).
run(function(injectables) {
  // here you can only inject instances in to config blocks.
});

```


</li>

