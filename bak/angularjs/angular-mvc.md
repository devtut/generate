---
metaTitle: "AngularJS - Angular MVC"
description: "The Static View with controller, Controller Function Definition, Adding information to the model"
---

# Angular MVC


In **AngularJS** the **MVC** pattern is implemented in JavaScript and HTML. The view is defined in HTML, while the model and controller are implemented in JavaScript. There are several ways that these components can be put together in AngularJS but the simplest form starts with the view.



## The Static View with controller


### mvc demo



## Controller Function Definition


```js
var indexController = myApp.controller("indexController", function ($scope) {
    // Application logic goes here
});

```



## Adding information to the model


```js
var indexController = myApp.controller("indexController", function ($scope) {
    // controller logic goes here
    $scope.message = "Hello Hacking World"
});

```

