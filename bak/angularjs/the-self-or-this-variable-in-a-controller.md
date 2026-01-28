---
metaTitle: "AngularJS - The Self Or This Variable In A Controller"
description: "Understanding The Purpose Of The Self Variable"
---

# The Self Or This Variable In A Controller


This is an explanation of a common pattern and generally considered best practice that you may see in AngularJS code.



## Understanding The Purpose Of The Self Variable


When using "controller as syntax" you would give your controller an alias in the html when using the ng-controller directive.

```js
<div ng-controller="MainCtrl as main">
</div>

```

You can then access properties and methods from the ****main**** variable that represents our controller instance. For example, let's access the ****greeting**** property of our controller and display it on the screen:

```js
<div ng-controller="MainCtrl as main">
    { { main.greeting } }
</div>

```

Now, in our controller, we need to set a value to the greeting property of our controller instance (as opposed to $scope or something else):

```js
angular
.module('ngNjOrg')
.controller('ForgotPasswordController',function ($log) {
  var self = this;

  self.greeting = "Hello World";
})

```

In order to have the HTML display correctly we needed to set the greeting property on ****this**** inside of our controller body. I am creating an intermediate variable named ****self**** that holds a reference to this. Why? Consider this code:

```js
angular
.module('ngNjOrg')
.controller('ForgotPasswordController',function ($log) {
  var self = this;

  self.greeting = "Hello World";

  function itsLate () {
    this.greeting = "Goodnight";  
  }

})

```

In this above code you may expect the text on the screen to update when the method **itsLate**  is called, but in fact it does not. JavaScript uses function level scoping rules so the "this" inside of itsLate refers to something different that "this" outside of the method body. However, we can get the desired result if we use the ****self**** variable:

```

angular
.module('ngNjOrg')
.controller('ForgotPasswordController',function ($log) {
  var self = this;

  self.greeting = "Hello World";

  function itsLate () {
    self.greeting = "Goodnight";  
  }

})

```

This is the beauty of using a "self" variable in your controllers- you can access this anywhere in your controller and can always be sure that it is referencing your controller instance.

