---
metaTitle: "AngularJS - Use of in-built directives"
description: "Hide/Show HTML Elements"
---

# Use of in-built directives



## Hide/Show HTML Elements


This example hide show html elements.

```js
<!DOCTYPE html>
<html ng-app="myDemoApp">
  <head>
    <script src="https://code.angularjs.org/1.5.8/angular.min.js"></script>
    <script>

      function HideShowController() {
        var vm = this;
        vm.show=false;
        vm.toggle= function() {
          vm.show=!vm.show;
        }
      }
      
      angular.module("myDemoApp", [/* module dependencies go here */])
        .controller("hideShowController", [HideShowController]);
    </script>
  </head>
  <body ng-cloak>
    <div ng-controller="hideShowController as vm">
      <a style="cursor: pointer;" ng-show="vm.show" ng-click="vm.toggle()">Show Me!</a>
      <a style="cursor: pointer;" ng-hide="vm.show" ng-click="vm.toggle()">Hide Me!</a>
    </div>
  </body>
</html>

```

[Live Demo](https://plnkr.co/edit/wbSmMu96Xz3svv0qilwh?p=preview)

Step by step explanation:

1. `ng-app="myDemoApp"`, the ngApp [directive](https://docs.angularjs.org/api/ng/directive/ngApp) tells angular that a DOM element is controlled by a specific [angular.module](https://docs.angularjs.org/api/ng/function/angular.module) named "myDemoApp".
1. `<script src="[//angular include]">` include angular js.
1. `HideShowController` function is defined containing another function named `toggle` which help to hide show the element.
1. `angular.module(...)` creates a new module.
1. `.controller(...)` [Angular Controller](https://docs.angularjs.org/api/ng/directive/ngController) and returns the module for chaining;
1. `ng-controller` [directive](https://docs.angularjs.org/api/ng/directive/ngController) is key aspect of how angular supports the principles behind the Model-View-Controller design pattern.
1. `ng-show` [directive](https://docs.angularjs.org/api/ng/directive/ngShow) shows the given HTML element if expression provided is true.
1. `ng-hide` [directive](https://docs.angularjs.org/api/ng/directive/ngHide) hides the given HTML element if expression provided is true.
1. `ng-click` [directive](https://docs.angularjs.org/api/ng/directive/ngClick) fires a toggle function inside controller

