---
metaTitle: "AngularJS - ng-view"
description: "ng-view, Registration navigation"
---

# ng-view


ng-view is one of in-build directive that angular uses as a container to switch between views. {info} ngRoute is no longer a part of the base angular.js file, so you'll need to include the angular-route.js file after your the base angular javascript file.
We can configure a route by using the “when” function of the $routeProvider. We need to first specify the route, then in a second parameter provide an object with a templateUrl property and a controller property.



## ng-view


`ng-view` is a directive used with `$route` to render a partial view in the main page layout. Here in this example, Index.html is our main file and when user lands on "/" route the templateURL home.html will be rendered in Index.html where `ng-view` is mentioned.

```js
angular.module('ngApp', ['ngRoute'])

.config(function($routeProvider){
  $routeProvider.when("/",
    {
      templateUrl: "home.html",
      controller: "homeCtrl"
    }
  );
});

angular.module('ngApp').controller('homeCtrl',['$scope', function($scope) {
  $scope.welcome= "Welcome to stackoverflow!";
}]);

//Index.html
<body ng-app="ngApp">
    <div ng-view></div>
</body>

//Home Template URL or home.html
<div><h2>{ {welcome} }</h2></div>

```



## Registration navigation


1. We injecting the module in the application

```

 var Registration=angular.module("myApp",["ngRoute"]);

```


1. now we use $routeProvider from "ngRoute"

```

   Registration.config(function($routeProvider) {

});

```


1. finally we integrating the route, we define  "/add" routing to the application in case application get "/add" it divert to regi.htm

```

Registration.config(function($routeProvider) {
    $routeProvider
    .when("/add", {
        templateUrl : "regi.htm"
    })
});

```

