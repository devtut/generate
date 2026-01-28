---
metaTitle: "AngularJS - ui-router"
description: "Basic Example, Multiple Views, Using resolve functions to load data, Nested Views / States"
---

# ui-router



## Basic Example


app.js

```js
angular.module('myApp', ['ui.router'])
  .controller('controllerOne', function() {
    this.message = 'Hello world from Controller One!';
  })
  .controller('controllerTwo', function() {
    this.message = 'Hello world from Controller Two!';
  })
  .controller('controllerThree', function() {
    this.message = 'Hello world from Controller Three!';
  })
  .config(function($stateProvider, $urlRouterProvider) {
    $stateProvider
      .state('one', {
        url: "/one",
        templateUrl: "view-one.html",
        controller: 'controllerOne',
        controllerAs: 'ctrlOne'
      })
      .state('two', {
        url: "/two",
        templateUrl: "view-two.html",
        controller: 'controllerTwo',
        controllerAs: 'ctrlTwo'
      })
      .state('three', {
        url: "/three",
        templateUrl: "view-three.html",
        controller: 'controllerThree',
        controllerAs: 'ctrlThree'
      });

      $urlRouterProvider.otherwise('/one');
  });

```

index.html

```js
<div ng-app="myApp">
  <nav>
    <!-- links to switch routes -->
    <a ui-sref="one">View One</a>
    <a ui-sref="two">View Two</a>
    <a ui-sref="three">View Three</a>
  </nav>
  <!-- views will be injected here -->
  <div ui-view></div>
  <!-- templates can live in normal html files -->
  <script type="text/ng-template" id="view-one.html">
    <h1>{ {ctrlOne.message} }</h1>
  </script>

  <script type="text/ng-template" id="view-two.html">
    <h1>{ {ctrlTwo.message} }</h1>
  </script>

  <script type="text/ng-template" id="view-three.html">
    <h1>{ {ctrlThree.message} }</h1>
  </script>
</div>

```



## Multiple Views


app.js

```js
angular.module('myApp', ['ui.router'])
  .controller('controllerOne', function() {
    this.message = 'Hello world from Controller One!';
  })
  .controller('controllerTwo', function() {
    this.message = 'Hello world from Controller Two!';
  })
  .controller('controllerThree', function() {
    this.message = 'Hello world from Controller Three!';
  })
  .controller('controllerFour', function() {
    this.message = 'Hello world from Controller Four!';
  })
  .config(function($stateProvider, $urlRouterProvider) {
    $stateProvider
      .state('one', {
        url: "/one",
        views: {
          "viewA": {
            templateUrl: "view-one.html",
            controller: 'controllerOne',
            controllerAs: 'ctrlOne'
          },
          "viewB": {
            templateUrl: "view-two.html",
            controller: 'controllerTwo',
            controllerAs: 'ctrlTwo'
          }
        }
      })
      .state('two', {
        url: "/two",
        views: {
          "viewA": {
            templateUrl: "view-three.html",
            controller: 'controllerThree',
            controllerAs: 'ctrlThree'
          },
          "viewB": {
            templateUrl: "view-four.html",
            controller: 'controllerFour',
            controllerAs: 'ctrlFour'
          }
        }
      });

    $urlRouterProvider.otherwise('/one');
  });

```

index.html

```js
<div ng-app="myApp">
  <nav>
    <!-- links to switch routes -->
    <a ui-sref="one">Route One</a>
    <a ui-sref="two">Route Two</a>
  </nav>
  <!-- views will be injected here -->
  <div ui-view="viewA"></div>
  <div ui-view="viewB"></div>
  <!-- templates can live in normal html files -->
  <script type="text/ng-template" id="view-one.html">
    <h1>{ {ctrlOne.message} }</h1>
  </script>

  <script type="text/ng-template" id="view-two.html">
    <h1>{ {ctrlTwo.message} }</h1>
  </script>

  <script type="text/ng-template" id="view-three.html">
    <h1>{ {ctrlThree.message} }</h1>
  </script>

  <script type="text/ng-template" id="view-four.html">
    <h1>{ {ctrlFour.message} }</h1>
  </script>
</div>

```



## Using resolve functions to load data


app.js

```js
angular.module('myApp', ['ui.router'])
  .service('User', ['$http', function User ($http) {
    this.getProfile = function (id) {
      return $http.get(...) // method to load data from API
    };
  }])
  .controller('profileCtrl', ['profile', function profileCtrl (profile) {
    // inject resolved data under the name of the resolve function
    // data will already be returned and processed
    this.profile = profile;
  }])
  .config(['$stateProvider', '$urlRouterProvider', function ($stateProvider, $urlRouterProvider) {
    $stateProvider
      .state('profile', {
        url: "/profile/:userId",
        templateUrl: "profile.html",
        controller: 'profileCtrl',
        controllerAs: 'vm',
        resolve: {
          profile: ['$stateParams', 'User', function ($stateParams, User) {
            // $stateParams will contain any parameter defined in your url
            return User.getProfile($stateParams.userId)
              // .then is only necessary if you need to process returned data
              .then(function (data) {
                return doSomeProcessing(data);
              });
          }]
        }
      }]);

      $urlRouterProvider.otherwise('/');
  });

```

profile.html

```js
<ul>
  <li>Name: { {vm.profile.name} }</li>
  <li>Age: { {vm.profile.age} }</li>
  <li>Sex: { {vm.profile.sex} }</li>
</ul>

```

View [UI-Router Wiki entry on resolves here](https://github.com/angular-ui/ui-router/wiki#resolve).

Resolve functions must be resolved before the `$stateChangeSuccess` event is fired, which means that the UI will not load until **all** resolve functions on the state have finished. This is a great way to ensure that data will be available to your controller and UI. However, you can see that a resolve function should be fast in order to avoid hanging the UI.



## Nested Views / States


**app.js**

```js
var app = angular.module('myApp',['ui.router']);

app.config(function($stateProvider,$urlRouterProvider) {

    $stateProvider

    .state('home', {
        url: '/home',
        templateUrl: 'home.html',
        controller: function($scope){
            $scope.text = 'This is the Home'
        }
    })

    .state('home.nested1',{
        url: '/nested1',
        templateUrl:'nested1.html',
        controller: function($scope){
            $scope.text1 = 'This is the nested view 1'
        }
    })

    .state('home.nested2',{
        url: '/nested2',
        templateUrl:'nested2.html',
        controller: function($scope){
            $scope.fruits = ['apple','mango','oranges'];
        }
    });

    $urlRouterProvider.otherwise('/home');

});

```

**index.html**

```

 <div ui-view></div>
   <script src="https://ajax.googleapis.com/ajax/libs/angularjs/1.5.8/angular.min.js"></script>
   <script src="angular-ui-router.min.js"></script>
   <script src="app.js"></script>

```

**home.html**

```js
<div>
<h1> { {text} } </h1>
<br>
    <a ui-sref="home.nested1">Show nested1</a>
    <br>
    <a ui-sref="home.nested2">Show nested2</a>
    <br>

    <div ui-view></div>
</div>

```

**nested1.html**

```js
<div>
<h1> { {text1} } </h1>
</div>

```

**nested2.html**

```js
<div>
    <ul>
      <li ng-repeat="fruit in fruits">{ { fruit } }</li>
    </ul>    
</div>

```



#### Remarks


**What is `ui-router`?**

> 
<p>Angular UI-Router is a client-side Single Page Application routing
framework for AngularJS.</p>
<p>Routing frameworks for SPAs update the browser's URL as the user
navigates through the app. Conversely, this allows changes to the
browser's URL to drive navigation through the app, thus allowing the
user to create a bookmark to a location deep within the SPA.</p>
<p>UI-Router applications are modeled as a hierarchical tree of states.
UI-Router provides a state machine to manage the transitions between
those application states in a transaction-like manner.</p>


**Useful links**

You can find the official API Documentation [here](http://angular-ui.github.io/ui-router/site/#/api/ui.router). For questions about `ui-router` VS `ng-router`, you can find a reasonably detailed answer [here](http://stackoverflow.com/questions/21023763/angularjs-difference-between-angular-route-and-angular-ui-router). Keep in mind `ng-router` has already released a new ng-router update called `ngNewRouter` (compatible with Angular 1.5+/2.0) which supports states just like ui-router. You can read more about `ngNewRouter` [here](https://medium.com/angularjs-meetup-south-london/angular-just-another-introduction-to-ngnewrouter-vs-ui-router-72bfcb228017#.gisqk1lx8).

