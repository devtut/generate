---
metaTitle: "AngularJS - HTTP Interceptor"
description: "Generic httpInterceptor step by step, Getting Started, Flash message on response using http interceptor"
---

# HTTP Interceptor


The $http service of AngularJS allows us to communicate with a backend and make HTTP requests. There are cases where we want to capture every request and manipulate it before sending it to the server. Other times we would like to capture the response and process it before completing the call. Global http error handling can be also a good example of such need. Interceptors are created exactly for such cases.



## Generic httpInterceptor step by step


Create an HTML file with the following content:

```js
<!DOCTYPE html>
<html>
<head>
  <title>Angular Interceptor Sample</title>
  <script src="https://code.angularjs.org/1.5.8/angular.min.js"></script>
  <script src="app.js"></script>
  <script src="appController.js"></script>
  <script src="genericInterceptor.js"></script>
</head>
<body ng-app="interceptorApp">
    <div ng-controller="appController as vm">
        <button ng-click="vm.sendRequest()">Send a request</button>
    </div>
</body>
</html>

```

Add a JavaScript file called 'app.js':

```js
var interceptorApp = angular.module('interceptorApp', []);

interceptorApp.config(function($httpProvider) {
        $httpProvider.interceptors.push('genericInterceptor');
});

```

Add another one called 'appController.js':

```js
(function() {
    'use strict';

    function appController($http) {
        var vm = this;

        vm.sendRequest = function(){
            $http.get('http://google.com').then(function(response){
                console.log(response);
            });
        };
    }

    angular.module('interceptorApp').controller('appController',['$http', appController]);
})();

```

And finally the file containing the interceptor itself 'genericInterceptor.js':

```js
(function() {
    "use strict";

    function genericInterceptor($q) {
        this.responseError = function (response) {
            return $q.reject(response);
        };

        this.requestError = function(request){
            if (canRecover(rejection)) {
                return responseOrNewPromise
            }
            return $q.reject(rejection);
        };

        this.response = function(response){
            return response;
        };

        this.request = function(config){
            return config;
        }
    } 

    angular.module('interceptorApp').service('genericInterceptor', genericInterceptor);
})();

```

The 'genericInterceptor' cover the possible functions which we can override adding extra behavior to our application.



## Getting Started


Angular's builtin [`$http` service](https://docs.angularjs.org/api/ng/service/$http) allows us to send HTTP requests. Oftentime, the need arise to do things before or after a request, for example adding to each request an authentication token or creating a generic error handling logic.



## Flash message on response using http interceptor


### In the view file

In the base html (index.html) where we usually include the angular scripts or the html that is shared across the app, leave an empty div element, the flash messages will be appearing inside this div element

```js
<div class="flashmessage" ng-if="isVisible">
    { {flashMessage} }
</div>

```

### Script File

In the config method of angular module, inject the httpProvider, the httpProvider has an interceptor array property, push the custom interceptor, In the current example the custom interceptor intercepts only the response and calls a method attached to rootScope.

```js
var interceptorTest = angular.module('interceptorTest', []);
    
    interceptorTest.config(['$httpProvider',function ($httpProvider) {

        $httpProvider.interceptors.push(["$rootScope",function ($rootScope) {
            return {     //intercept only the response
                        'response': function (response) 
                                    {
                                      $rootScope.showFeedBack(response.status,response.data.message);
                           
                                      return response;
                                    }
                   };
        }]);
        
    }])

```

Since only providers can be injected into the config method of an angular module (that is httpProvider and not the rootscope),  declare the method attached to rootscope inside the run method of angular module.

Also display the message inside $timeout so that the message will have the flash property, that is disappearing after a threshold time. In our example its 3000 ms.

```js
interceptorTest.run(["$rootScope","$timeout",function($rootScope,$timeout){
     $rootScope.showFeedBack = function(status,message){
        
        $rootScope.isVisible = true;
        $rootScope.flashMessage = message;
        $timeout(function(){$rootScope.isVisible = false },3000)
    }
}]);

```

### Common pitfalls

Trying to inject **$rootScope or any other services** inside **config** method of angular module, the lifecycle of angular app doesnt allow that and unknown provider error will be thrown. Only **providers** can be injected in **config** method of the angular module

