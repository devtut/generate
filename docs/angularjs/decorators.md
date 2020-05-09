---
metaTitle: "AngularJS - Decorators"
description: "Decorate service, factory, Decorate directive, Decorate filter"
---

# Decorators



## Decorate service, factory


Below is example of service decorator, overriding `null` date returned by service.

```js
angular.module('app', [])
  .config(function($provide) {
    $provide.decorator('myService', function($delegate) {
      $delegate.getDate = function() { // override with actual date object
        return new Date();
      };
      return $delegate;
    });
  })
  .service('myService', function() {
    this.getDate = function() {
      return null; // w/o decoration we'll be returning null
    };
  })
  .controller('myController', function(myService) {
    var vm = this;
    vm.date = myService.getDate();
  });

```

```js
<body ng-controller="myController as vm">
  <div ng-bind="vm.date | date:'fullDate'"></div>
</body>

```

[<img src="http://i.stack.imgur.com/qTaOZ.png" alt="fullDate" />](http://i.stack.imgur.com/qTaOZ.png)



## Decorate directive


Directives can be decorated just like services and we can modify or replace any of it's functionality. Note that directive itself is accessed at position 0 in $delegate array and name parameter in decorator must include `Directive` suffix (case sensitive).

So, if directive is called `myDate`, it can be accessed using `myDateDirective` using `$delegate[0]`.

Below is simple example where directive shows current time. We'll decorate it to update current time in one second intervals. Without decoration it will always show same time.

```js
<body>
  <my-date></my-date>
</body>

```

```js
angular.module('app', [])
  .config(function($provide) {
    $provide.decorator('myDateDirective', function($delegate, $interval) {
      var directive = $delegate[0]; // access directive

      directive.compile = function() { // modify compile fn
        return function(scope) {
          directive.link.apply(this, arguments);
          $interval(function() {
            scope.date = new Date(); // update date every second
          }, 1000);
        };
      };
      
      return $delegate;
    });
  })
  .directive('myDate', function() {
    return {
      restrict: 'E',
      template: '<span>Current time is { { date | date:\'MM:ss\' } }</span>',
      link: function(scope) {
        scope.date = new Date(); // get current date
      }
    };
  }); 

```

[<img src="http://i.stack.imgur.com/25CHL.png" alt="enter image description here" />](http://i.stack.imgur.com/25CHL.png)



## Decorate filter


When decorating filters, name parameter must include `Filter` suffix (case sensitive). If filter is called `repeat`, decorator parameter is `repeatFilter`. Below we'll decorate custom filter that repeats any given string **n** times so that result is reversed. You can also decorate angular's build-in filters the same way, although not recommended as it can affect the functionality of the framework.

```js
<body>
  <div ng-bind="'i can haz cheeseburger ' | repeat:2"></div>
</body>

angular.module('app', [])
  .config(function($provide) {
    $provide.decorator('repeatFilter', function($delegate) {
      return function reverse(input, count) {
        // reverse repeated string
        return ($delegate(input, count)).split('').reverse().join(''); 
      };
    });
  })
  .filter('repeat', function() {
    return function(input, count) {
      // repeat string n times
      return (input || '').repeat(count || 1);
    };
  });   

```

[<img src="http://i.stack.imgur.com/KrpoR.png" alt="example1" />](http://i.stack.imgur.com/KrpoR.png)

[<img src="http://i.stack.imgur.com/qFZoL.png" alt="example2" />](http://i.stack.imgur.com/qFZoL.png)



#### Syntax


- decorator(name, decorator);



#### Remarks


> 
<p>**Decorator** is function that allow a [service](http://stackoverflow.com/documentation/angularjs/5169/providers/18267/service#t=201608041619054034455), [factory](http://stackoverflow.com/documentation/angularjs/5169/providers/18266/factory#t=201608041619527352795), [directive](http://stackoverflow.com/documentation/angularjs/965/custom-directives#t=201608041536457908389)
or [filter](http://stackoverflow.com/documentation/angularjs/1401/filters#t=201608041537025734034) to be modified prior to its usage. Decorator is used to
override or modify the behavior of the service. The return value of
the decorator function may be the original service, or a new service
that replaces, or wraps and delegates to, the original service.</p>


Any decorating ****must be**** done in angular application's `config` phase by injecting `$provide` and using it's `$provide.decorator` function.

> 
<p>The decorator function has a `$delegate` object injected to provide
access to the service that matches the selector in the decorator. This
`$delegate` will be the service you are decorating. The return value of
the function provided to the decorator will take place of the service,
directive, or filter being decorated.</p>


One should consider using decorator only if any other approach is not appropriate or proves to be too tedious. If large application is using same service, and one part is changing service behavior, it's easy to create confusion and/or bugs in the process.

Typical use case would be when you have a 3rd party dependency which you can't upgrade but need it to work little differently or extend it.

