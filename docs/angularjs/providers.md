---
metaTitle: "AngularJS - Providers"
description: "Provider, Factory, Constant, Service, Value"
---

# Providers



## Provider


`Provider` is available both in configuration and run phases.

> 
<p>The Provider recipe is syntactically defined as a custom type that
implements a `$get` method.</p>
<p>You should use the Provider recipe only
when you want to expose an API for application-wide configuration that
must be made before the application starts. This is usually
interesting only for reusable services whose behavior might need to
vary slightly between applications.</p>


```js
angular.module('app',[])
  .provider('endpointProvider', function() {
    var uri = 'n/a';
    
    this.set = function(value) {
      uri = value;
    };

    this.$get = function() {
      return {
        get: function() {
          return uri;
        }
      };
    };
  })
  .config(function(endpointProviderProvider) {
    endpointProviderProvider.set('http://some.rest.endpoint');
  })   
  .controller('MainCtrl', function(endpointProvider) {
    var vm = this;
    vm.endpoint = endpointProvider.get();
  }); 

```

```js
<body ng-controller="MainCtrl as vm">
  <div>endpoint = { {::vm.endpoint } }</div>
</body>

```

> 
endpoint = [http://some.rest.endpoint](http://some.rest.endpoint)


Without `config` phase result would be

> 
endpoint = n/a




## Factory


`Factory` is available in run phase.

> 
<p>The Factory recipe constructs a new service using a function with zero
or more arguments (these are dependencies on other services). The
return value of this function is the service instance created by this
recipe.</p>
<p>Factory can create a service of any type, whether it be a primitive, object
literal, function, or even an instance of a custom type.</p>


```js
angular.module('app',[])
  .factory('endpointFactory', function() {
    return {
      get: function() {
        return 'http://some.rest.endpoint';
      }
    };
  })
  .controller('MainCtrl', function(endpointFactory) {
    var vm = this;
    vm.endpoint = endpointFactory.get();
  });

```

```js
<body ng-controller="MainCtrl as vm">
  <div>endpoint = { {::vm.endpoint } }</div>
</body>

```

> 
endpoint = [http://some.rest.endpoint](http://some.rest.endpoint)




## Constant


`Constant` is available both in configuration and run phases.

```js
angular.module('app',[])
  .constant('endpoint', 'http://some.rest.endpoint') // define
  .config(function(endpoint) {
    // do something with endpoint
    // available in both config- and run phases
  }) 
  .controller('MainCtrl', function(endpoint) {       // inject
    var vm = this;
    vm.endpoint = endpoint;                          // usage
  });

```

```js
<body ng-controller="MainCtrl as vm">
  <div>endpoint = { { ::vm.endpoint } }</div>
</body>

```

> 
endpoint = [http://some.rest.endpoint](http://some.rest.endpoint)




## Service


`Service` is available in run phase.

> 
<p>The Service recipe produces a service just like the Value or Factory
recipes, but it does so by <em>invoking a constructor with the new
operator</em>. The constructor can take zero or more arguments, which
represent dependencies needed by the instance of this type.</p>


```js
angular.module('app',[])
  .service('endpointService', function() {
    this.get = function() {
      return 'http://some.rest.endpoint';
    };
  })
  .controller('MainCtrl', function(endpointService) {
    var vm = this;
    vm.endpoint = endpointService.get();
  });

```

```js
<body ng-controller="MainCtrl as vm">
  <div>endpoint = { {::vm.endpoint } }</div>
</body>

```

> 
endpoint = [http://some.rest.endpoint](http://some.rest.endpoint)




## Value


`Value` is available both in configuration and run phases.

```js
angular.module('app',[])
  .value('endpoint', 'http://some.rest.endpoint') // define
  .run(function(endpoint) {
    // do something with endpoint
    // only available in run phase
  }) 
  .controller('MainCtrl', function(endpoint) {    // inject
    var vm = this;
    vm.endpoint = endpoint;                       // usage
  }); 

```

```js
<body ng-controller="MainCtrl as vm">
  <div>endpoint = { { ::vm.endpoint } }</div>
</body>

```

> 
endpoint = [http://some.rest.endpoint](http://some.rest.endpoint)




#### Syntax


- constant(name, value);
- value(name, value);
- factory(name, $getFn);
- service(name, constructor);
- provider(name, provider);



#### Remarks


Providers are singleton objects that can be injected, for example, into other services, controllers and directives. All providers are registered using different "recipes", where `Provider` is the most flexible one. All possible recipes are:

- Constant
- Value
- Factory
- Service
- Provider

Services, Factories and Providers are all lazy initialized, component is initialized only if application depends on it.

[Decorators](http://stackoverflow.com/documentation/angularjs/5255/decorators#t=201608061643096922119) are closely related to Providers. Decorators are used to intercept service or factory creation in order to change it's behavior or override (parts of) it.

