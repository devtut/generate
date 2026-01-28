---
metaTitle: "AngularJS - Lazy loading"
description: "Preparing your project for lazy loading, Usage, Usage with router, Using dependency injection, Using the directive"
---

# Lazy loading



## Preparing your project for lazy loading


After including `oclazyload.js` in your index file, declare `ocLazyLoad` as a dependency in `app.js`

```js
//Make sure you put the correct dependency! it is spelled different than the service!
angular.module('app', [
 'oc.lazyLoad',
 'ui-router'
])

```



## Usage


In order to lazily load files inject the `$ocLazyLoad` service into a controller or another service

```js
.controller('someCtrl', function($ocLazyLoad) {
 $ocLazyLoad.load('path/to/file.js').then(...);
});

```

Angular modules will be automatically loaded into angular.

Other variation:

```js
$ocLazyLoad.load([
  'bower_components/bootstrap/dist/js/bootstrap.js',
  'bower_components/bootstrap/dist/css/bootstrap.css',
  'partials/template1.html'
]);

```

For a complete list of variations visit the [official](https://oclazyload.readme.io/docs/oclazyload-service) documentation



## Usage with router


### UI-Router:

```js
.state('profile', {
 url: '/profile',
 controller: 'profileCtrl as vm'
 resolve: {
  module: function($ocLazyLoad) {
   return $ocLazyLoad.load([
    'path/to/profile/module.js',
    'path/to/profile/style.css'
   ]);
  }
 }
});

```

### ngRoute:

```js
.when('/profile', {
     controller: 'profileCtrl as vm'
     resolve: {
      module: function($ocLazyLoad) {
       return $ocLazyLoad.load([
        'path/to/profile/module.js',
        'path/to/profile/style.css'
       ]);
      }
     }
    });

```



## Using dependency injection


The following syntax allows you to specify dependencies in your `module.js` instead of explicit specification when using the service

```js
//lazy_module.js
angular.module('lazy', [
 'alreadyLoadedDependency1',
 'alreadyLoadedDependency2',
 ...
 [
  'path/to/lazily/loaded/dependency.js',
  'path/to/lazily/loaded/dependency.css'
 ]
]);

```

**Note**: this syntax will only work for lazily loaded modules!



## Using the directive


```js
<div oc-lazy-load="['path/to/lazy/loaded/directive.js', 'path/to/lazy/loaded/directive.html']">

<!-- myDirective available here -->
<my-directive></my-directive>

</div>

```



#### Remarks


1. If your lazy loaded dependencies require other lazy loaded dependencies make sure you load them in the right order!

```js
angular.module('lazy', [
 'alreadyLoadedDependency1',
 'alreadyLoadedDependency2',
 ...
 {
  files: [
  'path/to/lazily/loaded/dependency1.js',
  'path/to/lazily/loaded/dependency2.js', //<--- requires lazily loaded dependency1
  'path/to/lazily/loaded/dependency.css'
  ],
  serie: true //Sequential load instead of parallel
 }
]);

```

