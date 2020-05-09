---
metaTitle: "AngularJS - Profiling and Performance"
description: "7 Simple Performance Improvements, Bind Once, ng-if vs ng-show, Watchers, Always deregister listeners registered on other scopes other than the current scope, Scope functions and filters, Debounce Your Model"
---

# Profiling and Performance



## 7 Simple Performance Improvements


### **<strong>1) Use ng-repeat sparingly**</strong>

Using [**`ng-repeat`**](https://docs.angularjs.org/api/ng/directive/ngRepeat) in views generally results in poor performance, particularly when there are nested `ng-repeat`'s.

**This is super slow!**

```js
<div ng-repeat="user in userCollection">
  <div ng-repeat="details in user">
    { {details} }
  </div>
</div>

```

Try to avoid nested repeats as much as possible. One way to improve the performance of `ng-repeat` is to use `track by $index` (or some other id field). By default, `ng-repeat` tracks the whole object. With `track by`, Angular watches the object only by the `$index` or object id.

```js
<div ng-repeat="user in userCollection track by $index">
  { {user.data} }
</div>

```

**Use other approaches** like [pagination](http://stackoverflow.com/questions/11581209/pagination-on-a-list-using-ng-repeat), [virtual scrolls](http://stackoverflow.com/questions/33259241/how-can-i-make-an-virtual-scroll-with-angularjs), [infinite scrolls](http://stackoverflow.com/questions/21674266/angularjs-infinite-scroll-in-a-container) or [limitTo: begin](http://stackoverflow.com/a/38686531/3612903) whenever possible to avoid iterating over large collections.

### **2) Bind once**

Angular has bidirectional data binding. It comes with a cost of being slow if used too much.

**Slower Performance**

```js
<!-- Default data binding has a performance cost -->
<div>{ { my.data } }</div>

```

**Faster Performance** (AngularJS >= 1.3)

```js
<!-- Bind once is much faster -->
<div>{ { ::my.data } }</div>

<div ng-bind="::my.data"></div>

<!-- Use single binding notation in ng-repeat where only list display is needed  -->
<div ng-repeat="user in ::userCollection">
  { {::user.data} }
</div>

```

Using the "bind once" notation tells Angular to wait for the value to stabilize after the first series of digest cycles.  Angular will use that value in the DOM, then remove all watchers so that it becomes a static value and is no longer bound to the model.

The `{ {} }` is much slower.

This `ng-bind` is a directive and will place a watcher on the passed variable. So the `ng-bind` will only apply, when the passed value does actually change.

The brackets on the other hand will be dirty checked and refreshed in every `$digest`, even if it's not necessary.

### **3) Scope functions and filters take time**

AngularJS has a digest loop. All your functions are in a view and filters are executed every time the digest cycle runs. The digest loop will be executed whenever the model is updated and it can slow down your app (filter can be hit multiple times before the page is loaded).

**Avoid this:**

```js
<div ng-controller="bigCalulations as calc">
  <p>{ {calc.calculateMe()} }</p>
  <p>{ {calc.data | heavyFilter} }</p>
</div>

```

**Better approach**

```js
<div ng-controller="bigCalulations as calc">
  <p>{ {calc.preCalculatedValue} }</p>
  <p>{ {calc.data | lightFilter} }</p>
</div>

```

Where the controller can be:

```js
app.controller('bigCalulations', function(valueService) {
    // bad, because this is called in every digest loop
    this.calculateMe = function() {
        var t = 0;
        for(i = 0; i < 1000; i++) {
            t += i;
        }
        return t;
    }
    // good, because this is executed just once and logic is separated in service to    keep the controller light
    this.preCalulatedValue = valueService.valueCalculation(); // returns 499500
});

```

### **4) Watchers**

Watchers tremendously drop performance. With more watchers, the digest loop will take longer and the UI will slow down. If the watcher detects change, it will kick off the digest loop and re-render the view.

There are three ways to do manual watching for variable changes in Angular.

`$watch()` - watches for value changes

`$watchCollection()` - watches for changes in collection (watches more than regular `$watch`)

`$watch(..., true)` - **Avoid this** as much as possible, it will perform "deep watch" and will decline the performance (watches more than `watchCollection`)

Note that if you are binding variables in the view you are creating new watches - use `{ {::variable} }` to prevent creating a watch, especially in loops.

As a result you need to track how many watchers you are using. You can count the watchers with this script (credit to [@Words Like Jared](http://stackoverflow.com/users/569302) [Number of watchers](http://stackoverflow.com/questions/18499909/how-to-count-total-number-of-watches-on-a-page))

```js
(function() {
    var root = angular.element(document.getElementsByTagName('body')),
        watchers = [],
        f = function(element) {
        angular.forEach(['$scope', '$isolateScope'], function(scopeProperty) {
            if(element.data() && element.data().hasOwnProperty(scopeProperty)) {
                angular.forEach(element.data()[scopeProperty].$$watchers, function(watcher) {
                watchers.push(watcher);
                });
            }
        });

        angular.forEach(element.children(), function(childElement) {
            f(angular.element(childElement));
        });
    };
 
    f(root);
 
    // Remove duplicate watchers
    var watchersWithoutDuplicates = [];
    angular.forEach(watchers, function(item) {
        if(watchersWithoutDuplicates.indexOf(item) < 0) {
            watchersWithoutDuplicates.push(item);
        }
    });
    console.log(watchersWithoutDuplicates.length);
})();

```

### **5) ng-if / ng-show**

These functions are very similar in behavior. [**`ng-if`**](https://docs.angularjs.org/api/ng/directive/ngIf) removes elements from the DOM while [**`ng-show`**](https://docs.angularjs.org/api/ng/directive/ngShow) only hides the elements but keeps all handlers. If you have parts of the code you do not want to show, use `ng-if`.

It depends on the type of usage, but often one is more suitable than the other.

<li>
If the element is not needed, use `ng-if`
</li>
<li>
To quickly toggle on/off, use `ng-show/ng-hide`

```js
<div ng-repeat="user in userCollection">
  <p ng-if="user.hasTreeLegs">I am special<!-- some complicated DOM --></p>
  <p ng-show="user.hasSubscribed">I am awesome<!-- switch this setting on and off --></p>
</div>

```


</li>

If in doubt - use `ng-if` and test!

### **6) Disable debugging**

By default, bind directives and scopes leave extra classes and markup in the code to assist with various debugging tools. Disabling this option means that you no longer render these various elements during the digest cycle.

```js
angular.module('exampleApp', []).config(['$compileProvider', function ($compileProvider) {
    $compileProvider.debugInfoEnabled(false);
}]);

```

### **7) Use dependency injection to expose your resources**

Dependency Injection is a software design pattern in which an object is given its dependencies, rather than the object creating them itself. It is about removing the hard-coded dependencies and making it possible to change them whenever needed.

You might wonder about the performance cost associated with such string parsing of all injectable functions. Angular takes care of this by caching the $inject property after the first time. So this doesn’t happen everytime a function needs to be invoked.

PRO TIP: If you are looking for the approach with the best performance, go with the $inject property annotation approach. This approach entirely avoids the function definition parsing because this logic is wrapped within the following check in the annotate function: if (!($inject = fn.$inject)). If $inject is already available, no parsing required!

```js
var app = angular.module('DemoApp', []);

var DemoController = function (s, h) {
    h.get('https://api.github.com/users/angular/repos').success(function (repos) {
        s.repos = repos;
    });
}
// $inject property annotation
DemoController['$inject'] = ['$scope', '$http'];

app.controller('DemoController', DemoController);

```

PRO TIP 2: You can add an `ng-strict-di` directive on the same element as `ng-app` to opt into strict DI mode which will throw an error whenever a service tries to use implicit annotations. Example:

```js
<html ng-app="DemoApp" ng-strict-di>

```

Or if you use manual bootstrapping:

```js
angular.bootstrap(document, ['DemoApp'], {
    strictDi: true
});

```



## Bind Once


Angular has reputation for having awesome bidirectional data binding. By default, Angular continuously synchronizes values bound between model and view components any time data changes in either the model or view component.

This comes with a cost of being a bit slow if used too much.  This will have a larger performance hit:

**Bad performance:** `{ {my.data} }`

Add two colons `::` before the variable name to use one-time binding. In this case, the value only gets updated once my.data is defined. You are explicitly pointing not to watch for data changes. Angular won't perform any value checks, resulting with fewer expressions being evaluated on each digest cycle.

**Good performance examples using one-time binding**

```js
{ {::my.data} }
<span ng-bind="::my.data"></span>
<span ng-if="::my.data"></span>
<span ng-repeat="item in ::my.data">{ {item} }</span>
<span ng-class="::{ 'my-class': my.data }"></div>

```

**Note:** This however removes the bi-directional data binding for `my.data`, so whenever this field changes in your application, the same won't be reflected in the view automatically. So **use it only for values that won't change throughout the lifespan of your application**.



## ng-if vs ng-show


These functions are very similar in behaviour. The difference is that `ng-if` removes elements from the DOM. If there are large parts of the code that will not be shown, then `ng-if` is the way to go. `ng-show` will only hide the elements but will keep all the handlers.

### ng-if

The ngIf directive removes or recreates a portion of the DOM tree based on an expression. If the expression assigned to ngIf evaluates to a false value then the element is removed from the DOM, otherwise a clone of the element is reinserted into the DOM.

### ng-show

The ngShow directive shows or hides the given HTML element based on the expression provided to the ngShow attribute. The element is shown or hidden by removing or adding the ng-hide CSS class onto the element.

### Example

```js
<div ng-repeat="user in userCollection">
  <p ng-if="user.hasTreeLegs">I am special
    <!-- some complicated DOM -->
  </p>
  <p ng-show="user.hasSubscribed">I am aweosme
    <!-- switch this setting on and off -->
  </p>
</div>

```

### Conclusion

It depends from the type of usage, but often one is more suitable than the other (e.g., if 95% of the time the element is not needed, use `ng-if`; if you need to toggle the DOM element's visibility, use `ng-show`).

When in doubt, use `ng-if` and test!

**Note**: `ng-if` creates a new isolated scope, whereas `ng-show` and `ng-hide` don't. Use `$parent.property` if parent scope property is not directly accessible in it.



## Watchers


Watchers needed for watch some value and detect that this value is changed.

After call `$watch()` or `$watchCollection` new watcher add to internal watcher collection in current scope.

### So, what is watcher?

Watcher is a simple function, which is called on every digest cycle, and returns some value. Angular checks the returned value, if it is not the same as it was on the previous call - a callback that was passed in second parameter to function `$watch()` or `$watchCollection` will be executed.

```js
(function() {
  angular.module("app", []).controller("ctrl", function($scope) {
    $scope.value = 10;
    $scope.$watch(
      function() { return $scope.value; },
      function() { console.log("value changed"); }
    );
  }
})();

```

Watchers are performance killers. The more watchers you have, the longer they take to make a digest loop, the slower UI. If a watcher detects changes, it will kick off the digest loop (recalculation on all screen)

There are three ways to do manual watch for variable changes in Angular.

`$watch()` - just watches for value changes

`$watchCollection()` - watches for changes in collection (watches more than regular $watch)

`$watch(..., true)` - **Avoid this** as much as possible, it will perform "deep watch" and will kill the performance (watches more than watchCollection)

Note that if you are binding variables in the view, you are creating new watchers - use `{ {::variable} }` not to create watcher, especially in loops

As a result you need to track how many watchers are you using. You can count the watchers with this script (credit to [@Words Like Jared](http://stackoverflow.com/users/569302) - [How to count total number of watches on a page?](http://stackoverflow.com/questions/18499909/))

```js
(function() {
  var root = angular.element(document.getElementsByTagName("body")),
      watchers = [];

  var f = function(element) {

    angular.forEach(["$scope", "$isolateScope"], function(scopeProperty) {
      if(element.data() && element.data().hasOwnProperty(scopeProperty)) {
        angular.forEach(element.data()[scopeProperty].$$watchers, function(watcher) {
          watchers.push(watcher);
        });
      }
    });

    angular.forEach(element.children(), function(childElement) {
      f(angular.element(childElement));
    });

  };

  f(root);

  // Remove duplicate watchers
  var watchersWithoutDuplicates = [];
  angular.forEach(watchers, function(item) {
    if(watchersWithoutDuplicates.indexOf(item) < 0) {
      watchersWithoutDuplicates.push(item);
    }
  });

  console.log(watchersWithoutDuplicates.length);

})();

```

If you don't want to create your own script, there is an open source utility called [ng-stats](https://github.com/kentcdodds/ng-stats) that uses a real-time chart embedded into the page to give you insight into the number of watches Angular is managing, as well as the frequency and duration of digest cycles over time. The utility exposes a global function named `showAngularStats` that you can call to configure how you want the chart to work.

```js
showAngularStats({
  "position": "topleft",
  "digestTimeThreshold": 16,
  "autoload": true,
  "logDigest": true,
  "logWatches": true
});

```

The example code above displays the following chart on the page automatically ([interactive demo](http://kentcdodds.com/ng-stats/)).

[<img src="http://i.stack.imgur.com/x5e1A.png" alt="screenshot of ng-stats chart" />](http://i.stack.imgur.com/x5e1A.png)



## Always deregister listeners registered on other scopes other than the current scope


You must always unregister scopes other then your current scope as shown below:

```js
//always deregister these
$rootScope.$on(...);
$scope.$parent.$on(...);

```

You don't have to deregister listners on current scope as angular would take care of it:

```js
//no need to deregister this
$scope.$on(...);

```

`$rootScope.$on` listeners will remain in memory if you navigate to another controller. This will create a memory leak if the controller falls out of scope.

**Don't**

```js
angular.module('app').controller('badExampleController', badExample);
badExample.$inject = ['$scope', '$rootScope'];

function badExample($scope, $rootScope) {
    $rootScope.$on('post:created', function postCreated(event, data) {});
}

```

**Do**

```js
angular.module('app').controller('goodExampleController', goodExample);
goodExample.$inject = ['$scope', '$rootScope'];

function goodExample($scope, $rootScope) {
    var deregister = $rootScope.$on('post:created', function postCreated(event, data) {});

    $scope.$on('$destroy', function destroyScope() {
        deregister();
    });
}

```



## Scope functions and filters


AngularJS has digest loop and all your functions in a view and filters are executed every time the digest cycle is run. The digest loop will be executed whenever the model is updated and it can slow down your app (filter can be hit multiple times, before the page is loaded).

**You should avoid this:**

```js
<div ng-controller="bigCalulations as calc">
  <p>{ {calc.calculateMe()} }</p>
  <p>{ {calc.data | heavyFilter} }</p>
</div>

```

**Better approach**

```js
<div ng-controller="bigCalulations as calc">
  <p>{ {calc.preCalculatedValue} }</p>
  <p>{ {calc.data | lightFilter} }</p>
</div>

```

Where controller sample is:

```js
.controller("bigCalulations", function(valueService) {
  // bad, because this is called in every digest loop
  this.calculateMe = function() {
    var t = 0;
    for(i = 0; i < 1000; i++) {
      t = t + i;
    }
    return t;
  }
  //good, because it is executed just once and logic is separated in service to    keep the controller light
  this.preCalulatedValue = valueService.caluclateSumm(); // returns 499500
});

```



## Debounce Your Model


```js
<div ng-controller="ExampleController">
    <form name="userForm">
        Name:
        <input type="text" name="userName"
           ng-model="user.name"
           ng-model-options="{ debounce: 1000 }" />
            <button ng-click="userForm.userName.$rollbackViewValue();   user.name=''">Clear</button><br />
    </form>
    <pre>user.name = </pre>
</div>

```

The above example we are setting a debounce value of 1000 milliseconds which is 1 second. This is a considerable delay, but will prevent the input from repeatedly thrashing `ng-model` with many `$digest` cycles.

By using debounce on your input fields and anywhere else where an instant update is not required, you can increase the performance of your Angular apps quite substantially. Not only can you delay by time, but you can also delay when the action gets triggered. If you don’t want to update your ng-model on every keystroke, you can also update on blur as well.

