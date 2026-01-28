---
metaTitle: "AngularJS - Filters"
description: "Accessing a filtered list from outside an ng-repeat, Custom filter to remove values, Custom filter to format values, Performing filter in a child array, Using filters in a controller or service, Your First Filter"
---

# Filters



## Accessing a filtered list from outside an ng-repeat


Occasionally you will want to access the result of your filters from outside the `ng-repeat`, perhaps to indicate the number of items that have been filtered out. You can do this using `as [variablename]` syntax on the `ng-repeat`.

```js
<ul>
  <li ng-repeat="item in vm.listItems | filter:vm.myFilter as filtered">
    { {item.name} }
  </li>
</ul>
<span>Showing { {filtered.length} } of { {vm.listItems.length} }</span>

```



## Custom filter to remove values


A typical use case for a filter is to remove values from an array. In this example we pass in an array and remove any nulls found in it, returning the array.

```js
function removeNulls() {
    return function(list) {
        for (var i = list.length - 1; i >= 0; i--) {
            if (typeof list[i] === 'undefined' ||
                    list[i] === null) {
                list.splice(i, 1);
            }
        }
        return list;
    };
}

```

That would be used in the HTML like

```js
{ {listOfItems | removeNulls} }

```

or in a controller like

```js
listOfItems = removeNullsFilter(listOfItems);

```



## Custom filter to format values


Another use case for filters is to format a single value. In this example, we pass in a value and we are returned an appropriate true boolean value.

```js
function convertToBooleanValue() {
    return function(input) {
        if (typeof input !== 'undefined' &&
                input !== null &&
                (input === true || input === 1 || input === '1' || input
                        .toString().toLowerCase() === 'true')) {
            return true;
        }
        return false;
    };
}

```

Which in the HTML would be used like this:

```js
{ {isAvailable | convertToBooleanValue} }

```

Or in a controller like:

```js
var available = convertToBooleanValueFilter(isAvailable);

```



## Performing filter in a child array


This example was done in order to demonstrate how you can perform a deep filter in a **child** array without the necessity of a custom filter.

**Controller:**

```js
(function() {
  "use strict";
  angular
    .module('app', [])
    .controller('mainCtrl', mainCtrl);

  function mainCtrl() {
    var vm = this;
  
    vm.classifications = ["Saloons", "Sedans", "Commercial vehicle", "Sport car"];
    vm.cars = [  
       {  
          "name":"car1",
          "classifications":[  
             {  
                "name":"Saloons"
             },
             {  
                "name":"Sedans"
             }
          ]
       },
       {  
          "name":"car2",
          "classifications":[  
             {  
                "name":"Saloons"
             },
             {  
                "name":"Commercial vehicle"
             }
          ]
       },
       {  
          "name":"car3",
          "classifications":[  
             {  
                "name":"Sport car"
             },
             {  
                "name":"Sedans"
             }
          ]
       }
    ];
  }
})();

```

**View:**

```js
<body ng-app="app" ng-controller="mainCtrl as main">
  Filter car by classification:
  <select ng-model="classificationName"
          ng-options="classification for classification in main.classifications"></select>
  <br>
  <ul>
    <li ng-repeat="car in main.cars |
                   filter: { classifications: { name: classificationName } } track by $index"
                  ng-bind-template="{ {car.name} } - { {car.classifications | json} }">
    </li>
  </ul>
</body>

```

Check the complete <kbd>[**DEMO**](http://plnkr.co/edit/cJ9Mu7pEFufQCW6eWtbI?p=preview)</kbd>.



## Using filters in a controller or service


By injecting `$filter`, any defined filter in your Angular module may be used in controllers, services, directives or even other filters.



## Your First Filter


Filters are a special type of function that can modify how something is printed out to the page, or can be used to filter an array, or a `ng-repeat` action. You can create a filter by calling the `app.filter()` method, passing it a name and a function. See the examples below for details on syntax.

For example, let's create a filter that will change a string to be all uppercase (essentially a wrapper of the `.toUpperCase()` javascript function):

```js
var app = angular.module("MyApp", []);

// just like making a controller, you must give the
// filter a unique name, in this case "toUppercase"
app.filter('toUppercase', function(){
    // all the filter does is return a function,
    // which acts as the "filtering" function
    return function(rawString){
        // The filter function takes in the value,
        // which we modify in some way, then return
        // back.
        return rawString.toUpperCase();
    };
}); 

```

Let's take a closer look at what's happening above.

First, we're creating a filter called "toUppercase", which is just like a controller; `app.filter(...)`. Then, that filter's function returns the actual filter function. That function takes a single object, which is the object to be filtered, and should return the filtered version of the object.

**Note:** **In this situation, we're assuming the object being passed into the filter is a string, and therefore know to always use the filter only on strings. That being said, a further improvement to the filter could be made that loops through the object (if it's an array) and then makes every element that is a string uppercase.**

Now let's use our new filter in action. Our filter can be used in two ways, either in an angular template or as a javascript function (as an injected Angular reference).

### Javascript

Simply inject the angular `$filter` object to your controller, then use that to retrieve the filter function using its name.

```js
app.controller("MyController", function($scope, $filter){
    this.rawString = "Foo";
    this.capsString = $filter("toUppercase")(this.rawString);
});

```

### HTML

For an angular directive, use the pipe (`|`) symbol followed by the filter name in the directive after the actual string. For example, let's say we have a controller called `MyController` that has a string called `rawString` as a element of it.

```js
<div ng-controller="MyController as ctrl">
    <span>Capital rawString: { { ctrl.rawString | toUppercase } }</span>
</div>

```

**Editor's Note:** **Angular has a number of built in filters, including "uppercase", and the "toUppercase" filter is intended only as a demo to easily show off how filters work, but you do** not **need to built your own uppercase function.**

