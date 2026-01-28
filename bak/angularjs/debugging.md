---
metaTitle: "AngularJS - Debugging"
description: "Using ng-inspect chrome extension, Basic debugging in markup, Getting the Scope of element"
---

# Debugging



## Using ng-inspect chrome extension


[ng-inspect](https://chrome.google.com/webstore/detail/ng-inspect-for-angularjs/cidepfmbgngpdapgncfhpecbdhmnnemf) is a light weight Chrome extension for debugging AngularJS applications.

When a node is selected from the elements panel, the scope related info is displayed in the ng-inspect panel.

[<img src="https://i.stack.imgur.com/BQ1xj.jpg" alt="enter image description here" />](https://i.stack.imgur.com/BQ1xj.jpg)

Exposes few global variables for quick access of `scope/isolateScope`.

> 

```js
$s      -- scope of the selected node
$is     -- isolateScope of the selected node
$el     -- jQuery element reference of the selected node (requiers jQuery)
$events -- events present on the selected node (requires jQuery)

```




[<img src="https://i.stack.imgur.com/jn3Zo.jpg" alt="enter image description here" />](https://i.stack.imgur.com/jn3Zo.jpg)

Provides easy access to Services/Factories.

Use `$get()` to retrieve the instance of a service/factory by name.

[<img src="https://i.stack.imgur.com/8sRj5.jpg" alt="enter image description here" />](https://i.stack.imgur.com/8sRj5.jpg)

Performance of the application can be monitored by counting the no.of scopes,isolateScopes, watchers and listeners on the application.

Use `$count()` to get the count of scopes, isolateScopes, watchers and listeners.

[<img src="https://i.stack.imgur.com/Du8Za.jpg" alt="enter image description here" />](https://i.stack.imgur.com/Du8Za.jpg)

> 
Note: This extension will work only when the debugInfo is enabled.


Download ng-inspect [here](https://chrome.google.com/webstore/detail/ng-inspect-for-angularjs/cidepfmbgngpdapgncfhpecbdhmnnemf)



## Basic debugging in markup


**Scope testing & output of model**

```js
<div ng-app="demoApp" ng-controller="mainController as ctrl">
    { {$id} }
    <ul>
        <li ng-repeat="item in ctrl.items">
            { {$id} }<br/>
            { {item.text} }
        </li>
    </ul>
    { {$id} }
    <pre>
       { {ctrl.items | json : 2} }
    </pre>
</div>


angular.module('demoApp', [])
.controller('mainController', MainController);

function MainController() {
    var vm = this;
    vm.items = [{
        id: 0,
        text: 'first'
    },
    {
        id: 1,
        text: 'second'
    },
    {
        id: 2,
        text: 'third'
    }];
}

```

Sometimes it can help to see if there is a new scope to fix scoping issues. `$scope.$id` can be used in an expression everywhere in your markup to see if there is a new $scope.

In the example you can see that outside of the ul-tag is the same scope ($id=2) and inside the `ng-repeat` there are new child scopes for each iteration.

An output of the model in a pre-tag is useful to see the current data of your model. The `json` filter creates a nice looking formatted output.
The pre-tag is used because inside that tag any new-line character `\n` will be correctly displayed.

[demo](https://jsfiddle.net/awolf2904/jc27f3c4/)



## Getting the Scope of element


In an angular app everything goes around scope, if we could get an elements scope then it is easy to debug the angular app.
How to access the scope of element:

```js
angular.element(myDomElement).scope();
e.g.
angular.element(document.getElementById('yourElementId')).scope()  //accessing by ID

```

Getting the scope of  the controller:-

```

angular.element('[ng-controller=ctrl]').scope()

```

Another easy way to access a DOM element from the console (as jm mentioned) is to click on it in the 'elements' tab, and it automatically gets stored as $0.

```js
angular.element($0).scope();

```

