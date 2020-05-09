---
metaTitle: "AngularJS - digest loop walkthrough"
description: "$digest and $watch, the $scope tree, two way data binding"
---

# digest loop walkthrough



## $digest and $watch


Implementing two-way-data-binding, to achieve the result from the previous example, could be done with two core functions:

- **$digest** is called after a user interaction (binding DOM=>variable)
- **$watch** sets a callback to be called after variable changes (binding variable=>DOM)

****note: this is example is a demonstration, not the actual angular code****

```js
<input id="input"/>
<span id="span"></span>

```

The two functions we need:

```js
var $watches = [];
function $digest(){
    $watches.forEach(function($w){
        var val = $w.val();
        if($w.prevVal !== val){
            $w.callback(val, $w.prevVal);
            $w.prevVal = val;
        }
    })
}
function $watch(val, callback){
    $watches.push({val:val, callback:callback, prevVal: val() })
}

```

Now we could now use these functions to hook up a variable to the DOM (angular comes with built-in directives which will do this for you):

```js
var realVar;    
//this is usually done by ng-model directive
input1.addEventListener('keyup',function(e){
    realVar=e.target.value; 
    $digest()
}, true);

//this is usually done with { {expressions} } or ng-bind directive
$watch(function(){return realVar},function(val){
    span1.innerHTML = val;
});

```

Off-course, the real implementations are more complex, and support parameters such as **which element** to bind to, and **what variable** to use

A running example could be found here: [https://jsfiddle.net/azofxd4j/](https://jsfiddle.net/azofxd4j/)



## the $scope tree


The previous example is good enough when we need to bind a single html element, to a single variable.

In reality - we need to bind many elements to many variables:

```js
<span ng-repeat="number in [1,2,3,4,5]">{ {number} }</span>

```

This `ng-repeat` binds 5 elements to 5 variables called `number`, with a different value for each of them!

The way angular achieves this behavior is using a separate context for each element which needs separate variables. This context is called a scope.

Each scope contains properties, which are the variables bound to the DOM, and the `$digest` and `$watch` functions are implemented as methods of the scope.

The DOM is a tree, and variables need to be used in different levels of the tree:

```js
<div>
    <input ng-model="person.name" />
    <span ng-repeat="number in [1,2,3,4,5]">{ {number} } { {person.name} }</span>
</div>

```

But as we saw, the context(or scope) of variables inside `ng-repeat` is different to the context above it.
To solve this - angular implements scopes as a tree.

Each scope has an array of children, and calling its `$digest` method will run all of its children's `$digest` method.

This way - after changing the input - `$digest` is called for the div's scope, which then runs the `$digest` for its 5 children - which will update its content.

A simple implementation for a scope, could look like this:

```js
function $scope(){
    this.$children = [];
    this.$watches = [];
}

$scope.prototype.$digest = function(){
    this.$watches.forEach(function($w){
        var val = $w.val();
        if($w.prevVal !== val){
            $w.callback(val, $w.prevVal);
          $w.prevVal = val;
        }
    });
    this.$children.forEach(function(c){
        c.$digest();
    });
}

$scope.prototype.$watch = function(val, callback){
    this.$watches.push({val:val, callback:callback, prevVal: val() })
}

```

****note: this is example is a demonstration, not the actual angular code****



## two way data binding


Angular has some magic under its hood. it enables binding [DOM](https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model) to real js variables.

Angular uses a loop, named the "**digest loop**", which is called after any change of a variable - calling callbacks which update the DOM.

For example, the `ng-model` directive attaches a `keyup` [eventListener](https://developer.mozilla.org/en-US/docs/Web/Events) to this input:

```js
<input ng-model="variable" />

```

Every time the `keyup` event fires, the **digest loop** starts.

At some point, the **digest loop** iterates over a callback which updates the contents of this span:

```js
<span>{ {variable} }</span>

```

The basic life-cycle of this example, summarizes (very Schematically) how angular works::

<li>Angular scans html
<ul>
1. `ng-model` directive creates a `keyup` listener on input
1. `expression` inside span adds a callback to **digest cycle**
</ul>
</li>
<li>User interacts with input
<ul>
1. `keyup` listener starts **digest cycle**
1. **digest cycle** calles the callback
1. Callback updates span's contents
</ul>
</li>

- `keyup` listener starts **digest cycle**
- **digest cycle** calles the callback
- Callback updates span's contents



#### Syntax


- $scope.$watch(watchExpression, callback, [deep compare])
- $scope.$digest()
- $scope.$apply([exp])

