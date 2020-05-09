---
metaTitle: "AngularJS - Built-in directives"
description: "Angular expressions - Text vs. Number, ngIf, ngRepeat, ngCloak, Built-In Directives Cheat Sheet, ng-model-options, ngInclude, ngCopy, ngClick, ngPaste, ngOptions, ngModel, ngClass, ngSrc, ngList, ngShow and ngHide, ngDblclick, ngPattern, ngHref, ngMouseenter and ngMouseleave, ngDisabled, ngRequired, ngValue"
---

# Built-in directives




## Angular expressions - Text vs. Number


This example demonstrates how Angular expressions are evaluated when using `type="text"` and `type="number"` for the input element.
Consider the following controller and view:

**Controller**

```

var app = angular.module('app', []);

 app.controller('ctrl', function($scope) {
     $scope.textInput = {
         value: '5'
     };     
     $scope.numberInput = {
         value: 5
     };
 });

```

**View**

```js
<div ng-app="app" ng-controller="ctrl">
    <input type="text" ng-model="textInput.value">
    { { textInput.value + 5 } }
    <input type="number" ng-model="numberInput.value">
    { { numberInput.value + 5 } }
</div>

```


- When using `+` in an expression bound to **text** input, the operator will **concatenate** the strings (first example), displaying 55 on the screen`*`.
- When using `+` in an expression bound to **number** input, the operator return the **sum** of the numbers (second example), displaying 10 on the screen`*`.

`*` - That is until the user changes the value in the input field, afterward the display will change accordingly.

[Working Example](https://jsfiddle.net/fkfd9tar/)



## ngIf


[**`ng-if`**](https://docs.angularjs.org/api/ng/directive/ngIf) is a directive similar to [**`ng-show`**](https://docs.angularjs.org/api/ng/directive/ngShow) but inserts or removes the element from the DOM instead of simply hiding it. Angular 1.1.5 introduced ng-If directive. You can Use ng-if directive above 1.1.5 versions. This is useful because Angular will not process digests for elements inside a removed `ng-if` reducing the workload of Angular especially for complex data bindings.

Unlike `ng-show`, the `ng-if` directive creates a child scope which uses prototypal inheritance. This means that setting a primitive value on the child scope will not apply to the parent. To set a primitive on the parent scope the `$parent` property on the child scope will have to be used.

### JavaScript

```js
angular.module('MyApp', []);

angular.module('MyApp').controller('myController', ['$scope', '$window', function myController($scope, $window) {
    $scope.currentUser= $window.localStorage.getItem('userName');
}]);

```

### View

```js
<div ng-controller="myController">
    <div ng-if="currentUser">
        Hello, { {currentUser} }
    </div>
    <div ng-if="!currentUser">
        <a href="/login">Log In</a>
        <a href="/register">Register</a>
    </div>
</div>

```

### DOM If `currentUser` Is Not Undefined

```js
<div ng-controller="myController">
    <div ng-if="currentUser">
        Hello, { {currentUser} }
    </div>
    <!-- ng-if: !currentUser -->
</div>

```

### DOM If `currentUser` Is Undefined

```js
<div ng-controller="myController">
    <!-- ng-if: currentUser -->
    <div ng-if="!currentUser">
        <a href="/login">Log In</a>
        <a href="/register">Register</a>
    </div>
</div>

```

[Working Example](https://jsfiddle.net/beekalam/4wwq1a3w/)

### Function Promise

The ngIf directive accepts functions as well, which logically require to return true or false.

```js
<div ng-if="myFunction()">
    <span>Span text</span>
</div>

```

The span text will only appear if the function returns true.

```js
$scope.myFunction = function() {
    var result = false;
    // Code to determine the boolean value of result 
    return result;
};

```

As any Angular expression the function accepts any kind of variables.



## ngRepeat


`ng-repeat` is a built in directive in Angular which lets you iterate an array or an object and gives you the ability to repeat an element once for each item in the collection.

**ng-repeat an array**

```js
<ul>
    <li ng-repeat="item in itemCollection">
       { {item.Name} }
    </li>
</ul>

```

Where: <br />
**item** = individual item in the collection<br />
**itemCollection** = The array you are iterating

**ng-repeat an object**

```js
<ul>
    <li ng-repeat="(key, value) in myObject">
       { {key} } : { {value} }
    </li>
</ul>

```

Where: <br />
**key** = the property name<br />
**value** = the value of the property<br />
**myObject** = the object you are iterating

**filter your ng-repeat by user input**

```js
<input type="text" ng-model="searchText">
<ul>
    <li ng-repeat="string in stringArray | filter:searchText">
       { {string} }
    </li>
</ul>

```

Where: <br />
**searchText** = the text that the user wants to filter the list by<br />
**stringArray** = an array of strings, e.g. `['string', 'array']`<br />

You can also display or reference the filtered items elsewhere by assigning the filter output an alias with `as aliasName`, like so:

```js
<input type="text" ng-model="searchText">
<ul>
    <li ng-repeat="string in stringArray | filter:searchText as filteredStrings">
       { {string} }
    </li>
</ul>
<p>There are { {filteredStrings.length} } matching results</p>

```

**ng-repeat-start and ng-repeat-end**

To repeat multiple DOM elements by defining a start and an end point you can use the `ng-repeat-start` and `ng-repeat-end` directives.

```js
<ul>
    <li ng-repeat-start="item in [{a: 1, b: 2}, {a: 3, b:4}]">
        { {item.a} }
    </li>
    <li ng-repeat-end>
        { {item.b} }
    </li>
</ul>

```

Output:

- 1
- 2
- 3
- 4

It is important to always close `ng-repeat-start` with `ng-repeat-end`.

**Variables**

`ng-repeat` also exposes these variables inside the expression

|Variable|Type|Details
|---|---|---|---|---|---|---|---|---|---
|`$index`|Number|Equals to the index of the current iteration ($index===0 will evaluate to true at the first iterated element; see `$first`)
|`$first`|Boolean|Evaluates to true at the first iterated element
|`$last`|Boolean|Evaluates to true at the last iterated element
|`$middle`|Boolean|Evaluates to true if the element is between the `$first` and `$last`
|`$even`|Boolean|Evaluates to true at an even numbered iteration (equivalent to `$index%2===0`)
|`$odd`|Boolean|Evaluates to true at an odd numbered iteration (equivalent to `$index%2===1`)

**Performance considerations**

Rendering `ngRepeat` can become slow, especially when using large collections.

If the objects in the collection have an identifier property, you should always `track by` the identifier instead of the whole object, which is the default functionality. If no identifier is present, you can always use the built-in `$index`.

```js
<div ng-repeat="item in itemCollection track by item.id">
<div ng-repeat="item in itemCollection track by $index">

```

**Scope of ngRepeat**

`ngRepeat` will always create an isolated child scope so care must be taken if the parent scope needs to be accessed inside the repeat.

Here is a simple example showing how you can set a value in your parent scope from a click event inside of `ngRepeat`.

```js
scope val:  { {val} }<br/>
ctrlAs val: { {ctrl.val} }
<ul>
    <li ng-repeat="item in itemCollection">
        <a href="#" ng-click="$parent.val=item.value; ctrl.val=item.value;">
            { {item.label} } { {item.value} }
        </a>
    </li>
</ul>

$scope.val = 0;
this.val = 0;

$scope.itemCollection = [{
    id: 0,
    value: 4.99,
    label: 'Football'
},
{
    id: 1,
    value: 6.99,
    label: 'Baseball'
},
{
    id: 2,
    value: 9.99,
    label: 'Basketball'
}];

```

If there was only `val = item.value` at `ng-click` it won't update the `val` in the parent scope because of the isolated scope. That's why the parent scope is accessed with `$parent` reference or with the `controllerAs` syntax (e.g. `ng-controller="mainController as ctrl"`).

**Nested ng-repeat**

You can also use nested ng-repeat.

```js
<div ng-repeat="values in test">
    <div ng-repeat="i in values">
      [{ {$parent.$index} },{ {$index} }] { {i} }
    </div>
</div>

var app = angular.module("myApp", []);
app.controller("ctrl", function($scope) {
  $scope.test = [
    ['a', 'b', 'c'],
    ['d', 'e', 'f']
  ];
});

```

Here to access the index of parent ng-repeat inside child ng-repeat, you can use `$parent.$index`.



## ngCloak


> 
<p>The `ngCloak` directive is used to prevent the Angular html template
from being briefly displayed by the browser in its raw (uncompiled)
form while your application is loading. - [View source](https://docs.angularjs.org/api/ng/directive/ngCloak)</p>


**HTML**

```js
<div ng-cloak>
  <h1>Hello { { name } }</h1>
</div>

```

`ngCloak` can be applied to the body element, but the preferred
usage is to apply multiple ngCloak directives to small portions of the
page to permit progressive rendering of the browser view.

The `ngCloak` directive has no parameters.

See also: [Preventing flickering](http://www.ng-newsletter.com/25-days-of-angular/day-3)



## Built-In Directives Cheat Sheet


`ng-app`  Sets the AngularJS section.

`ng-init`  Sets a default variable value.

`ng-bind`    Alternative to { { } } template.

`ng-bind-template`    Binds multiple expressions to the view.

`ng-non-bindable`    States that the data isn't bindable.

`ng-bind-html`    Binds inner HTML property of an HTML element.

`ng-change`    Evaluates specified expression when the user changes the input.

`ng-checked`    Sets the checkbox.

`ng-class`    Sets the css class dynamically.

`ng-cloak`    Prevents displaying the content until AngularJS has taken control.

`ng-click`    Executes a method or expression when element is clicked.

`ng-controller`   Attaches a controller class to the view.

`ng-disabled`    Controls the form element's disabled property

`ng-form`    Sets a form

`ng-href`    Dynamically bind AngularJS variables to the href attribute.

`ng-include`    Used to fetch, compile and include an external HTML fragment to your page.

`ng-if`    Remove or recreates an element in the DOM depending on an expression

`ng-switch`    Conditionally switch control based on matching expression.

`ng-model`    Binds an input,select, textarea etc elements with model property.

`ng-readonly`    Used to set readonly attribute to an element.

`ng-repeat`    Used to loop through each item in a collection to create a new template.

`ng-selected`    Used to set selected option in  element.

`ng-show/ng-hide`    Show/Hide elements based on an expression.

`ng-src`    Dynamically bind AngularJS variables to the src attribute.

`ng-submit`    Bind angular expressions to onsubmit events.

`ng-value`    Bind angular expressions to the value of .

`ng-required`    Bind angular expressions to onsubmit events.

`ng-style`    Sets CSS style on an HTML element.

`ng-pattern`    Adds the pattern validator to ngModel.

`ng-maxlength`    Adds the maxlength validator to ngModel.

`ng-minlength`    Adds the minlength validator to ngModel.

`ng-classeven`    Works in conjunction with ngRepeat and take effect only on odd (even) rows.

`ng-classodd`    Works in conjunction with ngRepeat and take effect only on odd (even) rows.

`ng-cut`    Used to specify custom behavior on cut event.

`ng-copy`    Used to specify custom behavior on copy event.

`ng-paste`    Used to specify custom behavior on paste event.

`ng-options`    Used to dynamically generate a list of  elements for the  element.

`ng-list`    Used to convert string into list based on specified delimiter.

`ng-open`    Used to set the open attribute on the element, if the expression
inside ngOpen is truthy.

[Source (edited a bit)](http://www.techstrikers.com/AngularJS/angularjs-built-in-directives.php)



## ng-model-options


`ng-model-options` allows to change the default behavior of `ng-model`, this directive allows to register events that will fire when the ng-model is updated and to attach a debounce effect.

This directive accepts an expression that will evaluate to a definition object or a reference to a scope value.

**Example:**

```js
<input type="text" ng-model="myValue" ng-model-options="{'debounce': 500}">

```

The above example will attach a debounce effect of 500 milliseconds on `myValue`, which will cause the model to update 500 ms after the user finished typing over the `input` (that is, when the `myValue` finished updating).

Available object properties

<li>
`updateOn`: specifies which event should be bound to the input

```js
ng-model-options="{ updateOn: 'blur'}" // will update on blur

```


</li>
<li>
`debounce`: specifies a delay of some millisecond towards the model update

```js
ng-model-options="{'debounce': 500}" // will update the model after 1/2 second

```


</li>
<li>
`allowInvalid`: a boolean flag allowing for an invalid value to the model, circumventing default form validation, by default these values would be treated as `undefined`.
</li>
<li>
`getterSetter`: a boolean flag indicating if to treat the `ng-model` as a getter/setter function instead of a plain model value. The function will then run and return the model value.
**Example:**

```js
<input type="text" ng-model="myFunc" ng-model-options="{'getterSetter': true}">

$scope.myFunc = function() {return "value";}

```


</li>
<li>
`timezone`: defines the timezone for the model if the input is of the `date` or `time`. types
</li>



## ngInclude


**ng-include** allows you to delegate the control of one part of the page to a specific controller. You may want to do this because the complexity of that component is becoming such that you want to encapsulate all the logic in a dedicated controller.

An example is:

```

 <div ng-include
       src="'/gridview'"
       ng-controller='gridController as gc'>
  </div>

```

Note that the `/gridview` will need to be served by the web server as a distinct and legitimate url.

Also, note that the `src`-attribute accepts an Angular expression. This could be a variable or a function call for example or, like in this example, a string constant. In this case you need to make sure to **wrap the source URL in single quotes**, so it will be evaluated as a string constant. This is a common source of confusion.

Within the `/gridview` html, you can refer to the `gridController` as if it were wrapped around the page, eg:

```js
<div class="row">
  <button type="button" class="btn btn-default" ng-click="gc.doSomething()"></button>
</div>

```



## ngCopy


The `ngCopy` directive specifies behavior to be run on a copy event.

### Prevent a user from copying data

```js
<p ng-copy="blockCopy($event)">This paragraph cannot be copied</p>

```

In the controller

```js
$scope.blockCopy = function(event) {
    event.preventDefault();
    console.log("Copying won't work");
}

```



## ngClick


The `ng-click` directive attaches a click event to a DOM element.

The `ng-click` directive allows you to specify custom behavior when an element of DOM is clicked.

It is useful when you want to attach click events on buttons and handle them at your controller.

This directive accepts an expression with the events object available as `$event`

**HTML**

```js
<input ng-click="onClick($event)">Click me</input>

```

**Controller**

```js
.controller("ctrl", function($scope) {   
    $scope.onClick = function(evt) {
        console.debug("Hello click event: %o ",evt);
    } 
})

```

**HTML**

```js
<button ng-click="count = count + 1" ng-init="count=0">
  Increment
</button>
<span>
  count: { {count} }
</span>

```

**HTML**

```js
<button ng-click="count()" ng-init="count=0">
  Increment
</button>
<span>
  count: { {count} }
</span>

```

**Controller**

```js
...

$scope.count = function(){
    $scope.count = $scope.count + 1;
}
...

```

When the button is clicked, an invocation of the `onClick` function will print "Hello click event" followed by the event object.



## ngPaste


The `ngPaste` directive specifies custom behavior to run when a user pastes content

```js
<input ng-paste="paste=true" ng-init="paste=false" placeholder='paste here'>
pasted: { {paste} }

```



## ngOptions


`ngOptions` is a directive that simplifies the creation of a html dropdown box for the selection of an item from an array that will be stored in a model. The ngOptions attribute is used to dynamically generate a list of `<option>` elements for the `<select>` element using the array or object obtained by evaluating the ngOptions comprehension expression.

With `ng-options` the markup can be reduced to just a select tag and the directive will create the same select:

```js
<select ng-model="selectedFruitNgOptions" 
        ng-options="curFruit as curFruit.label for curFruit in fruit">
</select>

```

There is anther way of creating `select` options using `ng-repeat`, but it is not recommended to use `ng-repeat` as it is mostly used for general purpose like, the `forEach` just to loop. Whereas `ng-options` is specifically for creating `select` tag options.

Above example using `ng-repeat` would be

```js
<select ng-model="selectedFruit">
    <option ng-repeat="curFruit in fruit" value="{ {curFruit} }">
        { {curFruit.label} }
    </option>
</select>

```

[**FULL EXAMPLE**](https://jsfiddle.net/awolf2904/qb9kyr5h/)

Lets see the above example in detail also with some variations in it.

**Data model for the example:**

```js
$scope.fruit = [
    { label: "Apples", value: 4, id: 2 },
    { label: "Oranges", value: 2, id: 1 },
    { label: "Limes", value: 4, id: 4 },
    { label: "Lemons", value: 5, id: 3 }
];

```

```js
<!-- label for value in array -->
<select ng-options="f.label for f in fruit" ng-model="selectedFruit"></select>

```

**Option tag generated on selection:**

```

<option value="{ label: "Apples", value: 4, id: 2 }"> Apples </option>

```

**Effects:**

`f.label` will be the label of the `<option>` and the value will contain the entire object.

[**FULL EXAMPLE**](https://jsfiddle.net/Kunalh/qb9kyr5h/1/)

```js
<!-- select as label for value in array -->
<select ng-options="f.value as f.label for f in fruit" ng-model="selectedFruit"></select>

```

**Option tag generated on selection:**

```

<option value="4"> Apples </option>

```

**Effects:**

`f.value` (4) will be the value in this case while the label is still the same.

[**FULL EXAMPLE**](https://jsfiddle.net/Kunalh/qb9kyr5h/2/)

```js
<!-- label group by group for value in array -->
<select ng-options="f.label group by f.value for f in fruit" ng-model="selectedFruit"></select>

```

**Option tag generated on selection:**

```js
<option value="{ label: "Apples", value: 4, id: 2 }"> Apples </option>

```

**Effects:**

Options will be grouped based on there `value`. Options with same `value` will fall under one category

[**FULL EXAMPLE**](https://jsfiddle.net/Kunalh/qb9kyr5h/3/)

```js
<!-- label disable when disable for value in array -->
<select ng-options="f.label disable when f.value == 4 for f in fruit" ng-model="selectedFruit"></select>

```

**Option tag generated on selection:**

```js
<option disabled="" value="{ label: "Apples", value: 4, id: 2 }"> Apples </option>

```

**Effects:**

"Apples" and "Limes" will be disabled (unable to select) because of the condition `disable when f.value==4`. All options with `value=4` shall be disabled

[**FULL EXAMPLE**](https://jsfiddle.net/Kunalh/qb9kyr5h/4/)

```js
<!-- label group by group for value in array track by trackexpr -->
<select ng-options="f.value as f.label group by f.value for f in fruit track by f.id" ng-model="selectedFruit"></select>

```

**Option tag generated on selection:**

```js
<option value="4"> Apples </option>

```

**Effects:**

There is not visual change when using `trackBy`, but Angular will detect changes by the `id` instead of by reference which is most always a better solution.

[**FULL EXAMPLE**](https://jsfiddle.net/Kunalh/qb9kyr5h/5/)

```js
<!-- label for value in array | orderBy:orderexpr track by trackexpr -->
<select ng-options="f.label for f in fruit | orderBy:'id' track by f.id" ng-model="selectedFruit"></select>

```

**Option tag generated on selection:**

```js
<option disabled="" value="{ label: "Apples", value: 4, id: 2 }"> Apples </option>

```

**Effects:**

`orderBy` is a AngularJS standard filter which arranges options in ascending order(by default) so "Oranges" in this will appear 1st since its `id` = 1.

[**FULL EXAMPLE**](https://jsfiddle.net/Kunalh/qb9kyr5h/6/)

**All `<select>` with `ng-options` must have `ng-model` attached.**



## ngModel


With ng-model you can bind a variable to any type of input field. You can display the variable using double curly braces, eg `{ {myAge} }`.

```js
<input type="text" ng-model="myName">
<p>{ {myName} }</p>

```

As you type in the input field or change it in any way you will see the value in the paragraph update instantly.

The ng-model variable, in this instance, will be available in your controller as `$scope.myName`.  If you are using the `controllerAs` syntax:

```js
<div ng-controller="myCtrl as mc">
    <input type="text" ng-model="mc.myName">
    <p>{ {mc.myName} }</p>
</div>

```

You will need to refer to the controller's scope by pre-pending the controller's alias defined in the ng-controller attribute to the ng-model variable.  This way you won't need to inject `$scope` into your controller to reference your ng-model variable, the variable will be available as `this.myName` inside your controller's function.



## ngClass


Let's assume that you need to show the status of a user and you have several possible CSS classes that could be used.  Angular makes it very easy to choose from a list of several possible classes which allow you to specify an object list that include conditionals.  Angular is able to use the correct class based on the truthiness of the conditionals.

Your object should contain key/value pairs.  The key is a class name that will be applied when the value (conditional) evaluates to true.

```js
<style>
    .active { background-color: green; color: white; }
    .inactive { background-color: gray; color: white; }
    .adminUser { font-weight: bold; color: yellow; }
    .regularUser { color: white; }
</style>

<span ng-class="{ 
    active: user.active, 
    inactive: !user.active, 
    adminUser: user.level === 1, 
    regularUser: user.level === 2 
}">John Smith</span>

```

Angular will check the `$scope.user` object to see the `active` status and the `level` number.  Depending on the values in those variables, Angular will apply the matching style to the `<span>`.



## ngSrc


Using Angular markup like `{ {hash} }` in a src attribute doesn't work right. The browser will fetch from the URL with the literal text `{ {hash} }` until Angular replaces the expression inside `{ {hash} }`. `ng-src` directive overrides the original `src` attribute for the image tag element and solves the problem

```js
<div ng-init="pic = 'pic_angular.jpg'">
    <h1>Angular</h1>
    <img ng-src="{ {pic} }">
</div>

```



## ngList


The `ng-list` directive is used to convert a delimited string from a text input to an array of strings or vice versa.

The `ng-list` directive uses a default delimiter of `", "` (comma space).

You can set the delimiter manually by assigning `ng-list` a delimeter like this `ng-list="; "`.

In this case the delimiter is set to a semi colon followed by a space.

By default `ng-list` has an attribute `ng-trim` which is set to true. `ng-trim` when false, will respect white space in your delimiter. By default, `ng-list` does not take white space into account unless you set `ng-trim="false"`.

Example:

```js
angular.module('test', [])
  .controller('ngListExample', ['$scope', function($scope) {
    $scope.list = ['angular', 'is', 'cool!'];
}]);

```

A customer delimiter is set to be `;`. And the model of the input box is set to the array that was created on the scope.

```

 <body ng-app="test" ng-controller="ngListExample">
    <input ng-model="list" ng-list="; " ng-trim="false">
  </body>

```

The input box will display with the content: `angular; is; cool!`



## ngShow and ngHide


The `ng-show` directive shows or hides the HTML element based on if the expression passed to it is true or false. If the value of the expression is falsy then it will hide. If it is truthy then it will show.

The `ng-hide` directive is similar. However, if the value is falsy it will show the HTML element. When the expression is truthy it will hide it.

[Working JSBin Example](http://jsbin.com/zegulizita/edit?html,js,output)

**Controller**:

```js
var app = angular.module('app', []);
  
angular.module('app')
  .controller('ExampleController', ExampleController);
 
function ExampleController() {
  
  var vm = this;
  
  //Binding the username to HTML element
  vm.username = '';
  
  //A taken username
  vm.taken_username = 'StackOverflow';
  
}

```

**View**

```js
<section ng-controller="ExampleController as main">
    
    <p>Enter Password</p>
    <input ng-model="main.username" type="text">
    
    <hr>
    
    <!-- Will always show as long as StackOverflow is not typed in -->
    <!-- The expression is always true when it is not StackOverflow -->
    <div style="color:green;" ng-show="main.username != main.taken_username">
      Your username is free to use!
    </div>
    
    <!-- Will only show when StackOverflow is typed in -->
    <!-- The expression value becomes falsy -->
    <div style="color:red;" ng-hide="main.username != main.taken_username">
      Your username is taken!
    </div>
    
    <p>Enter 'StackOverflow' in username field to show ngHide directive.</p>
    
 </section>

```



## ngDblclick


The `ng-dblclick` directive is useful when you want to bind a double-click event into your DOM elements.

This directive accepts an expression

**HTML**

```js
<input type="number" ng-model="num = num + 1" ng-init="num=0">

<button ng-dblclick="num++">Double click me</button>

```

In the above example, the value held at the `input` will be incremented when the button is double clicked.



## ngPattern


The `ng-pattern` directive accepts an expression that evaluates to a regular expression pattern and uses that pattern to validate a textual input.

**Example:**

Lets say we want an `<input>` element to become valid when it's value (ng-model) is a valid IP address.

Template:

```js
<input type="text" ng-model="ipAddr" ng-pattern="ipRegex" name="ip" required>

```

Controller:

```js
$scope.ipRegex = /\b(?:(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\.){3}(?:25[0-5]|2[0-4][0-9]|[01]?[0-9][0-9]?)\b/;

```



## ngHref


ngHref is used instead of href attribute, if we have a angular expressions inside href value. The ngHref directive overrides the original href attribute of an html tag using href attribute such as  tag,  tag etc.

The ngHref directive makes sure the link is not broken even if the user clicks the link before AngularJS has evaluated the code.

**Example 1**

```js
<div ng-init="linkValue = 'http://stackoverflow.com'">
    <p>Go to <a ng-href="{ {linkValue} }">{ {linkValue} }</a>!</p>
</div>

```

**Example 2**
This example dynamically gets the href value from input box and load it as href value.

```js
<input ng-model="value" />
<a id="link" ng-href="{ {value} }">link</a>

```

**Example 3**

```js
<script>
angular.module('angularDoc', [])
.controller('myController', function($scope) {
  // Set some scope value.
  // Here we set bootstrap version.
  $scope.bootstrap_version = '3.3.7';
   
  // Set the default layout value
  $scope.layout = 'normal';
});
</script>
<!-- Insert it into Angular Code -->
<link rel="stylesheet" ng-href="//maxcdn.bootstrapcdn.com/bootstrap/{ { bootstrap_version } }/css/bootstrap.min.css">
<link rel="stylesheet" ng-href="layout-{ { layout } }.css">

```



## ngMouseenter and ngMouseleave


The `ng-mouseenter` and `ng-mouseleave` directives are useful to run events and apply CSS styling when you hover into or out of your DOM elements.

The `ng-mouseenter` directive runs an expression one a mouse enter event (when the user enters his mouse pointer over the DOM element this directive resides in)

**HTML**

```js
<div ng-mouseenter="applyStyle = true" ng-class="{'active': applyStyle}">

```

At the above example, when the user points his mouse over the `div`, `applyStyle` turns to `true`, which in turn applies the `.active` CSS class at the `ng-class`.

The `ng-mouseleave` directive runs an expression one a mouse exit event (when the user takes his mouse cursor away from the DOM element this directive resides in)

**HTML**

```js
<div ng-mouseenter="applyStyle = true" ng-mouseleaver="applyStyle = false" ng-class="{'active': applyStyle}">

```

Reusing the first example, now when the user takes him mouse pointer away from the div, the `.active` class is removed.



## ngDisabled


This directive is useful to limit input events based on certain existing conditions.

The `ng-disabled` directive accepts and expression that should evaluate to either a truthy or a falsy values.

`ng-disabled` is used to conditionally apply the `disabled` attribute on an `input` element.

**HTML**

```js
<input type="text" ng-model="vm.name">

<button ng-disabled="vm.name.length===0" ng-click="vm.submitMe">Submit</button>

```

`vm.name.length===0` is evaluated to true if the `input`'s length is 0, which is turn disables the button, disallowing the user to fire the click event of `ng-click`



## ngRequired


The `ng-required` adds or removes the `required` validation attribute on an element, which in turn will enable and disable the `require` validation key for the `input`.

It is used to optionally define if an `input` element is required to have a non-empty value. The directive is helpful when designing validation on complex HTML forms.

**HTML**

```js
<input type="checkbox" ng-model="someBooleanValue">
<input type="text" ng-model="username" ng-required="someBooleanValue">

```



## ngValue


Mostly used under `ng-repeat`
ngValue is useful when dynamically generating lists of radio buttons using ngRepeat

```js
<script>
   angular.module('valueExample', [])
     .controller('ExampleController', ['$scope', function($scope) {
       $scope.names = ['pizza', 'unicorns', 'robots'];
       $scope.my = { favorite: 'unicorns' };
     }]);
</script>
 <form ng-controller="ExampleController">
   <h2>Which is your favorite?</h2>
     <label ng-repeat="name in names" for="{ {name} }">
       { {name} }
       <input type="radio"
              ng-model="my.favorite"
              ng-value="name"
              id="{ {name} }"
              name="favorite">
     </label>
   <div>You chose { {my.favorite} }</div>
 </form>

```

[Working plnkr](https://plnkr.co/edit/QUTaDmcUlbrVX6urUGlB?p=preview)

