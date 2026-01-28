---
metaTitle: "AngularJS - ng-class directive"
description: "Three types of ng-class expressions"
---

# ng-class directive



## Three types of ng-class expressions


Angular supports three types of expressions in the `ng-class` directive.

### 1. String

```js
<span ng-class="MyClass">Sample Text</span>

```

Specifying an expression that evaluates to a string tells Angular to treat it as a $scope variable.  Angular will check the $scope and look for a variable called "MyClass".  Whatever text is contained in "MyClass" will become the actual class name that is applied to this `<span>`.  You can specify multiple classes by separating each class with a space.

In your controller, you may have a definition that looks like this:

```js
$scope.MyClass = "bold-red deleted error";

```

Angular will evaluate the expression `MyClass` and find the $scope definition.  It will apply the three classes "bold-red", "deleted", and "error" to the `<span>` element.

Specifying classes this way lets you easily change the class definitions in your controller.  For example, you may need to change the class based on other user interactions or new data that is loaded from the server.  Also, if you have a lot of expressions to evaluate, you can do so in a function that defines the final list of classes in a `$scope` variable.  This can be easier than trying to squeeze many evaluations into the `ng-class` attribute in your HTML template.

### 2. Object

This is the most commonly-used way of defining classes using `ng-class` because it easily lets you specify evaluations that determine which class to use.

Specify an object containing key-value pairs.  The key is the class name that will be applied if the value (a conditional) evaluates as true.

```js
<style>
    .red { color: red; font-weight: bold; }
    .blue { color: blue; }
    .green { color: green; }
    .highlighted { background-color: yellow; color: black; }
</style>

<span ng-class="{ red: ShowRed, blue: ShowBlue, green: ShowGreen, highlighted: IsHighlighted }">Sample Text</span>

<div>Red: <input type="checkbox" ng-model="ShowRed"></div>
<div>Green: <input type="checkbox" ng-model="ShowGreen"></div>
<div>Blue: <input type="checkbox" ng-model="ShowBlue"></div>
<div>Highlight: <input type="checkbox" ng-model="IsHighlighted"></div>

```

### 3. Array

An expression that evaluates to an array lets you use a combination of **strings** (see #1 above) and **conditional objects** (#2 above).

```js
<style>
    .bold { font-weight: bold; }
    .strike { text-decoration: line-through; }
    .orange { color: orange; }
</style>

<p ng-class="[ UserStyle, {orange: warning} ]">Array of Both Expression Types</p>
<input ng-model="UserStyle" placeholder="Type 'bold' and/or 'strike'"><br>
<label><input type="checkbox" ng-model="warning"> warning (apply "orange" class)</label>

```

This creates a text input field bound to the scope variable `UserStyle` which lets the user type in any class name(s).  These will be dynamically applied to the `<p>` element as the user types.  Also, the user can click on the checkbox that is data-bound to the `warning` scope variable.  This will also be dynamically applied to the `<p>` element.

