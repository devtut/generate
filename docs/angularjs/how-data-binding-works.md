---
metaTitle: "AngularJS - How data binding works"
description: "Data Binding Example"
---

# How data binding works



## Data Binding Example


```js
<p ng-bind="message"></p>

```

This 'message' has to be attached to the current elements controller's scope.

```js
$scope.message = "Hello World";

```

At a later point of time , even if the message model is updated , that updated value is reflected in the HTML element.
When angular compiles the template "Hello World" will be attached to the innerHTML of the current world. Angular maintains a Watching mechanism of all the directives atttached to the view. It has a Digest Cycle mechanism where it iterates through the Watchers array, it will update the DOM element if there is a change in the previous value of the model.

There is no periodic checking of Scope whether there is any change in the Objects attached to it. Not all the objects attached to scope are watched . Scope prototypically maintains a **$$WatchersArray** . Scope only iterates through this WatchersArray when $digest is called .

Angular adds a watcher to the WatchersArray for each of these

> 
<ol>
- { {expression} } — In your templates (and anywhere else where there’s an expression) or when we define ng-model. <br>
- $scope.$watch(‘expression/function’) — In your JavaScript we can just attach a scope object for angular to watch.
</ol>


**$watch** function takes in three parameters:<br>

> 
<ol>
- First one is a watcher function which just returns the object or we can just add an expression. <br>
</ol>


> 
<ol start="2">
- Second one is a listener function which will be called when there is a change in the object. All the things like DOM changes will be implemented in this function.<br>
</ol>


> 
<ol start="3">
<li>The third being an optional parameter which takes in a boolean . If its true , angular deep watches the object & if its false Angular just does a reference watching on the object.
Rough Implementation of $watch looks like this</li>
</ol>


```js
Scope.prototype.$watch = function(watchFn, listenerFn) {
   var watcher = {
       watchFn: watchFn,
       listenerFn: listenerFn || function() { },
       last: initWatchVal  // initWatchVal is typically undefined
   };
   this.$$watchers.push(watcher); // pushing the Watcher Object to Watchers  
};

```

There is an interesting thing in Angular called Digest Cycle. The $digest cycle starts as a result of a call to $scope.$digest(). Assume that you change a $scope model in a handler function through the ng-click directive. In that case AngularJS automatically triggers a $digest cycle by calling $digest().In addition to ng-click, there are several other built-in directives/services that let you change models (e.g. ng-model, $timeout, etc) and automatically trigger a $digest cycle.  The rough implementation of $digest looks like this.

```js
Scope.prototype.$digest = function() {
      var dirty;
      do {
          dirty = this.$$digestOnce();
      } while (dirty);
}
Scope.prototype.$$digestOnce = function() {
   var self = this;
   var newValue, oldValue, dirty;
   _.forEach(this.$$watchers, function(watcher) {
          newValue = watcher.watchFn(self);
          oldValue = watcher.last;   // It just remembers the last value for dirty checking
          if (newValue !== oldValue) { //Dirty checking of References 
   // For Deep checking the object , code of Value     
   // based checking of Object should be implemented here
             watcher.last = newValue;
             watcher.listenerFn(newValue,
                  (oldValue === initWatchVal ? newValue : oldValue),
                   self);
          dirty = true;
          }
     });
   return dirty;
 };

```

If we use JavaScript’s **setTimeout()** function to update a scope model, Angular has no way of knowing what you might change. In this case it’s our responsibility to call $apply() manually, which triggers a $digest cycle. Similarly, if you have a directive that sets up a DOM event listener and changes some models inside the handler function, you need to call $apply() to ensure the changes take effect. The big idea of $apply is that we can execute some code that isn't aware of Angular, that code may still change things on the scope. If we wrap that code in $apply , it will take care of calling $digest(). Rough implementation of $apply().

```js
Scope.prototype.$apply = function(expr) {
       try {
         return this.$eval(expr); //Evaluating code in the context of Scope
       } finally {
         this.$digest();
       }
};

```



#### Remarks


So while this Data Binding concept on a whole is easy on the developer, it is quite heavy on the Browser since Angular listens to every event change and runs the Digest Cycle. Because of this, whenever we attach some model to the view, make sure that Scope is as optimized as possible

