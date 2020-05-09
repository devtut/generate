---
metaTitle: "AngularJS - Components"
description: "Basic Components and LifeCycle Hooks, Components In angular JS"
---

# Components



## Basic Components and LifeCycle Hooks


### What’s a component?

<li>A component is basically a directive that uses a simpler
configuration and that is suitable for a component-based
architecture, which is what Angular 2 is all about.  Think of a
component as a widget: A piece of HTML code that you can reuse in
several different places in your web application.</li>

**Component**

```js
angular.module('myApp', [])
    .component('helloWorld', {
        template: '<span>Hello World!</span>'
    });

```

**Markup**

```js
<div ng-app="myApp"> 
  <hello-world> </hello-world>
</div>

```

[Live Demo](https://plnkr.co/edit/vXdChl?p=preview)

### Using External data in Component:

We could add a parameter to pass a name to our component, which would be used as follows:

```js
angular.module("myApp", [])
  .component("helloWorld",{
    template: '<span>Hello { {$ctrl.name} }!</span>',
    bindings: { name: '@' }
  });

```

Markup

```js
<div ng-app="myApp"> 
  <hello-world name="'John'" > </hello-world>
</div>

```

[Live Demo](https://plnkr.co/edit/ERw1vDRjikp10QDim805?p=preview)

### Using Controllers in Components

Let’s take a look at how to add a controller to it.

```js
angular.module("myApp", [])
  .component("helloWorld",{
      template: "Hello { {$ctrl.name} }, I'm { {$ctrl.myName} }!",
      bindings: { name: '@' },
      controller: function(){
        this.myName = 'Alain';
      }
  });

```

**Markup**

```js
<div ng-app="myApp">  
  <hello-world name="John"> </hello-world>
</div>

```

[CodePen Demo](http://codepen.io/mjunaidsalaat/pen/NAYXyP)

Parameters passed to the component are available in the controller's scope just before its `$onInit` function gets called by Angular. Consider this example:

```js
angular.module("myApp", [])
  .component("helloWorld",{
      template: "Hello { {$ctrl.name} }, I'm { {$ctrl.myName} }!",
      bindings: { name: '@' },
      controller: function(){
        this.$onInit = function() {
          this.myName = "Mac" + this.name;
        }
      }
  });

```

In the template from above, this would render "Hello John, I'm MacJohn!".

Note that `$ctrl` is the Angular default value for `controllerAs` if one is not specified.

[Live Demo](https://plnkr.co/edit/bxB3PG?p=preview)

### Using “require” as an Object

In some instances you may need to access data from a parent component inside your component.

This can be achieved by specifying that our component requires that parent component, the require will give us reference to the required component controller,
which can then be used in our controller as shown in the example below:

> 
Notice that required controllers are guaranteed to be ready only after the $onInit hook.


```js
angular.module("myApp", [])
  .component("helloWorld",{
      template: "Hello { {$ctrl.name} }, I'm { {$ctrl.myName} }!",
      bindings: { name: '@' },
      require: {
        parent: '^parentComponent'
      },
      controller: function () {
        // here this.parent might not be initiated yet

        this.$onInit = function() {
           // after $onInit, use this.parent to access required controller
           this.parent.foo();
        }

      }
  });

```

Keep in mind, though, that this creates a [tight coupling](https://en.wikipedia.org/wiki/Coupling_%28computer_programming%29) between the child and the parent.



## Components In angular JS


The components in angularJS can be visualised as a custom directive (< html > this in an HTML directive, and something like this will be a custom directive < ANYTHING >). A component contains a view and a controller. Controller contains the business logic which is binded with an view , which the user sees.
The component differs from a angular directive because it contains less configuration. An angular component can be defined like this.

```js
angular.module("myApp",[]).component("customer", {})

```

Components are defined on the angular modules. They contains two arguments, One is the name of the component and second one is a object which contains key value pair, which defines which view and which controller it is going to use like this .

```js
angular.module("myApp",[]).component("customer", {
    templateUrl : "customer.html", // your view here 
    controller: customerController, //your controller here
    controllerAs: "cust"        //alternate name for your controller 
})

```

"myApp" is the name of the app we are building and customer is the name of our component. Now for calling it in main html file we will just put it like this

```js
<customer></customer>

```

Now this directive will be replaced by the view you have specified and the business logic you have written in your controller.

NOTE : Remember component take a object as second argument while directive take a factory function as argument.



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|**=**|For using two-way data binding. This means that if you update that variable in your component scope, the change will be reflected on the parent scope.
|**<**|One-way bindings when we just want to read a value from a parent scope and not update it.
|**@**|String parameters.
|**&**|For callbacks in case your component needs to output something to its parent scope.
|-|-
|**LifeCycle Hooks**|**Details**  (requires angular.version >= 1.5.3 )
|**$onInit()**|Called on each controller after all the controllers on an element have been constructed and had their bindings initialized. This is a good place to put initialization code for your controller.
|**$onChanges(changesObj)**|Called whenever one-way bindings are updated. The `changesObj` is a hash whose keys are the names of the bound properties that have changed, and the values are an object of the form `{ currentValue, previousValue, isFirstChange() }` .
|**$onDestroy()**|Called on a controller when its containing scope is destroyed. Use this hook for releasing external resources, watches and event handlers.
|**$postLink()**|Called after this controller’s element and its children have been linked. This hook can be considered analogous to the ngAfterViewInit and ngAfterContentInit hooks in Angular 2.
|**$doCheck()**|Called on each turn of the digest cycle. Provides an opportunity to detect and act on changes. Any actions that you wish to take in response to the changes that you detect must be invoked from this hook; implementing this has no effect on when $onChanges is called.



#### Remarks


Component is a special kind of directive that uses a simpler configuration which is suitable for a component-based application structure. Components were introduced in Angular 1.5, the examples in this section **will not work** with older AngularJS  versions.

A complete developer guide about Components is avalable on  [https://docs.angularjs.org/guide/component](https://docs.angularjs.org/guide/component)

