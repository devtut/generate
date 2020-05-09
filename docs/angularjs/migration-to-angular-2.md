---
metaTitle: "AngularJS - Migration to Angular 2+"
description: "Converting your AngularJS app into a componend-oriented structure, Introducing Webpack and ES6 modules"
---

# Migration to Angular 2+


AngularJS has been totally rewritten using the TypeScript language and [renamed](http://angularjs.blogspot.nl/2016/12/ok-let-me-explain-its-going-to-be.html) to just Angular.

There is a lot that can be done to an AngularJS app to ease the migration process. As the [official upgrade guide](https://angular.io/docs/ts/latest/guide/upgrade.html) says, several "preparation steps" can be performed to refactor your app, making it better and closer to the new Angular style.



## Converting your AngularJS app into a componend-oriented structure


In the new Angular framework, **Components** are the main building blocks that compose the user interface.
So one of the first steps that helps an AngularJS app to be migrated to the new Angular is to refactor it into a more component-oriented structure.

Components were also introduced in the old AngularJS starting from version **1.5+**. Using Components in an AngularJS app will not only make its structure closer to the new Angular 2+, but it will also make it more modular and easier to maintain.

Before going further I recommend to look at the [official AngularJS documentation page about Components](https://docs.angularjs.org/guide/component), where their advantages and usage are well explained.

I would rather mention some tips about how to convert the old `ng-controller` oriented code to the new `component` oriented style.

### Start breaking your your app into components

All the component-oriented apps have typically one or few components that include other sub-components. So why not creating the first component which simply will contain your app (or a big piece of it).

Assume that we have a piece of code assigned to a controller, named `UserListController`, and we want to make a component of it, which we'll name `UserListComponent`.

**current HTML:**

```js
<div ng-controller="UserListController as listctrl">
    <ul>
        <li ng-repeat="user in myUserList">
            { { user } }
        </li>
    </ul>
</div>

```

**current JavaScript:**

```js
app.controller("UserListController", function($scope, SomeService) {

    $scope.myUserList = ['Shin', 'Helias', 'Kalhac'];

    this.someFunction = function() {
        // ...
    }

    // ...
}

```

**new HTML:**

```js
<user-list></user-list>

```

**new JavaScript:**

```js
app.component("UserList", {
    templateUrl: 'user-list.html',
    controller: UserListController
});

function UserListController(SomeService) {

    this.myUserList = ['Shin', 'Helias', 'Kalhac'];

    this.someFunction = function() {
        // ...
    }

    // ...
}

```

Note how we are no longer injecting `$scope` into the controller function and we are now declaring `this.myUserList` instead of `$scope.myUserList`;

**new template file** `user-list.component.html`:

```js
<ul>
    <li ng-repeat="user in $ctrl.myUserList">
        { { user } }
    </li>
</ul>

```

Note how we are now referring to the variable `myUserList`, which belongs to the controller, using `$ctrl.myUserList` from the html instead of `$scope.myUserList`.

That is because, as you probably figured out after reading the documentation, `$ctrl` in the template now refers to the controller function.

### What about controllers and routes?

In case your controller was bound to the template using the routing system instead of `ng-controller`, so if you have something like this:

```js
$stateProvider
  .state('users', {
    url: '/users',
    templateUrl: 'user-list.html',
    controller: 'UserListController'
  })
  // ..

```

you can just change your state declaration to:

```js
$stateProvider
    .state('users', {
      url: '/',
      template: '<user-list></user-list>'
    })
    // ..

```

### What's next?

Now that you have a component containing your app (whether it contains the entire application or a part of it, like a view), you should now start to break your component into multiple nested components, by wrapping parts of it into new sub-components, and so on.

You should start using the Component features like

<li>
**Inputs** and **Outputs** bindings
</li>
<li>
**lifecycle hooks** such as `$onInit()`, `$onChanges()`, etc...
</li>

After reading the [Component documentation](https://docs.angularjs.org/guide/component) you should already know how to use all those component features, but if you need a concrete example of a real simple app, you can check [this](https://github.com/ShinDarth/Othello/wiki/Refactoring-steps#break-the-application-into-angularjs-components).

Also, if inside your component's controller you have some functions that hold a lot of logic code, a good idea can be considering to move that logic into [services](https://docs.angularjs.org/guide/services).

### Conclusion

Adopting a component-based approach pushes your AngularJS one step closer to migrate it to the new Angular framework, but it also makes it better and much more modular.

Of course there are a lot of other steps you can do to go further into the new Angular 2+ direction, which I will list in the following examples.



## Introducing Webpack and ES6 modules


By using a ****module loader**** like [Webpack](https://github.com/webpack/webpack) we can benefit the built-in module system available in **[ES6](https://en.wikipedia.org/wiki/ECMAScript#6th_Edition_-_ECMAScript_2015)** (as well as in **TypeScript**). We can then use the [import](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Statements/import) and [export](https://developer.mozilla.org/en/docs/web/javascript/reference/statements/export) features that allow us to specify what pieces of code can we are going to share between different parts of the application.

When we then take our applications into production, module loaders also make it easier to package them all up into production bundles with batteries included.

