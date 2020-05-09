---
metaTitle: "AngularJS - Directives using ngModelController"
description: "A simple control: rating, A couple of complex controls: edit a full object"
---

# Directives using ngModelController



## A simple control: rating


Let us build a simple control, a rating widget, intended to be used as:

```js
<rating min="0" max="5" nullifier="true" ng-model="data.rating"></rating>

```

No fancy CSS for now; this would render as:

```js
0 1 2 3 4 5 x

```

Clicking on a number selects that rating; and clicking the "x" sets the rating to null.

```js
app.directive('rating', function() {

    function RatingController() {
        this._ngModel = null;
        this.rating = null;
        this.options = null;
        this.min = typeof this.min === 'number' ? this.min : 1;
        this.max = typeof this.max === 'number' ? this.max : 5;
    }
    
    RatingController.prototype.setNgModel = function(ngModel) {
        this._ngModel = ngModel;
        
        if( ngModel ) {
            // KEY POINT 1
            ngModel.$render = this._render.bind(this);
        }
    };
    
    RatingController.prototype._render = function() {
        this.rating = this._ngModel.$viewValue != null ? this._ngModel.$viewValue : -Number.MAX_VALUE;
    };
    
    RatingController.prototype._calculateOptions = function() {
        if( this.min == null || this.max == null ) {
            this.options = [];
        }
        else {
            this.options = new Array(this.max - this.min + 1);
            for( var i=0; i < this.options.length; i++ ) {
                this.options[i] = this.min + i;
            }
        }
    };
    
    RatingController.prototype.setValue = function(val) {
        this.rating = val;
        // KEY POINT 2
        this._ngModel.$setViewValue(val);
    };
    
    // KEY POINT 3
    Object.defineProperty(RatingController.prototype, 'min', {
        get: function() {
            return this._min;
        },
        set: function(val) {
            this._min = val;
            this._calculateOptions();
        }
    });
    
    Object.defineProperty(RatingController.prototype, 'max', {
        get: function() {
            return this._max;
        },
        set: function(val) {
            this._max = val;
            this._calculateOptions();
        }
    });
    
    return {
        restrict: 'E',
        scope: {
            // KEY POINT 3
            min: '<?',
            max: '<?',
            nullifier: '<?'
        },
        bindToController: true,
        controllerAs: 'ctrl',
        controller: RatingController,
        require: ['rating', 'ngModel'],
        link: function(scope, elem, attrs, ctrls) {
            ctrls[0].setNgModel(ctrls[1]);
        },
        template:
            '<span ng-repeat="o in ctrl.options" href="#" class="rating-option" ng-class="{\'rating-option-active\': o <= ctrl.rating}" ng-click="ctrl.setValue(o)">{ { o } }</span>' +
            '<span ng-if="ctrl.nullifier" ng-click="ctrl.setValue(null)" class="rating-nullifier">&#10006;</span>'
    };
});

```

Key points:

1. Implement `ngModel.$render` to transfer the model's **view value** to your view.
1. Call `ngModel.$setViewValue()` whenever you feel the view value should be updated.
1. The control can of course be parameterized; use `'<'` scope bindings for parameters, if in Angular >= 1.5 to clearly indicate input - one way binding. If you have to take action whenever a parameter changes, you can use a JavaScript property (see `Object.defineProperty()`) to save a few watches.

Note 1: In order not to overcomplicate the implementation, the rating values are inserted in an array - the `ctrl.options`. This is not needed; a more efficient, but also more complex, implementation could use DOM manipulation to insert/remove ratings when `min`/`max` change.

Note 2: With the exception of the `'<'` scope bindings, this example can be used in Angular < 1.5. If you are on Angular >= 1.5, it would be a good idea to tranform this to a component and use the `$onInit()` lifecycle hook to initialize `min` and `max`, instead of doing so in the controller's constructor.

And a necessary fiddle: [https://jsfiddle.net/h81mgxma/](https://jsfiddle.net/h81mgxma/)



## A couple of complex controls: edit a full object


A custom control does not have to limit itself to trivial things like primitives; it can edit more interesting things. Here we present two types of custom controls, one for editing persons and one for editing addresses. The address control is used to edit the person's address. An example of usage would be:

```js
<input-person ng-model="data.thePerson"></input-person>
<input-address ng-model="data.thePerson.address"></input-address>

```

The model for this example is deliberately simplistic:

```js
function Person(data) {
  data = data || {};
  this.name = data.name;
  this.address = data.address ? new Address(data.address) : null;
}

function Address(data) {
  data = data || {};
  this.street = data.street;
  this.number = data.number;
}

```

The address editor:

```js
app.directive('inputAddress', function() {

    InputAddressController.$inject = ['$scope'];
    function InputAddressController($scope) {
        this.$scope = $scope;
        this._ngModel = null;
        this.value = null;
        this._unwatch = angular.noop;
    }

    InputAddressController.prototype.setNgModel = function(ngModel) {
        this._ngModel = ngModel;
        
        if( ngModel ) {
            // KEY POINT 3
            ngModel.$render = this._render.bind(this);
        }
    };
    
    InputAddressController.prototype._makeWatch = function() {
        // KEY POINT 1
        this._unwatch = this.$scope.$watchCollection(
            (function() {
                return this.value;
            }).bind(this),
            (function(newval, oldval) {
                if( newval !== oldval ) { // skip the initial trigger
                    this._ngModel.$setViewValue(newval !== null ? new Address(newval) : null);
                }
            }).bind(this)
        );
    };
    
    InputAddressController.prototype._render = function() {
        // KEY POINT 2
        this._unwatch();
        this.value = this._ngModel.$viewValue ? new Address(this._ngModel.$viewValue) : null;
        this._makeWatch();
    };

    return {
        restrict: 'E',
        scope: {},
        bindToController: true,
        controllerAs: 'ctrl',
        controller: InputAddressController,
        require: ['inputAddress', 'ngModel'],
        link: function(scope, elem, attrs, ctrls) {
            ctrls[0].setNgModel(ctrls[1]);
        },
        template:
            '<div>' +
                '<label><span>Street:</span><input type="text" ng-model="ctrl.value.street" /></label>' +
                '<label><span>Number:</span><input type="text" ng-model="ctrl.value.number" /></label>' +
            '</div>'
    };
});

```

Key points:

1. We are editing an object; we do not want to change directly the object given to us from our parent (we want our model to be compatible with the immutability principle). So we create a shallow watch on the object being edited and update the model with `$setViewValue()` whenever a property changes. We pass a **copy** to our parent.
1. Whenever the model changes from the outside, we copy it and save the copy to our scope. Immutability principles again, though the internal copy is not immutable, the external could very well be. Additionally we rebuild the watch (`this_unwatch();this._makeWatch();`), to avoid triggering the watcher for changes pushed to us by the model. (We only want the watch to trigger for changes made in the UI.)
1. Other that the points above, we implement `ngModel.$render()` and call `ngModel.$setViewValue()` as we would for a simple control (see the rating example).

The code for the person custom control is almost identical. The template is using the `<input-address>`. In a more advanced implementation we could extract the controllers in a reusable module.

```js
app.directive('inputPerson', function() {

    InputPersonController.$inject = ['$scope'];
    function InputPersonController($scope) {
        this.$scope = $scope;
        this._ngModel = null;
        this.value = null;
        this._unwatch = angular.noop;
    }

    InputPersonController.prototype.setNgModel = function(ngModel) {
        this._ngModel = ngModel;
        
        if( ngModel ) {
            ngModel.$render = this._render.bind(this);
        }
    };
    
    InputPersonController.prototype._makeWatch = function() {
        this._unwatch = this.$scope.$watchCollection(
            (function() {
                return this.value;
            }).bind(this),
            (function(newval, oldval) {
                if( newval !== oldval ) { // skip the initial trigger
                    this._ngModel.$setViewValue(newval !== null ? new Person(newval) : null);
                }
            }).bind(this)
        );
    };
    
    InputPersonController.prototype._render = function() {
        this._unwatch();
        this.value = this._ngModel.$viewValue ? new Person(this._ngModel.$viewValue) : null;
        this._makeWatch();
    };

    return {
        restrict: 'E',
        scope: {},
        bindToController: true,
        controllerAs: 'ctrl',
        controller: InputPersonController,
        require: ['inputPerson', 'ngModel'],
        link: function(scope, elem, attrs, ctrls) {
            ctrls[0].setNgModel(ctrls[1]);
        },
        template:
            '<div>' +
                '<label><span>Name:</span><input type="text" ng-model="ctrl.value.name" /></label>' +
                '<input-address ng-model="ctrl.value.address"></input-address>' +
            '</div>'
    };
});

```

Note: Here the objects are typed, i.e. they have proper constructors. This is not obligatory; the model can be plain JSON objects. In this case just use `angular.copy()` instead of the constructors. An added advantage is that the controller becomes identical for the two controls and can easily be extracted into some common module.

The fiddle: [https://jsfiddle.net/3tzyqfko/2/](https://jsfiddle.net/3tzyqfko/2/)

Two versions of the fiddle having extracted the common code of the controllers: [https://jsfiddle.net/agj4cp0e/](https://jsfiddle.net/agj4cp0e/) and [https://jsfiddle.net/ugb6Lw8b/](https://jsfiddle.net/ugb6Lw8b/)

