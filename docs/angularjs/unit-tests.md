---
metaTitle: "AngularJS - Unit tests"
description: "Unit test a component (1.5+), Unit test a filter, Unit test a service, Unit test a controller, Unit test a directive"
---

# Unit tests



## Unit test a component (1.5+)


Component code:

```js
angular.module('myModule', []).component('myComponent', {
  bindings: {
    myValue: '<'
  },
  controller: function(MyService) {
    this.service = MyService;
    this.componentMethod = function() {
      return 2;
    };
  }
});

```

The test:

```js
describe('myComponent', function() {
  var component;

  var MyServiceFake = jasmine.createSpyObj(['serviceMethod']);

  beforeEach(function() {
    module('myModule');
    inject(function($componentController) {
      // 1st - component name, 2nd - controller injections, 3rd - bindings
      component = $componentController('myComponent', {
        MyService: MyServiceFake
      }, {
        myValue: 3
      });
    });
  });

  /** Here you test the injector. Useless. */

  it('injects the binding', function() {
    expect(component.myValue).toBe(3);
  });

  it('has some cool behavior', function() {
    expect(component.componentMethod()).toBe(2);
  });
});

```

[Run!](http://jsfiddle.net/fracz/3ua8o22a/)



## Unit test a filter


Filter code:

```js
angular.module('myModule', []).filter('multiplier', function() {
  return function(number, multiplier) {
    if (!angular.isNumber(number)) {
      throw new Error(number + " is not a number!");
    }
    if (!multiplier) {
      multiplier = 2;
    }
    return number * multiplier;
  }
});

```

The test:

```js
describe('multiplierFilter', function() {
  var filter;

  beforeEach(function() {
    module('myModule');
    inject(function(multiplierFilter) {
      filter = multiplierFilter;
    });
  });

  it('multiply by 2 by default', function() {
    expect(filter(2)).toBe(4);
    expect(filter(3)).toBe(6);
  });

  it('allow to specify custom multiplier', function() {
    expect(filter(2, 4)).toBe(8);
  });

  it('throws error on invalid input', function() {
    expect(function() {
      filter(null);
    }).toThrow();
  });
});

```

[Run!](http://jsfiddle.net/fracz/g2vLqcvx/)

**Remark:** In the `inject` call in the test, your filter needs to be specified by its name + **Filter**. The cause for this is that whenever you register a filter for your module, Angular register it with a `Filter` appended to its name.



## Unit test a service


Service Code

```js
angular.module('myModule', [])
  .service('myService', function() {
    this.doSomething = function(someNumber) {
      return someNumber + 2;
    }
  });

```

The test

```js
describe('myService', function() {
  var myService;
  beforeEach(function() {
    module('myModule');
    inject(function(_myService_) {
      myService = _myService_;
    });
  });
  it('should increment `num` by 2', function() {
    var result = myService.doSomething(4);
    expect(result).toEqual(6);
  });
});

```

[Run!](http://jsfiddle.net/fracz/4kmrqap6/)



## Unit test a controller


Controller code:

```js
angular.module('myModule', [])
  .controller('myController', function($scope) {
    $scope.num = 2;
    $scope.doSomething = function() {
      $scope.num += 2;
    }
  });

```

The test:

```js
describe('myController', function() {
  var $scope;
  beforeEach(function() {
    module('myModule');
    inject(function($controller, $rootScope) {
      $scope = $rootScope.$new();
      $controller('myController', {
        '$scope': $scope
      })
    });
  });
  it('should increment `num` by 2', function() {
    expect($scope.num).toEqual(2);
    $scope.doSomething();
    expect($scope.num).toEqual(4);
  });
});

```

[Run!](http://jsfiddle.net/fracz/xp7fdd8j/)



## Unit test a directive


Directive code

```js
angular.module('myModule', [])
  .directive('myDirective', function() {
    return {
      template: '<div>{ {greeting} } { {name} }!</div>',
      scope: {
        name: '=',
        greeting: '@'
      }
    };
  });

```

The test

```js
describe('myDirective', function() {
  var element, scope;
  beforeEach(function() {
    module('myModule');
    inject(function($compile, $rootScope) {
      scope = $rootScope.$new();
      element = angular.element("<my-directive name='name' greeting='Hello'></my-directive>");
      $compile(element)(scope);
      /* PLEASE NEVER USE scope.$digest(). scope.$apply use a protection to avoid to run a digest loop when there is already one, so, use scope.$apply() instead. */
      scope.$apply();
    })
  });

  it('has the text attribute injected', function() {
    expect(element.html()).toContain('Hello');
  });

  it('should have proper message after scope change', function() {
    scope.name = 'John';
    scope.$apply();
    expect(element.html()).toContain("John");
    scope.name = 'Alice';
    expect(element.html()).toContain("John");
    scope.$apply();
    expect(element.html()).toContain("Alice");
  });
});

```

[Run!](http://jsfiddle.net/fracz/kc06yrra)



#### Remarks


This topic provides examples for unit testing the various constructs in AngularJS. Unit tests are often written using using [Jasmine](http://jasmine.github.io/), a popular behavior driven testing framework. When unit testing angular constructs, you will need to include [ngMock](https://docs.angularjs.org/api/ngMock) as a dependency when running the unit tests.

