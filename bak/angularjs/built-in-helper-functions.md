---
metaTitle: "AngularJS - Built-in helper Functions"
description: "angular.equals, angular.toJson, angular.copy, angular.isString, angular.isArray, angular.merge, angular.isDefined and angular.isUndefined, angular.isDate, angular.isFunction, angular.noop, angular.isElement, angular.isNumber, angular.fromJson, angular.isObject, angular.identity, angular.forEach"
---

# Built-in helper Functions



## angular.equals


The `angular.equals` function compares and determines if 2 objects or values are equal, `angular.equals` performs a deep comparison and returns true if and only if at least 1 of the following conditions is met.

> 
angular.equals(value1, value2)


1. If the objects or values pass the `===` comparison
1. If both objects or values are of the same type, and all of their properties are also equal by using `angular.equals`
1. Both values are equal to `NaN`
1. Both values represent the same regular expression's result.

This function is helpful when you need to deep compare objects or arrays by their values or results rather than just references.

**Examples**

```js
angular.equals(1, 1) // true
angular.equals(1, 2) // false
angular.equals({}, {}) // true, note that {}==={} is false
angular.equals({a: 1}, {a: 1}) // true
angular.equals({a: 1}, {a: 2}) // false
angular.equals(NaN, NaN) // true

```



## angular.toJson


The function `angular.toJson` will take an object and serialize it into a JSON formatted string.

Unlike the native function `JSON.stringify`, This function will remove all properties beginning with `$$` (as angular usually prefixes internal properties with `$$`)

```js
angular.toJson(object)

```

As data needs to be serialized before passing through a network, this function is useful to turn any data you wish to transmit into JSON.

This function is also useful for debugging as it works similarly to a `.toString` method would act.

**Examples:**

```js
angular.toJson({name: "barf", occupation: "mog", $$somebizzareproperty: 42})
// "{"name":"barf","occupation":"mog"}"
angular.toJson(42)
// "42"
angular.toJson([1, "2", 3, "4"])
// "[1,"2",3,"4"]"
var fn = function(value) {return value}
angular.toJson(fn)
// undefined, functions have no representation in JSON

```



## angular.copy


The `angular.copy` function takes an object, array or a value and creates a deep copy of it.

> 
angular.copy()


**Example:**

Objects:

```js
let obj = {name: "vespa", occupation: "princess"};
let cpy = angular.copy(obj);
cpy.name = "yogurt"
// obj = {name: "vespa", occupation: "princess"}
// cpy = {name: "yogurt", occupation: "princess"}

```

Arrays:

```js
var w = [a, [b, [c, [d]]]];
var q = angular.copy(w);
// q = [a, [b, [c, [d]]]]

```

At the above example `angular.equals(w, q)` will evaluate to true because `.equals` tests equality by value.
however `w === q` will evaluate to false because strict comparison between objects and arrays is done by reference.



## angular.isString


The function `angular.isString` returns true if the object or value given to it is of the type `string`

> 
angular.isString(value1)


**Examples**

```js
angular.isString("hello") // true
angular.isString([1, 2]) // false
angular.isString(42) // false

```

This is the equivalent of performing

```js
typeof someValue === "string"

```



## angular.isArray


The `angular.isArray` function returns true if and only if the object or value passed to it is of the type `Array`.

> 
angular.isArray(value)


**Examples**

```js
angular.isArray([]) // true
angular.isArray([2, 3]) // true
angular.isArray({}) // false
angular.isArray(17) // false

```

It is the equivalent of

```js
Array.isArray(someValue)

```



## angular.merge


The function angular.merge takes all the enumerable properties from the source object to deeply extend the destination object.

The function returns a reference to the now extended destination object

> 
angular.merge(destination, source)


**Examples**

```js
angular.merge({}, {}) // {} 
angular.merge({name: "king roland"}, {password: "12345"})
// {name: "king roland", password: "12345"}
angular.merge({a: 1}, [4, 5, 6]) // {0: 4, 1: 5, 2: 6, a: 1}
angular.merge({a: 1}, {b: {c: {d: 2} }}) // {"a":1,"b":{"c":{"d":2} }}

```



## angular.isDefined and angular.isUndefined


The function `angular.isDefined` tests a value if it is defined

> 
angular.isDefined(someValue)


This is the equivalent of performing

```js
value !== undefined; // will evaluate to true is value is defined

```

**Examples**

```js
angular.isDefined(42) // true
angular.isDefined([1, 2]) // true
angular.isDefined(undefined) // false
angular.isDefined(null) // true

```

The function `angular.isUndefined` tests if a value is undefined (it is effectively the opposite of `angular.isDefined`)

> 
angular.isUndefined(someValue)


This is the equivalent of performing

```js
value === undefined; // will evaluate to true is value is undefined

```

Or just

```js
!angular.isDefined(value)

```

**Examples**

```js
angular.isUndefined(42) // false
angular.isUndefined(undefined) // true

```



## angular.isDate


The `angular.isDate` function returns true if and only if the object passed to it is of the type Date.

> 
angular.isDate(value)


**Examples**

```js
angular.isDate("lone star") // false
angular.isDate(new Date()) // true

```



## angular.isFunction


The function `angular.isFunction` determines and returns true if and only if the value passed to is a reference to a function.

The function returns a reference to the now extended destination object

> 
angular.isFunction(fn)


**Examples**

```js
var onClick = function(e) {return e};
angular.isFunction(onClick); // true

var someArray = ["pizza", "the", "hut"];
angular.isFunction(someArray ); // false

```



## angular.noop


The `angular.noop` is a function that performs no operations, you pass `angular.noop` when you need to provide a function argument that will do nothing.

> 
angular.noop()


A common use for `angular.noop` can be to provide an empty callback to a function that will otherwise throw an error when something else than a function is passed to it.

**Example:**

```js
$scope.onSomeChange = function(model, callback) {
    updateTheModel(model);
    if (angular.isFunction(callback)) {
        callback();
    } else {
        throw new Error("error: callback is not a function!");
    }
};

$scope.onSomeChange(42, function() {console.log("hello callback")});
// will update the model and print 'hello callback'
$scope.onSomeChange(42, angular.noop);
// will update the model

```

**Additional examples:**

```js
angular.noop() // undefined
angular.isFunction(angular.noop) // true

```



## angular.isElement


The `angular.isElement` returns true if the argument passed to it is a DOM Element or a jQuery wrapped Element.

> 
angular.isElement(elem)


This function is useful to type check if a passed argument is an element before being processed as such.

**Examples:**

```js
angular.isElement(document.querySelector("body"))
// true
angular.isElement(document.querySelector("#some_id"))
// false if "some_id" is not using as an id inside the selected DOM
angular.isElement("<div></div>")
// false

```



## angular.isNumber


The `angular.isNumber` function returns true if and only if the object or value passed to it is of the type Number, this includes +Infinity, -Infinity and NaN

> 
angular.isNumber(value)


This function will not cause a type coercion such as

```js
"23" == 23 // true 

```

**Examples**

```js
angular.isNumber("23") // false
angular.isNumber(23) // true
angular.isNumber(NaN) // true
angular.isNumber(Infinity) // true

```

This function will not cause a type coercion such as

```js
"23" == 23 // true 

```



## angular.fromJson


The function `angular.fromJson` will deserialize a valid JSON string and return an Object or an Array.

> 
angular.fromJson(string|object)


Note that this function is not limited to only strings, it will output a representation of any object passed to it.

Examples:

```js
angular.fromJson("{\"yogurt\": \"strawberries\"}")
// Object {yogurt: "strawberries"}
angular.fromJson('{jam: "raspberries"}')
// will throw an exception as the string is not a valid JSON
angular.fromJson(this)
// Window {external: Object, chrome: Object, _gaq: Y, angular: Object, ng339: 3…}
angular.fromJson([1, 2])
// [1, 2]
typeof angular.fromJson(new Date())
// "object"

```



## angular.isObject


The `angular.isObject` return true if and only if the argument passed to it is an object, this function will also return true for an Array and will return false for `null` even though `typeof null` is `object` .

> 
angular.isObject(value)


This function is useful for type checking when you need a defined object to process.

**Examples:**

```js
angular.isObject({name: "skroob", job: "president"})
// true
angular.isObject(null)
// false
angular.isObject([null])
// true
angular.isObject(new Date())
// true
angular.isObject(undefined)
// false

```



## angular.identity


The `angular.identity` function returns the first argument passed to it.

> 
angular.identity(argument)


This function is useful for functional programming, you can provide this function as a default in case an expected function was not passed.

**Examples:**

```js
angular.identity(42) // 42

```

```js
var mutate = function(fn, num) {
    return angular.isFunction(fn) ? fn(num) : angular.identity(num)
}

mutate(function(value) {return value-7}, 42) // 35
mutate(null, 42) // 42
mutate("mount. rushmore", 42) // 42

```



## angular.forEach


The `angular.forEach` accepts an object and an iterator function. It then runs the iterator function over each enumerable property/value of the object. This function also works on arrays.

Like the JS version of `Array.prototype.forEach` The function does not iterate over inherited properties (prototype properties), however the function will not attempt to process a `null` or an `undefined` value and will just return it.

> 
angular.forEach(object, function(value, key) { // function});


**Examples:**

```js
angular.forEach({"a": 12, "b": 34}, (value, key) => console.log("key: " + key + ", value: " + value))
// key: a, value: 12
// key: b, value: 34
angular.forEach([2, 4, 6, 8, 10], (value, key) => console.log(key))
// will print the array indices: 1, 2, 3, 4, 5
angular.forEach([2, 4, 6, 8, 10], (value, key) => console.log(value))
// will print the array values: 2, 4, 6, 7, 10
angular.forEach(undefined, (value, key) => console.log("key: " + key + ", value: " + value))
// undefined


```

