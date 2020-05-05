---
metaTitle: "JavsScript - Anti-patterns"
description: "Chaining assignments in var declarations."
---

# Anti-patterns



## Chaining assignments in var declarations.


Chaining assignments as part of a `var` declaration will create global variables unintentionally.

For example:

```js
(function foo() {    
    var a = b = 0;
})()
console.log('a: ' + a);
console.log('b: ' + b);

```

Will result in:

```js
Uncaught ReferenceError: a is not defined
'b: 0'

```

In the above example, `a` is local but `b` becomes global. This is because of the right to left evaluation of the `=` operator. So the above code actually evaluated as

```js
var a = (b = 0);

```

The correct way to chain var assignments is:

```js
var a, b;
a = b = 0;

```

Or:

```js
var a = 0, b = a;

```

This will make sure that both `a` and `b` will be local variables.

