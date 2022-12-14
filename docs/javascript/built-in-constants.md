---
metaTitle: "JavaScript - Built-in Constants"
description: "null, Testing for NaN using isNaN(), NaN, undefined and null, Infinity and -Infinity, Operations that return NaN, Math library functions that return NaN, Number constants"
---

# Built-in Constants



## null


`null` is used for representing the intentional absence of an object value and is a primitive value. Unlike `undefined`, it is not a property of the global object.

It is equal to `undefined` but not identical to it.

```js
null == undefined; // true
null === undefined; // false

```

**CAREFUL**: The `typeof` `null` is `'object'`.

```js
typeof null; // 'object';

```

To properly check if a value is `null`, compare it with the [strict equality operator](http://stackoverflow.com/documentation/javascript/208/boolean-logic/798/strictly-equal#t=201606190429161299094)

```js
var a = null;

a === null; // true

```



## Testing for NaN using isNaN()


### `window.isNaN()`

The global function `isNaN()` can be used to check if a certain value or expression evaluates to `NaN`. This function (in short) first checks if the value is a number, if not tries to convert it (*), and then checks if the resulting value is `NaN`. For this reason, **this testing method may cause confusion**.

<sup>(*) The "conversion" method is not that simple, see [ECMA-262 18.2.3](http://www.ecma-international.org/ecma-262/6.0/#sec-isnan-number) for a detailed explanation of the algorithm.</sup>

These examples will help you better understand the `isNaN()` behavior:

```js
isNaN(NaN);          // true
isNaN(1);            // false: 1 is a number
isNaN(-2e-4);        // false: -2e-4 is a number (-0.0002) in scientific notation
isNaN(Infinity);     // false: Infinity is a number
isNaN(true);         // false: converted to 1, which is a number
isNaN(false);        // false: converted to 0, which is a number
isNaN(null);         // false: converted to 0, which is a number
isNaN("");           // false: converted to 0, which is a number
isNaN(" ");          // false: converted to 0, which is a number
isNaN("45.3");       // false: string representing a number, converted to 45.3
isNaN("1.2e3");      // false: string representing a number, converted to 1.2e3
isNaN("Infinity");   // false: string representing a number, converted to Infinity
isNaN(new Date);     // false: Date object, converted to milliseconds since epoch
isNaN("10$");        // true : conversion fails, the dollar sign is not a digit
isNaN("hello");      // true : conversion fails, no digits at all
isNaN(undefined);    // true : converted to NaN
isNaN();             // true : converted to NaN (implicitly undefined)
isNaN(function(){}); // true : conversion fails
isNaN({});           // true : conversion fails
isNaN([1, 2]);       // true : converted to "1, 2", which can't be converted to a number

```

This last one is a bit tricky: checking if an `Array` is `NaN`. To do this, the `Number()` constructor first converts the array to a string, then to a number; this is the reason why `isNaN([])` and `isNaN([34])` both return `false`, but `isNaN([1, 2])` and `isNaN([true])` both return `true`: because they get converted to `""`, `"34"`, `"1,2"` and `"true"` respectively. In general, **an array is considered `NaN` by `isNaN()` unless it only holds one element whose string representation can be converted to a valid number**.

### `Number.isNaN()`

In ECMAScript 6, the `Number.isNaN()` function has been implemented primarily to avoid the problem of `window.isNaN()` of forcefully converting the parameter to a number. `Number.isNaN()`, indeed, **doesn't try to convert** the value to a number before testing. This also means that **only values of the type number, that are also `NaN`, return `true`** (which basically means only `Number.isNaN(NaN)`).

From [ECMA-262 20.1.2.4](http://www.ecma-international.org/ecma-262/6.0/#sec-number.isnan):

> 
When the `Number.isNaN` is called with one argument `number`, the following steps are taken:
<ol>
- If Type(number) is not Number, return `false`.
- If number is `NaN`, return `true`.
- Otherwise, return `false`.
</ol>


Some examples:

```js
// The one and only 
Number.isNaN(NaN);          // true

// Numbers
Number.isNaN(1);            // false
Number.isNaN(-2e-4);        // false
Number.isNaN(Infinity);     // false

// Values not of type number
Number.isNaN(true);         // false
Number.isNaN(false);        // false
Number.isNaN(null);         // false
Number.isNaN("");           // false
Number.isNaN(" ");          // false
Number.isNaN("45.3");       // false
Number.isNaN("1.2e3");      // false
Number.isNaN("Infinity");   // false
Number.isNaN(new Date);     // false
Number.isNaN("10$");        // false
Number.isNaN("hello");      // false
Number.isNaN(undefined);    // false
Number.isNaN();             // false
Number.isNaN(function(){}); // false
Number.isNaN({});           // false
Number.isNaN([]);           // false
Number.isNaN([1]);          // false
Number.isNaN([1, 2]);       // false
Number.isNaN([true]);       // false

```



## NaN


[`NaN`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/NaN) stands for "Not a Number." When a mathematical function or operation in JavaScript cannot return a specific number, it returns the value `NaN` instead.

It is a property of the global object, and a reference to [`Number.NaN`](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/Number/NaN)

```js
window.hasOwnProperty('NaN'); // true
NaN; // NaN

```

Perhaps confusingly, `NaN` is still considered a number.

```js
typeof NaN; // 'number'

```

Don't check for `NaN` using the equality operator. See [`isNaN`](https://stackoverflow.com/documentation/javascript/700/built-in-constants/1760/testing-for-nan-using-isnan#t=201607272211248187198) instead.

```js
NaN == NaN  // false
NaN === NaN // false

```



## undefined and null


At first glance it may appear that `null` and `undefined` are basically the same, however there are subtle but important differences.

`undefined` is the absence of a value in the compiler, because where it should be a value, there hasn't been put one, like the case of an unassigned variable.

<li>`undefined` is a global value that represents the absence of an assigned value.
<ul>
- `typeof undefined === 'undefined'`

- `typeof null === 'object'`

Setting a variable to `undefined` means the variable effectively does not exist. Some processes, such as JSON serialization, may strip `undefined` properties from objects. In contrast, `null` properties indicate will be preserved so you can explicitly convey the concept of an "empty" property.

The following evaluate to `undefined`:

<li>A variable when it is declared but not assigned a value (i.e. defined)
<ul>
<li>

```js
let foo;
console.log('is undefined?', foo === undefined);
// is undefined? true

```


</li>

<li>

```js
let foo = { a: 'a' };
console.log('is undefined?', foo.b === undefined);
// is undefined? true

```


</li>

<li>

```js
function foo() { return; }
console.log('is undefined?', foo() === undefined);
// is undefined? true

```


</li>

<li>

```js
function foo(param) { 
  console.log('is undefined?', param === undefined);
}
foo('a');
foo();
// is undefined? false
// is undefined? true

```


</li>

`undefined` is also a property of the global `window` object.

```js
// Only in browsers
console.log(window.undefined); // undefined
window.hasOwnProperty('undefined'); // true    

```

Before ECMAScript 5 you could actually change the value of the `window.undefined` property to any other value potentially breaking everything.



## Infinity and -Infinity


```js
1 / 0; // Infinity
// Wait! WHAAAT?

```

`Infinity` is a property of the global object (therefore a global variable) that represents mathematical infinity. It is a reference to `Number.POSITIVE_INFINITY`

It is greater than any other value, and you can get it by dividing by 0 or by evaluating the expression of a number that's so big that overflows. This actually means there is no division by 0 errors in JavaScript, there is Infinity!

There is also `-Infinity`  which is mathematical negative infinity, and it's lower than any other value.

To get `-Infinity` you negate `Infinity`, or get a reference to it in `Number.NEGATIVE_INFINITY`.

```js
- (Infinity); // -Infinity

```

Now let's have some fun with examples:

```js
Infinity > 123192310293; // true
-Infinity < -123192310293; // true
1 / 0; // Infinity
Math.pow(123123123, 9123192391023); // Infinity
Number.MAX_VALUE * 2; // Infinity
23 / Infinity; // 0
-Infinity; // -Infinity
-Infinity === Number.NEGATIVE_INFINITY; // true
-0; // -0 , yes there is a negative 0 in the language
0 === -0; // true
1 / -0; // -Infinity
1 / 0 === 1 / -0; // false
Infinity + Infinity; // Infinity

var a = 0, b = -0;

a === b; // true
1 / a === 1 / b; // false

// Try your own!

```



## Operations that return NaN


Mathematical operations on values other than numbers return NaN.

```js
"a" + 1
"b" * 3
"cde" - "e"
[1, 2, 3] * 2

```

An exception: Single-number arrays.

```js
[2] * [3]  // Returns 6

```

Also, remember that the `+` operator concatenates strings.

```js
"a" + "b"  // Returns "ab"

```

Dividing zero by zero returns `NaN`.

```js
0 / 0         // NaN

```

Note: In mathematics generally (unlike in JavaScript programming), dividing by zero is not possible.



## Math library functions that return NaN


Generally, `Math` functions that are given non-numeric arguments will return NaN.

```js
Math.floor("a")

```

The square root of a negative number returns NaN, because `Math.sqrt` does not support [imaginary](https://en.wikipedia.org/wiki/Imaginary_number) or [complex](https://en.wikipedia.org/wiki/Complex_number) numbers.

```js
Math.sqrt(-1)

```



## Number constants


The `Number` constructor has some built in constants that can be useful

In many cases the various operators in Javascript will break with values outside the range of (`Number.MIN_SAFE_INTEGER`, `Number.MAX_SAFE_INTEGER`)

Note that `Number.EPSILON` represents the different between one and the smallest `Number` greater than one, and thus the smallest possible difference between two different `Number` values. One reason to use this is due to the nature of how numbers are stored by JavaScript see [Check the equality of two numbers](http://stackoverflow.com/documentation/javascript/4562/numbers/16002/check-the-equality-of-two-numbers#t=201607291343405411584)

