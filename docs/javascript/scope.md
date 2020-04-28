---
metaTitle: "Scope"
description: "Closures, Hoisting, Difference between var and let, Apply and Call syntax and invocation., Arrow function invocation, Bound invocation, Method invocation, Anonymous invocation, Constructor invocation, Using let in loops instead of var (click handlers example)"
---

# Scope



## Closures


When a function is declared, variables in the context of its **declaration** are captured in its scope. For example, in the code below, the variable `x` is bound to a value in the outer scope, and then the reference to `x` is captured in the context of `bar`:

```js
var x = 4; // declaration in outer scope

function bar() {
    console.log(x); // outer scope is captured on declaration
}

bar(); // prints 4 to console

```

> 
Sample output: `4`


This concept of "capturing" scope is interesting because we can use and modify variables from an outer scope even after the outer scope exits. For example, consider the following:

```js
function foo() {
    var x = 4; // declaration in outer scope

    function bar() {
        console.log(x); // outer scope is captured on declaration
    }

    return bar;
    
    // x goes out of scope after foo returns
}

var barWithX = foo();
barWithX(); // we can still access x

```

> 
Sample output: `4`


In the above example, when `foo` is called, its context is captured in the function `bar`. So even after it returns, `bar` can still access and modify the variable `x`. The function `foo`, whose context is captured in another function, is said to be a **closure**.

### Private data

This lets us do some interesting things, such as defining "private" variables that are visible only to a specific function or set of functions. A contrived (but popular) example:

```js
function makeCounter() {
    var counter = 0;

    return {
        value: function () {
            return counter;
        },
        increment: function () {
            counter++;
        }
    };
}

var a = makeCounter();
var b = makeCounter();

a.increment();

console.log(a.value());
console.log(b.value());

```

> 
Sample output:
<pre>
1
0
</pre>


```js
var $ = jQuery;
// we've just polluted the global namespace by assigning window.$ to jQuery

```

In the following example, an IIFE is used to ensure that the `$` is bound to `jQuery` only in the context created by the closure:

```js
(function ($) {
    // $ is assigned to jQuery here
})(jQuery);
// but window.$ binding doesn't exist, so no pollution

```

See [the canonical answer on Stackoverflow](http://stackoverflow.com/a/111111/2209007) for more information on closures.



## Hoisting


### What is hoisting?

**Hoisting** is a mechanism which moves all variable and function declarations to the top of their scope. However, variable assignments still happen where they originally were.

For example, consider the following code:

```js
console.log(foo);  // → undefined
var foo = 42;
console.log(foo);  // → 42

```

The above code is the same as:

```js
var foo;             // → Hoisted variable declaration
console.log(foo);    // → undefined
foo = 42;            // → variable assignment remains in the same place
console.log(foo);    // → 42

```

Note that due to hoisting the above `undefined` is not the same as the `not defined` resulting from running:

```js
console.log(foo);    // → foo is not defined 

```

A similar principle applies to functions. When functions are assigned to a variable (i.e. a [function expression](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/function)), the variable declaration is hoisted while the assignment remains in the same place. The following two code snippets are equivalent.

```js
console.log(foo(2, 3));     // → foo is not a function

var foo = function(a, b) {
    return a * b;
}

```

```js
var foo;
console.log(foo(2, 3));     // → foo is not a function
foo = function(a, b) {
    return a * b;
}

```

When declaring [function statements](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/function), a different scenario occurs. Unlike function statements, function declarations are hoisted to the top of their scope. Consider the following code:

```js
console.log(foo(2, 3));  // → 6
function foo(a, b) {
    return a * b;
}

```

The above code is the same as the next code snippet due to hoisting:

```js
function foo(a, b) {
    return a * b;
}

console.log(foo(2, 3));  // → 6

```

Here are some examples of what is and what isn't hoisting:

```js
// Valid code:
foo();

function foo() {}

// Invalid code:
bar();                     // → TypeError: bar is not a function
var bar = function () {};


// Valid code:
foo();
function foo() {
    bar();
}
function bar() {}


// Invalid code:
foo();
function foo() {
    bar();                // → TypeError: bar is not a function
}
var bar = function () {};


// (E) valid:
function foo() {
    bar();
}
var bar = function(){};
foo();

```

### Limitations of Hoisting

Initializing a variable can not be Hoisted or In simple JavaScript Hoists declarations not initialization.

For example: The below scripts will give different outputs.

```js
var x = 2; 
var y = 4; 
alert(x + y);

```

This will give you an output of 6. But this...

```js
var x = 2; 
alert(x + y);
var y = 4; 

```

This will give you an output of NaN. Since we are initializing the value of y, the JavaScript Hoisting is not happening, so the y value will be undefined. The JavaScript will consider that y is not yet declared.

So the second example is same as of below.

```js
var x = 2; 
var y;
alert(x + y);
y = 4; 

```

This will give you an output of NaN.

[<img src="https://i.stack.imgur.com/aq14V.png" alt="enter image description here" />](https://i.stack.imgur.com/aq14V.png)



## Difference between var and let


**(Note: All examples using `let` are also valid for `const`)**

`var` is available in all versions of JavaScript, while `let` and `const` are part of ECMAScript 6 and [only available in some newer browsers](http://caniuse.com/#search=block%20level).

`var` is scoped to the containing function or the global space, depending when it is declared:

```js
var x = 4; // global scope

function DoThings() {
    var x = 7; // function scope
    console.log(x);
}

console.log(x); // >> 4
DoThings();     // >> 7
console.log(x); // >> 4

```

That means it "escapes" `if` statements and all similar block constructs:

```js
var x = 4;
if (true) {
    var x = 7;
}
console.log(x); // >> 7

for (var i = 0; i < 4; i++) {
    var j = 10;
}
console.log(i); // >> 4
console.log(j); // >> 10

```

By comparison, `let` is block scoped:

```js
let x = 4;

if (true) {
    let x = 7;
    console.log(x); // >> 7
}

console.log(x); // >> 4

for (let i = 0; i < 4; i++) {
    let j = 10;
}
console.log(i); // >> "ReferenceError: i is not defined"
console.log(j); // >> "ReferenceError: j is not defined"

```

Note that `i` and `j` are only declared in the `for` loop and are therefore undeclared outside of it.

There are several other crucial differences:

### Global variable declaration

In the top scope (outside any functions and blocks), `var` declarations put an element in the global object. `let` does not:

```js
var x = 4;
let y = 7;

console.log(this.x); // >> 4
console.log(this.y); // >> undefined

```

### Re-declaration

Declaring a variable twice using `var` doesn't produce an error (even though it's equivalent to declaring it once):

```js
var x = 4;
var x = 7;

```

With `let`, this produces an error:

```js
let x = 4;
let x = 7;

```

> 
TypeError: Identifier `x` has already been declared


The same is true when `y` is declared with `var`:

```js
var y = 4;
let y = 7;

```

> 
TypeError: Identifier `y` has already been declared


However variables declared with let can be reused (not re-declared) in a nested block

```js
let i = 5;    
{
   let i = 6;
   console.log(i); // >> 6
}
console.log(i); // >> 5

```

Within the block the outer `i` can be accessed, but if the within block has a `let` declaration for `i`, the outer `i` can not be accessed and will throw a `ReferenceError` if used before the second is declared.

```js
let i = 5;
{
    i = 6;  // outer i is unavailable within the Temporal Dead Zone
    let i;
}

```

> 
ReferenceError: i is not defined


### Hoisting

Variables declared both with `var` and `let` are [hoisted](http://stackoverflow.com/documentation/javascript/480/scope/1576/hoisting#t=201607211958234904044). The difference is that a variable declared with `var` can be referenced before its own assignment, since it gets automatically assigned (with `undefined` as its value), but `let` cannot–it specifically requires the variable to be declared before being invoked:

```js
console.log(x); // >> undefined
console.log(y); // >> "ReferenceError: `y` is not defined"
//OR >> "ReferenceError: can't access lexical declaration `y` before initialization"
var x = 4;
let y = 7;

```

The area between the start of a block and a `let` or `const` declaration is known as the [Temporal Dead Zone](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Statements/let#Temporal_dead_zone_and_errors_with_let), and any references to the variable in this area will cause a `ReferenceError`. This happens even if the [variable is assigned before being declared](http://stackoverflow.com/questions/41451181/does-let-override-a-global-declaration-and-throws-a-referenceerror):

```js
y=7; // >> "ReferenceError: `y` is not defined"
let y;

```

In non-strict-mode, [assigning a value to a variable without any declaration, automatically declares the variable in the global scope](http://stackoverflow.com/documentation/javascript/381/strict-mode/7424/changes-to-global-properties#t=201701041227599514039). In this case, instead of `y` being automatically declared in the global scope, `let` reserves the variable's name (`y`) and does not allow any access or assignment to it before the line where it is declared/initialized.



## Apply and Call syntax and invocation.


The `apply` and `call` methods in every function allow it to provide a custom value for `this`.

```js
function print() {
    console.log(this.toPrint);
}

print.apply({ toPrint: "Foo" }); // >> "Foo"
print.call({ toPrint: "Foo" }); // >> "Foo"

```

You might notice that the syntax for both the invocations used above are the same.
i.e. The signature looks similar.

But there is a small difference in their usage, since we are dealing with functions and changing their scopes, we still need to maintain the original arguments passed to the function. Both `apply` and `call` support passing arguments to the target function as follows:

```js
function speak() {
    var sentences = Array.prototype.slice.call(arguments);
    console.log(this.name+": "+sentences);
}
var person = { name: "Sunny" };
speak.apply(person, ["I", "Code", "Startups"]); // >> "Sunny: I Code Startups"
speak.call(person, "I", "<3", "Javascript"); // >> "Sunny: I <3 Javascript"

```

Notice that `apply` allows you to pass an `Array` or the `arguments` object (array-like) as the list of arguments, whereas, `call` needs you to pass each argument separately.

These two methods give you the freedom to get as fancy as you want, like implementing a poor version of the ECMAScript's native `bind` to create a function that will always be called as a method of an object from an original function.

```js
function bind (func, obj) { 
    return function () {
        return func.apply(obj, Array.prototype.slice.call(arguments, 1));
    }
}

var obj = { name: "Foo" };

function print() {
    console.log(this.name);
}

printObj = bind(print, obj);

printObj();

```

This will log

> 
"Foo"


The `bind` function has a lot going on

1. `obj` will be used as the value of `this`
1. forward the arguments to the function
1. and then return the value



## Arrow function invocation


When using arrow functions `this` takes the value from the enclosing execution context's `this` (that is, `this` in arrow functions has lexical scope rather than the usual dynamic scope). In global code (code that doesn't belong to any function) it would be the global object. And it keeps that way, even if you invoke the function declared with the arrow notation from any of the others methods here described.

```js
var globalThis = this; //"window" in a browser, or "global" in Node.js

var foo = (() => this);           

console.log(foo() === globalThis);          //true

var obj = { name: "Foo" };
console.log(foo.call(obj) === globalThis);  //true

```

See how `this` inherits the context rather than referring to the object the method was called on.

```js
var globalThis = this;

var obj = {
    withoutArrow: function() {
        return this;
    },
    withArrow: () => this
};

console.log(obj.withoutArrow() === obj);      //true
console.log(obj.withArrow() === globalThis);  //true

var fn = obj.withoutArrow; //no longer calling withoutArrow as a method
var fn2 = obj.withArrow;
console.log(fn() === globalThis);             //true
console.log(fn2() === globalThis);            //true

```



## Bound invocation


The `bind` method of every function allows you to create new version of that function with the context strictly bound to a specific object. It is specially useful to force a function to be called as a method of an object.

```js
var obj = { foo: 'bar' };

function foo() {
    return this.foo;
}

fooObj = foo.bind(obj);

fooObj();

```

This will log:

> 
bar




## Method invocation


Invoking a function as a method of an object the value of `this` will be that object.

```js
var obj = {
    name: "Foo",
    print: function () {
        console.log(this.name)
    }
}

```

We can now invoke print as a method of obj. `this` will be obj

```js
obj.print();

```

This will thus log:

> 
Foo




## Anonymous invocation


Invoking a function as an anonymous function, `this` will be the global object (`self` in the browser).

```js
function func() {
    return this;
}

func() === window; // true

```

In [ECMAScript 5's strict mode](http://stackoverflow.com/documentation/javascript/381/strict-mode#t=201606190406546634623), `this` will be `undefined` if the function is invoked anonymously.

```js
(function () {
    "use strict";
    func();
}())

```

This will output

> 
`undefined`




## Constructor invocation


When a function is invoked as a constructor with the `new` keyword `this` takes the value of the object being constructed

```js
function Obj(name) {
    this.name = name;
}

var obj = new Obj("Foo");

console.log(obj);

```

This will log

> 
{ name: "Foo" }




## Using let in loops instead of var (click handlers example)


Let's say we need to add a button for each piece of `loadedData` array (for instance, each button should be a slider showing the data; for the sake of simplicity, we'll just alert a message). One may try something like this:

```js
for(var i = 0; i < loadedData.length; i++)
    jQuery("#container").append("<a class='button'>"+loadedData[i].label+"</a>")
        .children().last() // now let's attach a handler to the button which is a child
        .on("click",function() { alert(loadedData[i].content); });

```

But instead of alerting, each button will cause the

> 
TypeError: loadedData[i] is undefined


error. This is because the scope of `i` is the global scope (or a function scope) and after the loop, `i == 3`. What we need is not to "remember the state of `i`". This can be done using `let`:

```js
for(let i = 0; i < loadedData.length; i++)
    jQuery("#container").append("<a class='button'>"+loadedData[i].label+"</a>")
        .children().last() // now let's attach a handler to the button which is a child
        .on("click",function() { alert(loadedData[i].content); });

```

An example of `loadedData` to be tested with this code:

```

   var loadedData = [
        { label:"apple",      content:"green and round" },
        { label:"blackberry", content:"small black or blue" },
        { label:"pineapple",  content:"weird stuff.. difficult to explain the shape" }
    ];

```

[A fiddle to illustrate this](https://jsfiddle.net/fvgqu7a2/2/)



#### Remarks


Scope is the context in which variables live and can be accessed by other code in the same scope. Because JavaScript can largely be used as a functional programming language, knowing the scope of variables and functions is important as it helps to prevent bugs and unexpected behavior at runtime.

