---
metaTitle: "Arrow Functions"
description: "Introduction, Lexical Scoping & Binding (Value of this), Arguments Object, Implicit Return, Arrow functions as a constructor, Explicit Return"
---

# Arrow Functions


Arrow functions are a concise way of writing [anonymous](http://stackoverflow.com/documentation/javascript/186/functions/726/anonymous-function#t=201701181955410570791), lexically scoped functions in [ECMAScript 2015 (ES6)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/New_in_JavaScript/ECMAScript_2015_support_in_Mozilla).



## Introduction


In JavaScript, functions may be anonymously defined using the "arrow" (`=>`) syntax, which is sometimes referred to as a **lambda expression** due to [Common Lisp similarities](http://stackoverflow.com/documentation/common-lisp/534/getting-started-with-common-lisp/27585/lambda-expressions-and-anonymous-functions#t=201701191214171703263).

The simplest form of an arrow function has its arguments on the left side of `=>` and the return value on the right side:

```js
item => item + 1 // -> function(item){return item + 1}

```

This function can be [immediately invoked](http://stackoverflow.com/documentation/javascript/186/functions/843/immediately-invoked-function-expressions#t=201701191225012327274) by providing an argument to the expression:

```js
(item => item + 1)(41) // -> 42

```

If an arrow function takes a single parameter, the parentheses around that parameter are optional. For example, the following expressions assign the same type of function into [constant variables](http://stackoverflow.com/documentation/javascript/3059/declarations-and-assignments/3965/declaring-and-initializing-constants#t=201701191238586119513):

```js
const foo = bar => bar + 1;
const bar = (baz) => baz + 1;

```

However, if the arrow function takes no parameters, or more than one parameter, a new set of parentheses **must** encase all the arguments:

```js
(() => "foo")() // -> "foo"

((bow, arrow) => bow + arrow)('I took an arrow ', 'to the knee...')
// -> "I took an arrow to the knee..."

```

If the function body doesn't consist of a single expression, it must be surrounded by brackets and use an explicit `return` statement for providing a result:

```js
(bar => {
  const baz = 41;
  return bar + baz;
})(1); // -> 42

```

If the arrow function's body consists only of an object literal, this object literal has to be enclosed in parentheses:

```js
(bar => ({ baz: 1 }))(); // -> Object {baz: 1}

```

The extra parentheses indicate that the opening and closing brackets are part of the object literal, i.e. they are not delimiters of the function body.



## Lexical Scoping & Binding (Value of "this")


Arrow functions are [lexically scoped](http://stackoverflow.com/questions/1047454/what-is-lexical-scope); this means that their `this` Binding is bound to the context of the surrounding scope. That is to say, whatever `this` refers to can be preserved by using an arrow function.

Take a look at the following example. The class `Cow` has a method that allows for it to print out the sound it makes after 1 second.

```js
class Cow {

  constructor() {
    this.sound = "moo";
  }

  makeSoundLater() {
    setTimeout(() => console.log(this.sound), 1000);
  }
}

const betsy = new Cow();

betsy.makeSoundLater();

```

In the `makeSoundLater()` method, the `this` context refers to the current instance of the `Cow` object, so in the case where I call `betsy.makeSoundLater()`, the `this` context refers to `betsy`.

By using the arrow function, I **preserve** the `this` context so that I can make reference to `this.sound` when it comes time to print it out, which will properly print out "moo".

If you had used a regular [function](http://stackoverflow.com/documentation/javascript/186/functions) in place of the arrow function, you would lose the context of being within the class, and not be able to directly access the `sound` property.



## Arguments Object


Arrow functions do not expose an arguments object; therefore, `arguments` would simply refer to a variable in the current scope.

```js
const arguments = [true];
const foo = x => console.log(arguments[0]);

foo(false); // -> true

```

Due to this, arrow functions are also **not** aware of their caller/callee.

While the lack of an arguments object can be a limitation in some edge cases, rest parameters are generally a suitable alternative.

```js
const arguments = [true];
const foo = (...arguments) => console.log(arguments[0]);

foo(false); // -> false

```



## Implicit Return


Arrow functions may implicitly return values by simply omitting the curly braces that traditionally wrap a function's body if their body only contains a single expression.

```js
const foo = x => x + 1;
foo(1); // -> 2

```

When using implicit returns, object literals must be wrapped in parenthesis so that the curly braces are not mistaken for the opening of the function's body.

```js
const foo = () => { bar: 1 } // foo() returns undefined
const foo = () => ({ bar: 1 }) // foo() returns {bar: 1}

```



## Arrow functions as a constructor


Arrow functions will throw a `TypeError` when used with the `new` keyword.



## Explicit Return


Arrow functions can behave very similar to classic [functions](http://stackoverflow.com/documentation/javascript/186/functions) in that you may explicitly return a value from them using the `return` keyword; simply wrap your function's body in curly braces, and return a value:

```js
const foo = x => {
  return x + 1;
}

foo(1); // -> 2

```



#### Syntax


<li>
x => y                // Implicit return
</li>
<li>
x => { return y }     // Explicit return
</li>
<li>
(x, y, z) => { ... }  // Multiple arguments
</li>
<li>
async () => { ... }   // Async arrow functions
</li>
<li>
(() => { ... })()     // Immediately-invoked function expression
</li>
<li>
const myFunc = x
=> x*2 // A line break before the arrow will throw a 'Unexpected token' error
</li>
<li>
const myFunc = x =>
x*2  // A line break after the arrow is a valid syntax
</li>



#### Remarks


For more information on functions in JavaScript, please view the [Functions](http://stackoverflow.com/documentation/javascript/186/functions) documentation.

Arrow functions are part of the ECMAScript 6 specification, so [browser support](http://caniuse.com/#feat=arrow-functions) may be limited. The following table shows the earliest browser versions that support arrow functions.

<th align="center">Chrome</th><th align="center">Edge</th><th align="center">Firefox</th><th align="center">Internet Explorer</th><th align="center">Opera</th><th align="center">Opera Mini</th><th align="center">Safari</th>
|------
<td align="center">45</td><td align="center">12</td><td align="center">22</td><td align="center">**Currently unavailable**</td><td align="center">32</td><td align="center">**Currently unavailable**</td><td align="center">10</td>

