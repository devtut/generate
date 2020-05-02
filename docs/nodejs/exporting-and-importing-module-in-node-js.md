---
metaTitle: "Exporting and Importing Module in node.js"
description: "Using a simple module in node.js, Exporting with ES6 syntax, Using Imports In ES6"
---

# Exporting and Importing Module in node.js



## Using a simple module in node.js


What is a node.js module ([link to article](https://www.sitepoint.com/understanding-module-exports-exports-node-js/)):

> 
A module encapsulates related code into a single unit of code. When creating a module, this can be interpreted as moving all related functions into a file.


Now lets see an example. Imagine all files are in same directory:

File: `printer.js`

```js
"use strict";

exports.printHelloWorld = function (){
    console.log("Hello World!!!");
}

```

Another way of using modules:

File `animals.js`

```js
"use strict";

module.exports = {
    lion: function() {
        console.log("ROAARR!!!");
    }

};

```

File: `app.js`

Run this file by going to your directory and typing: `node app.js`

```js
"use strict";

//require('./path/to/module.js') node which module to load
var printer = require('./printer');
var animals = require('./animals');

printer.printHelloWorld(); //prints "Hello World!!!"
animals.lion(); //prints "ROAARR!!!"

```



## Exporting with ES6 syntax


This is the equivalent of [the other example](http://stackoverflow.com/documentation/node.js/1173/exporting-a-module-in-node-js/3787/using-a-simple-module-in-node-js) but using ES6 instead.

```js
export function printHelloWorld() {
  console.log("Hello World!!!");
}

```



## Using Imports In ES6


Node.js is built against modern versions of V8. By keeping up-to-date with the latest releases of this engine, we ensure new features from the JavaScript ECMA-262 specification are brought to Node.js developers in a timely manner, as well as continued performance and stability improvements.

All ECMAScript 2015 (ES6) features are split into three groups for shipping, staged, and in progress features:

All shipping features, which V8 considers stable, are turned on by default on Node.js and do NOT require any kind of runtime flag.
Staged features, which are almost-completed features that are not considered stable by the V8 team, require a runtime flag: --harmony.
In progress features can be activated individually by their respective harmony flag, although this is highly discouraged unless for testing purposes. Note: these flags are exposed by V8 and will potentially change without any deprecation notice.

Currently ES6 supports import statements natively [Refer here](https://developer.mozilla.org/en/docs/web/javascript/reference/statements/import)

So if we have a file called `fun.js`…

```js
export default function say(what){
  console.log(what);
}

export function sayLoud(whoot) {
  say(whoot.toUpperCase());
}

```

…and if there was another file named `app.js` where we want to put our previously defined functions to use, there are three ways how to import them.

**Import default**

```js
import say from './fun';
say('Hello Stack Overflow!!');  // Output: Hello Stack Overflow!!

```

Imports the `say()` function because it is marked as the default export in the source file (`export default …`)

**Named imports**

```js
import { sayLoud } from './fun';
sayLoud('JS modules are awesome.'); // Output: JS MODULES ARE AWESOME.

```

Named imports allow us to import exactly the parts of a module we actually need. We do this by explicitly naming them. In our case by naming `sayLoud` in curly brackets within the import statement.

**Bundled import**

```js
import * as i from './fun';
i.say('What?'); // Output: What?
i.sayLoud('Whoot!'); // Output: WHOOT!

```

If we want to have it all, this is the way to go. By using the syntax `* as i` we have the `import` statement provide us with an object `i` that holds all exports of our `fun` module as correspondingly named properties.

****Paths****

Keep in mind that you have to explicitly mark your import paths as **relative paths** even if the file to be imported resided in the same directory like the file you are importing into by using `./`. Imports from unprefixed paths like

```js
import express from 'express';

```

will be looked up in the local and global `node_modules` folders and will throw an error if no matching modules are found.

