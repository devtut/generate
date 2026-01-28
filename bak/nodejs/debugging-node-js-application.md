---
metaTitle: "Node.js - Debugging Node.js application"
description: "Core node.js debugger and node inspector"
---

# Debugging Node.js application



## Core node.js debugger and node inspector


### Using core debugger

Node.js provides a build in non graphical debugging utility. To start the build in the debugger, start the application with this command:

```js
node debug filename.js

```

Consider the following simple Node.js application contained in the `debugDemo.js`

```js
'use strict';

function addTwoNumber(a, b){
// function returns the sum of the two numbers
debugger
  return a + b;
}

var result = addTwoNumber(5, 9);
console.log(result);

```

The keyword `debugger` will stop the debugger at that point in the code.

### Command reference

1. Stepping

```js
cont, c - Continue execution
next, n - Step next
step, s - Step in
out, o - Step out

```


1. Breakpoints

```js
setBreakpoint(), sb() - Set breakpoint on current line
setBreakpoint(line), sb(line) - Set breakpoint on specific line

```

To Debug the above code run the following command

```js
node debug debugDemo.js

```

Once the above commands runs you will see the following output. To exit from the debugger interface, type `process.exit()`

[<img src="https://i.stack.imgur.com/XSJMF.png" alt="enter image description here" />](https://i.stack.imgur.com/XSJMF.png)

Use `watch(expression)` command to add the variable or expression whose value you want to watch and `restart` to restart the app and debugging.

Use `repl` to enter code interactively. The repl mode has the same context as the line you are debugging. This allows you to examine the contents of variables and test out lines of code. Press `Ctrl+C` to leave the debug repl.

### Using Built-in Node inspector

You can run node's [built in](https://nodejs.org/api/debugger.html#debugger_v8_inspector_integration_for_node_js) v8 inspector! The [node-inspector](https://github.com/node-inspector/node-inspector) plug-in is not needed anymore.

Simply pass the inspector flag and you'll be provided with a URL to the inspector

```js
node --inspect server.js

```

### Using Node inspector

Install the node inspector:

```js
npm install -g node-inspector

```

Run your app with the node-debug command:

```js
node-debug filename.js

```

After that, hit in Chrome:

```js
http://localhost:8080/debug?port=5858

```

Sometimes port 8080 might not be available on your computer. You may get the following error:

> 
Cannot start the server at 0.0.0.0:8080. Error: listen EACCES.


In this case, start the node inspector on a different port using the following command.

```js
$node-inspector --web-port=6500

```

You will see something like this:

[<img src="https://i.stack.imgur.com/JpaL6.png" alt="enter image description here" />](https://i.stack.imgur.com/JpaL6.png)

