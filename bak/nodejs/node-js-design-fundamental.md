---
metaTitle: "Node.js - Node.js Design Fundamental"
description: "The Node.js philosophy"
---

# Node.js Design Fundamental



## The Node.js philosophy


**Small Core**, **Small Module** :-

Build small and single purpose modules not in term of code size only, but also in term of scope that serves a single purpose

```

   a - "Small is beautiful"
    b - "Make each program do one thing well."

```

**The Reactor Pattern**

The Reactor Pattern is the heart of the `node.js` asynchronous nature. Allowed the system to be implemented as a single-threaded process with a series of event generators and event handlers, with the help of event loop that runs continuously.

**The non-blocking I/O engine of Node.js – libuv** -

**The Observer Pattern**(EventEmitter)
maintains a list of dependents/observers and notifies them

```js
var events = require('events');
var eventEmitter = new events.EventEmitter();

var ringBell = function ringBell()
{
  console.log('tring tring tring');
}
eventEmitter.on('doorOpen', ringBell);

eventEmitter.emit('doorOpen');

```

