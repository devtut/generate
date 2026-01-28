---
metaTitle: "Node.js - Event Emitters"
description: "Basics, HTTP Analytics through an Event Emitter, Get the names of the events that are subscribed to, Get the number of listeners registered to listen for a specific event"
---

# Event Emitters



## Basics


Event Emitters are built into Node, and are for pub-sub, a pattern where a **publisher** will emit events, which **subscribers** can listen and react to. In Node jargon, publishers are called **Event Emitters**, and they emit events, while subscribers are called **listeners**, and they react to the events.

In the above example, the dog is the publisher/EventEmitter, while the function that checks the item was the subscriber/listener. You can make more listeners too:

There can also be multiple listeners for a single event, and even remove listeners:

If you want to listen to a event only once, you can use:

Which will remove the listener automatically without race conditions.



## HTTP Analytics through an Event Emitter


In the HTTP server code (e.g. `server.js`):

```js
const EventEmitter = require('events')
const serverEvents = new EventEmitter()

// Set up an HTTP server
const http = require('http')
const httpServer = http.createServer((request, response) => {
  // Handler the request...
  // Then emit an event about what happened
  serverEvents.emit('request', request.method, request.url)
});

// Expose the event emitter
module.exports = serverEvents

```

In supervisor code (e.g. `supervisor.js`):

```js
const server = require('./server.js')
// Since the server exported an event emitter, we can listen to it for changes:
server.on('request', (method, url) => {
  console.log(`Got a request: ${method} ${url}`)
})

```

Whenever the server gets a request, it will emit an event called `request` which the supervisor is listening for, and then the supervisor can react to the event.



## Get the names of the events that are subscribed to


The function **EventEmitter.eventNames()** will return an array containing the names of the events currently subscribed to.

```js
const EventEmitter = require("events");
class MyEmitter extends EventEmitter{}

var emitter = new MyEmitter();

emitter
.on("message", function(){ //listen for message event
    console.log("a message was emitted!");
})
.on("message", function(){ //listen for message event
    console.log("this is not the right message");
})
.on("data", function(){ //listen for data event
    console.log("a data just occured!!");
});

console.log(emitter.eventNames()); //=> ["message","data"]
emitter.removeAllListeners("data");//=> removeAllListeners to data event
console.log(emitter.eventNames()); //=> ["message"]

```

[Run in RunKit](https://runkit.com/594bb4eaaac7e6001294132c/594bb635aac7e600129413e7)



## Get the number of listeners registered to listen for a specific event


The function Emitter.listenerCount(eventName) will return the number of listeners that are currently listening for the event provided as argument

```js
const EventEmitter = require("events");
class MyEmitter extends EventEmitter{}
var emitter = new MyEmitter();

emitter
.on("data", ()=>{ // add listener for data event
    console.log("data event emitter");
});

console.log(emitter.listenerCount("data"))    // => 1
console.log(emitter.listenerCount("message")) // => 0

emitter.on("message", function mListener(){ //add listener for message event
    console.log("message event emitted");
});
console.log(emitter.listenerCount("data"))    // => 1
console.log(emitter.listenerCount("message")) // => 1

emitter.once("data", (stuff)=>{ //add another listener for data event
    console.log(`Tell me my ${stuff}`);
})

console.log(emitter.listenerCount("data"))   // => 2
console.log(emitter.listenerCount("message"))// => 1

```



#### Remarks


When an event "fires" (which means the same as "publishing an event" or "emitting an event"), each listener will be called synchronously ([source](https://nodejs.org/dist/latest-v6.x/docs/api/events.html#events_asynchronous_vs_synchronous)), along with any accompanying data that was passed in to `emit()`, no matter how many arguments you pass in:

The listeners will be called in the order they were registered:

But if you need a listener to fire first, before all of the other listeners that have already been added, you can use `prependListener()` like so:

If you need to listen to an event, but you only want to hear about it once, you can use `once` instead of `on`, or `prependOnceListener` instead of `prependListener`. After the event is fired and the listener gets called, the listener will automatically be removed, and won't be called again the next time the event is fired.

Finally, if you want to remove all of the listeners and start over, feel free to do just that:

