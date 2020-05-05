---
metaTitle: "JavsScript - Server-sent events"
description: "Setting up a basic event stream to the server, Closing an event stream, Binding event listeners to EventSource"
---

# Server-sent events



## Setting up a basic event stream to the server


You can setup your client browser to listen in incoming server events using the `EventSource` object. You will need to supply the constructor a string of the path to the server' API enpoint the will subscribe the client to the server events.

Example:

`var eventSource = new EventSource("api/my-events");`

Events have names with which they are categorized and sent, and a listener must be setup to listen to each such event by name. the default event name is `message` and in order to listen to it you must use the appropriate event listener, `.onmessage`

```js
evtSource.onmessage = function(event) {
  var data = JSON.parse(event.data);
  // do something with data
}

```

The above function will run everytime the server will push an event to the client. Data is sent as `text/plain`, if you send JSON data you may want to parse it.



## Closing an event stream


An event stream to the server can be closed using the `EventSource.close()` method

```js
var eventSource = new EventSource("api/my-events");
// do things ...
eventSource.close(); // you will not receive anymore events from this object

```

The `.close()` method does nothing is the stream is already closed.



## Binding event listeners to EventSource


You can bind event listeners to the `EventSource` object to listen to different events channels using the `.addEventListener` method.

> 
EventSource.addEventListener(name: String, callback: Function, [options])


**name**: The name related to the name of the channel the server is emitting events to.

**callback**: The callback function runs every time an event bound to the channel is emitted, the function provides the `event` as an argument.

**options**: Options that characterize the behavior of the event listener.

The following example shows a heartbeat event stream from the server, the server sends events on the `heartbeat` channel and this routine will always run when an event in accepted.

```js
var eventSource = new EventSource("api/heartbeat");
...
eventSource.addEventListener("heartbeat", function(event) {
  var status = event.data;
  if (status=='OK') { 
    // do something
  }
});

```



#### Syntax


- new EventSource("api/stream");
- eventSource.onmessage=function(event){}
- eventSource.onerror=function(event){};
- eventSource.addEventListener=function(name, callback, options){};
- eventSource.readyState;
- eventSource.url;
- eventSource.close();

