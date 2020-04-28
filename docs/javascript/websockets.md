---
metaTitle: "WebSockets"
description: "Working with string messages, Establish a web socket connection, Working with binary messages, Making a secure web socket connection"
---

# WebSockets


WebSocket is protocol, which enables two-way communication between a client and server:

The goal WebSocket is to provide a mechanism for browser-based
applications that need two-way communication with servers that does
not rely on opening multiple HTTP connections. ([RFC 6455](https://tools.ietf.org/html/rfc6455))

WebSocket works over HTTP protocol.



## Working with string messages


```js
var wsHost = "ws://my-sites-url.com/path/to/echo-web-socket-handler";
var ws = new WebSocket(wsHost);
var value = "an example message";

//onmessage : Event Listener - Triggered when we receive message form server
ws.onmessage = function(message) {
    if (message === value) {
        console.log("The echo host sent the correct message.");
    } else {
        console.log("Expected: " + value);
        console.log("Received: " + message);
    }
};

//onopen : Event Listener - event is triggered when websockets readyState changes to open which means now we are ready to send and receives messages from server
ws.onopen = function() {
    //send is used to send the message to server
    ws.send(value);
};

```



## Establish a web socket connection


```js
var wsHost = "ws://my-sites-url.com/path/to/web-socket-handler";
var ws = new WebSocket(wsHost);

```



## Working with binary messages


```js
var wsHost = "http://my-sites-url.com/path/to/echo-web-socket-handler";
var ws = new WebSocket(wsHost);
var buffer = new ArrayBuffer(5); // 5 byte buffer
var bufferView = new DataView(buffer);

bufferView.setFloat32(0, Math.PI);
bufferView.setUint8(4, 127);

ws.binaryType = 'arraybuffer';

ws.onmessage = function(message) {
    var view = new DataView(message.data);
    console.log('Uint8:', view.getUint8(4), 'Float32:', view.getFloat32(0))
};

ws.onopen = function() {
    ws.send(buffer);
};

```



## Making a secure web socket connection


```js
var sck = "wss://site.com/wss-handler";
var wss = new WebSocket(sck);

```

This uses the `wss` instead of `ws` to make a secure web socket connection which make use of HTTPS instead of HTTP



#### Syntax


- new WebSocket(url)
- ws.binaryType /* delivery type of received message: "arraybuffer" or "blob" */
- ws.close()
- ws.send(data)
- ws.onmessage = function(message) { /* ... */ }
- ws.onopen = function() { /* ... */ }
- ws.onerror = function() { /* ... */ }
- ws.onclose = function() { /* ... */ }



#### Parameters


|Parameter|Details
|------
|url|The server url supporting this web socket connection.
|data|The content to send to the host.
|message|The message received from the host.

