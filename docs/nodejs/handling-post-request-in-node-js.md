---
metaTitle: "Node.js - Handling POST request in Node.js"
description: "Sample node.js server that just handles POST requests"
---

# Handling POST request in Node.js



## Sample node.js server that just handles POST requests


```js
'use strict';

const http = require('http');

const PORT = 8080;
const server = http.createServer((request, response) => {
  let buffer = '';
  request.on('data', chunk => {
    buffer += chunk;
  });
  request.on('end', () => {
    const responseString = `Received string ${buffer}`;
    console.log(`Responding with: ${responseString}`);
    response.writeHead(200, "Content-Type: text/plain");
    response.end(responseString);
  });
}).listen(PORT, () => {
  console.log(`Listening on ${PORT}`);
});

```



#### Remarks


Node.js uses [streams](https://nodejs.org/api/stream.html#stream_stream) to handle incoming data.

Quoting from the docs,

> 
A stream is an abstract interface for working with streaming data in Node.js. The stream module provides a base API that makes it easy to build objects that implement the stream interface.


To handle in request body of a POST request, use the `request` object, which is a readable stream. Data streams are emitted as `data` events on the `request` object.

```

 request.on('data', chunk => {
    buffer += chunk;
  });
  request.on('end', () => {
    // POST request body is now available as `buffer`
  });

```

Simply create an empty buffer string and append the buffer data as it received via `data` events.

**NOTE**

1. Buffer data received on `data` events is of type [Buffer](https://nodejs.org/api/buffer.html)
<li>Create new buffer string to collect buffered data from data events
**for every request** i.e. create `buffer` string inside the request handler.</li>

