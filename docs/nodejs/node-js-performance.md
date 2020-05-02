---
metaTitle: "Node.js Performance"
description: "Enable gzip, Event Loop, Increase maxSockets"
---

# Node.js Performance




## Enable gzip


```js
const http = require('http')
const fs   = require('fs')
const zlib = require('zlib')

http.createServer((request, response) => {
  const stream          = fs.createReadStream('index.html')
  const acceptsEncoding = request.headers['accept-encoding']

  let encoder = {
    hasEncoder     : false,
    contentEncoding: {},
    createEncoder  : () => throw 'There is no encoder'
  }

  if (!acceptsEncoding) {
    acceptsEncoding = ''
  }

  if (acceptsEncoding.match(/\bdeflate\b/)) {
    encoder = {
      hasEncoder     : true,
      contentEncoding: { 'content-encoding': 'deflate' },
      createEncoder  : zlib.createDeflate
    }
  } else if (acceptsEncoding.match(/\bgzip\b/)) {
    encoder = {
      hasEncoder     : true,
      contentEncoding: { 'content-encoding': 'gzip' },
      createEncoder  : zlib.createGzip
    }
  }

  response.writeHead(200, encoder.contentEncoding)

  if (encoder.hasEncoder) {
    stream = stream.pipe(encoder.createEncoder())
  }

  stream.pipe(response)

}).listen(1337)

```



## Event Loop


### Blocking Operation Example

```js
let loop = (i, max) => {
  while (i < max) i++
  return i
}

// This operation will block Node.js
// Because, it's CPU-bound
// You should be careful about this kind of code
loop(0, 1e+12)

```

### Non-Blocking IO Operation Example

```js
let i = 0

const step = max => {
  while (i < max) i++
  console.log('i = %d', i)
}

const tick = max => process.nextTick(step, max)

// this will postpone tick run step's while-loop to event loop cycles
// any other IO-bound operation (like filesystem reading) can take place
// in parallel
tick(1e+6)
tick(1e+7)
console.log('this will output before all of tick operations. i = %d', i)
console.log('because tick operations will be postponed')
tick(1e+8)

```

[<img src="https://i.stack.imgur.com/mWb6l.png" alt="event loop diagram" />](https://i.stack.imgur.com/mWb6l.png)

In simpler terms, Event Loop is a single-threaded queue mechanism which executes your CPU-bound code until end of its execution and IO-bound code in a non-blocking fashion.

**However, Node.js under the carpet uses multi-threading for some of its operations through [libuv](http://libuv.org/) Library.**

### Performance Considerations

- Non-blocking operations will not block the queue and will not effect the performance of the loop.
- However, CPU-bound operations will block the queue, so you should be careful not to do CPU-bound operations in your Node.js code.

Node.js non-blocks IO because it offloads the work to the operating system kernel, and when the IO operation supplies data (**as an event**), it will notify your code with your supplied callbacks.



## Increase maxSockets


### Basics

```js
require('http').globalAgent.maxSockets = 25

// You can change 25 to Infinity or to a different value by experimenting

```

Node.js by default is using `maxSockets = Infinity` at the same time (since [v0.12.0](https://nodejs.org/dist/v0.12.0/docs/api/http.html#http_agent_maxsockets)). Until Node v0.12.0, the default was `maxSockets = 5` (see [v0.11.0](https://nodejs.org/dist/v0.11.0/docs/api/http.html#http_agent_maxsockets)). So, after more than 5 requests they will be queued. If you want concurrency, increase this number.

### Setting your own agent

`http` API is using a **"[Global Agent](https://nodejs.org/api/http.html#http_class_http_agent)"**. You can supply your own agent. Like this:

```js
const http = require('http')
const myGloriousAgent = new http.Agent({ keepAlive: true })
myGloriousAgent.maxSockets = Infinity

http.request({ ..., agent: myGloriousAgent }, ...)

```

### Turning off Socket Pooling entirely

```js
const http = require('http')
const options = {.....}

options.agent = false

const request = http.request(options)

```

### Pitfalls

<li>
You should do the same thing for `https` API if you want the same effects
</li>
<li>
Beware that, for example, [AWS](http://docs.aws.amazon.com/sdk-for-javascript/v2/developer-guide/node-configuring-maxsockets.html) will use 50 instead of `Infinity`.
</li>

