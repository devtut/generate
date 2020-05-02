---
metaTitle: "Deploying Node.js application without downtime."
description: "Deployment using PM2 without downtime."
---

# Deploying Node.js application without downtime.




## Deployment using PM2 without downtime.


`ecosystem.json`

```js
{
    "name": "app-name",
    "script": "server",
    "exec_mode": "cluster",
    "instances": 0,    
    "wait_ready": true
    "listen_timeout": 10000,
    "kill_timeout": 5000,
}

```

```js
wait_ready

```

Instead of reload waiting for listen event, wait for process.send('ready');

```js
listen_timeout

```

Time in ms before forcing a reload if app not listening.

```js
kill_timeout

```

Time in ms before sending a final SIGKLL.

`server.js`

```js
const http = require('http');
const express = require('express');

const app = express();
const server = http.Server(app);
const port = 80;

server.listen(port, function() {
    process.send('ready');
});

process.on('SIGINT', function() {
    server.close(function() {
        process.exit(0);
    });
});

```

You might need to wait for your application to have etablished connections with your DBs/caches/workers/whatever. PM2 needs to wait before considering your application as online. To do this, you need to provide `wait_ready: true` in a process file. This will make PM2 listen for that event. In your application you will need to add `process.send('ready');` when you want your application to be considered as ready.

When a process is stopped/restarted by PM2, some system signals are sent to your process in a given order.

First a `SIGINT` a signal is sent to your processes, signal you can catch to know that your process is going to be stopped. If your application does not exit by itself before 1.6s (customizable) it will receive a `SIGKILL` signal to force the process exit. So if your application need to clean-up something states or jobs you can catch the `SIGINT` signal to prepare your application to exit.

