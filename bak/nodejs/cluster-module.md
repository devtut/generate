---
metaTitle: "Node.js - Cluster Module"
description: "Hello World, Cluster Example"
---

# Cluster Module



## Hello World


This is your `cluster.js`:

```js
const cluster = require('cluster');
const http = require('http');
const numCPUs = require('os').cpus().length;

if (cluster.isMaster) {
  // Fork workers.
  for (let i = 0; i < numCPUs; i++) {
    cluster.fork();
  }

  cluster.on('exit', (worker, code, signal) => {
    console.log(`worker ${worker.process.pid} died`);
  });
} else {
  // Workers can share any TCP connection
  // In this case it is an HTTP server
  require('./server.js')();
}

```

This is your main `server.js`:

```js
const http = require('http');

function startServer() {
    const server = http.createServer((req, res) => {
      res.writeHead(200);
      res.end('Hello Http');
    });

    server.listen(3000);
}

if(!module.parent) {
    // Start server if file is run directly
    startServer();
} else {
    // Export server, if file is referenced via cluster
    module.exports = startServer;
}

```

In this example, we host a basic web server, however, we spin up workers (child processes) using the built-in **cluster** module. The number of processes forker depend on the number of CPU cores available. This enables a Node.js application to take advantage of multi-core CPUs, since a single instance of Node.js runs in a single thread. The application will now share the port 8000 across all the processes. Loads will automatically be distributed between workers using the Round-Robin method by default.



## Cluster Example


A single instance of `Node.js` runs in a single thread. To take advantage of multi-core systems, application can be launched in a cluster of Node.js processes to handle the load.

The `cluster` module allows you to easily create child processes that all share server ports.

Following example create the worker child process in main process that handles the load across multiple cores.

**Example**

```js
const cluster = require('cluster');
const http = require('http');
const numCPUs = require('os').cpus().length; //number of CPUS

if (cluster.isMaster) {
  // Fork workers.
  for (var i = 0; i < numCPUs; i++) {
    cluster.fork();    //creating child process
  }

  //on exit of cluster
  cluster.on('exit', (worker, code, signal) => {
      if (signal) {
        console.log(`worker was killed by signal: ${signal}`);
      } else if (code !== 0) {
        console.log(`worker exited with error code: ${code}`);
      } else {
        console.log('worker success!');
      }
  });
} else {
  // Workers can share any TCP connection
  // In this case it is an HTTP server
  http.createServer((req, res) => {
    res.writeHead(200);
    res.end('hello world\n');
  }).listen(3000);
}

```



#### Syntax


- const cluster  = require("cluster")
- cluster.fork()
- cluster.isMaster
- cluster.isWorker
- cluster.schedulingPolicy
- cluster.setupMaster(settings)
- cluster.settings
- cluster.worker // in worker
- cluster.workers // in master



#### Remarks


Note that `cluster.fork()` spawns a child process that begins executing the current script from the beginning, in contrast to the `fork()` system call in **C** which clones the current process and continues from the instruction after the system call in both parent and child process.

The Node.js Documentation has a more complete guide to clusters [here](https://nodejs.org/api/cluster.html)

