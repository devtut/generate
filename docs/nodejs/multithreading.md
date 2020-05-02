---
metaTitle: "Multithreading"
description: "Cluster, Child Process"
---

# Multithreading


Node.js has been designed to be single threaded. So for all practical purposes, applications that launch with Node will run on a single thread.

However, Node.js itself runs multi-threaded. I/O operations and the like will run from a thread pool. Further will any instance of a node application run on a different thread, therefore to run multi-threaded applications one launches multiple instances.



## Cluster


The `cluster` module allows one to start the same application multiple times.

Clustering is desirable when the different instances have the same flow of execution and don't depend on one another. In this scenario, you have one master that can start forks and the forks (or children). The children work independently and have their one space of Ram and Event Loop.

Setting up clusters can be beneficial for websites / APIs. Any thread can serve any customer, as it doesn't depend on other threads. A Database (like Redis) would be used to share Cookies, as **variables can't be shared!** between the threads.

```js
// runs in each instance
var cluster = require('cluster');
var numCPUs = require('os').cpus().length;

console.log('I am always called');

if (cluster.isMaster) {
    // runs only once (within the master);
    console.log('I am the master, launching workers!');
    for(var i = 0; i < numCPUs; i++) cluster.fork();

} else {
    // runs in each fork
    console.log('I am a fork!');
  
    // here one could start, as an example, a web server
  
}

console.log('I am always called as well');

```



## Child Process


Child Processes are the way to go when one wants to run processes independently with different initialization and concerns. Like forks in clusters, a `child_process` runs in its thread, but unlike forks, it has a way to communicate with its parent.

The communication goes both ways, so parent and child can listen for messages and send messages.

**Parent** (../parent.js)

```js
var child_process = require('child_process');
console.log('[Parent]', 'initalize');

var child1 = child_process.fork(__dirname + '/child');
child1.on('message', function(msg) { 
    console.log('[Parent]', 'Answer from child: ', msg); 
});

// one can send as many messages as one want
child1.send('Hello'); // Hello to you too :)
child1.send('Hello'); // Hello to you too :)

// one can also have multiple children
var child2 = child_process.fork(__dirname + '/child');

```

**Child** (../child.js)

```js
// here would one initialize this child
// this will be executed only once
console.log('[Child]', 'initalize');

// here one listens for new tasks from the parent
process.on('message', function(messageFromParent) {
    
    //do some intense work here
    console.log('[Child]', 'Child doing some intense work');

    if(messageFromParent == 'Hello') process.send('Hello to you too :)');
    else process.send('what?');
    
})

```

Next to message one can listen to [many events](https://nodejs.org/api/child_process.html#child_process_class_childprocess) like 'error', 'connected' or 'disconnect'.

Starting a child process has a certain cost associated with it. One would want to spawn as few of them as possible.



#### Remarks


Understanding the [Event Loop](https://nodejs.org/en/docs/guides/event-loop-timers-and-nexttick/) is important to understand how and why to use multiple threads.

