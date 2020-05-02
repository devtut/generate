---
metaTitle: "Getting started with Node.js"
description: "Hello World HTTP server, Hello World command line, Hello World with Express, Installing and Running Node.js, Debugging Your NodeJS Application, Hello World basic routing, Hello World in the REPL, Deploying your application online, Core modules, TLS Socket: server and client, How to get a basic HTTPS web server up and running!"
---

# Getting started with Node.js



## Hello World HTTP server


First, [install Node.js](http://stackoverflow.com/documentation/node.js/1294/installing-node-js) for your platform.

In this example we'll create an HTTP server listening on port 1337, which sends `Hello, World!` to the browser. Note that, instead of using port 1337, you can use any port number of your choice which is currently not in use by any other service.

The `http` module is a Node.js [****core module****](http://stackoverflow.com/documentation/node.js/340/getting-started-with-node-js/30139/core-modules) (a module included in Node.js's source, that does not require installing additional resources). The `http` module provides the functionality to create an HTTP server using the [`http.createServer()`](https://nodejs.org/api/http.html#http_http_createserver_requestlistener) method.
To create the application, create a file containing the
following JavaScript code.

```js
const http = require('http'); // Loads the http module

http.createServer((request, response) => {

    // 1. Tell the browser everything is OK (Status code 200), and the data is in plain text
    response.writeHead(200, {
        'Content-Type': 'text/plain'
    });

    // 2. Write the announced text to the body of the page
    response.write('Hello, World!\n');

    // 3. Tell the server that all of the response headers and body have been sent
    response.end();

}).listen(1337); // 4. Tells the server what port to be on

```

Save the file with any file name. In this case, if we name it `hello.js` we can run the application by going to the directory the file is in and using the following command:

```js
node hello.js

```

The created server can then be accessed with the URL [http://localhost:1337](http://localhost:1337)
or [http://127.0.0.1:1337](http://127.0.0.1:1337) in the browser.

A simple web page will appear with a “Hello, World!” text at the top, as shown in the screenshot below.

[<img src="https://i.stack.imgur.com/Oq3Y4.png" alt="Screenshot" />](https://i.stack.imgur.com/Oq3Y4.png)

[Editable online example.](https://glitch.com/edit/#!/node-hello-world)



## Hello World command line


Node.js can also be used to create command line utilities. The example below reads the first argument from the command line and prints a Hello message.

To run this code on an Unix System:

1. Create a new file and paste the code below. The filename is irrelevant.
1. Make this file executable with `chmod 700 FILE_NAME`
1. Run the app with `./APP_NAME David`

On Windows you do step 1 and run it with `node APP_NAME David`

```js
#!/usr/bin/env node

'use strict';

/*
    The command line arguments are stored in the `process.argv` array, 
    which has the following structure:
    [0] The path of the executable that started the Node.js process
    [1] The path to this application
    [2-n] the command line arguments

    Example: [ '/bin/node', '/path/to/yourscript', 'arg1', 'arg2', ... ]
    src: https://nodejs.org/api/process.html#process_process_argv
 */

// Store the first argument as username.
var username = process.argv[2];

// Check if the username hasn't been provided.
if (!username) {

    // Extract the filename
    var appName = process.argv[1].split(require('path').sep).pop();

    //  Give the user an example on how to use the app.
    console.error('Missing argument! Example: %s YOUR_NAME', appName);

    // Exit the app (success: 0, error: 1). 
    // An error will stop the execution chain. For example:
    //   ./app.js && ls       -> won't execute ls
    //   ./app.js David && ls -> will execute ls
    process.exit(1);
}

// Print the message to the console.
console.log('Hello %s!', username);

```



## Hello World with Express


The following example uses Express to create an HTTP server listening on port 3000, which responds with "Hello, World!". Express is a commonly-used web framework that is useful for creating HTTP APIs.

First, create a new folder, e.g. `myApp`. Go into `myApp` and make a new JavaScript file containing the following code (let's name it `hello.js` for example). Then install the express module using `npm install --save express` from the command line. **Refer to [this documentation](http://stackoverflow.com/documentation/node.js/482/npm/1588/installing-packages#t=20170324115118574098) for more information on how to install packages**.

```js
// Import the top-level function of express
const express = require('express');

// Creates an Express application using the top-level function
const app = express();

// Define port number as 3000
const port = 3000;

// Routes HTTP GET requests to the specified path "/" with the specified callback function
app.get('/', function(request, response) {
  response.send('Hello, World!');
});

// Make the app listen on port 3000
app.listen(port, function() {
  console.log('Server listening on http://localhost:' + port);
});

```

From the command line, run the following command:

```js
node hello.js

```

Open your browser and navigate to `http://localhost:3000` or `http://127.0.0.1:3000` to see the response.

For more information about the Express framework, you can check the [Web Apps With Express](http://stackoverflow.com/documentation/node.js/483/web-apps-with-express#t=201608021642089193322) section



## Installing and Running Node.js


To begin, install Node.js on your development computer.

**Windows:** Navigate to the [download page](https://nodejs.org/en/download/) and download/run the installer.

**Mac:** Navigate to the [download page](https://nodejs.org/en/download/) and download/run the installer. Alternatively, you can install Node via Homebrew using `brew install node`. Homebrew is a command-line package mananger for Macintosh, and more information about it can be found on the [Homebrew website](http://brew.sh/).

**Linux:** Follow the instructions for your distro on the [command line installation page](https://nodejs.org/en/download/package-manager/).

### Running a Node Program

To run a Node.js program, simply run `node app.js` or `nodejs app.js`, where `app.js` is the filename of your node app source code. You do not need to include the `.js` suffix for Node to find the script you'd like to run.

Alternatively under UNIX-based operating systems, a Node program may be executed as a terminal script. To do so, it needs to begin with a shebang pointing to the Node interpreter, such as `#!/usr/bin/env node`. The file also has to be set as executable, which can be done using `chmod`. Now the script can be directly run from the command line.



## Debugging Your NodeJS Application


You can use the node-inspector. Run this command to install it via npm:

```js
npm install -g node-inspector

```

Then you can debug your application using

```js
node-debug app.js

```

The Github repository can be found here: [https://github.com/node-inspector/node-inspector](https://github.com/node-inspector/node-inspector)

### Debugging natively

You can also debug node.js natively by starting it like this:

```js
node debug your-script.js

```

To breakpoint your debugger exactly in a code line you want, use this:

```js
debugger;

```

For more information see [here](https://nodejs.org/api/debugger.html).

In node.js 8 use the following command:

```js
node --inspect-brk your-script.js

```

Then open `about://inspect` in a recent version of Google Chrome and select your Node script to get the debugging experience of Chrome's DevTools.



## Hello World basic routing


Once you understand how to create an [HTTP Server](http://stackoverflow.com/documentation/node.js/340/hello-world/1169/hello-world-http-server) with node, it's important to understand how to make it "do" things based on the path that a user has navigated to. This phenomenon is called, "routing".

The most basic example of this would be to check `if (request.url === 'some/path/here')`, and then call a function that responds with a new file.

An example of this can be seen here:

```js
const http = require('http');

function index (request, response) {
    response.writeHead(200);
    response.end('Hello, World!');
}

http.createServer(function (request, response) {
    
    if (request.url === '/') {
        return index(request, response);
    }

    response.writeHead(404);
    response.end(http.STATUS_CODES[404]);

}).listen(1337);

```

If you continue to define your "routes" like this, though, you'll end up with one massive callback function, and we don't want a giant mess like that, so let's see if we can clean this up.

First, let's store all of our routes in an object:

```js
var routes = {
    '/': function index (request, response) {
        response.writeHead(200);
        response.end('Hello, World!');
    },
    '/foo': function foo (request, response) {
        response.writeHead(200);
        response.end('You are now viewing "foo"');
    }
}

```

Now that we've stored 2 routes in an object, we can now check for them in our main callback:

```js
http.createServer(function (request, response) {
    
    if (request.url in routes) {
        return routes[request.url](request, response);
    }

    response.writeHead(404);
    response.end(http.STATUS_CODES[404]);

}).listen(1337);

```

Now every time you try to navigate your website, it will check for the existence of that path in your routes, and it will call the respective function. If no route is found, the server will respond with a 404 (Not Found).

And there you have it--routing with the HTTP Server API is very simple.



## Hello World in the REPL


When called without arguments, Node.js starts a REPL (Read-Eval-Print-Loop) also known as the “**Node shell**”.

At a command prompt type `node`.

```js
$ node
>

```

At the Node shell prompt `>` type "Hello World!"

```js
$ node
> "Hello World!"
'Hello World!'

```



## Deploying your application online


When you deploy your app to a (Node.js-specific) hosted environment, this environment usually offers a `PORT`-environment variable that you can use to run your server on. Changing the port number to `process.env.PORT` allows you to access the application.

For example,

```js
http.createServer(function(request, response) {
   // your server code
}).listen(process.env.PORT);

```

Also, if you would like to access this offline while debugging, you can use this:

```js
http.createServer(function(request, response) {
  // your server code
}).listen(process.env.PORT || 3000);

```

where `3000` is the offline port number.



## Core modules


Node.js is a Javascript engine (Google's V8 engine for Chrome, written in C++) that allows to run Javascript outside the browser. While numerous libraries are available for extending Node's functionalities, the engine comes with a set of **core modules** implementing basic functionalities.

There's currently 34 core modules included in Node:

```js
[ 'assert',
  'buffer',
  'c/c++_addons',
  'child_process',
  'cluster',
  'console',
  'crypto',
  'deprecated_apis',
  'dns',
  'domain',
  'Events',
  'fs',
  'http',
  'https',
  'module',
  'net',
  'os',
  'path',
  'punycode',
  'querystring',
  'readline',
  'repl',
  'stream',
  'string_decoder',
  'timers',
  'tls_(ssl)',
  'tracing',
  'tty',
  'dgram',
  'url',
  'util',
  'v8',
  'vm',
  'zlib' ]

```

This list was obtained from the Node documentation API [https://nodejs.org/api/all.html](https://nodejs.org/api/all.html) (JSON file: [https://nodejs.org/api/all.json](https://nodejs.org/api/all.json)).

### All core modules at-a-glance

**assert**   <p>The `assert` module provides a simple set of assertion tests that can be used to
test invariants.

**buffer**   <p>Prior to the introduction of [`TypedArray`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Refer%0D%0Aence/Global_Objects/TypedArray) in ECMAScript 2015 (ES6), the
JavaScript language had no mechanism for reading or manipulating streams
of binary data. The `Buffer` class was introduced as part of the Node.js
API to make it possible to interact with octet streams in the context of things
like TCP streams and file system operations.

Now that [`TypedArray`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/TypedArra%0D%0Ay) has been added in ES6, the `Buffer` class implements the
<a href="https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Uint8Array" rel="nofollow noreferrer"><code>Uin
t8Array</code></a> API in a manner that is more optimized and suitable for Node.js&#39;
use cases.

**c/c++_addons**    <p>Node.js Addons are dynamically-linked shared objects, written in C or C++, that
can be loaded into Node.js using the `require()` function
, and used
just as if they were an ordinary Node.js module. They are used primarily to
provide an interface between JavaScript running in Node.js and C/C++ libraries.

**child_process**    <p>The `child_process` module provides the ability to spawn child processes in
a manner that is similar, but not identical, to popen(3). 

[**cluster**](http://stackoverflow.com/documentation/node.js/2817/cluster-module)          <p>A single instance of Node.js runs in a single thread. To take advantage of multi-core systems the user will sometimes want to launch a cluster of Node.js
processes to handle the load.
The cluster module allows you to easily create child processes that
all share server ports.

**console**          <p>The `console` module provides a simple debugging console that is similar to the
JavaScript console mechanism provided by web browsers.

**crypto**   <p>The `crypto` module provides cryptographic functionality that includes a set of
wrappers for OpenSSL's hash, HMAC, cipher, decipher, sign and verify functions.

**deprecated_apis**          <p>Node.js may deprecate APIs when either: (a) use of the API is considered to be
unsafe, (b) an improved alternative API has been made available, or (c)
breaking changes to the API are expected in a future major release.

**dns** <p>The `dns` module contains functions belonging to two different categories:

<li>Functions that use the underlying operating system facilities to perform
name resolution, and that do not necessarily perform any network communication.
This category contains only one function: `dns.lookup()`.</li>
<li>Functions that connect to an actual DNS server to perform name resolution,
and that **always** use the network to perform DNS queries. This category
contains all functions in the `dns` module **except** `dns.lookup()`.</li>

**domain**   <p>****This module is pending deprecation****. Once a replacement API has been
finalized, this module will be fully deprecated. Most end users should
**not** have cause to use this module. Users who absolutely must have
the functionality that domains provide may rely on it for the time being
but should expect to have to migrate to a different solution
in the future.

[**Events**](http://stackoverflow.com/documentation/node.js/1623/event-emitters)   <p>Much of the Node.js core API is built around an idiomatic asynchronous
event-driven architecture in which certain kinds of objects (called "emitters")
periodically emit named events that cause Function objects ("listeners") to be
called.

**fs**       <p>File I/O is provided by simple wrappers around standard POSIX functions.  To use this module do `require('fs')`. All the methods have asynchronous and synchronous forms.

[**http**](http://stackoverflow.com/documentation/node.js/2973/http)

The HTTP interfaces in Node.js are designed to support many features
of the protocol which have been traditionally difficult to use.
In particular, large, possibly chunk-encoded, messages. The interface is
careful to never buffer entire requests or responses--the
user is able to stream data.

**https**    <p>HTTPS is the HTTP protocol over TLS/SSL. In Node.js this is implemented as a
separate module.

**module**   <p>Node.js has a simple module loading system.  In Node.js, files and modules
are in one-to-one correspondence (each file is treated as a separate module).

**net**      <p>The `net` module provides you with an asynchronous network wrapper. It contains
functions for creating both servers and clients (called streams). You can include
this module with `require('net');`.

**os**       <p>The `os` module provides a number of operating system-related utility methods.

**path**     <p>The `path` module provides utilities for working with file and directory paths.

**punycode**         <p>****The version of the punycode module bundled in Node.js is being deprecated****.

**querystring**      <p>The `querystring` module provides utilities for parsing and formatting URL query strings.

[**readline**](http://stackoverflow.com/documentation/node.js/1431/readline)         <p>The `readline` module provides an interface for reading data from a Readable
stream (such as `process.stdin`) one line at a time.

**repl**     <p>The `repl` module provides a Read-Eval-Print-Loop (REPL) implementation that
is available both as a standalone program or includible in other applications.

[**stream**](http://stackoverflow.com/documentation/node.js/2974/using-streams)   <p>A stream is an abstract interface for working with streaming data in Node.js.
The `stream` module provides a base API that makes it easy to build objects
that implement the stream interface.

There are many stream objects provided by Node.js. For instance, a
request to an HTTP server and `process.stdout`
are both stream instances.

**string_decoder**   <p>The `string_decoder` module provides an API for decoding `Buffer` objects into
strings in a manner that preserves encoded multi-byte UTF-8 and UTF-16
characters.

**timers**   <p>The `timer` module exposes a global API for scheduling functions to
be called at some future period of time. Because the timer functions are
globals, there is no need to call `require('timers')` to use the API.

The timer functions within Node.js implement a similar API as the timers API
provided by Web Browsers but use a different internal implementation that is
built around [the Node.js Event Loop](https://nodejs.org/en/docs/guides/event-loop-timers-and-nexttick).

**tls_(ssl)**        <p>The `tls` module provides an implementation of the Transport Layer Security
(TLS) and Secure Socket Layer (SSL) protocols that is built on top of OpenSSL.

**tracing**          <p>Trace Event provides a mechanism to centralize tracing information generated by
V8, Node core, and userspace code.

Tracing can be enabled by passing the `--trace-events-enabled` flag when starting a
Node.js application.

**tty**      <p>The `tty` module provides the `tty.ReadStream` and `tty.WriteStream` classes.
In most cases, it will not be necessary or possible to use this module directly.

**dgram**    <p>The `dgram` module provides an implementation of UDP Datagram sockets.

**url**      <p>The `url` module provides utilities for URL resolution and parsing.

**util**     <p>The `util` module is primarily designed to support the needs of Node.js' own internal APIs. However, many of the utilities are useful for application and module developers as well.

**v8**       <p>The `v8` module exposes APIs that are specific to the version of [V8](https://developers.google.com/v8/) built into the Node.js binary.

**Note**: The APIs and implementation are subject to change at any time.

**vm**       <p>The `vm` module provides APIs for compiling and running code within V8 Virtual Machine contexts.
JavaScript code can be compiled and run immediately or compiled, saved, and run
later.

**Note**: The vm module is not a security mechanism.
****Do not use it to run untrusted code****.

**zlib**     <p>The `zlib` module provides compression functionality implemented using Gzip and
Deflate/Inflate.



## TLS Socket: server and client


The only major differences between this and a regular TCP connection are the private Key and the public certificate that you’ll have to set into an option object.

### How to Create a Key and Certificate

The first step in this security process is the creation of a private Key. And what is this private key? Basically, it's a set of random noise that's used to encrypt information. In theory, you could create one key, and use it to encrypt whatever you want. But it is best practice to have different keys for specific things. Because if someone steals your private key, it's similar to having someone steal your house keys. Imagine if you used the same key to lock your car, garage, office, etc.

`openssl genrsa -out private-key.pem 1024`

Once we have our private key, we can create a CSR (certificate signing request), which is our request to have the private key signed by a fancy authority. That is why you have to input information related to your company. This information will be seen by the signing authority, and used to verify you. In our case, it doesn’t matter what you type, since in the next step we're going to sign our certificate ourselves.

`openssl req -new -key private-key.pem -out csr.pem`

Now that we have our paper work filled out, it's time to pretend that we're a cool signing authority.

`openssl x509 -req -in csr.pem -signkey private-key.pem -out public-cert.pem`

Now that you have the private key and the public cert, you can establish a secure connection between two NodeJS apps. And, as you can see in the example code, it is a very simple process.

### Important!

Since we created the public cert ourselves, in all honesty, our certificate is worthless, because we are nobodies. The NodeJS server won't trust such a certificate by default, and that is why we need to tell it to actually trust our cert with the following option rejectUnauthorized: false. **Very important**: never set this variable to true in a production environment.

### TLS Socket Server

```js
'use strict';

var tls = require('tls');
var fs = require('fs');

const PORT = 1337;
const HOST = '127.0.0.1'

var options = {
    key: fs.readFileSync('private-key.pem'),
    cert: fs.readFileSync('public-cert.pem')
};

var server = tls.createServer(options, function(socket) {

    // Send a friendly message
    socket.write("I am the server sending you a message.");

    // Print the data that we received
    socket.on('data', function(data) {

        console.log('Received: %s [it is %d bytes long]',
            data.toString().replace(/(\n)/gm,""),
            data.length);

    });

    // Let us know when the transmission is over
    socket.on('end', function() {

        console.log('EOT (End Of Transmission)');

    });

});

// Start listening on a specific port and address
server.listen(PORT, HOST, function() {

    console.log("I'm listening at %s, on port %s", HOST, PORT);

});

// When an error occurs, show it.
server.on('error', function(error) {

    console.error(error);

    // Close the connection after the error occurred.
    server.destroy();

});

```

### TLS Socket Client

```js
'use strict';

var tls = require('tls');
var fs = require('fs');

const PORT = 1337;
const HOST = '127.0.0.1'

// Pass the certs to the server and let it know to process even unauthorized certs.
var options = {
    key: fs.readFileSync('private-key.pem'),
    cert: fs.readFileSync('public-cert.pem'),
    rejectUnauthorized: false
};

var client = tls.connect(PORT, HOST, options, function() {

    // Check if the authorization worked
    if (client.authorized) {
        console.log("Connection authorized by a Certificate Authority.");
    } else {
        console.log("Connection not authorized: " + client.authorizationError)
    }

    // Send a friendly message
    client.write("I am the client sending you a message.");

});

client.on("data", function(data) {

    console.log('Received: %s [it is %d bytes long]',
        data.toString().replace(/(\n)/gm,""),
        data.length);

    // Close the connection after receiving the message
    client.end();

});

client.on('close', function() {

    console.log("Connection closed");

});

// When an error ocoures, show it.
client.on('error', function(error) {

    console.error(error);

    // Close the connection after the error occurred.
    client.destroy();

});

```



## How to get a basic HTTPS web server up and running!


Once you have node.js installed on your system, you can just follow the procedure below to get a basic web server running with support for both HTTP and HTTPS!

### Step 1 : Build a Certificate Authority

<li>
create the folder where you want to store your key & certificate :
`mkdir conf`
</li>

<li>
go to that directory :
`cd conf`
</li>

<li>
grab this `ca.cnf` file to use as a configuration shortcut :
`wget https://raw.githubusercontent.com/anders94/https-authorized-clients/master/keys/ca.cnf`
</li>

<li>
create a new certificate authority using this configuration :
`openssl req -new -x509 -days 9999 -config ca.cnf -keyout ca-key.pem -out ca-cert.pem`
</li>

<li>
now that we have our certificate authority in `ca-key.pem` and `ca-cert.pem`, let's generate a private key for the server :
`openssl genrsa -out key.pem 4096`
</li>

<li>
grab this `server.cnf` file to use as a configuration shortcut :
`wget https://raw.githubusercontent.com/anders94/https-authorized-clients/master/keys/server.cnf`
</li>

<li>
generate the certificate signing request using this configuration :
`openssl req -new -config server.cnf -key key.pem -out csr.pem`
</li>

<li>
sign the request :
`openssl x509 -req -extfile server.cnf -days 999 -passin "pass:password" -in csr.pem -CA ca-cert.pem -CAkey ca-key.pem -CAcreateserial -out cert.pem`
</li>

### Step 2 : Install your certificate as a root certificate

<li>
copy your certificate to your root certificates' folder :
`sudo cp ca-crt.pem /usr/local/share/ca-certificates/ca-crt.pem`
</li>

<li>
update CA store :
`sudo update-ca-certificates`
</li>

### Step 3 : Starting your node server

First, you want to create a `server.js` file that contains your actual server code.

The minimal setup for an HTTPS server in Node.js would be something like this :

```js
var https = require('https');
var fs = require('fs');

var httpsOptions = {
    key: fs.readFileSync('path/to/server-key.pem'),
    cert: fs.readFileSync('path/to/server-crt.pem')
};

var app = function (req, res) {
  res.writeHead(200);
  res.end("hello world\n");
}

https.createServer(httpsOptions, app).listen(4433);

```

If you also want to support http requests, you need to make just this small modification :

```js
var http = require('http');
var https = require('https');
var fs = require('fs');

var httpsOptions = {
    key: fs.readFileSync('path/to/server-key.pem'),
    cert: fs.readFileSync('path/to/server-crt.pem')
};

var app = function (req, res) {
  res.writeHead(200);
  res.end("hello world\n");
}

http.createServer(app).listen(8888);
https.createServer(httpsOptions, app).listen(4433);

```


<li>
go to the directory where your `server.js` is located :
`cd /path/to`
</li>

<li>
run `server.js` :
`node server.js`
</li>



#### Remarks


Node.js is an event-based, non-blocking, asynchronous I/O framework that uses Google's V8 JavaScript engine. It is used for developing applications that make heavy use of the ability to run JavaScript both on the client, as well as on server side and therefore benefit from the re-usability of code and the lack of context switching. It is open-source and cross-platform. Node.js applications are written in pure JavaScript and can be run within Node.js environment on Windows, Linux etc…

