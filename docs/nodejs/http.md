---
metaTitle: "http"
description: "http server, http client"
---

# http



## http server


A basic example of HTTP server.

write following code in http_server.js file:

```js
var http = require('http');

var httpPort = 80;

http.createServer(handler).listen(httpPort, start_callback);

function handler(req, res) {
    
    var clientIP = req.connection.remoteAddress;
    var connectUsing = req.connection.encrypted ? 'SSL' : 'HTTP';
    console.log('Request received: '+ connectUsing + ' ' + req.method + ' ' + req.url);
    console.log('Client IP: ' + clientIP);

    res.writeHead(200, "OK", {'Content-Type': 'text/plain'});
    res.write("OK");
    res.end();        
    return;
}

function start_callback(){
    console.log('Start HTTP on port ' + httpPort)
}

```

then from your http_server.js location run this command:

```js
node http_server.js

```

you should see this result:

```js
> Start HTTP on port 80

```

now you need to test your server, you need to open your internet browser and navigate to this url:

```js
http://127.0.0.1:80

```

if your machine running Linux server you can test it like this:

```js
curl 127.0.0.1:80

```

you should see following result:

```js
ok

```

in your console, that running the app, you will see this results:

```js
> Request received: HTTP GET /
> Client IP: ::ffff:127.0.0.1

```



## http client


a basic example for http client:

write the follwing code in http_client.js file:

```js
var http = require('http');

var options = {
  hostname: '127.0.0.1',
  port: 80,
  path: '/',
  method: 'GET'
};

var req = http.request(options, function(res) {
    console.log('STATUS: ' + res.statusCode);
    console.log('HEADERS: ' + JSON.stringify(res.headers));
    res.setEncoding('utf8');
    res.on('data', function (chunk) {
        console.log('Response: ' + chunk);
    });
    res.on('end', function (chunk) {
        console.log('Response ENDED');
    });
});

req.on('error', function(e) {
    console.log('problem with request: ' + e.message);
});


req.end();

```

then from your http_client.js location run this command:

```js
node http_client.js

```

you should see this result:

```js
> STATUS: 200
> HEADERS: {"content-type":"text/plain","date":"Thu, 21 Jul 2016 11:27:17 GMT","connection":"close","transfer-encoding":"chunked"}
> Response: OK
> Response ENDED

```

note: this example depend on http server example.

