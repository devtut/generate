---
metaTitle: "Node.js - Securing Node.js applications"
description: "SSL/TLS in Node.js, Preventing Cross Site Request Forgery (CSRF), Setting up an HTTPS server, Using HTTPS, Secure express.js 3 Application"
---

# Securing Node.js applications




## SSL/TLS in Node.js


If you choose to handle SSL/TLS in your Node.js application, consider that you are also responsible for maintaining SSL/TLS attack prevention at this point. In many server-client architectures, SSL/TLS terminates on a reverse proxy, both to reduce application complexity and reduce the scope of security configuration.

If your Node.js application should handle SSL/TLS, it can be secured by loading the key and cert files.

If your certificate provider requires a certificate authority (CA) chain, it can be added in the `ca` option as an array. A chain with multiple entries in a single file must be split into multiple files and entered in the same order into the array as Node.js does not currently support multiple ca entries in one file. An example is provided in the code below for files `1_ca.crt` and `2_ca.crt`. If the `ca` array is required and not set properly, client browsers may display messages that they could not verify the authenticity of the certificate.

**Example**

```js
const https = require('https');
const fs = require('fs');

const options = {
  key: fs.readFileSync('privatekey.pem'),
  cert: fs.readFileSync('certificate.pem'),
  ca: [fs.readFileSync('1_ca.crt'), fs.readFileSync('2_ca.crt')]
};

https.createServer(options, (req, res) => {
  res.writeHead(200);
  res.end('hello world\n');
}).listen(8000);

```



## Preventing Cross Site Request Forgery (CSRF)


**CSRF** is an attack which forces end user to execute unwanted actions on a web application in which he/she is currently authenticated.

It can happen because cookies are sent with every request to a website - even when those requests come from a different site.

We can use `csurf` module for creating csrf token and validating it.

**Example**

```js
var express = require('express')
var cookieParser = require('cookie-parser')    //for cookie parsing
var csrf = require('csurf')    //csrf module
var bodyParser = require('body-parser')    //for body parsing

// setup route middlewares
var csrfProtection = csrf({ cookie: true })
var parseForm = bodyParser.urlencoded({ extended: false })

// create express app
var app = express()

// parse cookies
app.use(cookieParser())

app.get('/form', csrfProtection, function(req, res) {
  // generate and pass the csrfToken to the view
  res.render('send', { csrfToken: req.csrfToken() })
})

app.post('/process', parseForm, csrfProtection, function(req, res) {
  res.send('data is being processed')
})

```

So, when we access `GET /form`, it will pass the csrf token `csrfToken` to the view.

Now, inside the view, set the csrfToken value as the value of a hidden input field named `_csrf`.

e.g. for `handlebar` templates

```js
<form action="/process" method="POST">
    <input type="hidden" name="_csrf" value="{{csrfToken}}">
    Name: <input type="text" name="name">
    <button type="submit">Submit</button>
</form>

```

e.g. for `jade` templates

```js
form(action="/process" method="post")
    input(type="hidden", name="_csrf", value=csrfToken)

    span Name:
    input(type="text", name="name", required=true)
    br

    input(type="submit")

```

e.g. for `ejs` templates

```js
<form action="/process" method="POST">
    <input type="hidden" name="_csrf" value="<%= csrfToken %>">
    Name: <input type="text" name="name">
    <button type="submit">Submit</button>
</form>

```



## Setting up an HTTPS server


Once you have node.js installed on your system, just follow the procedure below to get a basic web server running with support for both HTTP and HTTPS!

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



## Using HTTPS


The minimal setup for an HTTPS server in Node.js would be something like this :

```js
const https = require('https');
const fs = require('fs');

const httpsOptions = {
    key: fs.readFileSync('path/to/server-key.pem'),
    cert: fs.readFileSync('path/to/server-crt.pem')
};

const app = function (req, res) {
  res.writeHead(200);
  res.end("hello world\n");
}

https.createServer(httpsOptions, app).listen(4433);

```

If you also want to support http requests, you need to make just this small modification:

```js
const http = require('http');
const https = require('https');
const fs = require('fs');

const httpsOptions = {
    key: fs.readFileSync('path/to/server-key.pem'),
    cert: fs.readFileSync('path/to/server-crt.pem')
};

const app = function (req, res) {
  res.writeHead(200);
  res.end("hello world\n");
}

http.createServer(app).listen(8888);
https.createServer(httpsOptions, app).listen(4433);

```



## Secure express.js 3 Application


The configuration to make a secure connection using express.js (Since version 3):

```js
var fs = require('fs');
var http = require('http');
var https = require('https');
var privateKey  = fs.readFileSync('sslcert/server.key', 'utf8');
var certificate = fs.readFileSync('sslcert/server.crt', 'utf8');

// Define your key and cert

var credentials = {key: privateKey, cert: certificate};
var express = require('express');
var app = express();

// your express configuration here

var httpServer = http.createServer(app);
var httpsServer = https.createServer(credentials, app);

// Using port 8080 for http and 8443 for https 

httpServer.listen(8080);
httpsServer.listen(8443);

```

In that way you provide express middleware to the native http/https server

If you want your app running on ports below 1024, you will need to use sudo command (not recommended) or use a reverse proxy (e.g. nginx, haproxy).

