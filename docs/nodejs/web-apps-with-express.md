---
metaTitle: "Web Apps With Express"
description: "Basic routing, Getting Started, Modular express application, Using a Template Engine, JSON API with ExpressJS, Serving static files, Getting info from the request, Error Handling, Adding Middleware, Hook: How to execute code before any req and after any res, Error handling in Express, Named routes in Django-style, Setting cookies with cookie-parser, Custom middleware in Express, Hello World, Using middleware and the next callback, Error handling, Handling POST Requests"
---

# Web Apps With Express


Express is a minimal and flexible Node.js web application framework, providing a robust set of features for building web applications.

The official website of Express is [expressjs.com](http://expressjs.com). The source can be found [on GitHub](https://github.com/strongloop/express).



## Basic routing


First create an express app:

```js
const express = require('express');
const app = express();

```

Then you can define routes like this:

```js
app.get('/someUri', function (req, res, next) {})

```

That structure works for all HTTP methods, and expects a path as the first argument, and a handler for that path, which receives the request and response objects. So, for the basic HTTP methods, these are the routes

```js
// GET www.domain.com/myPath
app.get('/myPath', function (req, res, next) {})

// POST www.domain.com/myPath
app.post('/myPath', function (req, res, next) {})

// PUT www.domain.com/myPath
app.put('/myPath', function (req, res, next) {})

// DELETE www.domain.com/myPath
app.delete('/myPath', function (req, res, next) {})

```

You can check the complete list of supported verbs [here](http://expressjs.com/en/4x/api.html#app.METHOD). If you want to define the same behavior for a route and all HTTP methods, you can use:

```js
app.all('/myPath', function (req, res, next) {}) 

```

or

```js
app.use('/myPath', function (req, res, next) {})

```

or

```js
app.use('*', function (req, res, next) {})

// * wildcard will route for all paths

```

You can chain your route definitions for a single path

```js
app.route('/myPath')
  .get(function (req, res, next) {})
  .post(function (req, res, next) {})
  .put(function (req, res, next) {})

```

You can also add functions to any HTTP method. They will run before the final callback and take the parameters (req, res, next) as arguments.

```js
// GET www.domain.com/myPath
app.get('/myPath', myFunction, function (req, res, next) {})

```

Your final callbacks can be stored in an external file to avoid putting too much code in one file:

```js
// other.js
exports.doSomething = function(req, res, next) {/* do some stuff */};

```

And then in the file containing your routes:

```js
const other = require('./other.js');
app.get('/someUri', myFunction, other.doSomething);

```

This will make your code much cleaner.



## Getting Started


You will first need to create a directory, access it in your shell and install Express using [npm](https://stackoverflow.com/documentation/node.js/482/npm#t=201608051756069974761) by running `npm install express --save`

Create a file and name it `app.js` and add the following code which creates a new Express server and adds one endpoint to it (`/ping`) with the `app.get` method:

```js
const express = require('express');

const app = express();

app.get('/ping', (request, response) => {
    response.send('pong');
});

app.listen(8080, 'localhost');

```

To run your script use the following command in your shell:

```js
> node app.js

```

Your application will accept connections on localhost port 8080. If the hostname argument to `app.listen` is omitted, then server will accept connections on the machine's IP address as well as localhost. If port value is 0, the operating system will assign an available port.

Once your script is running, you can test it in a shell to confirm that you get the expected response, "pong", from the server:

```js
> curl http://localhost:8080/ping
pong

```

You can also open a web browser, navigate to the url [http://localhost:8080/ping](http://localhost:8080/ping) to view the output



## Modular express application


To make express web application modular use router factories:

Module:

```js
// greet.js
const express = require('express');

module.exports = function(options = {}) { // Router factory
    const router = express.Router();

    router.get('/greet', (req, res, next) => {
        res.end(options.greeting);
    });

    return router;
};

```

Application:

```js
// app.js
const express = require('express');
const greetMiddleware = require('./greet.js');

express()
    .use('/api/v1/', greetMiddleware({ greeting:'Hello world' }))
    .listen(8080);

```

This will make your application modular, customisable and your code reusable.

When accessing `http://<hostname>:8080/api/v1/greet` the output will be `Hello world`

### More complicated example

Example with services that shows middleware factory advantages.

Module:

```js
// greet.js
const express = require('express');

module.exports = function(options = {}) { // Router factory
    const router = express.Router();
    // Get controller
    const {service} = options;

    router.get('/greet', (req, res, next) => {
        res.end(
            service.createGreeting(req.query.name || 'Stranger')
        );
    });

    return router;
};

```

Application:

```js
// app.js
const express = require('express');
const greetMiddleware = require('./greet.js');

class GreetingService {
    constructor(greeting = 'Hello') {
        this.greeting = greeting;
    }

    createGreeting(name) {
        return `${this.greeting}, ${name}!`;
    }
}

express()
    .use('/api/v1/service1', greetMiddleware({
        service: new GreetingService('Hello'),
    }))
    .use('/api/v1/service2', greetMiddleware({
        service: new GreetingService('Hi'),
    }))
    .listen(8080);

```

When accessing `http://<hostname>:8080/api/v1/service1/greet?name=World` the output will be `Hello, World` and accessing `http://<hostname>:8080/api/v1/service2/greet?name=World` the output will be `Hi, World`.



## Using a Template Engine


### Using a Template Engine

The following code will setup Jade as template engine. (Note: Jade has been renamed to `pug` as of December 2015.)

```js
const express = require('express');  //Imports the express module
const app = express();  //Creates an instance of the express module

const PORT = 3000; //Randomly chosen port

app.set('view engine','jade'); //Sets jade as the View Engine / Template Engine
app.set('views','src/views'); //Sets the directory where all the views (.jade files) are stored.

//Creates a Root Route
app.get('/',function(req, res){
    res.render('index');  //renders the index.jade file into html and returns as a response. The render function optionally takes the data to pass to the view.
});

//Starts the Express server with a callback
app.listen(PORT, function(err) {
    if (!err) {
        console.log('Server is running at port', PORT);
    } else {
        console.log(JSON.stringify(err));
    }
});

```

Similarly, other Template Engines could be used too such as `Handlebars`(`hbs`) or `ejs`.  Remember to `npm install` the Template Engine too. For Handlebars we use `hbs` package, for Jade we have a `jade` package and for EJS, we have an `ejs` package.

### EJS Template Example

With EJS (like other express templates), you can run server code and access your server variables from you HTML.<br/>
In EJS it's done using "`<%`" as start tag and "`%>`" as end tag, variables passed as the render params can be accessed using `<%=var_name%>`<br/>
For instance, if you have supplies array in your server code <br/>
you can loop over it using

```js
<h1><%= title %></h1>
   <ul>
<% for(var i=0; i<supplies.length; i++) { %>
    <li>
        <a href='supplies/<%= supplies[i] %>'>
            <%= supplies[i] %>
        </a>
    </li>
<% } %>

```

As you can see in the example every time you switch between server side code and HTML you need to close the current EJS tag and open a new one later, here we wanted to create `li` inside the `for` command so we needed to close our EJS tag at the end of the `for` and create new tag just for the curly brackets <br/>
another example <br/>
if we want to put input default version to be a variable from the server side we use `<%=` <br/>
for example:

```

Message:<br>
<input type="text" value="<%= message %>" name="message" required>

```

Here the message variable passed from your server side will be the default value of your input, please be noticed that if you didn't pass message variable from your server side, EJS will throw an exception. You can pass parameters using `res.render('index', {message: message});` (for ejs file called index.ejs).<br/><br/> In the EJS tags you can also use `if` , `while` or any other javascript command you want.



## JSON API with ExpressJS


```js
var express = require('express');
var cors = require('cors'); // Use cors module for enable Cross-origin resource sharing

var app = express();
app.use(cors()); // for all routes

var port = process.env.PORT || 8080;

app.get('/', function(req, res) {
    var info = {
        'string_value': 'StackOverflow',
        'number_value': 8476
    }
    res.json(info);

    // or
    /* res.send(JSON.stringify({
        string_value: 'StackOverflow',
        number_value: 8476
    })) */

  //you can add a status code to the json response
   /* res.status(200).json(info) */
})

app.listen(port, function() {
    console.log('Node.js listening on port ' + port)
})

```

On `http://localhost:8080/` output object

```js
{
    string_value: "StackOverflow",
    number_value: 8476
}

```



## Serving static files


When building a webserver with Express it's often required to serve a combination of dynamic content and static files.

For example, you may have index.html and script.js which are static files kept in the file system.

It is common to use folder named 'public' to have static files.
In this case the folder structure may look like:

```js
project root
├── server.js
├── package.json 
└── public
    ├── index.html
    └── script.js

```

This is how to configure Express to serve static files:

```js
const express = require('express');
const app = express();

app.use(express.static('public'));

```

Note: once the folder is configured, index.html, script.js and all the files in the "public" folder will be available in at the root path (you must not specify `/public/` in the url). This is because, express looks up for the files relative to the static folder configured. You can specify  **virtual path prefix** as shown below:

```js
app.use('/static', express.static('public'));

```

will make the resources available under the `/static/` prefix.

### Multiple folders

It is possible to define multiple folders at the same time:

```js
app.use(express.static('public'));
app.use(express.static('images'));
app.use(express.static('files'));

```

When serving the resources Express will examine the folder in definition order. In case of files with the same name, the one in the first matching folder will be served.



## Getting info from the request


To get info from the requesting url (notice that `req` is the request object in the handler function of routes). Consider this route definition **`/settings/:user_id`** and this particular example **`/settings/32135?field=name`**

```js
// get the full path
req.originalUrl // => /settings/32135?field=name

// get the user_id param
req.params.user_id // => 32135     

// get the query value of the field
req.query.field // => 'name'

```

You can also get headers of the request, like this

```js
req.get('Content-Type')
// "text/plain"

```

To simplify getting other info you can use middlewares. For example, to get the body info of the request, you can use the [body-parser](https://github.com/expressjs/body-parser) middleware, which will transform raw request body into usable format.

```js
var app = require('express')();
var bodyParser = require('body-parser');

app.use(bodyParser.json()); // for parsing application/json
app.use(bodyParser.urlencoded({ extended: true })); // for parsing application/x-www-form-urlencoded

```

Now suppose a request like this

```js
PUT /settings/32135
{
  "name": "Peter"
}

```

You can access the posted name like this

```js
req.body.name
// "Peter"

```

In a similar way, you can access cookies from the request, you also need a middleware like [cookie-parser](https://github.com/expressjs/cookie-parser?_ga=1.220663448.1060402334.1435240424)

```js
req.cookies.name

```



## Error Handling


**Basic Error Handling**

By default, Express will look for an 'error' view in the `/views` directory to render. Simply create the 'error' view and place it in the views directory to handle errors. Errors are written with the error message, status and stack trace, for example:

**views/error.pug**

```js
html
  body
      h1= message
      h2= error.status
      p= error.stack

```

**Advanced Error Handling**

Define your error-handling middleware functions at the very end of the middleware function stack. These have four arguments instead of three `(err, req, res, next)` for example:

**app.js**

```js
// catch 404 and forward to error handler
app.use(function(req, res, next) {
    var err = new Error('Not Found');
    err.status = 404;

    //pass error to the next matching route.
    next(err);
});

// handle error, print stacktrace
app.use(function(err, req, res, next) {
    res.status(err.status || 500);

    res.render('error', {
        message: err.message,
        error: err
    });
});

```

You can define several error-handling middleware functions, just as you would with regular middleware functions.



## Adding Middleware


Middleware functions are functions that have access to the request object (req), the response object (res), and the next middleware function in the application’s request-response cycle.

Middleware functions can execute any code, make changes to `res` and `req` objects, end response cycle and call next middleware.

Very common example of middleware is `cors` module. To add CORS support, simply install it, require it and put this line:

```js
app.use(cors());

```

before any routers or routing functions.



## Hook: How to execute code before any req and after any res


`app.use()` and middleware can be used for "before" and a combination of the [close](https://nodejs.org/api/http.html#http_event_close_1) and [finish](https://nodejs.org/api/stream.html#stream_event_finish) events can be used for "after".

```js
app.use(function (req, res, next) {
    function afterResponse() {
        res.removeListener('finish', afterResponse);
        res.removeListener('close', afterResponse);

        // actions after response
    }
    res.on('finish', afterResponse);
    res.on('close', afterResponse);

    // action before request
    // eventually calling `next()`
    next();
});
...
app.use(app.router);

```

An example of this is the [logger](http://www.senchalabs.org/connect/logger.html) middleware, which will append to the log after the response by default.

Just make sure this "middleware" is used before `app.router` as order does matter.

Original post is [**here**](http://stackoverflow.com/questions/20175806/before-and-after-hooks-for-a-request-in-express-to-be-executed-before-any-req-a)



## Error handling in Express


In Express, you can define unified error handler for handling errors occurred in application. Define then handler at the end of all routes and logic code.

**Example**

```js
var express = require('express');
var app = express();

//GET /names/john
app.get('/names/:name', function(req, res, next){
    if (req.params.name == 'john'){
        return res.send('Valid Name');
    } else{
        next(new Error('Not valid name'));    //pass to error handler
    }
});

//error handler
app.use(function(err, req, res, next){
    console.log(err.stack);    // e.g., Not valid name
    return res.status(500).send('Internal Server Occured');
});

app.listen(3000);

```



## Named routes in Django-style


One big problem is that valuable named routes is not supported by Express out of the box. Solution is to install supported third-party package, for example [express-reverse](https://github.com/dizlexik/express-reverse):

```js
npm install express-reverse

```

Plug it in your project:

```js
var app = require('express')();
require('express-reverse')(app);

```

Then use it like:

```js
app.get('test', '/hello', function(req, res) {
  res.end('hello');
});

```

The downside of this approach is that you cant use `route` Express module as shown in [Advanced router usage](http://stackoverflow.com/documentation/node.js/483/web-apps-with-express/4433/advanced-router-usage#t=201608011024155231028). The workaround is to pass your `app` as a parameter to you router factory:

```js
require('./middlewares/routing')(app);

```

And use it like:

```js
module.exports = (app) => {
    app.get('test', '/hello', function(req, res) {
      res.end('hello');
    });
};

```

You can figure it out from now on, how define functions to merge it with specified custom namespaces and point at appropriate controllers.



## Setting cookies with cookie-parser


The following is an example for setting and reading cookies using the [cookie-parser](https://github.com/expressjs/cookie-parser) module:

```js
var express = require('express');
var cookieParser = require('cookie-parser'); // module for parsing cookies
var app = express();
app.use(cookieParser());

app.get('/setcookie', function(req, res){
    // setting cookies
    res.cookie('username', 'john doe', { maxAge: 900000, httpOnly: true });
    return res.send('Cookie has been set');
});

app.get('/getcookie', function(req, res) {
    var username = req.cookies['username'];
    if (username) {
        return res.send(username);        
    }

    return res.send('No cookie found');
});

app.listen(3000);

```



## Custom middleware in Express


In Express, you can define middlewares that can be used for checking requests or setting some headers in response.

```js
app.use(function(req, res, next){ });    // signature

```

**Example**

The following code adds `user` to the request object and pass the control to the next matching route.

```js
var express = require('express');
var app = express();

//each request will pass through it
app.use(function(req, res, next){
    req.user = 'testuser';
    next();    // it will pass the control to next matching route
});

app.get('/', function(req, res){
    var user = req.user;
    console.log(user); // testuser
    return res.send(user);
});

app.listen(3000);

```



## Hello World


Here we create a basic hello world server using Express.
Routes:

- '/'
- '/wiki'

And for rest will give "404" , i.e. page not found.

```js
'use strict';

const port = process.env.PORT || 3000;

var app = require('express')();
    app.listen(port);

app.get('/',(req,res)=>res.send('HelloWorld!'));
app.get('/wiki',(req,res)=>res.send('This is wiki page.'));
app.use((req,res)=>res.send('404-PageNotFound'));

```

**Note:** We have put 404 route as the last route as Express stacks routes in order and processes them for each request sequentially.



## Using middleware and the next callback


Express passes a `next` callback to every route handler and middleware function that can be used to break logic for single routes across multiple handlers. Calling `next()` with no arguments tells express to continue to the next matching middleware or route handler. Calling `next(err)` with an error will trigger any error handler middleware. Calling `next('route')` will bypass any subsequent middleware on the current route and jump to the next matching route. This allows domain logic to be decoupled into reusable components that are self-contained, simpler to test, and easier to maintain and change.

**Multiple matching routes**

Requests to `/api/foo` or to `/api/bar` will run the initial handler to look up the member and then pass control to the actual handler for each route.

```js
app.get('/api', function(req, res, next) {
  // Both /api/foo and /api/bar will run this
  lookupMember(function(err, member) {
    if (err) return next(err);
    req.member = member;
    next();
  });
});

app.get('/api/foo', function(req, res, next) {
  // Only /api/foo will run this
  doSomethingWithMember(req.member);
});

app.get('/api/bar', function(req, res, next) {
  // Only /api/bar will run this
  doSomethingDifferentWithMember(req.member);
});

```

**Error handler**

Error handlers are middleware with the signature `function(err, req, res, next)`. They could be set up per route (e.g. `app.get('/foo', function(err, req, res, next)`) but typically, a single error handler that renders an error page is sufficient.

```js
app.get('/foo', function(req, res, next) {
  doSomethingAsync(function(err, data) {
    if (err) return next(err);
    renderPage(data);
  });
});

// In the case that doSomethingAsync return an error, this special
// error handler middleware will be called with the error as the 
// first parameter.
app.use(function(err, req, res, next) {
  renderErrorPage(err);
});

```

**Middleware**

Each of the functions above is actually a middleware function that is run whenever a request matches the route defined, but any number of middleware functions can be defined on a single route. This allows middleware to be defined in separate files and common logic to be reused across multiple routes.

```js
app.get('/bananas', function(req, res, next) {
  getMember(function(err, member) {
    if (err) return next(err);
    // If there's no member, don't try to look
    // up data. Just go render the page now.
    if (!member) return next('route');
    // Otherwise, call the next middleware and fetch
    // the member's data.
    req.member = member;
    next();
  });
}, function(req, res, next) {
  getMemberData(req.member, function(err, data) {
    if (err) return next(err);
    // If this member has no data, don't bother
    // parsing it. Just go render the page now.
    if (!data) return next('route');
    // Otherwise, call the next middleware and parse
    // the member's data. THEN render the page.
    req.member.data = data;
    next();
  });
}, function(req, res, next) {
  req.member.parsedData = parseMemberData(req.member.data);
  next();
});

app.get('/bananas', function(req, res, next) {
  renderBananas(req.member);
});

```

In this example, each middleware function would be either in it's own file or in a variable elsewhere in the file so that it could be reused in other routes.



## Error handling


Basic docs can be found [**here**](http://expressjs.com/en/guide/error-handling.html)

```js
app.get('/path/:id(\\d+)', function (req, res, next) { // please note: "next" is passed
    if (req.params.id == 0) // validate param
        return next(new Error('Id is 0')); // go to first Error handler, see below

    // Catch error on sync operation
    var data;
    try {
        data = JSON.parse('/file.json');
    } catch (err) {
        return next(err);
    }

    // If some critical error then stop application
    if (!data)
        throw new Error('Smth wrong');

    // If you need send extra info to Error handler
    // then send custom error (see Appendix B)
    if (smth)
        next(new MyError('smth wrong', arg1, arg2))

    // Finish request by res.render or res.end
    res.status(200).end('OK');
});    

// Be sure: order of app.use have matter
// Error handler
app.use(function(err, req, res, next)) {
    if (smth-check, e.g. req.url != 'POST') 
        return next(err); // go-to Error handler 2.

    console.log(req.url, err.message);

    if (req.xhr) // if req via ajax then send json else render error-page
        res.json(err);
    else 
        res.render('error.html', {error: err.message});  
});

// Error handler 2
app.use(function(err, req, res, next)) {
    // do smth here e.g. check that error is MyError
    if (err instanceof MyError) {
        console.log(err.message, err.arg1, err.arg2);
    }     
    ...
    res.end();
});

```

Appendix A

```js
// "In Express, 404 responses are not the result of an error, 
// so the error-handler middleware will not capture them." 
// You can change it.
app.use(function(req, res, next) {
    next(new Error(404)); 
});

```

Appendix B

```js
// How to define custom error
var util = require('util');
...
function MyError(message, arg1, arg2) {
    this.message = message;
    this.arg1 = arg1;
    this.arg2 = arg2;
    Error.captureStackTrace(this, MyError);
}
util.inherits(MyError, Error);
MyError.prototype.name = 'MyError';

```



## Handling POST Requests


Just like you handle get requests in Express with app.get method, you can use app.post method to handle post requests.

But before you can handle POST requests, you will need to use the `body-parser` middleware. It simply parses the body of `POST`, `PUT`, `DELETE` and other requests.

`Body-Parser` middleware parses the body of the request and turns it into an object available in `req.body`

```js
var bodyParser = require('body-parser');

const express = require('express');

const app = express();

// Parses the body for POST, PUT, DELETE, etc.
app.use(bodyParser.json());

app.use(bodyParser.urlencoded({ extended: true }));

app.post('/post-data-here', function(req, res, next){

    console.log(req.body); // req.body contains the parsed body of the request.

});

app.listen(8080, 'localhost');

```



#### Syntax


- app.get(path [, middleware], callback[, callback...])
- app.put(path [, middleware], callback[, callback...])
- app.post(path [, middleware], callback[, callback...])
- app['delete'](path [, middleware], callback[, callback...])
- app.use(path [, middleware], callback[, callback...])
- app.use(callback)



#### Parameters


|Parameter|Details
|---|---|---
|`path`|Specifies the path portion or the URL that the given callback will handle.
|`middleware`|One or more functions which will be called before the callback. Essentially a chaining of multiple `callback` functions. Useful for more specific handling for example authorization or error handling.
|`callback`|A function that will be used to handle requests to the specified `path`. It will be called like `callback(request, response, next)`, where `request`, `response`, and `next` are described below.
|**callback** `request`|An object encapsulating details about the HTTP request that the callback is being called to handle.
|`response`|An object that is used to specify how the server should respond to the request.
|`next`|A callback that passes control on to the next matching route. It accepts an optional error object.

