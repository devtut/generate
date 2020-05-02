---
metaTitle: "Using IISNode to host Node.js Web Apps in IIS"
description: "Using an IIS Virtual Directory or Nested Application via <appSettings>, Getting Started, Basic Hello World Example using Express, Using Socket.io with IISNode"
---

# Using IISNode to host Node.js Web Apps in IIS



## Using an IIS Virtual Directory or Nested Application via <appSettings>


Using a Virtual Directory or Nested Application in IIS is a common scenario and most likely one that you'll want to take advantage of when using IISNode.

IISNode doesn't provide direct support for Virtual Directories or Nested Applications via configuration so to achieve this we'll need to take advantage of a feature of IISNode that isn't part of the configuration and is much lesser known. All children of the `<appSettings>` element with the `Web.config` are added to the `process.env` object as properties using the appSetting key.

Lets create a Virtual Directory in our `<appSettings>`

```js
<appSettings>
  <add key="virtualDirPath" value="/foo" />
</appSettings>

```

Within our Node.js App we can access the `virtualDirPath` setting

```js
console.log(process.env.virtualDirPath); // prints /foo

```

Now that we can use the `<appSettings>` element for configuration, lets take advantage of that and use it in our server code.

```js
// Access the virtualDirPath appSettings and give it a default value of '/'
// in the event that it doesn't exist or isn't set
var virtualDirPath = process.env.virtualDirPath || '/';

// We also want to make sure that our virtualDirPath 
// always starts with a forward slash
if (!virtualDirPath.startsWith('/', 0))
  virtualDirPath = '/' + virtualDirPath;

// Setup a route at the index of our app    
server.get(virtualDirPath, (req, res) => {
    return res.status(200).send('Hello World');
});

```

We can use the virtualDirPath with our static resources as well

```js
// Public Directory
server.use(express.static(path.join(virtualDirPath, 'public')));
// Bower
server.use('/bower_components', express.static(path.join(virtualDirPath, 'bower_components')));

```

Lets put all of that together

```js
const express = require('express');
const server = express();

const port = process.env.PORT || 3000;

// Access the virtualDirPath appSettings and give it a default value of '/'
// in the event that it doesn't exist or isn't set
var virtualDirPath = process.env.virtualDirPath || '/';

// We also want to make sure that our virtualDirPath 
// always starts with a forward slash
if (!virtualDirPath.startsWith('/', 0))
  virtualDirPath = '/' + virtualDirPath;

// Public Directory
server.use(express.static(path.join(virtualDirPath, 'public')));
// Bower
server.use('/bower_components', express.static(path.join(virtualDirPath, 'bower_components')));

// Setup a route at the index of our app    
server.get(virtualDirPath, (req, res) => {
    return res.status(200).send('Hello World');
});

server.listen(port, () => {
    console.log(`Listening on ${port}`);
});

```



## Getting Started


[IISNode](https://github.com/tjanczuk/iisnode) allows Node.js Web Apps to be hosted on IIS 7/8 just like a .NET application would. Of course, you can self host your `node.exe` process on Windows but why do that when you can just run your app in IIS.

IISNode will handle scaling over multiple cores, process manageement of `node.exe`, and auto-recycle your IIS Application whenever your app is updated, just to name a few of its [benefits](https://tomasz.janczuk.org/2011/08/hosting-nodejs-applications-in-iis-on.html).

### Requirements

IISNode does have a few requirements before you can host your Node.js app in IIS.

1. Node.js must be installed on the IIS host, 32-bit or 64-bit, either are supported.
1. IISNode installed [x86](https://github.com/azure/iisnode/releases/download/v0.2.21/iisnode-full-v0.2.21-x86.msi) or [x64](https://github.com/azure/iisnode/releases/download/v0.2.21/iisnode-full-v0.2.21-x64.msi), this should match the bitness of your IIS Host.
<li>The [Microsoft URL-Rewrite Module for IIS](http://www.iis.net/downloads/microsoft/url-rewrite) installed on your IIS host.
<ul>
1. This is key, otherwise requests to your Node.js app won't function as expected.
</ul>
</li>
1. A `Web.config` in the root folder of your Node.js app.
1. IISNode configuration via an `iisnode.yml` file or an `<iisnode>` element within your `Web.config`.



## Basic Hello World Example using Express


To get this example working, you'll need to create an IIS 7/8 app on your IIS host and add the directory containing the Node.js Web App as the Physical Directory. Ensure that your Application/Application Pool Identity can access the Node.js install. This example uses the Node.js 64-bit installation.

### Project Strucure

This is the basic project structure of a IISNode/Node.js Web app. It looks almost identical to any non-IISNode Web App except for the addition of the `Web.config`.

```js
- /app_root
  - package.json
  - server.js
  - Web.config

```

### server.js - Express Application

```js
const express = require('express');
const server = express();

// We need to get the port that IISNode passes into us 
// using the PORT environment variable, if it isn't set use a default value
const port = process.env.PORT || 3000;

// Setup a route at the index of our app    
server.get('/', (req, res) => {
    return res.status(200).send('Hello World');
});

server.listen(port, () => {
    console.log(`Listening on ${port}`);
});

```

### Configuration & Web.config

The `Web.config` is just like any other IIS `Web.config` except the following two things must be present, URL `<rewrite><rules>` and an IISNode `<handler>`. Both of these elements are children of the `<system.webServer>` element.

### Configuration

You can configure IISNode by using a [`iisnode.yml`](https://raw.githubusercontent.com/tjanczuk/iisnode/master/src/samples/configuration/iisnode.yml) file or by adding the [`<iisnode>`](https://github.com/tjanczuk/iisnode/blob/master/src/samples/configuration/web.config#L11) element as a child of `<system.webServer>` in your `Web.config`. Both of these configuration can be used in conjunction with one another however, in this case, `Web.config` will need to specify the `iisnode.yml` file **AND** [any configuration conflicts will be take from the `iisnode.yml` file instead](https://github.com/tjanczuk/iisnode/blob/master/src/samples/configuration/web.config#L105-L111). This configuration overriding cannot happen the other way around.

### IISNode Handler

In order for IIS to know that `server.js` contains our Node.js Web App we need to explicitly tell it that. We can do this by adding the IISNode `<handler>` to the `<handlers>` element.

```js
<handlers>
  <add name="iisnode" path="server.js" verb="*" modules="iisnode"/>
</handlers>

```

### URL-Rewrite Rules

The final part of the configuration is ensuring that traffic intended for our Node.js app coming into our IIS instance is being directed to IISNode. Without URL rewrite rules, we would need to visit our app by going to `http://<host>/server.js` and even worse, when trying to request a resource supplied by `server.js` you'll get a `404`. This is why URL rewriting is necessary for IISNode web apps.

```js
<rewrite>
    <rules>
        <!-- First we consider whether the incoming URL matches a physical file in the /public folder -->
        <rule name="StaticContent" patternSyntax="Wildcard">
            <action type="Rewrite" url="public/{R:0}" logRewrittenUrl="true"/>
            <conditions>
                <add input="{REQUEST_FILENAME}" matchType="IsFile" negate="true"/>
            </conditions>
            <match url="*.*"/>
        </rule>

        <!-- All other URLs are mapped to the Node.js application entry point -->
        <rule name="DynamicContent">
            <conditions>
                <add input="{REQUEST_FILENAME}" matchType="IsFile" negate="True"/>
            </conditions>
            <action type="Rewrite" url="server.js"/>
        </rule>
    </rules>
</rewrite>

```

[This is a working `Web.config` file for this example](https://gist.github.com/pbaio/f63918181d8d7f8ee1d2), setup for a 64-bit Node.js install.

That's it, now visit your IIS Site and see your Node.js application working.



## Using Socket.io with IISNode


To get Socket.io working with IISNode, the only changes necessary when not using a Virtual Directory/Nested Application are within the `Web.config`.

Since Socket.io sends requests starting with `/socket.io`, IISNode needs to communicate to IIS that these should also be handled IISNode and aren't just static file requests or other traffic. This requires a different `<handler>` than standard IISNode apps.

```js
<handlers>
    <add name="iisnode-socketio" path="server.js" verb="*" modules="iisnode" />
</handlers>

```

In addition to the changes to the `<handlers>` we also need to add an additional URL rewrite rule. The rewrite rule sends all `/socket.io` traffic to our server file where the Socket.io server is running.

```js
<rule name="SocketIO" patternSyntax="ECMAScript">
    <match url="socket.io.+"/>
    <action type="Rewrite" url="server.js"/>
</rule>

```

If you are using IIS 8, you'll need to disable your webSockets setting in your `Web.config` in addition to adding the above handler and rewrite rules. This is unnecessary in IIS 7 since there is no webSocket support.

`<webSocket enabled="false" />`



#### Remarks


### Virtual Directory / Nested Application with Views Pitfall

If you're going to be using Express to render views using a View Engine, you'll need to pass the `virtualDirPath` value in to your views

```js
`res.render('index', { virtualDirPath: virtualDirPath });`

```

The reason for doing this is to make your hyperlinks to other views host by your app and static resource paths to know where the site is being hosted without needing to modify all views after deployment. This is one of the more annoying and tedious pitfalls of using Virtual Directories with IISNode.

### Versions

All of the examples above work with

- Express v4.x
- IIS 7.x/8.x
- Socket.io v1.3.x or greater

