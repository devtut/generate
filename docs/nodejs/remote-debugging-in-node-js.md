---
metaTitle: "Node.js - Remote Debugging in Node.JS"
description: "NodeJS run configuration, IntelliJ/Webstorm Configuration, Use the proxy for debugging via port on Linux"
---

# Remote Debugging in Node.JS



## NodeJS run configuration


To set up Node remote debugging, simply run the node process with the `--debug` flag. You can add a port on which the debugger should run using `--debug=<port>`.

When your node process starts up you should see the message

```js
Debugger listening on port <port>

```

Which will tell you that everything is good to go.

Then you set up the remote debugging target in your specific IDE.



## IntelliJ/Webstorm Configuration


1. Make sure that the NodeJS plugin is enabled
1. Select your run configurations (screen)

[<img src="http://i.stack.imgur.com/74hst.png" alt="Run configurations" />](http://i.stack.imgur.com/74hst.png)

1. Select **+** > **Node.js Remote Debug**

[<img src="http://i.stack.imgur.com/MVlrq.png" alt="Add new configuration" />](http://i.stack.imgur.com/MVlrq.png)

1. Make sure you enter the port selected above as well as the correct host

[<img src="http://i.stack.imgur.com/x7Hbu.png" alt="Configure port and host" />](http://i.stack.imgur.com/x7Hbu.png)

Once those are configured simply run the debug target as you normally would and it will stop on your breakpoints.



## Use the proxy for debugging via port on Linux


If you start your application on Linux, use the proxy for debugging via port, for example:

```js
socat TCP-LISTEN:9958,fork TCP:127.0.0.1:5858 &

```

Use port 9958 for remote debugging then.

