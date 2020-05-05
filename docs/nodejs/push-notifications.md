---
metaTitle: "Push notifications"
description: "Web notification, Apple"
---

# Push notifications


So if you wanna make web app notification I suggest you to use Push.js or SoneSignal framework for Web/mobile app.

Push is the fastest way to get up and running with Javascript notifications. A fairly new addition to the official specification, the Notification API allows modern browsers such as Chrome, Safari, Firefox, and IE 9+ to push notifications to a user’s desktop.

You will have to use Socket.io and some backend framework, I will user Express for this example.



## Web notification


First, you will need to install [Push.js](https://pushjs.org/) module.

```js
$ npm install push.js --save

```

Or import it to your front-end app through [CDN](https://cdnjs.com/libraries/push.js)

```js
<script src="./push.min.js"></script> <!-- CDN link -->

```

After you are done with that, you should be good to go. This is how it should look like if u wanna make simple notification:

```js
Push.create('Hello World!')

```

I will assume that you know how to setup [Socket.io](https://socket.io/) with your app. Here is some code example of my backend app with express:

```js
var app = require('express')();
var server = require('http').Server(app);
var io = require('socket.io')(server);

server.listen(80);

app.get('/', function (req, res) {
  res.sendfile(__dirname + '/index.html');
});

io.on('connection', function (socket) {
  
  socket.emit('pushNotification', { success: true, msg: 'hello' });

});

```

After your server is all set up, you should be able to move on to front-end stuff. Now all we have to do is import Socket.io [CDN](https://cdnjs.com/libraries/socket.io) and add this code to my **index.html** file:

```js
<script src="../socket.io.js"></script> <!-- CDN link -->
<script>
  var socket = io.connect('http://localhost');
  socket.on('pushNotification', function (data) {
    console.log(data);
    Push.create("Hello world!", {
        body: data.msg, //this should print "hello"
        icon: '/icon.png',
        timeout: 4000,
        onClick: function () {
            window.focus();
            this.close();
        }
    });
  });
</script>

```

There you go, now you should be able to display your notification, this also works on any Android device, and if u wanna use [Firebase](https://firebase.google.com/) cloud messaging, you can use it with this module, [Here](https://github.com/Nickersoft/push-fcm-plugin) is link for that example written by Nick (creator of Push.js)



## Apple


Keep in mind that this will not work on Apple devices (I didnt test them all), but if you want to make push notifications check [OneSignal](https://onesignal.com/) plugin.



#### Parameters


|module/framework|description
|---|---|---
|node.js/express|Simple backe-end framework for Node.js application, very easy to use and extremely powerful
|Socket.io|Socket.IO enables real-time bidirectional event-based communication. It works on every platform, browser or device, focusing equally on reliability and speed.
|Push.js|The world's most versatile desktop notifications framework
|OneSignal|Just another form off push notifications for Apple devices
|Firebase|Firebase is Google's mobile platform that helps you quickly develop high-quality apps and grow your business.

