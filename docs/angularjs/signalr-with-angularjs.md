---
metaTitle: "AngularJS - SignalR with AngularJs"
description: "SignalR And AngularJs [ ChatProject ]"
---

# SignalR with AngularJs


In this Article we focus on "How to create a simple project using AngularJs And SignalR",
in this training you need to know about "how create app with angularjs", "how to create/use service on angular" And basic knowledge about SignalR" for this we recommend [https://www.codeproject.com/Tips/590660/Introduction-to-SignalR)](https://www.codeproject.com/Tips/590660/Introduction-to-SignalR)).



## SignalR And AngularJs [ ChatProject ]


**step 1: Create Project**

```js
- Application
    - app.js
    - Controllers
        - appController.js
    - Factories
        - SignalR-factory.js
- index.html
- Scripts
    - angular.js
    - jquery.js
    - jquery.signalR.min.js
- Hubs

```

> 
SignalR version use: signalR-2.2.1


**Step 2: Startup.cs And ChatHub.cs**

Go to your **"/Hubs"** directory and Add 2 files **[Startup.cs, ChatHub.cs]**

**Startup.cs**

```js
using Microsoft.Owin;
using Owin;
[assembly: OwinStartup(typeof(SignalR.Hubs.Startup))]

namespace SignalR.Hubs
{
    public class Startup
    {
        public void Configuration(IAppBuilder app)
        {
            app.MapSignalR();
        }
    }
}

```

**ChatHub.cs**

```js
using Microsoft.AspNet.SignalR;

namespace SignalR.Hubs
{
    public class ChatHub : Hub
    {
        public void Send(string name, string message, string time)
        {
            Clients.All.broadcastMessage(name, message, time);
        }
    }
}

```

**step 3: create angular app**

Go to your **"/Application"** directory and Add **[app.js]** file

**app.js**

```js
var app = angular.module("app", []);

```

**step 4: create SignalR Factory**

Go to your **"/Application/Factories"** directory and Add **[SignalR-factory.js]** file

**SignalR-factory.js**

```js
app.factory("signalR", function () {
    var factory = {};

    factory.url = function (url) {
        $.connection.hub.url = url;
    }

    factory.setHubName = function (hubName) {
        factory.hub = hubName;
    }

    factory.connectToHub = function () {
        return $.connection[factory.hub];
    }

    factory.client = function () {
        var hub = factory.connectToHub();
        return hub.client;
    }

    factory.server = function () {
        var hub = factory.connectToHub();
        return hub.server;
    }

    factory.start = function (fn) {
        return $.connection.hub.start().done(fn);
    }

    return factory;
});

```

**step 5: update app.js**

```js
var app = angular.module("app", []);

app.run(function(signalR) {
    signalR.url("http://localhost:21991/signalr");
});

```

> 
localhost:21991/signalr | **this is your SignalR Hubs Urls**


**step 6: add controller**

Go to your **"/Application/Controllers"** directory and Add **[appController.js]** file

```js
app.controller("ctrl", function ($scope, signalR) {
    $scope.messages = [];
    $scope.user = {};

    signalR.setHubName("chatHub");

    signalR.client().broadcastMessage = function (name, message, time) {
        var newChat = { name: name, message: message, time: time };

        $scope.$apply(function() {
            $scope.messages.push(newChat);
        });
    };

    signalR.start(function () {
        $scope.send = function () {
            var dt = new Date();
            var time = dt.getHours() + ":" + dt.getMinutes() + ":" + dt.getSeconds();

            signalR.server().send($scope.user.name, $scope.user.message, time);
        }
    });
});

```

> 
signalR.setHubName("chatHub") | **[ChatHub] (public class) >** **ChatHub.cs**


> 
**Note:** do not insert **HubName** with upper Case, **first letter** is lower Case.


> 
**signalR.client()** | this method try to connect to your hubs and get all functions in the Hubs, in this sample we have "chatHub", to get "broadcastMessage()" function;


**step 7: add index.html in route of directory**

**index.html**

```js
<!DOCTYPE html>
<html ng-app="app" ng-controller="ctrl">
<head>
    <meta charset="utf-8" />
    <title>SignalR Simple Chat</title>
</head>
<body>
    <form>
        <input type="text" placeholder="name" ng-model="user.name" />
        <input type="text" placeholder="message" ng-model="user.message" />
        <button ng-click="send()">send</button>

        <ul>
            <li ng-repeat="item in messages">
                <b ng-bind="item.name"></b> <small ng-bind="item.time"></small> : { {item.message} }
            </li>
        </ul>
    </form>

    <script src="Scripts/angular.min.js"></script>
    <script src="Scripts/jquery-1.6.4.min.js"></script>
    <script src="Scripts/jquery.signalR-2.2.1.min.js"></script>
    <script src="signalr/hubs"></script>
    <script src="app.js"></script>
    <script src="SignalR-factory.js"></script>
</body>
</html

```

> 
Result with Image


> 
[User 1](https://i.stack.imgur.com/slA02.jpg) (send and receive)
[User 2](https://i.stack.imgur.com/zwvIf.jpg) (send and receive)


