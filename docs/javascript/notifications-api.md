---
metaTitle: "Notifications API"
description: "Requesting Permission to send notifications, Sending Notifications, Closing a notification, Notification events"
---

# Notifications API



## Requesting Permission to send notifications


We use `Notification.requestPermission` to ask the user if he/she wants to receive notifications from our website.

```js
Notification.requestPermission(function() {
    if (Notification.permission === 'granted') {
        // user approved.
        // use of new Notification(...) syntax will now be successful
    } else if (Notification.permission === 'denied') {
        // user denied.
    } else { // Notification.permission === 'default'
        // user didn’t make a decision.
        // You can’t send notifications until they grant permission.
    }
});

```

Since Firefox 47
The `.requestPermission` method can also return a promise when handling the user's decision for granting permission

```js
Notification.requestPermission().then(function(permission) {
    if (!('permission' in Notification)) {
        Notification.permission = permission;
    }
    // you got permission !
    }, function(rejection) {
    // handle rejection here.
    }
);

```



## Sending Notifications


After the user has approved a [request for permission to send notifications](http://stackoverflow.com/documentation/javascript/696/notifications-api/2305/requesting-permission-to-send-notifications), we can send a simple notification that says Hello to the user:

```js
new Notification('Hello', { body: 'Hello, world!', icon: 'url to an .ico image' });

```

This will send a notification like this:

> 
<h3>Hello</h3>
Hello, world!




## Closing a notification


You can close a notification by using the `.close()` method.

```js
let notification = new Notification(title, options);
// do some work, then close the notification
notification.close()

```

You can utilize the `setTimeout` function to auto-close the notification sometime in the future.

```js
let notification = new Notification(title, options);
setTimeout(() => {
    notification.close()
}, 4000);

```

The above code will spawn a notification and close it after 4 seconds.



## Notification events


The Notification API specifications support 2 events that can be fired by a Notification.

1. The `click` event.

This event will run when you click on the notification body (excluding the closing X and the Notifications configuration button).

Example:

```js
notification.onclick = function(event) {
    console.debug("you click me and this is my event object: ", event);
}

```


1. The `error` event

The notification will fire this event whenever something wrong will happen, like being unable to display

```js
notification.onerror = function(event) {
    console.debug("There was an error: ", event);
}

```



#### Syntax


- Notification.requestPermission(**callback**)
- Notification.requestPermission().then(**callback**, **rejectFunc**)
- new Notification(**title**, **options**)
- **notification**.close()



#### Remarks


The Notifications API was designed to allow browser access to notifying the client.

[Support by browsers](http://caniuse.com/#feat=notifications) might be limited. Also support by the operating system may be limited.

The following table gives an overview of the earliest browser versions that provide support for notifications.

<th align="center">Chrome</th><th align="center">Edge</th><th align="center">Firefox</th><th align="center">Internet Explorer</th><th align="center">Opera</th><th align="center">Opera Mini</th><th align="center">Safari</th>
|------
<td align="center">29</td><td align="center">14</td><td align="center">46</td><td align="center">no support</td><td align="center">38</td><td align="center">no support</td><td align="center">9.1</td>

