---
metaTitle: "JavsScript - Battery Status API"
description: "Battery Events, Getting current battery level, Is battery charging?, Get time left until battery is empty, Get time left until battery is fully charged"
---

# Battery Status API




## Battery Events


```js
// Get the battery API
navigator.getBattery().then(function(battery) {
    battery.addEventListener('chargingchange', function(){
        console.log( 'New charging state: ', battery.charging );
    });

    battery.addEventListener('levelchange', function(){
        console.log( 'New battery level: ', battery.level * 100 + "%" );
    });

    battery.addEventListener('chargingtimechange', function(){
        console.log( 'New time left until full: ', battery.chargingTime, " seconds" );
    });

    battery.addEventListener('dischargingtimechange', function(){
        console.log( 'New time left until empty: ', battery.dischargingTime, " seconds" );
    });
});

```



## Getting current battery level


```js
// Get the battery API
navigator.getBattery().then(function(battery) {
    // Battery level is between 0 and 1, so we multiply it by 100 to get in percents
    console.log("Battery level: " + battery.level * 100 + "%");
});

```



## Is battery charging?


```js
// Get the battery API
navigator.getBattery().then(function(battery) {
    if (battery.charging) {
        console.log("Battery is charging");
    } else {
        console.log("Battery is discharging");
    }
});

```



## Get time left until battery is empty


```js
// Get the battery API
navigator.getBattery().then(function(battery) {
    console.log( "Battery will drain in ", battery.dischargingTime, " seconds" );
});

```



## Get time left until battery is fully charged


```js
// Get the battery API
navigator.getBattery().then(function(battery) {
    console.log( "Battery will get fully charged in ", battery.chargingTime, " seconds" );
});

```



#### Remarks


<li>
Note that the Battery Status API is no longer available due to privacy reasons where it could be used by remote trackers for user fingerprinting.
</li>
<li>
The Battery Status API is an Application Programming Interface for the client's battery status. It provides information on:
<ul>
1. battery charging state via `'chargingchange'` event and `battery.charging`;
1. battery level via `'levelchange'` event and `battery.level`;
1. charging time via `'chargingtimechange'` event and `battery.chargingTime`;
1. discharging time via `'dischargingtimechange'` event and `battery.dischargingTime`.
</ul>
</li>
<li>
MDN Docs: [https://developer.mozilla.org/en/docs/Web/API/Battery_status_API](https://developer.mozilla.org/en/docs/Web/API/Battery_status_API)
</li>

