---
metaTitle: "JavsScript - Vibration API"
description: "Single vibration, Check for support, Vibration patterns"
---

# Vibration API




## Single vibration


Vibrate the device for 100ms:

```js
window.navigator.vibrate(100);

```

or

```js
window.navigator.vibrate([100]);

```



## Check for support


Check if browser supports vibrations

```js
if ('vibrate' in window.navigator)
    // browser has support for vibrations
else
    // no support

```



## Vibration patterns


An array of values describes periods of time in which the device is vibrating and not vibrating.

```js
window.navigator.vibrate([200, 100, 200]);

```



#### Syntax


- let success = window.navigator.vibrate( pattern );



#### Remarks


[Support by browsers](http://caniuse.com/#feat=vibration) might be limited. Also support by the operating system may be limited.

The following table gives an overview of the earliest browser versions that provide support for vibrations.

|Chrome|Edge|Firefox|Internet Explorer|Opera|Opera Mini|Safari</th>
|---|---|---|---|---|---|---|---|---|---
|30|**no support**|16|**no support**|17|**no support**|**no support**</td>

