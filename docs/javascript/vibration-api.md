---
metaTitle: "Vibration API"
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

<th align="center">Chrome</th><th align="center">Edge</th><th align="center">Firefox</th><th align="center">Internet Explorer</th><th align="center">Opera</th><th align="center">Opera Mini</th><th align="center">Safari</th>
|------
<td align="center">30</td><td align="center">**no support**</td><td align="center">16</td><td align="center">**no support**</td><td align="center">17</td><td align="center">**no support**</td><td align="center">**no support**</td>

