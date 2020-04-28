---
metaTitle: "Timestamps"
description: "High-resolution timestamps, Low-resolution timestamps, Get Timestamp in Seconds, Support for legacy browsers"
---

# Timestamps



## High-resolution timestamps


[`performance.now()`](https://developer.mozilla.org/en-US/docs/Web/API/Performance/now) returns a precise timestamp: The number of milliseconds, including microseconds, since the current web page started to load.

More generally, it returns the time elapsed since the [`performanceTiming.navigationStart`](https://developer.mozilla.org/en-US/docs/Web/API/PerformanceTiming/navigationStart) event.

```
t = performance.now();

```

For example, in a web browser's main context, `performance.now()` returns `6288.319` if the web page began to load 6288 milliseconds and 319 microseconds ago.



## Low-resolution timestamps


[`Date.now()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now) returns the number of whole milliseconds that have elapsed since 1 January 1970 00:00:00 UTC.

```
t = Date.now();

```

For example, `Date.now()` returns `1461069314` if it was called on 19 April 2016 at 12:35:14 GMT.



## Get Timestamp in Seconds


To get the timestamp in seconds

```
Math.floor((new Date().getTime()) / 1000)

```



## Support for legacy browsers


In older browsers where `Date.now()` is unavailable, use [`(new Date()).getTime()`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/getTime) instead:

```
t = (new Date()).getTime();

```

Or, to provide a `Date.now()` function for use in older browsers, [use this polyfill](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date/now#Polyfill):

```
if (!Date.now) {
  Date.now = function now() {
    return new Date().getTime();
  };
}

```



#### Syntax


- millisecondsAndMicrosecondsSincePageLoad = performance.now();
- millisecondsSinceYear1970 = Date.now();
- millisecondsSinceYear1970 = (new Date()).getTime();



#### Remarks


`performance.now()` is [available in modern web browsers](http://caniuse.com/#feat=high-resolution-time) and provides reliable timestamps with sub-millisecond resolution.

Since `Date.now()` and `(new Date()).getTime()` are based on the system time, they [often get skewed by a few milliseconds when the system time is automatically synchronized](http://gent.ilcore.com/2012/06/better-timer-for-javascript.html).

