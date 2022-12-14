---
metaTitle: "JavaScript - Geolocation"
description: "Get a user's latitude and longitude, More descriptive error codes, Get updates when a user's location changes"
---

# Geolocation



## Get a user's latitude and longitude


```js
if (navigator.geolocation) {
  navigator.geolocation.getCurrentPosition(geolocationSuccess, geolocationFailure);
} else {
  console.log("Geolocation is not supported by this browser.");
}

// Function that will be called if the query succeeds
var geolocationSuccess = function(pos) {
  console.log("Your location is " + pos.coords.latitude + "°, " + pos.coords.longitude + "°.");
};

// Function that will be called if the query fails
var geolocationFailure = function(err) {
  console.log("ERROR (" + err.code + "): " + err.message);
};

```



## More descriptive error codes


In the event that geolocation fails, your callback function will receive a `PositionError` object. The object will include an attribute named `code` that will have a value of `1`, `2`, or `3`. Each of these numbers signifies a different kind of error; the `getErrorCode()` function below takes the `PositionError.code` as its only argument and returns a string with the name of the error that occurred.

```js
var getErrorCode = function(err) {
  switch (err.code) {
    case err.PERMISSION_DENIED:
      return "PERMISSION_DENIED";
    case err.POSITION_UNAVAILABLE:
      return "POSITION_UNAVAILABLE";
    case err.TIMEOUT:
      return "TIMEOUT";
    default:
      return "UNKNOWN_ERROR";
  }
};

```

It can be used in `geolocationFailure()` like so:

```js
var geolocationFailure = function(err) {
  console.log("ERROR (" + getErrorCode(err) + "): " + err.message);
};

```



## Get updates when a user's location changes


You can also receive regular updates of the user's location; for example, as they move around while using a mobile device. Location tracking over time can be very sensitive, so be sure to explain to the user ahead of time why you're requesting this permission and how you'll use the data.

```js
if (navigator.geolocation) {
    //after the user indicates that they want to turn on continuous location-tracking
    var watchId = navigator.geolocation.watchPosition(updateLocation, geolocationFailure);
} else {
    console.log("Geolocation is not supported by this browser.");
}

var updateLocation = function(position) {
    console.log("New position at: " + position.coords.latitude + ", " + position.coords.longitude);
};

```

To turn off continuous updates:

```js
navigator.geolocation.clearWatch(watchId);

```



#### Syntax


- navigator.geolocation.getCurrentPosition(**successFunc**, **failureFunc**)
- navigator.geolocation.watchPosition(**updateFunc**, **failureFunc**)
- navigator.geolocation.clearWatch(**watchId**)



#### Remarks


The Geolocation API does what you might expect: retrieve information about the client's whereabouts, represented in latitude and longitude. However, it is up to the user to agree to give away their location.

This API is defined in the W3C [Geolocation API Specification](https://www.w3.org/TR/geolocation-API/). Features for obtaining civic addresses and to enable geofencing / triggering of events have been explored, but are not widely implemented.

To check if the browser supports the Geolocation API:

```js
if(navigator.geolocation){
    // Horray! Support!
} else {
    // No support...
}

```

