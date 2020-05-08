---
metaTitle: "Android - CleverTap"
description: "Setting the debug level, Get an instance of the SDK to record events"
---

# CleverTap


Quick hacks for the analytics and engagement SDK provided by CleverTap - Android



## Setting the debug level


In your custom application class, override the `onCreate()` method, add the line below:

```java
CleverTapAPI.setDebugLevel(1);

```



## Get an instance of the SDK to record events


```java
CleverTapAPI cleverTap;
try {
  cleverTap = CleverTapAPI.getInstance(getApplicationContext());
} catch (CleverTapMetaDataNotFoundException e) {
  // thrown if you haven't specified your CleverTap Account ID or Token in your AndroidManifest.xml
} catch (CleverTapPermissionsNotSatisfied e) {
  // thrown if you haven’t requested the required permissions in your AndroidManifest.xml
}

```



#### Remarks


Get your CleverTap credentials from [https://clevertap.com](https://clevertap.com).

