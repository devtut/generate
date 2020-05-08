---
metaTitle: "Android - Firebase Crash Reporting"
description: "How to report an error, How to add Firebase Crash Reporting to your app"
---

# Firebase Crash Reporting



## How to report an error


[Firebase Crash Reporting](http://stackoverflow.com/documentation/android/3843/firebase/13605/how-to-add-firebase-crash-reporting-to-your-app#t=201608090640190791722) automatically generates reports for fatal errors (or uncaught exceptions).

You can create your custom report using:

```java
FirebaseCrash.report(new Exception("My first Android non-fatal error"));

```

You can check in the log when FirebaseCrash initialized the module:

> 
<p>07–20 08:57:24.442 D/FirebaseCrashApiImpl: **FirebaseCrash reporting API initialized**
07–20 08:57:24.442 I/FirebaseCrash: **FirebaseCrash reporting initialize**d com.google.firebase.crash.internal.zzg@3333d325
07–20 08:57:24.442 D/FirebaseApp: **Initialized class com.google.firebase.crash.FirebaseCrash.**</p>


And then when it sent the exception:

> 
<p>07–20 08:57:47.052 D/FirebaseCrashApiImpl: **throwable java.lang.Exception: My first Android non-fatal error**
07–20 08:58:18.822 D/FirebaseCrashSenderServiceImpl: **Response code: 200**
07–20 08:58:18.822 D/FirebaseCrashSenderServiceImpl: **Report sent**</p>


You can add custom logs to your report with

```java
FirebaseCrash.log("Activity created");

```



## How to add Firebase Crash Reporting to your app


In order to add **Firebase Crash Reporting** to your app, perform the following steps:

<li>
Create an app on the **Firebase Console** [here](https://console.firebase.google.com/).
</li>
<li>
Copy the `google-services.json` file from your project into your in `app/` directory.
</li>
<li>
Add the following rules to your root-level **build.gradle** file in order to include the `google-services` plugin:

```java
buildscript {
    // ...
    dependencies {
        // ...
        classpath 'com.google.gms:google-services:3.0.0'
    }
}

```


</li>
<li>
In your module Gradle file, add the `apply plugin` line at the bottom of the file to enable the Gradle plugin:

```java
apply plugin: 'com.google.gms.google-services'

```


</li>
<li>
Add the dependency for **Crash Reporting** to your app-level **build.gradle** file:

```java
compile 'com.google.firebase:firebase-crash:10.2.1'

```


</li>
<li>
You can then fire a custom exception from your application by using the following line:

```java
FirebaseCrash.report(new Exception("Non Fatal Error logging"));

```


All your fatal exceptions will be reported to your **Firebase Console**.
</li>
<li>
If you want to add custom logs to a console, you can use the following code:

```java
FirebaseCrash.log("Level 2 completed.");

```


</li>

For more information, please visit:

- [Official documentation](https://firebase.google.com/docs/crash/android)
- [Stack Overflow dedicated topic](http://stackoverflow.com/documentation/firebase/4669/crash-reporting#t=201608090639072764156)

