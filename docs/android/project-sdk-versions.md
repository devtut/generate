---
metaTitle: "Android - Project SDK versions"
description: "Defining project SDK versions"
---

# Project SDK versions


An Android application needs to run on all kinds of devices. Each device may have a different version on Android running on it.

Now, each Android version might not support all the features that your app requires, and so while building an app, you need to keep the minimum and maximum Android version in mind.



## Defining project SDK versions


In your `build.gradle` file of main module(**app**), define your minimum and target version number.

```java
android {
    //the version of sdk source used to compile your project
    compileSdkVersion 23

    defaultConfig {
        //the minimum sdk version required by device to run your app
        minSdkVersion 19
        //you normally don't need to set max sdk limit so that your app can support future versions of android without updating app
        //maxSdkVersion 23
        //
        //the latest sdk version of android on which you are targeting(building and testing) your app, it should be same as compileSdkVersion
        targetSdkVersion 23
    }
}

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|SDK Version|The SDK version for each field is the Android release's SDK API level integer. For example, Froyo (Android 2.2) corresponds to API level 8. These integers are also defined in [`Build.VERSION_CODES`](http://developer.android.com/reference/android/os/Build.VERSION_CODES.html).



#### Remarks


There are four relevant SDK versions in every project:

<li>
`targetSdkVersion` is the latest version of Android that you have tested against.
The framework will use `targetSdkVersion` to determine when to enable certain compatibility behaviors. For example, targeting API level 23 or above will opt you in to [the runtime permissions model](http://developer.android.com/training/permissions/requesting.html).
</li>
<li>
`minSdkVersion` is the minimum version of Android that your application supports. Users running any version of Android older than this version will not be able to install your application or see it in the Play Store.
</li>
<li>
`maxSdkVersion` is the maximum version of Android that your application supports. Users running any version of Android newer than this version will not be able to install your application or see it in the Play Store. This should generally not be used as most applications will work on newer versions of Android without any additional effort.
</li>
<li>
`compileSdkVersion` is the version of the Android SDK that your application will be compiled with. It should generally be the latest version of Android that has been publicly released. This defines which APIs you can access when writing your code. You cannot call methods introduced in API level 23 if your `compileSdkVersion` is set to 22 or lower.
</li>

