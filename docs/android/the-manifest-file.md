---
metaTitle: "Android - The Manifest File"
description: "Declaring Components, Declaring permissions in your manifest file"
---

# The Manifest File


The Manifest is an obligatory file named exactly "AndroidManifest.xml" and located in the app's root directory. It specifies the app name, icon, Java package name, version, declaration of Activities, Services, app permissions and other information.



## Declaring Components


The primary task of the manifest is to inform the system about the app's components. For example, a manifest file can declare an activity as follows:

```java
<?xml version="1.0" encoding="utf-8"?>
<manifest ... >
    <application android:icon="@drawable/app_icon.png" ... >
        <activity android:name="com.example.project.ExampleActivity"
                  android:label="@string/example_label" ... >
        </activity>
        ...
    </application>
</manifest>

```

In the `<application>` element, the `android:icon` attribute points to resources for an icon that identifies the app.

In the  element, the `android:name` attribute specifies the fully qualified class name of the Activity subclass and the android:label attribute specifies a string to use as the user-visible label for the activity.

You must declare all app components this way:

-`<activity>` elements for activities

-`<service>` elements for services

-`<receiver>` elements for broadcast receivers

-`<provider>` elements for content providers

Activities, services, and content providers that you include in your source but do not declare in the manifest are not visible to the system and, consequently, can never run. However, broadcast receivers can be either declared in the manifest or created dynamically in code (as `BroadcastReceiver` objects) and registered with the system by calling `registerReceiver()`.

For more about how to structure the manifest file for your app, see The AndroidManifest.xml File documentation.



## Declaring permissions in your manifest file


Any permission required by your application to access a protected part of the API or to interact with other applications must be declared in your `AndroidManifest.xml` file. This is done using the `<uses-permission />` tag.

**Syntax**

```

<uses-permission android:name="string"
    android:maxSdkVersion="integer"/>

```

**android:name:** This is the name of the required permission

**android:maxSdkVersion:** The highest API level at which this permission should be granted to your app. Setting this permission is optional and should only be set if the permission your app requires is no longer needed at a certain API level.

Sample AndroidManifest.xml:

```java
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="com.android.samplepackage">
       
    <!-- request internet permission -->
    <uses-permission android:name="android.permission.INTERNET" />

    <!-- request camera permission -->
    <uses-permission android:name="android.permission.CAMERA"/>

    <!-- request permission to write to external storage -->
    <uses-permission
         android:name="android.permission.WRITE_EXTERNAL_STORAGE"
         android:maxSdkVersion="18" />

    <application>....</application>
</manifest>

```

* Also see the [Permissions](http://stackoverflow.com/documentation/android/1525/permissions-in-api-23#t=201607271704493154496) topic.

