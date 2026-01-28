---
metaTitle: "Android - Multidex and the Dex Method Limit"
description: "Enabling Multidex, Multidex by extending Application, Multidex by extending MultiDexApplication, Multidex by using MultiDexApplication directly, Counting Method References On Every Build (Dexcount Gradle Plugin)"
---

# Multidex and the Dex Method Limit


DEX means Android app's (APK) executable bytecode files in the form of Dalvik Executable (DEX) files, which contain the compiled code used to run your app.

The Dalvik Executable specification limits the total number of methods that can be referenced within a single DEX file to 65,536 (64K)—including Android framework methods, library methods, and methods in your own code.

To overcome this limit requires configure your app build process to generate more than one DEX file, known as a Multidex.



## Enabling Multidex


In order to enable a multidex configuration you need:

- to change your Gradle build configuration
- to use a `MultiDexApplication` or enable the MultiDex in your `Application` class

### Gradle configuration

In `app/build.gradle` add these parts:

```java
android {
    compileSdkVersion 24
    buildToolsVersion "24.0.1"

    defaultConfig {
        ...
        minSdkVersion 14
        targetSdkVersion 24
        ...

        // Enabling multidex support.
        multiDexEnabled true
    }
    ...
}

dependencies {
  compile 'com.android.support:multidex:1.0.1'
}

```

### Enable MultiDex in your Application

Then proceed with one of three options:

- [Multidex by extending Application](http://stackoverflow.com/documentation/android/1887/multidex-and-the-dex-method-limit/6003/multidex-by-extending-application#t=20160807053003773542)

- [Multidex by extending MultiDexApplication](http://stackoverflow.com/documentation/android/1887/multidex-and-the-dex-method-limit/18691/multidex-by-extending-multidexapplication#t=20160807053003773542)

- [Multidex by using MultiDexApplication directly](http://stackoverflow.com/documentation/android/1887/multidex-and-the-dex-method-limit/6002/multidex-by-using-multidexapplication-directly#t=20160807053003773542)

When these configuration settings are added to an app, the Android build tools construct a primary dex (classes.dex) and supporting (classes2.dex, classes3.dex) as needed.<br />
The build system will then package them into an APK file for distribution.



## Multidex by extending Application


Use this option if your project requires an `Application` subclass.

Specify this `Application` subclass using the `android:name` property in the manifest file inside the `application` tag.

In the `Application` subclass, add the `attachBaseContext()` method override, and in that method call `MultiDex.install()`:

```java
package com.example;

import android.app.Application;
import android.content.Context;

/**
 * Extended application that support multidex 
 */
public class MyApplication extends Application {

    @Override
    protected void attachBaseContext(Context base) {
        super.attachBaseContext(base);
        MultiDex.install(this);
    }
}

```

Ensure that the `Application` subclass is specified in the `application` tag of your AndroidManifest.xml:

```java
<application
    android:name="com.example.MyApplication"
    android:icon="@drawable/ic_launcher"
    android:label="@string/app_name">
</application>

```



## Multidex by extending MultiDexApplication


This is very similar to using an `Application` subclass and overriding the `attachBaseContext()` method.

However, using this method, you don't need to override `attachBaseContext()` as this is already done in the `MultiDexApplication` superclass.

Extend `MultiDexApplication` instead of `Application`:

```java
package com.example;

import android.support.multidex.MultiDexApplication;
import android.content.Context;

/**
 * Extended MultiDexApplication 
 */
public class MyApplication extends MultiDexApplication {

     // No need to override attachBaseContext()

     //..........
}

```

Add this class to your AndroidManifest.xml exactly as if you were extending Application:

```java
<application
    android:name="com.example.MyApplication"
    android:icon="@drawable/ic_launcher"
    android:label="@string/app_name">
</application>

```



## Multidex by using MultiDexApplication directly


Use this option if you don't need an `Application` subclass.

This is the simplest option, but this way you can't provide your own `Application` subclass.  If an `Application` subclass is needed, you will have to switch to one of the other options to do so.

For this option, simply specify the fully-qualified class name `android.support.multidex.MultiDexApplication` for the `android:name` property of the `application` tag in the AndroidManifest.xml:

```java
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="com.example.android.multidex.myapplication">
    <application
        ...
        android:name="android.support.multidex.MultiDexApplication">
        ...
    </application>
</manifest>

```



## Counting Method References On Every Build (Dexcount Gradle Plugin)


The [dexcount plugin](https://github.com/KeepSafe/dexcount-gradle-plugin) counts methods and class resource count after a successful build.

Add the plugin in the `app/build.gradle`:

```java
apply plugin: 'com.android.application'

buildscript {
    repositories {
        mavenCentral() // or jcenter()
    }

    dependencies {
        classpath 'com.getkeepsafe.dexcount:dexcount-gradle-plugin:0.5.5'
    }
}

```

Apply the plugin in the `app/build.gradle` file:

```java
apply plugin: 'com.getkeepsafe.dexcount'

```

Look for the output data generated by the plugin in:

**../app/build/outputs/dexcount**

Especially useful is the .html chart in:

**../app/build/outputs/dexcount/debugChart/index.html**



#### Remarks


### What is dex?

Dex is the name of the file format and encoding to which Android Java code is compiled.  Early versions of Android would load and execute `dex` binaries directly in a virtual machine named Dalvik.  More recent versions of Android use the Android Runtime (ART), which treats `dex` files as an intermediate representation and performs further compilations on it prior to running the application.

Dex is a very old file format, in terms of the lifespan of smartphones, and was designed for devices whose main memory was measured in tens of megabytes.  The design limitations of those days have remained with us to this day.

### The problem:

The `dex` file format encodes a limit to the number of methods that can be referenced in a single binary.  Because the portion of the file format that stores the number of references is two bytes long, the maximum number of method references is `0xFFFF`, or 65535.  If an application contains more than that number of method references, it will fail to compile.

### What to do about it:

Google has provided a way around this problem, called Multidex. It has compile-time and run-time components.  As its name implies, at compile-time it will divide code between one or more `dex` files.  At runtime, it will teach the default `ClassLoader` how to look up classes from these files.

This approach works well on newer devices, but has some substantial drawbacks.  It can increase application startup time dramatically, and on older devices can cause `Application Not Responding` failures.

Multidex, while effective, should be avoided if possible.

### How to avoid the limit:

Before configuring your app to enable use of 64K or more method references, you should take steps to reduce the total number of references called by your app code, including methods defined by your app code or included libraries. The following strategies can help you avoid hitting the dex reference limit:

- **Review your app's direct and transitive dependencies** - Ensure any large library dependency you include in your app is used in a manner that outweighs the amount of code being added to the application. A common anti-pattern is to include a very large library because a few utility methods were useful. Reducing your app code dependencies can often help you avoid the dex reference limit.
- **Remove unused code with ProGuard** - Configure the [ProGuard settings](http://stackoverflow.com/documentation/android-gradle/5257/shrink-code-and-resources#t=20160916064154250555) for your app to run ProGuard and ensure you have shrinking enabled for release builds. Enabling shrinking ensures you are not shipping unused code with your APKs.

The first point requires diligence and discipline on the part of the developer.  When incorporating third-party libraries, one must consider the size of the library.  For example, two popular JSON libraries are Jackson and Gson.  Functionally they are quite similar, but Gson tends to see greater use in Android.  One reason is that Jackson weighs in around 9,000 methods, whereas Gson contributes 1,900.

There are several tools available to help developers keep track of the size of their application:

- [dexcount-gradle-plugin](https://github.com/keepsafe/dexcount-gradle-plugin) reports the number of method references in your APK or AAR on each build
- [dex-method-counts](https://github.com/mihaip/dex-method-counts) is a commandline tool that counts the number of method references in an APK
- [www.methodscount.com](http://www.methodscount.com) is a web service which will count the method references in any APK that you upload.

