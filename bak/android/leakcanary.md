---
metaTitle: "Android - Leakcanary"
description: "Implementing a Leak Canary in  Android Application"
---

# Leakcanary


Leak Canary is an Android and Java library used to detect leak in the application



## Implementing a Leak Canary in  Android Application


In your **build.gradle** you need to add the below dependencies:

```java
debugCompile 'com.squareup.leakcanary:leakcanary-android:1.5.1'
releaseCompile 'com.squareup.leakcanary:leakcanary-android-no-op:1.5.1'
testCompile 'com.squareup.leakcanary:leakcanary-android-no-op:1.5.1'

```

In your `Application` class you need to add the below code inside your `onCreate()`:

```java
LeakCanary.install(this);

```

That's all you need to do for **LeakCanary**, it will automatically show notifications when there is a leak in your build.



#### Remarks


You can see the example in link below

[https://github.com/square/leakcanary](https://github.com/square/leakcanary)

