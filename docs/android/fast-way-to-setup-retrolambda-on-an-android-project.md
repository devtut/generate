---
metaTitle: "Android - Fast way to setup Retrolambda on an android project."
description: "Setup and example how to use:"
---

# Fast way to setup Retrolambda on an android project.


Retrolambda is a library which allows to use Java 8 lambda expressions, method references and try-with-resources statements on Java 7, 6 or 5.

The Gradle Retrolambda Plug-in allows to integrate Retrolambda into a Gradle based build. This allows for example to use these constructs in an Android application, as standard Android development currently does not yet support Java 8.



## Setup and example how to use:


**Setup Steps:**

<li>
Download and install jdk8.
</li>
<li>
Add the following to your project’s main build.gradle

```java
buildscript {
    repositories {
        mavenCentral()
    }

    dependencies {
        classpath 'me.tatarka:gradle-retrolambda:3.2.3'
    }
}

```


</li>
<li>
Now add this to your application module’s build.gradle

```java
apply plugin: 'com.android.application' // or apply plugin: 'java'
apply plugin: 'me.tatarka.retrolambda'

```


</li>
<li>
Add these lines to your application module’s build.gradle to inform the IDE of the language level:

```java
android {
    compileOptions {
        sourceCompatibility JavaVersion.VERSION_1_8
        targetCompatibility JavaVersion.VERSION_1_8
    }
}

```


</li>

**Example:**

So things like this:

```java
button.setOnClickListener(new View.OnClickListener() {
    @Override
    public void onClick(View v) {
        log("Clicked");
    }
});

```

Become this:

```java
button.setOnClickListener(v -> log("Clicked"));

```

