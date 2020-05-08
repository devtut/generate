---
metaTitle: "Android - Creating your own libraries for Android applications"
description: "Create a library available on Jitpack.io, Creating library project, Using library in project as a module"
---

# Creating your own libraries for Android applications



## Create a library available on Jitpack.io


Perform the following steps to create the library:

<li>
Create a GitHub account.
</li>
<li>
Create a Git repository containing your library project.
</li>
<li>
Modify your library project's `build.gradle` file by adding the following code:

```java
apply plugin: 'com.github.dcendents.android-maven'

...

// Build a jar with source files.
task sourcesJar(type: Jar) {
    from android.sourceSets.main.java.srcDirs
    classifier = 'sources'
}

task javadoc(type: Javadoc) {
    failOnError  false
    source = android.sourceSets.main.java.sourceFiles
    classpath += project.files(android.getBootClasspath().join(File.pathSeparator))
    classpath += configurations.compile
}

// Build a jar with javadoc.
task javadocJar(type: Jar, dependsOn: javadoc) {
    classifier = 'javadoc'
    from javadoc.destinationDir
}

artifacts {
    archives sourcesJar
    archives javadocJar
}

```


Make sure that you commit/push the above changes to GitHub.
</li>
<li>
Create a release from the current code on Github.
</li>
<li>
Run `gradlew install` on your code.
</li>
<li>
Your library is now available by the following dependency:

```java
compile 'com.github.[YourUser]:[github repository name]:[release tag]'

```


</li>



## Creating library project


To create a libary , you should use `File -> New -> New Module -> Android Library`. This will create a basic library project.

When that's done, you must have a project that is set up the following manner:

```java
[project root directory]
    [library root directory]
    [gradle]
    build.gradle //project level
    gradle.properties
    gradlew
    gradlew.bat
    local.properties
    settings.gradle //this is important!

```

Your `settings.gradle` file must contain the following:

```java
include ':[library root directory]'

```

Your `[library root directory]` must contain the following:

```java
[libs]
[src]
   [main]
      [java]
         [library package]
   [test]
      [java]
         [library package]
build.gradle //"app"-level
proguard-rules.pro

```

Your "app"-level `build.gradle` file must contain the following:

```java
apply plugin: 'com.android.library'

android {
    compileSdkVersion 23
    buildToolsVersion "23.0.2"

    defaultConfig {
        minSdkVersion 14
        targetSdkVersion 23
    }
}

```

With that, your project should be working fine!



## Using library in project as a module


To use the library, you must include it as a dependency with the following line:

```java
compile project(':[library root directory]')

```

