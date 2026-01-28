---
metaTitle: "Android - Publish a library to Maven Repositories"
description: "Publish .aar file to Maven"
---

# Publish a library to Maven Repositories




## Publish .aar file to Maven


In order to publish to a repository in Maven format ,“maven-publish” plugin for gradle can be used.

The plugin should be added to `build.gradle` file in library module.

```java
apply plugin: 'maven-publish'

```

You should define the publication and its identity attributes in `build.gradle` file too.
This identity attributes will be shown in the generated pom file and in future for importing this publication you will use them.You also need to define which artifacts you want to publish,for example i just want to publish generated .aar file after building the library.

```java
publishing {
    publications {
        myPulication(MavenPublication) {
            groupId 'com.example.project'
            version '1.0.2'
            artifactId 'myProject'
            artifact("$buildDir/outputs/aar/myProject.aar")
        }
    }
}

```

You will also need to define your repository url

```java
publishing{
    repositories {
        maven {
            url "http://www.myrepository.com"
        }
    }
}

```

Here is full library `build.gradle` file

```java
apply plugin: 'com.android.library'
apply plugin: 'maven-publish'

buildscript {
   ...
}
android {
    ...
}
publishing {
    publications {
        myPulication(MavenPublication) {
            groupId 'com.example.project'
            version '1.0.2'
            artifactId 'myProject'
            artifact("$buildDir/outputs/aar/myProject.aar")
        }
    }
    repositories {
        maven {
            url "http://www.myrepository.com"
        }
    }
}

```

For publishing you can run gradle console command

> 
gradle publish


or you can run from gradle tasks panel

[<img src="https://i.stack.imgur.com/G76PM.png" alt="gradle tasks panel" />](https://i.stack.imgur.com/G76PM.png)

