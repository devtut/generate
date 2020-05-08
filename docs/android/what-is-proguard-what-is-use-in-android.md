---
metaTitle: "Android - What is ProGuard? What is use in Android?"
description: "Shrink your code and resources with proguard"
---

# What is ProGuard? What is use in Android?


Proguard is free Java class file shrinker, optimizer, obfuscator, and preverifier. It detects and removes unused classes, fields, methods, and attributes. It optimizes bytecode and removes unused instructions. It renames the remaining classes, fields, and methods using short meaningless names.



## Shrink your code and resources with proguard


To make your APK file as small as possible, you should enable shrinking to remove unused code and resources in your release build. This page describes how to do that and how to specify what code and resources to keep or discard during the build.

Code shrinking is available with ProGuard, which detects and removes unused classes, fields, methods, and attributes from your packaged app, including those from included code libraries (making it a valuable tool for working around the 64k reference limit). ProGuard also optimizes the bytecode, removes unused code instructions, and obfuscates the remaining classes, fields, and methods with short names. The obfuscated code makes your APK difficult to reverse engineer, which is especially valuable when your app uses security-sensitive features, such as licensing verification.

Resource shrinking is available with the Android plugin for Gradle, which removes unused resources from your packaged app, including unused resources in code libraries. It works in conjunction with code shrinking such that once unused code has been removed, any resources no longer referenced can be safely removed as well.

**Shrink Your Code**

To enable code shrinking with `ProGuard`, add `minifyEnabled` true to the appropriate build type in your `build.gradle` file.

Be aware that code shrinking slows down the build time, so you should avoid using it on your debug build if possible. However, it's important that you do enable code shrinking on your final    APK used for testing, because it might introduce bugs if you do not sufficiently customize which code to keep.

For example, the following snippet from a `build.gradle` file enables code shrinking for the release build:

```java
android {
    buildTypes {
        release {
            minifyEnabled true
            proguardFiles getDefaultProguardFile('proguard-android.txt'),
                    'proguard-rules.pro'
        }
    }
    ...
}

```

In addition to the `minifyEnabled` property, the `proguardFiles` property defines the `ProGuard rules`:

The getDefaultProguardFile('proguard-android.txt') method gets the default ProGuard settings from the Android SDK `tools/proguard/ folder`.
Tip: For even more code shrinking, try the `proguard-android-optimize.txt` file that's in the same location. It includes the same ProGuard rules, but with other optimizations that perform analysis at the bytecode level—inside and across methods—to reduce your APK size further and help it run faster.
The `proguard-rules.pro` file is where you can add custom ProGuard rules. By default, this file is located at the root of the module (next to the build.gradle file).
To add more ProGuard rules that are specific to each build variant, add another proguardFiles property in the corresponding `productFlavor` block. For example, the following Gradle file adds flavor2-rules.pro to the flavor2 product flavor. Now flavor2 uses all three ProGuard rules because those from the release block are also applied.

```java
android {
    ...
    buildTypes {
        release {
            minifyEnabled true
            proguardFiles getDefaultProguardFile('proguard-android.txt'),
                   'proguard-rules.pro'
        }
    }
    productFlavors {
        flavor1 {
        }
        flavor2 {
            proguardFile 'flavor2-rules.pro'
        }
    }
}

```

