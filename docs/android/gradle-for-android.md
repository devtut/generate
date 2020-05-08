---
metaTitle: "Android - Gradle for Android"
description: "A basic build.gradle file, Define and use Build Configuration Fields, Centralizing dependencies via dependencies.gradle file, Sign APK without exposing keystore password, Adding product flavor-specific dependencies, Specifying different application IDs for build types and product flavors, Versioning your builds via version.properties file, Defining product flavors, Adding product flavor-specific resources, Changing output apk name and add version name:, Directory structure for flavor-specific resources, Why are there two build.gradle files in an Android Studio project?, Enable Proguard using gradle, Enable experimental NDK plugin support for Gradle and AndroidStudio, Ignoring build variant, Seeing dependency tree, Display signing information, Executing a shell script from gradle, Disable image compression for a smaller APK file size, Show all gradle project tasks, Delete unaligned apk automatically , Debugging your Gradle errors, Use gradle.properties for central versionnumber/buildconfigurations, Defining build types"
---

# Gradle for Android


Gradle is a JVM-based build system that enables developers to write high-level scripts that can be used to automate the process of compilation and application production. It is a flexible plugin-based system, which allows you to automate various aspects of the build process; including compiling and signing a `.jar`, downloading and managing external dependencies, injecting fields into the `AndroidManifest` or utilising specific SDK versions.



## A basic build.gradle file


This is an example of a default `build.gradle` file in a module.

```java
apply plugin: 'com.android.application'

android {
    compileSdkVersion 25
    buildToolsVersion '25.0.3'

    signingConfigs {
        applicationName {
            keyAlias 'applicationName'
            keyPassword 'password'
            storeFile file('../key/applicationName.jks')
            storePassword 'keystorePassword'
        }
    }
    defaultConfig {
        applicationId 'com.company.applicationName'
        minSdkVersion 14
        targetSdkVersion 25
        versionCode 1
        versionName '1.0'
        signingConfig signingConfigs.applicationName
    }
    buildTypes {
        release {
            minifyEnabled true
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
        }
    }
}

dependencies {
    compile fileTree(dir: 'libs', include: ['*.jar'])

    compile 'com.android.support:appcompat-v7:25.3.1'
    compile 'com.android.support:design:25.3.1'

    testCompile 'junit:junit:4.12'
}

```

### DSL (domain-specific language)

Each block in the file above is called a `DSL` (domain-specific language).

### Plugins

The first line, `apply plugin: 'com.android.application'`, applies the [Android plugin for Gradle](http://stackoverflow.com/documentation/android-gradle/2092/introduction-to-android-gradle/19904/android-plugin-for-gradle#t=201609150649318227543) to the build and makes the `android {}` block available to declare Android-specific build options.

For an **Android Application**:

```java
apply plugin: 'com.android.application'

```

For an **Android Library**:

```java
apply plugin: 'com.android.library'

```

### [Understanding the DSLs](http://stackoverflow.com/documentation/android-gradle/2161/configure-your-build-with-gradle/7077/the-module-file-example#t=201608060824289248387) in the sample above

The second part, The `android {...}` block, is the Android `DSL` which contains information about your project.

For example, you can set the `compileSdkVersion` which specifies the Android API level , Which should be used by Gradle to compile your app.<br />
The sub-block `defaultConfig` holds the defaults for your manifest. You can `override` them with [Product Flavors](http://stackoverflow.com/documentation/android-gradle/2929/configure-product-flavors#t=201609150655525420888).

You can find more info in these examples:

- [DSL for the app module](http://stackoverflow.com/documentation/android-gradle/2161/configure-your-build-with-gradle/7077/the-module-file-example#t=201609150656268962378)
- [Build Types](http://stackoverflow.com/documentation/android-gradle/3281/configure-build-types#t=201609150657045925473)
- [Product Flavors](http://stackoverflow.com/documentation/android-gradle/2929/configure-product-flavors#t=201609150656541535557)
- [Signing settings](http://stackoverflow.com/documentation/android-gradle/5249/configure-signing-settings#t=201609150657547602413)

### [Dependencies](http://stackoverflow.com/documentation/android-gradle/3289/declare-dependencies#t=201608040616412368066)

The `dependencies` block is defined outside the `android` block `{...}` : This means it's not defined by the Android plugin but it's standard Gradle.<br />
The `dependencies` block specifies what external libraries (typically Android libraries, but Java libraries are also valid) you wish to include in your app. Gradle will automatically download these dependencies for you (if there is no local copy available), you just need to add similar `compile` lines when you wish to add another library.

Let's look at one of the lines present here:

> 
`compile 'com.android.support:design:25.3.1'`


This line basically says

> 
add a dependency on the Android support design library to my project.


Gradle will ensure that the library is downloaded and present so that you can use it in your app, and its code will also be included in your app.

If you're familiar with Maven, this syntax is the **GroupId**, a colon, **ArtifactId**, another colon, then the version of the dependency you wish to include, giving you full control over versioning.

While it is possible to specify artifact versions using the plus (+) sign, best practice is to avoid doing so; it can lead to issues if the library gets updated with breaking changes without your knowledge, which would likely lead to crashes in your app.

You can add different kind of dependencies:

- [local binary dependencies](http://stackoverflow.com/documentation/android-gradle/3289/declare-dependencies/11291/local-binary-dependencies#t=201609150702149292729)
- [module dependencies](http://stackoverflow.com/documentation/android-gradle/3289/declare-dependencies/11290/module-dependencies#t=201609150702254302343)
- [remote dependencies](http://stackoverflow.com/documentation/android-gradle/3289/declare-dependencies/11292/remote-binary-dependencies#t=201609150702366671434)

A particular attention should be dedicated to the [aar flat dependencies](http://stackoverflow.com/documentation/android-gradle/3037/how-to-include-aar-files-in-a-project-in-android#t=201609150703402541119).

You can find more details in [this topic.](http://stackoverflow.com/documentation/android-gradle/3289/declare-dependencies#t=201609150704148588267)

Note about the **-v7 in **appcompat-v7****

> 
`compile 'com.android.support:appcompat-v7:25.3.1'`


This simply means that this **library** (`appcompat`) is compatible with the Android API level 7 and forward.

Note about the **junit:junit:4.12**

This is Testing dependency for Unit testing.

### Specifying dependencies specific to different build configurations

You can specify that a dependency should only be used for a certain [build configuration](http://stackoverflow.com/documentation/android-gradle/3289/declare-dependencies/17861/declare-dependencies-for-configurations#t=201609150706071099895) or you can define different dependencies for the [build types](http://stackoverflow.com/documentation/android-gradle/3289/declare-dependencies/22988/declare-dependencies-for-build-types#t=201609150709405530227) or the [product flavors](http://stackoverflow.com/documentation/android-gradle/3289/declare-dependencies/18698/declare-dependencies-for-flavors#t=201609150706431102381) (e.g., debug, test or release) by using `debugCompile`, `testCompile` or `releaseCompile` instead of the usual `compile`.

This is helpful for keeping test- and debug- related dependencies out of your release build, which will keep your release `APK` as slim as possible and help to ensure that any debug information cannot be used to obtain internal information about your app.

### [signingConfig](http://stackoverflow.com/documentation/android-gradle/5249/configure-signing-settings#t=201608060822323313637)

The `signingConfig` allows you to configure your Gradle to include `keystore` information and ensure that the APK built using these configurations are signed and ready for Play Store release.

Here you can find a [dedicated topic](http://stackoverflow.com/documentation/android-gradle/5249/configure-signing-settings#t=201609150711373069035).

**Note**: It's not recommended though to keep the signing credentials inside your Gradle file. To remove the signing configurations, just omit the `signingConfigs` portion.<br />
You can specify them in different ways:

- storing in an [external file](http://stackoverflow.com/documentation/android-gradle/5249/configure-signing-settings/18657/define-the-signing-configuration-in-an-external-file#t=201609150713390867222)
- storing them in [setting environment variables](http://stackoverflow.com/documentation/android-gradle/5249/configure-signing-settings/18658/define-the-signing-configuration-setting-environment-variables#t=201609150713570980288).

See this topic for more details : [Sign APK without exposing keystore password](http://stackoverflow.com/documentation/android/95/gradle-for-android/7078/sign-apk-without-exposing-keystore-password#t=201608090310148888637).

**You can find further information about Gradle for Android in the [dedicated Gradle topic](http://stackoverflow.com/documentation/android-gradle/2161/configure-your-build-with-gradle#t=201608060745337157221)**.



## Define and use Build Configuration Fields


### **BuildConfigField**

Gradle allows `buildConfigField` lines to define constants. These constants will be accessible at runtime as static fields of the `BuildConfig` class. This can be used to create [flavors](http://stackoverflow.com/documentation/android/95/gradle-for-android/401/defining-product-flavors#t=20160804061141663974) by defining all fields within the `defaultConfig` block, then overriding them for individual build flavors as needed.

This example defines the build date and flags the build for production rather than test:

```java
android {
    ...
    defaultConfig {
        ...
        // defining the build date
        buildConfigField "long", "BUILD_DATE", System.currentTimeMillis() + "L"
        // define whether this build is a production build
        buildConfigField "boolean", "IS_PRODUCTION", "false"
        // note that to define a string you need to escape it
        buildConfigField "String", "API_KEY", "\"my_api_key\""
    }

    productFlavors {
        prod {
            // override the productive flag for the flavor "prod"
            buildConfigField "boolean", "IS_PRODUCTION", "true"
            resValue 'string', 'app_name', 'My App Name'
        }
        dev {
            // inherit default fields
            resValue 'string', 'app_name', 'My App Name - Dev'
        }
    }
}

```

The automatically-generated <package_name>.`BuildConfig`.java in the gen folder contains the following fields based on the directive above:

```java
public class BuildConfig {
    // ... other generated fields ...
    public static final long BUILD_DATE = 1469504547000L;
    public static final boolean IS_PRODUCTION = false;
    public static final String API_KEY = "my_api_key";
}

```

The defined fields can now be used within the app at runtime by accessing the generated `BuildConfig` class:

```java
public void example() {
    // format the build date
    SimpleDateFormat dateFormat = new SimpleDateFormat("yyyy/MM/dd");
    String buildDate = dateFormat.format(new Date(BuildConfig.BUILD_DATE));
    Log.d("build date", buildDate);
    
    // do something depending whether this is a productive build
    if (BuildConfig.IS_PRODUCTION) {
        connectToProductionApiEndpoint();
    } else {
        connectToStagingApiEndpoint();
    }
}

```

### ResValue

The `resValue` in the `productFlavors` creates a resource value. It can be any type of resource (`string`, `dimen`, `color`, etc.). This is similar to defining a resource in the appropriate file: e.g. defining string in a `strings.xml` file. The advantage being that the one defined in gradle can be modified based on your productFlavor/buildVariant. To access the value, write the same code as if you were accessing a res from the resources file:

```java
getResources().getString(R.string.app_name)

```

The important thing is that resources defined this way cannot modify existing resources defined in files. They can only create new resource values.

Some libraries (such as the Google Maps Android API) require an API key provided in the Manifest as a `meta-data` tag. If different keys are needed for debugging and production builds, specify a manifest placeholder filled in by Gradle.

In your `AndroidManifest.xml` file:

```java
<meta-data
    android:name="com.google.android.geo.API_KEY"
    android:value="${MAPS_API_KEY}"/>

```

And then set the field accordingly in your `build.gradle` file:

```java
android {
    defaultConfig {
        ...
        // Your development key
        manifestPlaceholders = [ MAPS_API_KEY: "AIza..." ]
    }

    productFlavors {
        prod {
            // Your production key
            manifestPlaceholders = [ MAPS_API_KEY: "AIza..." ]
        }
    }
}

```

The Android build system generates a number of fields automatically and places them in `BuildConfig.java`. These fields are:

|Field|Description
|---|---|---|---|---|---|---|---|---|---
|`DEBUG`|a `Boolean` stating if the app is in debug or release mode
|`APPLICATION_ID`|a `String` containing the ID of the application (e.g. `com.example.app`)
|`BUILD_TYPE`|a `String` containing the build type of the application (usually either `debug` or `release`)
|`FLAVOR`|a `String` containing the particular flavor of the build
|`VERSION_CODE`|an `int` containing the version (build) number.<br/>This is the same as `versionCode` in `build.gradle` or `versionCode` in `AndroidManifest.xml`
|`VERSION_NAME`|a `String` containing the version (build) name.<br/>This is the same as `versionName` in `build.gradle` or `versionName` in `AndroidManifest.xml`

In addition to the above, if you have defined multiple dimensions of flavor then each dimension will have its own value.  For example, if you had two dimensions of flavor for `color` and `size` you will also have the following variables:

|Field|Description
|---|---|---|---|---|---|---|---|---|---
|`FLAVOR_color`|a `String` containing the value for the 'color' flavor.
|`FLAVOR_size`|a `String` containing the value for the 'size' flavor.



## Centralizing dependencies via "dependencies.gradle" file


When working with multi-module projects, it is helpful to centralize dependencies in a single location rather than having them spread across many build files, especially for common libraries such as the Android support libraries and the [Firebase libraries](http://stackoverflow.com/documentation/firebase/816/introduction-to-firebase#t=201608060827057525928).

One recommended way is to separate the Gradle build files, with one `build.gradle` per module, as well as one in the project root and another one for the dependencies, for example:

```java
root
  +- gradleScript/
  |     dependencies.gradle
  +- module1/
  |     build.gradle
  +- module2/
  |     build.gradle
  +- build.gradle

```

Then, all of your dependencies can be located in `gradleScript/dependencies.gradle`:

```java
ext {
    // Version
    supportVersion = '24.1.0'

    // Support Libraries dependencies
    supportDependencies = [
            design:            "com.android.support:design:${supportVersion}",
            recyclerView:      "com.android.support:recyclerview-v7:${supportVersion}",
            cardView:          "com.android.support:cardview-v7:${supportVersion}",
            appCompat:         "com.android.support:appcompat-v7:${supportVersion}",
            supportAnnotation: "com.android.support:support-annotations:${supportVersion}",
    ]

    firebaseVersion = '9.2.0';

    firebaseDependencies = [
            core:         "com.google.firebase:firebase-core:${firebaseVersion}",
            database:     "com.google.firebase:firebase-database:${firebaseVersion}",
            storage:      "com.google.firebase:firebase-storage:${firebaseVersion}",
            crash:        "com.google.firebase:firebase-crash:${firebaseVersion}",
            auth:         "com.google.firebase:firebase-auth:${firebaseVersion}",
            messaging:    "com.google.firebase:firebase-messaging:${firebaseVersion}",
            remoteConfig: "com.google.firebase:firebase-config:${firebaseVersion}",
            invites:      "com.google.firebase:firebase-invites:${firebaseVersion}",
            adMod:        "com.google.firebase:firebase-ads:${firebaseVersion}",
            appIndexing:  "com.google.android.gms:play-services-appindexing:${firebaseVersion}",
    ];
}

```

Which can then be applied from that file in the top level file `build.gradle` like so:

```java
// Load dependencies
apply from: 'gradleScript/dependencies.gradle'

```

and in the `module1/build.gradle` like so:

```java
// Module build file
dependencies {
    // ...
    compile supportDependencies.appCompat
    compile supportDependencies.design
    compile firebaseDependencies.crash
}

```

### **Another approach**

A less verbose approach for centralizing library dependencies versions can be achieved by declaring the version number as a variable once, and using it everywhere.

In the workspace root `build.gradle` add this:

```java
ext.v = [
    supportVersion:'24.1.1',
]

```

And in every module that uses the same library add the needed libraries

```java
compile "com.android.support:support-v4:${v.supportVersion}"
compile "com.android.support:recyclerview-v7:${v.supportVersion}"
compile "com.android.support:design:${v.supportVersion}"
compile "com.android.support:support-annotations:${v.supportVersion}"

```



## Sign APK without exposing keystore password


You can define the signing configuration to sign the apk in the `build.gradle` file
using these properties:

- `storeFile` : the keystore file
- `storePassword`: the keystore password
- `keyAlias`: a key alias name
- `keyPassword`: A key alias password

In many case you may need to avoid this kind of info in the `build.gradle` file.

### Method A: Configure release signing using a keystore.properties file

It's possible to configure your app's `build.gradle` so that it will read your signing configuration information from a properties file like `keystore.properties`.

Setting up signing like this is beneficial because:

- Your signing configuration information is separate from your `build.gradle` file
- You do not have to intervene during the signing process in order to provide passwords for your keystore file
- You can easily exclude the `keystore.properties` file from version control

First, create a file called `keystore.properties` in the root of your project with content like this (replacing the values with your own):

```java
storeFile=keystore.jks
storePassword=storePassword
keyAlias=keyAlias
keyPassword=keyPassword


```

Now, in your app's `build.gradle` file, set up the `signingConfigs` block as follows:

```java
android {
...

    signingConfigs {
        release {
            def propsFile = rootProject.file('keystore.properties')
            if (propsFile.exists()) {
                def props = new Properties()
                props.load(new FileInputStream(propsFile))
                storeFile = file(props['storeFile'])
                storePassword = props['storePassword']
                keyAlias = props['keyAlias']
                keyPassword = props['keyPassword']
            }
        }
    }
}

```

That's really all there is to it, **but don't forget to exclude both your keystore file and your `keystore.properties` file from version control**.

A couple of things to note:

- The `storeFile` path specified in the `keystore.properties` file should be relative to your app's `build.gradle` file. This example assumes that the keystore file is in the same directory as the app's `build.gradle` file.
- This example has the `keystore.properties` file in the root of the project. If you put it somewhere else, be sure to change the value in `rootProject.file('keystore.properties')` to the location of yours, relative to the root of your project.

### Method B: By using an environment variable

The same can be achieved also without a properties file, making the password harder to find:

```java
android {

  signingConfigs {
    release {
        storeFile file('/your/keystore/location/key')
        keyAlias 'your_alias'
        String ps = System.getenv("ps")
        if (ps == null) {
             throw new GradleException('missing ps env variable')
        }
        keyPassword ps
        storePassword ps
    }
}

```

The `"ps"` environment variable can be global, but a safer approach can be by adding it to the shell of Android Studio only.
<br/>In linux this can be done by editing Android Studio's `Desktop Entry`

```java
Exec=sh -c "export ps=myPassword123 ; /path/to/studio.sh"

```

You can find more details in [this topic](http://stackoverflow.com/documentation/android-gradle/5249/configure-signing-settings#t=201608060810223146528).



## Adding product flavor-specific dependencies


Dependencies can be added for a specific [product flavor](http://stackoverflow.com/documentation/android-gradle/2929/configure-product-flavors#t=201609161745387396696), similar to how they can be added for specific build configurations.

For this example, assume that we have already defined two product flavors called `free` and `paid` (more on defining [flavors here](http://stackoverflow.com/documentation/android-gradle/2929/configure-product-flavors/9932/how-to-configure-the-build-gradle-file#t=201609161746456086524)).<br />
We can then add the AdMob dependency for the `free` flavor, and the Picasso library for the `paid` one like so:

```java
android {
    ...

    productFlavors {
        free {
            applicationId "com.example.app.free"
            versionName "1.0-free"
        }
        paid {
            applicationId "com.example.app.paid"
            versionName "1.0-paid"
        }
    }
}

...
dependencies {
    ...
    // Add AdMob only for free flavor
    freeCompile 'com.android.support:appcompat-v7:23.1.1'
    freeCompile 'com.google.android.gms:play-services-ads:8.4.0'
    freeCompile 'com.android.support:support-v4:23.1.1'

    // Add picasso only for paid flavor
    paidCompile 'com.squareup.picasso:picasso:2.5.2'
} 
...

```



## Specifying different application IDs for build types and product flavors


You can specify different application IDs or package names for each `buildType` or [`productFlavor`](http://stackoverflow.com/documentation/android/95/gradle-for-android/401/defining-product-flavors#t=201608040613545973062) using the **applicationIdSuffix** configuration attribute:

Example of suffixing the `applicationId` for each `buildType`:

```java
defaultConfig {
    applicationId "com.package.android"
    minSdkVersion 17
    targetSdkVersion 23
    versionCode 1
    versionName "1.0"
}

buildTypes {
    release {
        debuggable false      
    }

    development {
        debuggable true
        applicationIdSuffix ".dev"
    }

    testing {
        debuggable true
        applicationIdSuffix ".qa"
    }
}

```

Our resulting `applicationIds` would now be:

- com.package.android for `release`
- com.package.android.**dev** for `development`
- com.package.android.**qa** for `testing`

This can be done for `productFlavors` as well:

```java
productFlavors {
    free {
        applicationIdSuffix ".free"
    }
    paid {
        applicationIdSuffix ".paid"
    }
}

```

The resulting `applicationIds` would be:

- com.package.android.**free** for the `free` flavor
- com.package.android.**paid** for the `paid` flavor



## Versioning your builds via "version.properties" file


You can use Gradle to auto-increment your package version each time you build it.  To do so create a `version.properties` file in the same directory as your `build.gradle` with the following contents:

```java
VERSION_MAJOR=0
VERSION_MINOR=1
VERSION_BUILD=1

```

(Changing the values for major and minor as you see fit).  Then in your `build.gradle` add the following code to the `android` section:

```java
// Read version information from local file and increment as appropriate
def versionPropsFile = file('version.properties')
if (versionPropsFile.canRead()) {
  def Properties versionProps = new Properties()

  versionProps.load(new FileInputStream(versionPropsFile))

  def versionMajor = versionProps['VERSION_MAJOR'].toInteger()
  def versionMinor = versionProps['VERSION_MINOR'].toInteger()
  def versionBuild = versionProps['VERSION_BUILD'].toInteger() + 1

  // Update the build number in the local file
  versionProps['VERSION_BUILD'] = versionBuild.toString()
  versionProps.store(versionPropsFile.newWriter(), null)

  defaultConfig {
    versionCode versionBuild
    versionName "${versionMajor}.${versionMinor}." + String.format("%05d", versionBuild)
  }
}

```

The information can be accessed in Java as a string `BuildConfig.VERSION_NAME` for the complete {major}.{minor}.{build} number and as an integer `BuildConfig.VERSION_CODE` for just the build number.



## Defining product flavors


[Product flavors](http://stackoverflow.com/documentation/android-gradle/2929/configure-product-flavors#t=201608060748315130296) are defined in the `build.gradle` file inside the `android { ... }` block as seen below.

```java
...
android {
    ...
    productFlavors {
        free {
            applicationId "com.example.app.free"
            versionName "1.0-free"
        }
        paid {
            applicationId "com.example.app.paid"
            versionName "1.0-paid"
        }
    }
}

```

By doing this, we now have two additional product flavors: `free` and `paid`. Each can have its own specific configuration and attributes. For example, both of our new flavors has a separate `applicationId` and `versionName` than our existing `main` flavor (available by default, so not shown here).



## Adding product flavor-specific resources


Resources can be added for a specific [product flavor](http://stackoverflow.com/documentation/android/95/gradle-for-android/401/defining-product-flavors#t=201608040613153911086).

For this example, assume that we have already defined two product flavors called `free` and `paid`. In order to add product flavor-specific resources, we create additional resource folders alongside the `main/res` folder, which we can then add resources to like usual. For this example, we'll define a string, `status`, for each product flavor:

**/src/**main**/res/values/strings.xml**

```java
<resources>
    <string name="status">Default</string>
</resources>

```

**/src/**free**/res/values/strings.xml**

```java
<resources>
    <string name="status">Free</string>
</resources>

```

**/src/**paid**/res/values/strings.xml**

```java
<resources>
    <string name="status">Paid</string>
</resources>

```

The product flavor-specific `status` strings will override the value for `status` in the `main` flavor.



## Changing output apk name and add version name:


This is the code for changing output application file name (.apk). The name can be configured by assigning a different value to `newName`

```java
android {

    applicationVariants.all { variant ->
        def newName = "ApkName";
        variant.outputs.each { output ->
            def apk = output.outputFile;

            newName += "-v" + defaultConfig.versionName;
            if (variant.buildType.name == "release") {
                newName += "-release.apk";
            } else {
                newName += ".apk";
            }
            if (!output.zipAlign) {
                newName = newName.replace(".apk", "-unaligned.apk");
            }

            output.outputFile = new File(apk.parentFile, newName);
            logger.info("INFO: Set outputFile to " 
                        + output.outputFile 
                        + " for [" + output.name + "]");
        }
    }
}

```



## Directory structure for flavor-specific resources


Different flavors of application builds can contain different resources.  To create a flavor-specific resource make a directory with the lower-case name of your flavor in the `src` directory and add your resources in the same way you would normally.

For example, if you had a flavour `Development` and wanted to provide a distinct launcher icon for it you would create a directory `src/development/res/drawable-mdpi` and inside that directory create an `ic_launcher.png` file with your development-specific icon.

The directory structure will look like this:

```java
src/
  main/
    res/
      drawable-mdpi/
        ic_launcher.png  <-- the default launcher icon
  development/
    res/
      drawable-mdpi/
        ic_launcher.png  <-- the launcher icon used when the product flavor is 'Development'

```

(Of course, in this case you would also create icons for drawable-hdpi, drawable-xhdpi **etc**).



## Why are there two build.gradle files in an Android Studio project?


`<PROJECT_ROOT>\app\build.gradle` is specific for **app module**.

`<PROJECT_ROOT>\build.gradle` is a **"Top-level build file"** where you can add configuration options common to all sub-projects/modules.

If you use another module in your project, as a local library you would have another `build.gradle` file:
`<PROJECT_ROOT>\module\build.gradle`

In the top level file you can specify common properties as the buildscript block or some common properties.

```java
buildscript {
    repositories {
        mavenCentral()
    }

    dependencies {
       classpath 'com.android.tools.build:gradle:2.2.0'
       classpath 'com.google.gms:google-services:3.0.0'
    }
}

ext {
    compileSdkVersion = 23
    buildToolsVersion = "23.0.1"
}

```

In the `app\build.gradle` you define only the properties for the module:

```java
apply plugin: 'com.android.application'


android {
    compileSdkVersion rootProject.ext.compileSdkVersion
    buildToolsVersion rootProject.ext.buildToolsVersion
}

dependencies {
    //.....
}

```



## Enable Proguard using gradle


For enabling Proguard configurations for your application you need to enable it in your module-level gradle file. You need to set the value of `minifyEnabled` to `true`.

```java
buildTypes {
        release {
            minifyEnabled true
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
        }
    }

```

The above code will apply your Proguard configurations contained in the default Android SDK combined with the "proguard-rules.pro" file on your module to your released apk.



## Enable experimental NDK plugin support for Gradle and AndroidStudio


Enable and configure the experimental Gradle plugin to improve AndroidStudio's NDK support. Check that you fulfill the following requirements:

- Gradle 2.10 (for this example)
- Android NDK r10 or later
- Android SDK with build tools v19.0.0 or later

### Configure MyApp/build.gradle file

Edit the dependencies.classpath line in build.gradle from e.g.

```java
classpath 'com.android.tools.build:gradle:2.1.2'

```

to

```java
classpath 'com.android.tools.build:gradle-experimental:0.7.2'

```

(v0.7.2 was the latest version at the time of writing. Check the latest version yourself and adapt your line accordingly)

The build.gradle file should look similar to this:

```java
buildscript {
    repositories {
        jcenter()
    }
    dependencies {
        classpath 'com.android.tools.build:gradle-experimental:0.7.2'
    }
}

allprojects {
    repositories {
        jcenter()
    }
}

task clean(type: Delete) {
    delete rootProject.buildDir
}

```

### Configure MyApp/app/build.gradle file

Edit the build.gradle file to look similar to the following example. Your version numbers may look different.

```java
apply plugin: 'com.android.model.application'

model {
    android {
        compileSdkVersion 19
        buildToolsVersion "24.0.1"

        defaultConfig {
            applicationId "com.example.mydomain.myapp"
            minSdkVersion.apiLevel 19
            targetSdkVersion.apiLevel 19
            versionCode 1
            versionName "1.0"
        }
        buildTypes {
            release {
                minifyEnabled false
                proguardFiles.add(file('proguard-android.txt'))
            }
        }
        ndk {
            moduleName "myLib"
            
            /* The following lines are examples of a some optional flags that 
               you may set to configure your build environment
            */ 
            cppFlags.add("-I${file("path/to/my/includes/dir")}".toString())
            cppFlags.add("-std=c++11")
            ldLibs.addAll(['log', 'm'])
            stl = "c++_static"
            abiFilters.add("armeabi-v7a")
        }
    }
}

dependencies {
    compile fileTree(dir: 'libs', include: ['*.jar'])
}

```

Sync and check that there are no errors in the Gradle files before proceeding.

### Test if plugin is enabled

First make sure you have downloaded the Android NDK module. Then create an new app in AndroidStudio and add the following to the ActivityMain file:

```java
public class MainActivity implements Activity {
    onCreate() {
        // Pregenerated code. Not important here
    }
    static {
        System.loadLibrary("myLib");
    }
    public static native String getString();
}

```

The `getString()` part should be highlighted red saying that the corresponding JNI function could not be found. Hover your mouse over the function call until a red lightbulb appears. Click the bulb and select `create function JNI_...`. This should generate a myLib.c file in the myApp/app/src/main/jni directory with the correct JNI function call. It should look similar to this:

```java
#include <jni.h>

JNIEXPORT jstring JNICALL 
Java_com_example_mydomain_myapp_MainActivity_getString(JNIEnv *env, jobject instance)     
{
    // TODO

    return (*env)->NewStringUTF(env, returnValue);
}

```

If it doesn't look like this, then the plugin has not correctly been configured or the NDK has not been downloaded



## Ignoring build variant


For some reasons you may want to ignore your build variants. For example: you have 'mock' product flavour and you use it only for debug purposes, such as unit/instrumentation tests.

Let's ignore **mockRelease** variant from our project. Open **build.gradle** file and write:

```

   // Remove mockRelease as it's not needed.
    android.variantFilter { variant ->
        if (variant.buildType.name.equals('release') && variant.getFlavors().get(0).name.equals('mock')) {
            variant.setIgnore(true);
        }
    }

```



## Seeing dependency tree


Use the task dependencies. Depending on how your modules are set up, it may be either `./gradlew dependencies` or to see the dependencies of module app use `./gradlew :app:dependencies`

The example following build.gradle file

```java
dependencies {
    compile 'com.android.support:design:23.2.1'
    compile 'com.android.support:cardview-v7:23.1.1'

    compile 'com.google.android.gms:play-services:6.5.87'
}

```

will produce the following graph:

```java
Parallel execution is an incubating feature.
:app:dependencies

------------------------------------------------------------
Project :app
------------------------------------------------------------
. . .
_releaseApk - ## Internal use, do not manually configure ##
+--- com.android.support:design:23.2.1
|    +--- com.android.support:support-v4:23.2.1
|    |    \--- com.android.support:support-annotations:23.2.1
|    +--- com.android.support:appcompat-v7:23.2.1
|    |    +--- com.android.support:support-v4:23.2.1 (*)
|    |    +--- com.android.support:animated-vector-drawable:23.2.1
|    |    |    \--- com.android.support:support-vector-drawable:23.2.1
|    |    |         \--- com.android.support:support-v4:23.2.1 (*)
|    |    \--- com.android.support:support-vector-drawable:23.2.1 (*)
|    \--- com.android.support:recyclerview-v7:23.2.1
|         +--- com.android.support:support-v4:23.2.1 (*)
|         \--- com.android.support:support-annotations:23.2.1
+--- com.android.support:cardview-v7:23.1.1
\--- com.google.android.gms:play-services:6.5.87
     \--- com.android.support:support-v4:21.0.0 -> 23.2.1 (*)

. . .

```

Here you can see the project is directly including `com.android.support:design` version 23.2.1, which itself is bringing `com.android.support:support-v4` with version 23.2.1. However, `com.google.android.gms:play-services` itself has a dependency on the same `support-v4` but with an older version 21.0.0, which is a conflict detected by gradle.

`(*)` are used when gradle skips the subtree because those dependencies were already listed previously.



## Display signing information


In some circumstances (for example obtaining a Google API key) you need to find your keystore fingerprint. Gradle has a convenient task that display all the signing information, including keystore fingerprints:

```java
./gradlew signingReport

```

This is a sample output:

```java
:app:signingReport
Variant: release
Config: none
----------
Variant: debug
Config: debug
Store: /Users/user/.android/debug.keystore
Alias: AndroidDebugKey
MD5: 25:08:76:A9:7C:0C:19:35:99:02:7B:00:AA:1E:49:CA
SHA1: 26:BE:89:58:00:8C:5A:7D:A3:A9:D3:60:4A:30:53:7A:3D:4E:05:55
Valid until: Saturday 18 June 2044
----------
Variant: debugAndroidTest
Config: debug
Store: /Users/user/.android/debug.keystore
Alias: AndroidDebugKey
MD5: 25:08:76:A9:7C:0C:19:35:99:02:7B:00:AA:1E:49:CA
SHA1: 26:BE:89:58:00:8C:5A:7D:A3:A9:D3:60:4A:30:53:7A:3D:4E:05:55
Valid until: Saturday 18 June 2044
----------
Variant: debugUnitTest
Config: debug
Store: /Users/user/.android/debug.keystore
Alias: AndroidDebugKey
MD5: 25:08:76:A9:7C:0C:19:35:99:02:7B:00:AA:1E:49:CA
SHA1: 26:BE:89:58:00:8C:5A:7D:A3:A9:D3:60:4A:30:53:7A:3D:4E:05:55
Valid until: Saturday 18 June 2044
----------
Variant: releaseUnitTest
Config: none
----------

```



## Executing a shell script from gradle


A shell script is a very versatile way to extend your build to basically anything you can think of.

As an exmaple, here is a simple script to compile protobuf files and add the result java files to the source directory for further compilation:

```java
def compilePb() {
    exec {
        // NOTICE: gradle will fail if there's an error in the protoc file...
        executable "../pbScript.sh"
    }
}

project.afterEvaluate {
    compilePb()
}

```

The 'pbScript.sh' shell script for this example, located in the project's root folder:

```java
#!/usr/bin/env bash
pp=/home/myself/my/proto

/usr/local/bin/protoc -I=$pp \
 --java_out=./src/main/java \
  --proto_path=$pp \
 $pp/my.proto \
 --proto_path=$pp \
 $pp/my_other.proto

```



## Disable image compression for a smaller APK file size


If you are optimizing all images manually, disable APT Cruncher for a smaller APK file size.

```java
android {
    
    aaptOptions {
        cruncherEnabled = false
    }
}

```



## Show all gradle project tasks


```java
gradlew tasks -- show all tasks

 

Android tasks
-------------
androidDependencies - Displays the Android dependencies of the project.
signingReport - Displays the signing info for each variant.
sourceSets - Prints out all the source sets defined in this project.

Build tasks
-----------
assemble - Assembles all variants of all applications and secondary packages.
assembleAndroidTest - Assembles all the Test applications.
assembleDebug - Assembles all Debug builds.
assembleRelease - Assembles all Release builds.
build - Assembles and tests this project.
buildDependents - Assembles and tests this project and all projects that depend on it.
buildNeeded - Assembles and tests this project and all projects it depends on.
classes - Assembles main classes.
clean - Deletes the build directory.
compileDebugAndroidTestSources
compileDebugSources
compileDebugUnitTestSources
compileReleaseSources
compileReleaseUnitTestSources
extractDebugAnnotations - Extracts Android annotations for the debug variant into the archive file
extractReleaseAnnotations - Extracts Android annotations for the release variant into the archive file
jar - Assembles a jar archive containing the main classes.
mockableAndroidJar - Creates a version of android.jar that's suitable for unit tests.
testClasses - Assembles test classes.

Build Setup tasks
-----------------
init - Initializes a new Gradle build. [incubating]
wrapper - Generates Gradle wrapper files. [incubating]

Documentation tasks
-------------------
javadoc - Generates Javadoc API documentation for the main source code.

Help tasks
----------
buildEnvironment - Displays all buildscript dependencies declared in root project 'LeitnerBoxPro'.
components - Displays the components produced by root project 'LeitnerBoxPro'. [incubating]
dependencies - Displays all dependencies declared in root project 'LeitnerBoxPro'.
dependencyInsight - Displays the insight into a specific dependency in root project 'LeitnerBoxPro'.
help - Displays a help message.
model - Displays the configuration model of root project 'LeitnerBoxPro'. [incubating]
projects - Displays the sub-projects of root project 'LeitnerBoxPro'.
properties - Displays the properties of root project 'LeitnerBoxPro'.
tasks - Displays the tasks runnable from root project 'LeitnerBoxPro' (some of the displayed tasks may belong to subprojects)
.

Install tasks
-------------
installDebug - Installs the Debug build.
installDebugAndroidTest - Installs the android (on device) tests for the Debug build.
uninstallAll - Uninstall all applications.
uninstallDebug - Uninstalls the Debug build.
uninstallDebugAndroidTest - Uninstalls the android (on device) tests for the Debug build.
uninstallRelease - Uninstalls the Release build.

Verification tasks
------------------
check - Runs all checks.
connectedAndroidTest - Installs and runs instrumentation tests for all flavors on connected devices.
connectedCheck - Runs all device checks on currently connected devices.
connectedDebugAndroidTest - Installs and runs the tests for debug on connected devices.
deviceAndroidTest - Installs and runs instrumentation tests using all Device Providers.
deviceCheck - Runs all device checks using Device Providers and Test Servers.
lint - Runs lint on all variants.
lintDebug - Runs lint on the Debug build.
lintRelease - Runs lint on the Release build.
test - Run unit tests for all variants.
testDebugUnitTest - Run unit tests for the debug build.
testReleaseUnitTest - Run unit tests for the release build.

Other tasks
-----------
assembleDefault
clean
jarDebugClasses
jarReleaseClasses
transformResourcesWithMergeJavaResForDebugUnitTest
transformResourcesWithMergeJavaResForReleaseUnitTest

```



## Delete "unaligned" apk automatically 


If you don't need automatically generated apk files with `unaligned` suffix (which you probably don't), you may add the following code to `build.gradle` file:

```java
// delete unaligned files
android.applicationVariants.all { variant ->
  variant.assemble.doLast {
    variant.outputs.each { output ->
        println "aligned " + output.outputFile
        println "unaligned " + output.packageApplication.outputFile

        File unaligned = output.packageApplication.outputFile;
        File aligned = output.outputFile
        if (!unaligned.getName().equalsIgnoreCase(aligned.getName())) {
            println "deleting " + unaligned.getName()
            unaligned.delete()
        }
    }
  }
}

```

**From [here](http://stackoverflow.com/a/30826071/4378400)**



## Debugging your Gradle errors


The following is an excerpt from [Gradle - What is a non-zero exit value and how do I fix it?](http://stackoverflow.com/questions/36698816/gradle-what-is-a-non-zero-exit-value-and-how-do-i-fix-it), see it for the full discussion.

Let's say you are developing an application and you get some Gradle error that appears that generally will look like so.

```java
:module:someTask FAILED
FAILURE: Build failed with an exception.
* What went wrong:
Execution failed for task ':module:someTask'.
> some message here...  finished with non-zero exit value X
* Try:
Run with --stacktrace option to get the stack trace. Run with --info or --debug option to get more log output.
BUILD FAILED
Total time: Y.ZZ secs

```

You search here on StackOverflow for your problem, and people say to clean and rebuild your project, or enable [MultiDex](http://developer.android.com/tools/building/multidex.html), and when you try that, it just isn't fixing the problem.

[There are ways to get more information](http://stackoverflow.com/questions/21674091/how-to-add-stacktrace-or-debug-option-when-building-android-studio-project), but the Gradle output itself should point at the actual error in the few lines above that message between :`module:someTask FAILED` and the last `:module:someOtherTask` that passed. Therefore, if you ask a question about your error, please edit your questions to include more context to the error.

So, you get a "non-zero exit value." Well, that number is a good indicator of what you should try to fix. Here are a few occur most frequently.

- `1` is a just a general error code and the error is likely in the Gradle output
- `2` seems to be related to overlapping dependencies or project misconfiguration.
- `3` seems to be from including too many dependencies, or a memory issue.

The general solutions for the above (after attempting a Clean and Rebuild of the project) are:

- `1` - Address the error that is mentioned. Generally, this is a compile-time error, meaning some piece of code in your project is not valid. This includes both XML and Java for an Android project.
- `2` & `3` - Many answers here tell you to enable [multidex](http://developer.android.com/tools/building/multidex.html). While it may fix the problem, it is most likely a workaround. If you don't understand why you are using it (see the link), you probably don't need it. General solutions involve cutting back your overuse of library dependencies (such as all of Google Play Services, when you only need to use one library, like Maps or Sign-In, for example).



## Use gradle.properties for central versionnumber/buildconfigurations


You can define central config info's in

- a separate gradle include file [Centralizing dependencies via "dependencies.gradle" file](http://stackoverflow.com/documentation/android/95/gradle-for-android/2911/centralizing-dependencies)
- a stand alone properties file [Versioning your builds via "version.properties" file](http://stackoverflow.com/documentation/android/95/gradle-for-android/7837/versioning-your-builds)

or do it with root `gradle.properties` file

the project structure

```java
root
  +- module1/
  |     build.gradle
  +- module2/
  |     build.gradle
  +- build.gradle
  +- gradle.properties

```

global setting for all submodules in gradle.properties

```java
# used for manifest
# todo increment for every release
appVersionCode=19
appVersionName=0.5.2.160726

# android tools settings
appCompileSdkVersion=23
appBuildToolsVersion=23.0.2

```

usage in a submodule

```java
apply plugin: 'com.android.application'
android {
    // appXXX are defined in gradle.properties
    compileSdkVersion = Integer.valueOf(appCompileSdkVersion)
    buildToolsVersion = appBuildToolsVersion

    defaultConfig {
        // appXXX are defined in gradle.properties
        versionCode = Long.valueOf(appVersionCode)
        versionName = appVersionName
    }
}

dependencies {
    ...
}

```

**Note:** If you want to publish your app in the F-Droid app store you have to use magic numbers in the gradle file because else f-droid robot cannot read current versionnumner to detect/verify version changes.



## Defining build types


You can create and configure [build types](http://stackoverflow.com/documentation/android-gradle/3281/configure-build-types#t=201608060748576379678) in the module-level `build.gradle` file inside the `android {}` block.



#### Syntax


<li>
`apply plugin`: The plugins which should been used normally just `'com.android.application'` or `'com.android.library'`.
</li>
<li>
`android`: The main configuration of your app
<ul>
- `compileSdkVersion`: The compile SDK version
- `buildToolsVersion`: The build tools version
<li>`defaultConfig`: The default settings which can been overwritten by flavors and build types
<ul>
- `applicationId`: The application id you use e.g. in the PlayStore mostly the same as your package name
- `minSdkVersion`: The minimal required SDK version
- `targetSdkVersion`: The SDK version you compile against (should be always the newst one)
- `versionCode`: The internal version number which needs to be bigger on each update
- `versionName`: The version number the user can see in the app details page

`dependencies`: The maven or local dependencies of your app

- `compile` a single dependency
- `testCompile`: a dependency for the unit or integration tests



#### Remarks


**See also**

- [The official gradle homepage](http://gradle.org/)
- [How to configure gradle builds](http://developer.android.com/tools/building/configuring-gradle.html)
- [The android plugin for gradle](http://developer.android.com/tools/building/plugin-for-gradle.html)
- [Android Gradle DSL](http://google.github.io/android-gradle-dsl/current/)

### Gradle for Android - Extended documentation:

There is another tag where you can find more topics and examples about the use of gradle in Android.<br />
[http://stackoverflow.com/documentation/android-gradle/topics](http://stackoverflow.com/documentation/android-gradle/topics)

