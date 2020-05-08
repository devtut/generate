---
metaTitle: "Android - Sign your Android App for Release"
description: "Sign your App, Configure the build.gradle with signing configuration"
---

# Sign your Android App for Release


Android requires that all APKs be signed for release.



## Sign your App


<li>
In the menu bar, click Build > Generate Signed APK.
</li>
<li>
Select the module you would like to release from the drop down and click Next.
</li>
<li>
To Create a new keystore, click Create new. Now fill the required information and press ok in New Key Store.
</li>

[<img src="https://i.stack.imgur.com/LcUK6.png" alt="New Key Store" />](https://i.stack.imgur.com/LcUK6.png)

[<img src="https://i.stack.imgur.com/am4I5.png" alt="Generate Signed APK" />](https://i.stack.imgur.com/am4I5.png)

<li>
On the Generate Signed APK Wizard fields are already populated for you if you just created new key store otherwise fill it and click next.
</li>
<li>
<p>On the next window, select a destination for the signed APK, select the build type
and click finish.</p>
</li>



## Configure the build.gradle with signing configuration


You can define the  signing configuration to sign the apk in the `build.gradle` file.

You can define:

- `storeFile` : the keystore file
- `storePassword`: the keystore password
- `keyAlias`: a key alias name
- `keyPassword`: A key alias password

You have to **define** the `signingConfigs` block to create a signing configuration:

```java
android {
    signingConfigs {

        myConfig {
            storeFile file("myFile.keystore")
            storePassword "xxxx"
            keyAlias "xxxx"
            keyPassword "xxxx"
        }
    }
    //....
}

```

Then you can **assign** it to one or more build types.

```java
android {

    buildTypes {
        release {
            signingConfig signingConfigs.myConfig
        }
    }
}

```

