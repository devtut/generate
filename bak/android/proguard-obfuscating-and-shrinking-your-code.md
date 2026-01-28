---
metaTitle: "Android - ProGuard - Obfuscating and Shrinking your code"
description: "Rules for some of the widely used Libraries, Remove trace logging (and other) statements at build time, Protecting your code from hackers, Enable ProGuard for your build, Enabling ProGuard with a custom obfuscation configuration file"
---

# ProGuard - Obfuscating and Shrinking your code




## Rules for some of the widely used Libraries


Currently it contains rules for following libraries:-

1. ButterKnife
1. RxJava
1. Android Support Library
1. Android Design Support Library
1. Retrofit
1. Gson and Jackson
1. Otto
1. Crashlitycs
1. Picasso
1. Volley
1. OkHttp3
1. Parcelable

```java
#Butterknife
-keep class butterknife.** { *; }
-keepnames class * { @butterknife.Bind *;}

-dontwarn butterknife.internal.**
-keep class **$$ViewBinder { *; }

-keepclasseswithmembernames class * {
    @butterknife.* <fields>;
}

-keepclasseswithmembernames class * {
    @butterknife.* <methods>;
}

# rxjava
-keep class rx.schedulers.Schedulers {
    public static <methods>;
}
-keep class rx.schedulers.ImmediateScheduler {
    public <methods>;
}
-keep class rx.schedulers.TestScheduler {
    public <methods>;
}
-keep class rx.schedulers.Schedulers {
    public static ** test();
}
-keepclassmembers class rx.internal.util.unsafe.*ArrayQueue*Field* {
    long producerIndex;
    long consumerIndex;
}
-keepclassmembers class rx.internal.util.unsafe.BaseLinkedQueueProducerNodeRef {
    long producerNode;
    long consumerNode;
}

# Support library
-dontwarn android.support.**
-dontwarn android.support.v4.**
-keep class android.support.v4.** { *; }
-keep interface android.support.v4.** { *; }
-dontwarn android.support.v7.**
-keep class android.support.v7.** { *; }
-keep interface android.support.v7.** { *; }

# support design
-dontwarn android.support.design.**
-keep class android.support.design.** { *; }
-keep interface android.support.design.** { *; }
-keep public class android.support.design.R$* { *; }

# retrofit
-dontwarn okio.**
-keepattributes Signature
-keepattributes *Annotation*
-keep class com.squareup.okhttp.** { *; }
-keep interface com.squareup.okhttp.** { *; }
-dontwarn com.squareup.okhttp.**

-dontwarn rx.**
-dontwarn retrofit.**
-keep class retrofit.** { *; }
-keepclasseswithmembers class * {
    @retrofit.http.* <methods>;
}

-keep class sun.misc.Unsafe { *; }
#your package path where your gson models are stored
-keep class com.abc.model.** { *; }

# Keep these for GSON and Jackson
-keepattributes Signature
-keepattributes *Annotation*
-keepattributes EnclosingMethod
-keep class sun.misc.Unsafe { *; }
-keep class com.google.gson.** { *; }
  
#keep otto
-keepattributes *Annotation*
-keepclassmembers class ** {
    @com.squareup.otto.Subscribe public *;
    @com.squareup.otto.Produce public *;
}

# Crashlitycs 2.+
-keep class com.crashlytics.** { *; }
-keep class com.crashlytics.android.**
-keepattributes SourceFile, LineNumberTable, *Annotation*
# If you are using custom exceptions, add this line so that custom exception types are skipped during obfuscation:
-keep public class * extends java.lang.Exception
# For Fabric to properly de-obfuscate your crash reports, you need to remove this line from your ProGuard config:
# -printmapping mapping.txt

# Picasso
-dontwarn com.squareup.okhttp.** 

# Volley
-keep class com.android.volley.toolbox.ImageLoader { *; }

# OkHttp3
-keep class okhttp3.** { *; }
-keep interface okhttp3.** { *; }
-dontwarn okhttp3.**

# Needed for Parcelable/SafeParcelable Creators to not get stripped
-keepnames class * implements android.os.Parcelable {
    public static final ** CREATOR;
}

```



## Remove trace logging (and other) statements at build time


If you want to remove calls to certain methods, assuming they return void and have no side affects (as in, calling them doesn't change any system values, reference arguments, statics, etc.) then you can have ProGuard remove them from the output after the build is complete.

For example, I find this useful in removing debug/verbose logging statements useful in debugging, but generating the strings for them is unnecessary in production.

```java
# Remove the debug and verbose level Logging statements.
# That means the code to generate the arguments to these methods will also not be called.
# ONLY WORKS IF -dontoptimize IS _NOT_ USED in any ProGuard configs
-assumenosideeffects class android.util.Log {
    public static *** d(...);
    public static *** v(...);
}

```

Note: If `-dontoptimize` is used in any ProGuard config so that it is not minifying/removing unused code, then this will not strip out the statements. (But who would not want to remove unused code, right?)

Note2: this call will remove the call to log, but will not protect you code. The Strings will actually remain in the generated apk. Read more in [this post](http://stackoverflow.com/documentation/android/4500/proguard-obfuscating-and-shrinking-your-code/16840/protecting-your-code-from-hackers#t=201608091625190003433).



## Protecting your code from hackers


Obfuscation is often considered as a magic solution for code protection, by  making your code harder to understand if it ever gets de-compiled by hackers.

But if you're thinking that removing the `Log.x(..)` actually removes the information the hackers need, you'll have a nasty surprise.

Removing all your log calls with:

```java
-assumenosideeffects class android.util.Log {
    public static *** d(...);
    ...etc
}

```

will indeed remove the Log call itself, but usually **not** the Strings you put into them.<br>

If for example inside your log call you type a common log message such as: `Log.d(MyTag,"Score="+score);`, the compiler converts the `+` to a 'new StringBuilder()' outside the Log call. ProGuard doesn't change this new object. <br>

Your de-compiled code will still have a hanging `StringBuilder` for `"Score="`, appended with the obfuscated version for `score` variable (let's say it was converted to `b`).<br>
Now the hacker knows what is `b`, and make sense of your code.

A good practice to actually remove these residuals from your code is either not put them there in the first place (Use String formatter instead, with proguard rules to remove them), or to wrap your `Log` calls with:

```

   if (BuildConfig.DEBUG) {
        Log.d(TAG,".."+var);
    }

```

**Tip:**

Test how well protected your obfuscated code is by de-compiling it yourself!

1. [dex2jar](https://github.com/pxb1988/dex2jar) - converts the apk to jar
1. [jd](http://jd.benow.ca/) - decompiles the jar and opens it in a gui editor



## Enable ProGuard for your build


For enabling `ProGuard` configurations for your application you need to enable it in your module level gradle file. you need to set the value of `minifyEnabled true`.

You can also enable `shrinkResources true` which will remove resources that `ProGuard` flaggs as unused.

```java
buildTypes {
        release {
            minifyEnabled true
            shrinkResources true
            proguardFiles getDefaultProguardFile('proguard-android.txt'), 'proguard-rules.pro'
        }
    }

```

The above code will apply your ProGuard configurations contained in `proguard-rules.pro` ("proguard-project.txt" in Eclipse) to your released apk.

To enable you to later determine the line on which an exception occurred in a stack trace, "proguard-rules.pro" should contain following lines:

```java
-renamesourcefileattribute SourceFile    
-keepattributes SourceFile,LineNumberTable

```

To enable Proguard in Eclipse add `proguard.config=${sdk.dir}/tools/proguard/proguard-android.txt:proguard-project.txt` to "project.properties"



## Enabling ProGuard with a custom obfuscation configuration file


ProGuard allows the developer to obfuscate, shrink and optimize his code.

**#1 The first step of the procedure is to enable proguard on the build**.

This can be done by **setting the 'minifyEnabled' command to true** on your desired build

**#2 The second step is to specify which proguard files are we using for the given build**

This can be done by **setting the 'proguardFiles' line with the proper filenames**

```java
buildTypes {
    debug {
        minifyEnabled false
    }
    testRelease {
        minifyEnabled true
        proguardFiles getDefaultProguardFile('proguard-android-optimize.txt'), 'proguard-rules-tests.pro'
    }
    productionRelease {
        minifyEnabled true
        proguardFiles getDefaultProguardFile('proguard-android-optimize.txt'), 'proguard-rules-tests.pro', 'proguard-rules-release.pro'
    }
}

```

**#3 The developer can then edit his proguard file with the rules he desires.**

That can be done by editting the file (for example 'proguard-rules-tests.pro') and adding the desired constraints. **The following file serves as an example proguard file**

```java
// default & basic optimization configurations
-optimizationpasses 5
-dontpreverify
-repackageclasses ''
-allowaccessmodification
-optimizations !code/simplification/arithmetic
-keepattributes *Annotation*

-verbose

-dump obfuscation/class_files.txt
-printseeds obfuscation/seeds.txt
-printusage obfuscation/unused.txt // unused classes that are stripped out in the process
-printmapping obfuscation/mapping.txt // mapping file that shows the obfuscated names of the classes after proguad is applied

// the developer can specify keywords for the obfuscation (I myself use fruits for obfuscation names once in a while :-) )
-obfuscationdictionary obfuscation/keywords.txt
-classobfuscationdictionary obfuscation/keywords.txt
-packageobfuscationdictionary obfuscation/keywords.txt

```

Finally, whenever the developer runs and/or generates his new .APK file, the custom proguard configurations will be applied thus fulfilling the requirements.

