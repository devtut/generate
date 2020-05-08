---
metaTitle: "Android - Xposed"
description: "Creating a Xposed Module, Hooking a method"
---

# Xposed



## Creating a Xposed Module


****Xposed is a framework that allows you to hook method calls of other apps. When you do a modification by decompiling an APK, you can insert/change commands directly wherever you want. However, you will need to recompile/sign the APK afterwards and you can only distribute the whole package. With Xposed, you can inject your own code before or after methods, or replace whole methods completely. Unfortunately, you can only install Xposed on rooted devices. You should use Xposed whenever you want to manipulate the behavior of other apps or the core Android system and don't want to go through the hassle of decompiling, recompiling and signing APKs.****

First, you create a standard app without an Activity in Android Studio.

Then you have to include the following code in your **build.gradle**:

```java
repositories {
    jcenter();
}

```

After that you add the following dependencies:

```java
provided 'de.robv.android.xposed:api:82'
provided 'de.robv.android.xposed:api:82:sources'

```

Now you have to place these tags inside the  **application** tag found in the **AndroidManifest.xml** so Xposed recognizes your module:

```java
<meta-data
        android:name="xposedmodule"
        android:value="true" />
<meta-data
        android:name="xposeddescription"
        android:value="YOUR_MODULE_DESCRIPTION" />
<meta-data
        android:name="xposedminversion"
        android:value="82" />

```

NOTE: Always replace **82** with the [latest Xposed version](https://bintray.com/rovo89/de.robv.android.xposed/api).



## Hooking a method


Create a new class implementing `IXposedHookLoadPackage` and implement the `handleLoadPackage` method:

```java
public class MultiPatcher implements IXposedHookLoadPackage
{
    @Override
    public void handleLoadPackage(XC_LoadPackage.LoadPackageParam loadPackageParam) throws Throwable
    {
        
    }
}

```

Inside the method, you check `loadPackageParam.packageName` for the package name of the app you want to hook:

```java
@Override
public void handleLoadPackage(XC_LoadPackage.LoadPackageParam loadPackageParam) throws Throwable
{
    if (!loadPackageParam.packageName.equals("other.package.name"))
    {
        return;
    }
}

```

Now you can hook your method and either manipulate it before it's code is run, or after:

```java
@Override
public void handleLoadPackage(XC_LoadPackage.LoadPackageParam loadPackageParam) throws Throwable
{
    if (!loadPackageParam.packageName.equals("other.package.name"))
    {
        return;
    }

    XposedHelpers.findAndHookMethod(
        "other.package.name",
        loadPackageParam.classLoader,
        "otherMethodName",
        YourFirstParameter.class,
        YourSecondParameter.class,
        new XC_MethodHook()
    {
        @Override
        protected void beforeHookedMethod(MethodHookParam param) throws Throwable
        {
            Object[] args = param.args;
        
            args[0] = true;
            args[1] = "example string";
            args[2] = 1;

            Object thisObject = param.thisObject;

            // Do something with the instance of the class
        }

        @Override
        protected void afterHookedMethod(MethodHookParam param) throws Throwable
        {
            Object result = param.getResult();

            param.setResult(result + "example string");
        }
    });
}

```

