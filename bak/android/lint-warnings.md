---
metaTitle: "Android - Lint Warnings"
description: "Using tools:ignore in xml files, Configure LintOptions with gradle, Configuring lint checking in Java and XML source files, Importing resources without Deprecated error, How to configure the lint.xml file, Mark Suppress Warnings "
---

# Lint Warnings



## Using tools:ignore in xml files


The attribute `tools:ignore` can be used in xml files to dismiss lint warnings.

**BUT dismissing lint warnings with this technique is most of the time the wrong way to proceed.**

A lint warning must be understood and fixed... it can be ignored if and only if you have a full understanding of it's meaning and a strong reason to ignore it.

Here is a use case where it legitimate to ignore a lint warning:

- You are developing a system-app (signed with the device manufacturer key)
- Your app need to change the device date (or any other protected action)

Then you can do this in your manifest : (i.e. requesting the protected permission and ignoring the lint warning because you know that in your case the permission will be granted)

```java
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
      xmlns:tools="http://schemas.android.com/tools"
      ...>
    <uses-permission android:name="android.permission.SET_TIME"
        tools:ignore="ProtectedPermissions"/>

```



## Configure LintOptions with gradle


You can configure lint by adding a `lintOptions` section in the `build.gradle` file:

```java
android {

    //.....

    lintOptions {
        // turn off checking the given issue id's
        disable 'TypographyFractions','TypographyQuotes'

        // turn on the given issue id's
        enable 'RtlHardcoded','RtlCompat', 'RtlEnabled'

        // check *only* the given issue id's
        check 'NewApi', 'InlinedApi'

       // set to true to turn off analysis progress reporting by lint
       quiet true
 
       // if true, stop the gradle build if errors are found
       abortOnError false
   
       // if true, only report errors
       ignoreWarnings true
    }
}

```

You can run lint for a specific variant (see below), e.g. `./gradlew lintRelease`, or for all variants (`./gradlew lint`), in which case it produces a report which describes which specific variants a given issue applies to.

Check here for the [DSL reference for all available options](http://google.github.io/android-gradle-dsl/current/com.android.build.gradle.internal.dsl.LintOptions.html#com.android.build.gradle.internal.dsl.LintOptions).



## Configuring lint checking in Java and XML source files


You can disable Lint checking from your Java and XML source files.

### Configuring lint checking in Java

To disable Lint checking specifically for a **Java class** or method in your Android project, add the `@SuppressLint` **annotation** to that Java code.

Example:

```java
@SuppressLint("NewApi")
@Override
public void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.main);

```

To disable checking for all Lint issues:

```java
@SuppressLint("all")

```

### Configuring lint checking in XML

You can use the `tools:ignore` attribute to disable Lint checking for specific sections of your **XML files**.

For example:

```java
tools:ignore="NewApi,StringFormatInvalid"

```

To suppress checking for all Lint issues in the XML element, use

```java
tools:ignore="all"

```



## Importing resources without "Deprecated" error


Using the Android API 23 or higher, very often such situation can be seen:

[<img src="http://i.stack.imgur.com/kvH7C.png" alt="enter image description here" />](http://i.stack.imgur.com/kvH7C.png)

This situation is caused by the structural change of the Android API regarding getting the resources.
Now the function:

```java
public int getColor(@ColorRes int id, @Nullable Theme theme) throws NotFoundException    

```

should be used. But the android.support.v4 library has another solution.

Add the following dependency to the build.gradle file:

```java
com.android.support:support-v4:24.0.0

```

Then all methods from support library are available:

```java
ContextCompat.getColor(context, R.color.colorPrimaryDark);
ContextCompat.getDrawable(context, R.drawable.btn_check);
ContextCompat.getColorStateList(context, R.color.colorPrimary);
DrawableCompat.setTint(drawable);
ContextCompat.getColor(context,R.color.colorPrimaryDark));

```

Moreover more methods from support library can be used:

```java
ViewCompat.setElevation(textView, 1F);
ViewCompat.animate(textView);
TextViewCompat.setTextAppearance(textView, R.style.AppThemeTextStyle);
...

```



## How to configure the lint.xml file


You can specify your Lint checking preferences in the `lint.xml` file. If you are creating this file manually, place it in the root directory of your Android project. If you are configuring Lint preferences in Android Studio, the lint.xml file is automatically created and added to your Android project for you.

Example:

```java
<?xml version="1.0" encoding="UTF-8"?>
    <lint>
        <!-- list of issues to configure -->
</lint>

```

By setting the severity attribute value in the  tag, you can disable Lint checking for an issue or change the severity level for an issue.

The following example shows the contents of a `lint.xml` file.

```java
<?xml version="1.0" encoding="UTF-8"?>
<lint>
    <!-- Disable the given check in this project -->
    <issue id="IconMissingDensityFolder" severity="ignore" />

    <!-- Ignore the ObsoleteLayoutParam issue in the specified files -->
    <issue id="ObsoleteLayoutParam">
        <ignore path="res/layout/activation.xml" />
        <ignore path="res/layout-xlarge/activation.xml" />
    </issue>

    <!-- Ignore the UselessLeaf issue in the specified file -->
    <issue id="UselessLeaf">
        <ignore path="res/layout/main.xml" />
    </issue>

    <!-- Change the severity of hardcoded strings to "error" -->
    <issue id="HardcodedText" severity="error" />
</lint>

```



## Mark Suppress Warnings 


It's good practice to mark some warnings in your code. For example, some deprecated methods is need for your testing, or old support version. But Lint checking will mark that code with warnings. For avoiding this problem, you need use annotation @SuppressWarnings.

For example, add ignoring to warnings to deprecated methods. You need to put warnings description in annotation also:

```java
@SuppressWarnings("deprecated");
public void setAnotherColor (int newColor) {
    getApplicationContext().getResources().getColor(newColor)
}

```

Using this annotation you can ignore all warnings, including Lint, Android, and other. Using Suppress Warnings, helps to understand code correctly!



#### Remarks


The **Lint tool** checks your Android project source files for potential bugs and optimization improvements for correctness, security, performance, usability, accessibility, and internationalization. You can run Lint from the command-line or from Android Studio.

### Official documentation:

[https://developer.android.com/studio/write/lint.html](https://developer.android.com/studio/write/lint.html)

