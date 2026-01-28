---
metaTitle: "Android - Internationalization and localization (I18N and L10N)"
description: "Planning for localization : enable RTL support in Manifest, Planning for localization : Add RTL support in Layouts, Planning for localization : Test layouts for RTL, Coding for Localization : Creating default strings and resources, Coding for localization : Providing alternative strings, Coding for localization : Providing alternate layouts"
---

# Internationalization and localization (I18N and L10N)


Internationalization (i18n) and Localization (L10n) are used to adapt software according to differences in languages, regional differences and target audience.

Internationalization : the process of planning for future localization i.e. making the software design flexible to an extent that it can adjust and adapt to future localization efforts.

Localization : the process of adapting the software to a particular region/country/market (locale).



## Planning for localization : enable RTL support in Manifest


RTL (Right-to-left) support is an essential part in planning for i18n and L10n. Unlike English language which is written from left to right, many languages like Arabic, Japanese, Hebrew, etc. are written from right to left. To appeal to a more global audience, it is a good idea to plan your layouts to support these language from the very beginning of the project, so that adding localization is easier later on.

RTL support can be enabled in an Android app by adding the `supportsRtl` tag in the `AndroidManifest`, like so :

```java
<application
    ...
    android:supportsRtl="true"
    ...>
...
</application>

```



## Planning for localization : Add RTL support in Layouts


Starting SDK 17 (Android 4.2), RTL support was added in Android layouts and is an essential part of localization. Going forward, the `left/right` notation in layouts should be replaced by `start/end` notation. If, however, your project has a `minSdk` value less than `17`, then both `left/right` and `start/end` notation should be used in layouts.

For relative layouts, `alignParentStart` and `alignParentEnd` should be used, like so:

```java
<RelativeLayout
    android:layout_width="match_parent"
    android:layout_height="match_parent">
    <TextView
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_alignParentTop="true"
        android:layout_alignParentLeft="true"
        android:layout_alignParentStart="true"/>
    <TextView
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_alignParentTop="true"
        android:layout_alignParentRight="true"
        android:layout_alignParentEnd="true"/>
</RelativeLayout>

```

For specifying gravity and layout gravity, similar notation should be used, like so :

```

   <TextView
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_gravity="left|start"
        android:gravity="left|start"/>
    <TextView
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:layout_gravity="right|end"
        android:gravity="right|end"/>

```

Paddings and margins should also be specified accordingly, like so :

```java
<include layout="@layout/notification"
    android:layout_width="fill_parent"
    android:layout_height="wrap_content"
    android:layout_marginLeft="12dp"
    android:layout_marginStart="12dp"
    android:paddingLeft="128dp"
    android:paddingStart="128dp"
    android:layout_toLeftOf="@id/cancel_action"
    android:layout_toStartOf="@id/cancel_action"/>
<include layout="@layout/notification2"
    android:layout_width="fill_parent"
    android:layout_height="wrap_content"
    android:layout_marginRight="12dp"
    android:layout_marginEnd="12dp"
    android:paddingRight="128dp"
    android:paddingEnd="128dp"
    android:layout_toRightOf="@id/cancel_action"
    android:layout_toEndOf="@id/cancel_action"/>

```



## Planning for localization : Test layouts for RTL


To test if the layouts that have been created are RTL compatible, do the following :

> 
Go to Settings -> Developer options -> Drawing -> Force RTL layout direction


Enabling this option would force the device to use RTL locales and you can easily verify all parts of the app for RTL support. Note that you don't need to actually add any new locales/ language support up till this point.



## Coding for Localization : Creating default strings and resources


The first step for coding for localization is to create default resources. This step is so implicit that many developers do not even think about it. However, creating default resources is important because if the device runs on an unsupported locale, it would load all of its resources from the default folders. If even one of the resources is missing from the default folders, the app would simply crash.

The default set of strings should be put in the following folder at the specified location:

```java
res/values/strings.xml 

```

This file should contain the strings in the language that majority users of the app are expected to speak.

Also, default resources for the app should be placed at the following folders and locations :

```java
res/drawable/
res/layout/

```

If your app requires folders like `anim`, or `xml`, the default resources should be added to the following folders and locations:

```java
res/anim/
res/xml/
res/raw/

```



## Coding for localization : Providing alternative strings


To provide translations in other languages (locales), we need to create a `strings.xml` in a separate folder by the following convention :

```java
res/values-<locale>/strings.xml

```

An example for the same is given below:

[<img src="https://i.stack.imgur.com/UeQ7U.png" alt="Example for locales" />](https://i.stack.imgur.com/UeQ7U.png)

In this example, we have default English strings in the file `res/values/strings.xml`, French translations are provided in the folder `res/values-fr/strings.xml` and Japanese translations are provided in the folder `res/values-ja/strings.xml`

Other translations for other locales can similarly be added to the app.

> 
A complete list of locale codes can be found here : [ISO 639 codes](http://www.loc.gov/standards/iso639-2/php/code_list.php)


**Non-translatable Strings:**

Your project may have certain strings that are not to be translated. Strings which are used as keys for SharedPreferences or strings which are used as symbols, fall in this category. These strings should be stored only in the default `strings.xml` and should be marked with a `translatable="false"` attribute. e.g.

```java
<string name="pref_widget_display_label_hot">Hot News</string>
<string name="pref_widget_display_key" translatable="false">widget_display</string>
<string name="pref_widget_display_hot" translatable="false">0</string>

```

This attribute is important because translations are often carried out by professionals who are bilingual. This would allow these persons involved in translations to identify strings which are not to be translated, thus saving time and money.



## Coding for localization : Providing alternate layouts


Creating language specific layouts is often unnecessary if you have specified the correct `start/end` notation, as described in the earlier example. However, there may be situations where the defaults layouts may not work correctly for certain languages. Sometimes, left-to-right layouts may not translate for RTL languages. It is necessary to provide the correct layouts in such cases.

To provide complete optimization for RTL layouts, we can use entirely separate layout files using the `ldrtl` resource qualifier (`ldrtl` stands for layout-direction-right-to-left}). For example, we can save your default layout files in `res/layout/` and our RTL optimized layouts in `res/layout-ldrtl/`.

The `ldrtl` qualifier is great for drawable resources, so that you can provide graphics that are oriented in the direction corresponding to the reading direction.

Here is a great post which describes the precedence of the `ldrtl` layouts : [Language specific layouts](http://stackoverflow.com/a/18628543/783707)



#### Remarks


To test a device for localization, the device or the emulator can be rebooted in a particular locale by using `adb` as follows :

1. Run adb using the command : `adb shell`
1. Run the following command at the adb command prompt : `setprop persist.sys.locale [BCP-47 language tag];stop;sleep 5;start` where [BCP-47 language tag] is the language specific code as described here : [BCP47 codes](https://tools.ietf.org/html/bcp47)

e.g. to check Japanese localization in the app, use the command : `setprop persist.sys.locale ja-JP;stop;sleep 5;start`

