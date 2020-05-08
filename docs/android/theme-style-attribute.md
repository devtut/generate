---
metaTitle: "Android - Theme, Style, Attribute"
description: "Define primary, primary dark, and accent colors, Navigation Bar Color (API 21+), Multiple Themes in one App, Use Custom Theme Per Activity, Light Status Bar (API 23+), Use Custom Theme Globally, Overscroll Color (API 21+), Ripple Color (API 21+), Translucent Navigation and Status Bars (API 19+), Theme inheritance"
---

# Theme, Style, Attribute




## Define primary, primary dark, and accent colors


You can customize your [theme’s color palette](https://developer.android.com/training/material/theme.html?#ColorPalette).

Using **framework** APIs

```java
<style name="AppTheme" parent="Theme.Material">
    <item name="android:colorPrimary">@color/primary</item>
    <item name="android:colorPrimaryDark">@color/primary_dark</item>
    <item name="android:colorAccent">@color/accent</item>
</style>

```

Using the **Appcompat support library** (and `AppCompatActivity`)

```java
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="colorPrimary">@color/primary</item>
    <item name="colorPrimaryDark">@color/primary_dark</item>
    <item name="colorAccent">@color/accent</item>
</style>

```



## Navigation Bar Color (API 21+)


This attribute is used to change the navigation bar (one, that contain Back, Home Recent button). Usually it is black, however it's color can be changed.

```java
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="android:navigationBarColor">@color/my_color</item>
</style>

```



## Multiple Themes in one App


Using more than one theme in your Android application, you can add custom colors to every theme, to be like this:

[<img src="https://i.stack.imgur.com/JSaApm.png" alt="OneTheme" />](https://i.stack.imgur.com/JSaAp.png)
[<img src="https://i.stack.imgur.com/GqYq1m.png" alt="TwoTheme" />](https://i.stack.imgur.com/GqYq1.png)

First, we have to add our themes to `style.xml` like this:

```java
<style name="OneTheme" parent="Theme.AppCompat.Light.DarkActionBar">

</style>

<!--  -->
<style name="TwoTheme" parent="Theme.AppCompat.Light.DarkActionBar" >

</style>
......

```

Above you can see **OneTheme** and **TwoTheme**.

Now, go to your `AndroidManifest.xml` and add this line:
`android:theme="@style/OneTheme"` to your **application** tag, this will make  **OneTheme** the default theme:

```java
<application
        android:theme="@style/OneTheme"
        ...>

```

Create new xml file named `attrs.xml` and add this code :

```java
<?xml version="1.0" encoding="utf-8"?>
<resources>
    <attr name="custom_red" format="color" />
    <attr name="custom_blue" format="color" />
    <attr name="custom_green" format="color" />
</resources>
<!-- add all colors you need (just color's name) -->

```

Go back to `style.xml` and add these colors with its values for each theme :

```java
<style name="OneTheme" parent="Theme.AppCompat.Light.DarkActionBar">
    <item name="custom_red">#8b030c</item>
    <item name="custom_blue">#0f1b8b</item>
    <item name="custom_green">#1c7806</item>
</style>

<style name="TwoTheme" parent="Theme.AppCompat.Light.DarkActionBar" >
    <item name="custom_red">#ff606b</item>
    <item name="custom_blue">#99cfff</item>
    <item name="custom_green">#62e642</item>
</style>

```

Now you have custom colors for each theme, let's add these color to our views.

Add **custom_blue** color to the TextView by using "?attr/" :

Go to your imageView and add this color :

```java
<TextView>
    android:id="@+id/txte_view"
    android:textColor="?attr/custom_blue" />

```

Mow we can change the theme just by single line `setTheme(R.style.TwoTheme);` this line must be before `setContentView()` method in `onCreate()` method, like this `Activity.java` :

```java
@Override
protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setTheme(R.style.TwoTheme);
    setContentView(R.layout.main_activity);
    ....
}

```

### change theme for all activities at once

If we want to change the theme for all activities, we have to create new class named MyActivity extends `AppCompatActivity` class (or `Activity` class) and add line `setTheme(R.style.TwoTheme);` to **onCreate()** method:

```java
public class MyActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (new MySettings(this).isDarkTheme())
            setTheme(R.style.TwoTheme);
    }
}

```

Finally, go to all your activities add make all of them extend the **MyActivity** base class:

```java
public class MainActivity extends MyActivity {
    ....
}

```

In order to change the theme, just go to **MyActivity** and change `R.style.TwoTheme` to your theme (`R.style.OneTheme` , `R.style.ThreeTheme` ....).



## Use Custom Theme Per Activity


In themes.xml:

```java
<style name="MyActivityTheme" parent="Theme.AppCompat">
    <!-- Theme attributes here -->
</style>

```

In AndroidManifest.xml:

```java
<application
    android:icon="@mipmap/ic_launcher"
    android:label="@string/app_name"
    android:theme="@style/Theme.AppCompat">
    
    <activity
        android:name=".MyActivity"
        android:theme="@style/MyActivityTheme" />

</application>

```



## Light Status Bar (API 23+)


This attribute can change the background of the Status Bar icons (at the top of the screen) to white.

```java
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="android:windowLightStatusBar">true</item>
</style>

```



## Use Custom Theme Globally


In themes.xml:

```java
<style name="AppTheme" parent="Theme.AppCompat">
    <!-- Theme attributes here -->
</style>

```

In AndroidManifest.xml:

```java
<application
    android:icon="@mipmap/ic_launcher"
    android:label="@string/app_name"
    android:theme="@style/AppTheme">
    
    <!-- Activity declarations here -->

</application>

```



## Overscroll Color (API 21+)


```java
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="android:colorEdgeEffect">@color/my_color</item>
</style>

```



## Ripple Color (API 21+)


The [ripple](http://stackoverflow.com/documentation/android/124/material-design-for-all-android-versions/14200/rippledrawable#t=201609190854016182166) animation is shown when user presses clickable views.

You can use the same ripple color used by your app assigning the  `?android:colorControlHighlight` in your views. You can customize this color by changing the `android:colorControlHighlight` attribute in your theme:

This effect color can be changed:

```java
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="android:colorControlHighlight">@color/my_color</item>
</style>

```

Or, if you are using a Material Theme:

```java
<style name="AppTheme" parent="android:Theme.Material.Light">
    <item name="android:colorControlHighlight">@color/your_custom_color</item>
</style>

```



## Translucent Navigation and Status Bars (API 19+)


The navigation bar (at the bottom of the screen) can be transparent. Here is the way to achieve it.

```java
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="android:windowTranslucentNavigation">true</item>
</style>

```

The Status Bar (top of the screen) can be made transparent, by applying this attribute to the style:

```java
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="android:windowTranslucentStatus">true</item>
</style>

```



## Theme inheritance


When defining themes, one usually uses the theme provided by the system, and then changes modifies the look to fit his own application. For example, this is how the `Theme.AppCompat` theme is inherited:

```java
<style name="AppTheme" parent="Theme.AppCompat">
    <item name="colorPrimary">@color/colorPrimary</item>
    <item name="colorPrimaryDark">@color/colorPrimaryDark</item>
    <item name="colorAccent">@color/colorAccent</item>
</style>

```

This theme now has all the properties of the standard `Theme.AppCompat` theme, except the ones we explicitly changed.

There is also a shortcut when inheriting, usually used when one inherits from his own theme:

```java
<style name="AppTheme.Red">
    <item name="colorAccent">@color/red</item>
</style>

```

Since it already has `AppTheme.` in the start of it's name, it automatically inherits it, without needing to define the `parent` theme. This is useful when you need to create specific styles for a part (for example, a single Activity) of your app.

