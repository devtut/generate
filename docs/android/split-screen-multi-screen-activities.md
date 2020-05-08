---
metaTitle: "Android - Split Screen / Multi-Screen Activities"
description: "Split Screen introduced in Android Nougat implemented."
---

# Split Screen / Multi-Screen Activities



## Split Screen introduced in Android Nougat implemented.


Set this attribute in your manifest's  or  element to enable or disable multi-window display:

```java
android:resizeableActivity=["true" | "false"]

```

If this attribute is set to true, the activity can be launched in split-screen and freeform modes. If the attribute is set to false, the activity does not support multi-window mode. If this value is false, and the user attempts to launch the activity in multi-window mode, the activity takes over the full screen.

If your app targets API level 24, but you do not specify a value for this attribute, the attribute's value defaults to true.

The following code shows how to specify an activity's default size and location, and its minimum size, when the activity is displayed in freeform mode:

```java
<--These are default values suggested by google.-->
<activity android:name=".MyActivity">
<layout android:defaultHeight="500dp"
      android:defaultWidth="600dp"
      android:gravity="top|end"
      android:minHeight="450dp"
      android:minWidth="300dp" />
</activity>

```

**Disabled features in multi-window mode**

Certain features are disabled or ignored when a device is in multi-window mode, because they don’t make sense for an activity which may be sharing the device screen with other activities or apps. Such features include:

<li>
Some System UI customization options are disabled; for example, apps cannot hide the status bar if they are not running in full-screen mode.
</li>
<li>
The system ignores changes to the **android:screenOrientation** attribute.
</li>

**If your app targets API level 23 or lower**

If your app targets API level 23 or lower and the user attempts to use the app in multi-window mode, the system forcibly resizes the app unless the app declares a fixed orientation.

If your app does not declare a fixed orientation, you should launch your app on a device running Android 7.0 or higher and attempt to put the app in split-screen mode. Verify that the user experience is acceptable when the app is forcibly resized.

If the app declares a fixed orientation, you should attempt to put the app in multi-window mode. Verify that when you do so, the app remains in full-screen mode.

