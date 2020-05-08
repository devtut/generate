---
metaTitle: "Android - Creating Overlay (always-on-top) Windows"
description: "Popup overlay, Granting SYSTEM_ALERT_WINDOW Permission on android 6.0 and above"
---

# Creating Overlay (always-on-top) Windows



## Popup overlay


In order to put your view on top of every application, you have to assign your view to the corresponding window manager. For that you need the system alert permission, which can be requested by adding the following line to your manifest file:

```java
<uses-permission android:name="android.permission.SYSTEM_ALERT_WINDOW" />

```

**Note:** If your application gets destroyed, your view will be removed from the window manager. Therefore, it is better to create the view and assign it to the window manager by a foreground service.

### Assigning a view to the WindowManager

You can retrieve a window manager instance as follows:

```java
WindowManager mWindowManager = (WindowManager) mContext.getSystemService(Context.WINDOW_SERVICE);

```

In order to define the position of your view, you have to create some layout parameters as follows:

```java
WindowManager.LayoutParams mLayoutParams = new WindowManager.LayoutParams(
        ViewGroup.LayoutParams.MATCH_PARENT,
        ViewGroup.LayoutParams.MATCH_PARENT,
        WindowManager.LayoutParams.TYPE_PHONE,
        WindowManager.LayoutParams.FLAG_TURN_SCREEN_ON,
        PixelFormat.TRANSLUCENT);
mLayoutParams.gravity = Gravity.CENTER_HORIZONTAL | Gravity.CENTER_VERTICAL;

```

Now, you can assign your view together with the created layout parameters to the window manager instance as follows:

```java
mWindowManager.addView(yourView, mLayoutParams);

```

Voila! Your view has been successfully placed on top of all other applications.

**Note:** You view will not be put on top of the keyguard.



## Granting SYSTEM_ALERT_WINDOW Permission on android 6.0 and above


From android 6.0 this permission needs to grant dynamically,

```java
<uses-permission android:name="android.permission.SYSTEM_ALERT_WINDOW"/>

```

Throwing below permission denied error on 6.0,

```java
Caused by: android.view.WindowManager$BadTokenException: Unable to add window android.view.ViewRootImpl$W@86fb55b -- permission denied for this window type

```

Solution :-

Requesting Overlay permission as below,

```java
if(!Settings.canDrawOverlays(this)){
    // ask for setting 
     Intent intent = new Intent(Settings.ACTION_MANAGE_OVERLAY_PERMISSION,
     Uri.parse("package:" + getPackageName()));
     startActivityForResult(intent, REQUEST_OVERLAY_PERMISSION);
}

```

Check for the result,

```java
@Override
protected void onActivityResult(int requestCode, int resultCode, Intent data) {
    if (requestCode == REQUEST_OVERLAY_PERMISSION) {
        if (Settings.canDrawOverlays(this)) {
            // permission granted...
        }else{
            // permission not granted...
        }
    }
}

```

