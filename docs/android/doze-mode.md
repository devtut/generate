---
metaTitle: "Android - Doze Mode"
description: "Whitelisting an Android application programmatically, Exclude app from using doze mode"
---

# Doze Mode



## Whitelisting an Android application programmatically


Whitelisting won't disable the doze mode for your app, but you can do that by using network and hold-wake locks.

Whitelisting an Android application programmatically can be done as follows:

```java
boolean isIgnoringBatteryOptimizations = pm.isIgnoringBatteryOptimizations(getPackageName());
if(!isIgnoringBatteryOptimizations){
    Intent intent = new Intent();
    intent.setAction(Settings.ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS);
    intent.setData(Uri.parse("package:" + getPackageName()));
    startActivityForResult(intent, MY_IGNORE_OPTIMIZATION_REQUEST);
}

```

The result of starting the activity above can be verfied by the following code:

```java
@Override
protected void onActivityResult(int requestCode, int resultCode, Intent data) {
    if (requestCode == MY_IGNORE_OPTIMIZATION_REQUEST) {
        PowerManager pm = (PowerManager)getSystemService(Context.POWER_SERVICE);
        boolean isIgnoringBatteryOptimizations = pm.isIgnoringBatteryOptimizations(getPackageName());
        if(isIgnoringBatteryOptimizations){
            // Ignoring battery optimization
        }else{
           // Not ignoring battery optimization
        }
    }
}

```



## Exclude app from using doze mode


1. Open phone's settings
1. open battery
1. open menu and select "battery optimization"
1. from the dropdown menu select "all apps"
1. select the app you want to whitelist
1. select "don't optimize"

Now this app will show under not optimized apps.

An app can check whether it's whitelisted by calling `isIgnoringBatteryOptimizations()`



#### Remarks


Doze Mode is a set of changes and rules that put your phone to sleep when idle.

On Android 6.0 Marshmallow:
Doze mode gets activated after a while the screen is off, the device is stationary and it's running on battery.
[<img src="https://i.stack.imgur.com/ye77o.png" alt="Doze Mode on Marshmallow" />](https://i.stack.imgur.com/ye77o.png)
As you can see in the diagram above, when Doze Mode gets activated, the device doesn't get any wakelocks, network access, jobs/syncs, Alarms, GPS/Wi-fi scans.

On Android 7.0 Nougat:
Imagine if your phone is on your pocket (the screen is off, it's running on battery, but it's not stationary) you might want to get the Doze Mode features as well, right?
So that's why Google announced the Extended Doze Mode: It runs when the screen is off, but not stationary.
[<img src="https://i.stack.imgur.com/AjCkv.png" alt="Doze Mode on Nougat" />](https://i.stack.imgur.com/AjCkv.png)
As you can see in this diagram, only Network Access and jobs/syncs are disabled.
Note that the Extended Doze doesn't replace the first Doze Mode. They work together, depending on the phone state (stationary or not).
Here are the distinctions:
[<img src="https://i.stack.imgur.com/PuM06.png" alt="Doze Mode distinctions" />](https://i.stack.imgur.com/PuM06.png)
Developers should be aware that:

- Doze might keep temporary wakelock and network access for High-priority GCM (Google Cloud Messaging) messages (for cases wwhere the user needs an immediate notification);
- Foreground services (such as a music playback) will continue to work.

You can find more information here: [https://developer.android.com/training/monitoring-device-state/doze-standby.html](https://developer.android.com/training/monitoring-device-state/doze-standby.html)

