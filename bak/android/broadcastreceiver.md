---
metaTitle: "Android - BroadcastReceiver"
description: "Using LocalBroadcastManager, BroadcastReceiver Basics, Introduction to Broadcast receiver, Enabling and disabling a Broadcast Receiver programmatically, Sticky Broadcast, Using ordered broadcasts, Bluetooth Broadcast receiver, BroadcastReceiver to handle BOOT_COMPLETED events, Example of a LocalBroadcastManager, Communicate two activities through custom Broadcast receiver, Android stopped state"
---

# BroadcastReceiver


BroadcastReceiver (receiver) is an Android component which allows you to register for system or application events. All registered receivers for an event are notified by the Android runtime once this event happens.

for example, a broadcast announcing that the screen has turned off, the battery is low, or a picture was captured.

Applications can also initiate broadcasts—for example, to let other applications know that some data has been downloaded to the device and is available for them to use.



## Using LocalBroadcastManager


**LocalBroadcastManager** is used to send Broadcast [Intents](http://stackoverflow.com/documentation/android/103/intent) within an application, without exposing them to unwanted listeners.

Using **LocalBroadcastManager** is more efficient and safer than using `context.sendBroadcast()` directly, because you don't need to worry about any broadcasts faked by other Applications, which may pose a security hazard.

Here is a simple example of sending and receiving local broadcasts:

```java
BroadcastReceiver receiver = new BroadcastReceiver() {
    @Override
    public void onReceive(Context context, Intent intent) {
        if (intent.getAction().equals("Some Action")) {
            //Do something
        }
    }
});

LocalBroadcastManager manager = LocalBroadcastManager.getInstance(mContext);
manager.registerReceiver(receiver, new IntentFilter("Some Action"));

// onReceive() will be called as a result of this call:
manager.sendBroadcast(new Intent("Some Action"));//See also sendBroadcastSync

//Remember to unregister the receiver when you are done with it:
manager.unregisterReceiver(receiver);

```



## BroadcastReceiver Basics


BroadcastReceivers are used to receive broadcast [Intents](http://stackoverflow.com/documentation/android/103/intent) that are sent by the Android OS, other apps, or within the same app.

Each Intent is created with an **Intent Filter**, which requires a String **action**. Additional information can be configured in the Intent.

Likewise, BroadcastReceivers register to receive Intents with a particular Intent Filter. They can be registered programmatically:

```java
mContext.registerReceiver(new BroadcastReceiver() {
    @Override
   public void onReceive(Context context, Intent intent) {
      //Your implementation goes here.
   }
}, new IntentFilter("Some Action"));

```

or in the `AndroidManifest.xml` file:

```java
<receiver android:name=".MyBroadcastReceiver">
    <intent-filter>
        <action android:name="Some Action"/>
    </intent-filter>
</receiver>

```

To receive the Intent, set the Action to something documented by Android OS, by another app or API, or within your own application, using `sendBroadcast`:

```java
mContext.sendBroadcast(new Intent("Some Action"));

```

Additionally, the Intent can contain information, such as Strings, primitives, and **Parcelables**, that can be viewed in `onReceive`.



## Introduction to Broadcast receiver


A Broadcast receiver is an Android component which allows you to register for system or application events.

A receiver can be registered via the `AndroidManifest.xml` file or dynamically via the `Context.registerReceiver()` method.

```java
public class MyReceiver extends BroadcastReceiver {
   @Override
   public void onReceive(Context context, Intent intent) {
      //Your implementation goes here.
   }
}

```

Here I have taken an example of `ACTION_BOOT_COMPLETED` which is fired by the system once the Android has completed the boot process.

You can register a reciever in manifest file like this:

```java
<application
   android:icon="@drawable/ic_launcher"
   android:label="@string/app_name"
   android:theme="@style/AppTheme" >
   <receiver android:name="MyReceiver">
      <intent-filter>
         <action android:name="android.intent.action.BOOT_COMPLETED">
         </action>
      </intent-filter>
   </receiver>
</application>

```

Now device gets booted, `onReceive()` method will be called and then you can do your work (e.g. start a service, start an alarm).



## Enabling and disabling a Broadcast Receiver programmatically


To enable or disable a `BroadcastReceiver`, we need to get a reference to the `PackageManager` and we need a `ComponentName` object containing the class of the receiver we want to enable/disable:

```java
ComponentName componentName = new ComponentName(context, MyBroadcastReceiver.class);
PackageManager packageManager = context.getPackageManager();

```

Now we can call the following method to enable the `BroadcastReceiver`:

```java
packageManager.setComponentEnabledSetting(
    componentName,
    PackageManager.COMPONENT_ENABLED_STATE_ENABLED,
    PackageManager.DONT_KILL_APP);

```

Or we can instead use `COMPONENT_ENABLED_STATE_DISABLED` to disable the receiver:

```java
packageManager.setComponentEnabledSetting(
    componentName,
    PackageManager.COMPONENT_ENABLED_STATE_DISABLED,
    PackageManager.DONT_KILL_APP);

```



## Sticky Broadcast


If we are using method sendStickyBroadcast(intent) the corresponding intent is sticky, meaning the intent you are sending stays around after broadcast is complete.
A StickyBroadcast as the name suggests is a mechanism to read the data from a broadcast, after the broadcast is complete.
This can be used in a scenario where you may want to check say in an `Activity's onCreate()` the value of a key in the intent before that Activity was launched.

```java
Intent intent = new Intent("com.org.action");
intent.putExtra("anIntegerKey", 0);
sendStickyBroadcast(intent);

```



## Using ordered broadcasts


Ordered broadcasts are used when you need to specify a priority for broadcast listeners.

In this example `firstReceiver` will receive broadcast always before than a `secondReceiver`:

```java
final int highPriority = 2;
final int lowPriority = 1;
final String action = "action";

// intent filter for first receiver with high priority
final IntentFilter firstFilter = new IntentFilter(action);
first Filter.setPriority(highPriority);
final BroadcastReceiver firstReceiver = new MyReceiver();

// intent filter for second receiver with low priority
final IntentFilter secondFilter = new IntentFilter(action);
secondFilter.setPriority(lowPriority);
final BroadcastReceiver secondReceiver = new MyReceiver();

// register our receivers
context.registerReceiver(firstReceiver, firstFilter);
context.registerReceiver(secondReceiver, secondFilter);

// send ordered broadcast
context.sendOrderedBroadcast(new Intent(action), null);

```

Furthermore broadcast receiver can abort ordered broadcast:

```java
@Override
public void onReceive(final Context context, final Intent intent) {
    abortBroadcast();
}

```

in this case all receivers with lower priority will not receive a broadcast message.



## Bluetooth Broadcast receiver


### add permission in your manifest file

```java
<uses-permission android:name="android.permission.BLUETOOTH" />

```

### In your Fragment(or Activity)

- Add the receiver method

```java
private BroadcastReceiver mBluetoothStatusChangedReceiver = new BroadcastReceiver() {
    @Override
    public void onReceive(Context context, Intent intent) {
        final Bundle extras = intent.getExtras();
        final int bluetoothState = extras.getInt(Constants.BUNDLE_BLUETOOTH_STATE);
        switch(bluetoothState) {
            case BluetoothAdapter.STATE_OFF:
                // Bluetooth OFF
                break;
            case BluetoothAdapter.STATE_TURNING_OFF:
                // Turning OFF
                break;
            case BluetoothAdapter.STATE_ON:
                // Bluetooth ON
                break;
            case BluetoothAdapter.STATE_TURNING_ON:
                // Turning ON
                break;
    }
};

```

### Register broadcast

- Call this method on onResume()

```java
private void registerBroadcastManager(){
    final LocalBroadcastManager manager = LocalBroadcastManager.getInstance(getActivity());
    manager.registerReceiver(mBluetoothStatusChangedReceiver, new IntentFilter(Constants.BROADCAST_BLUETOOTH_STATE));
}

```

### Unregister broadcast

- Call this method on onPause()

```java
private void unregisterBroadcastManager(){
    final LocalBroadcastManager manager = LocalBroadcastManager.getInstance(getActivity());
    // Beacon機能用
    manager.unregisterReceiver(mBluetoothStatusChangedReceiver);
}

```



## BroadcastReceiver to handle BOOT_COMPLETED events


Example below shows how to create a `BroadcastReceiver` which is able to receive `BOOT_COMPLETED` events. This way, you are able to start a `Service` or start an `Activity` as soon device was powered up.

Also, you can use `BOOT_COMPLETED` events to restore your alarms since they are destroyed when device is powered off.

**NOTE:** The user needs to have started the application at least once before you can receive the `BOOT_COMPLETED` action.

**AndroidManifest.xml**

```java
<manifest xmlns:android="http://schemas.android.com/apk/res/android"
    package="com.test.example" >
    ...
    <uses-permission android:name="android.permission.RECEIVE_BOOT_COMPLETED" />

    ...

    <application>
        ...

        <receiver android:name="com.test.example.MyCustomBroadcastReceiver">
        <intent-filter>
                <!-- REGISTER TO RECEIVE BOOT_COMPLETED EVENTS -->
                <action android:name="android.intent.action.BOOT_COMPLETED" />
            </intent-filter>
        </receiver>
    </application>
</manifest>

```

**MyCustomBroadcastReceiver.java**

```java
public class MyCustomBroadcastReceiver extends BroadcastReceiver {

    @Override
    public void onReceive(Context context, Intent intent) {
        String action = intent.getAction();

        if(action != null) {
            if (action.equals(Intent.ACTION_BOOT_COMPLETED) ) {
                // TO-DO: Code to handle BOOT COMPLETED EVENT
                // TO-DO: I can start an service.. display a notification... start an activity
            } 
        }
    }
}

```



## Example of a LocalBroadcastManager


A BroadcastReceiver is basically a mechanism to relay Intents through the OS to perform specific actions.
A classic definition being

> 
<p>"A Broadcast receiver is an Android component which allows you to
register for system or application events."</p>


[LocalBroadcastManager](https://developer.android.com/reference/android/support/v4/content/LocalBroadcastManager.html) is a way to send or receive broadcasts within an application process. This mechanism has a lot of advantages

1. since the data remains inside the application process, the data cannot be leaked.
1. LocalBroadcasts are resolved faster, since the resolution of a normal broadcast happens at runtime throughout the OS.

A simple example of a LocalBroastManager is:

**SenderActivity**

```

 Intent intent = new Intent("anEvent");
  intent.putExtra("key", "This is an event");
  LocalBroadcastManager.getInstance(this).sendBroadcast(intent);

```

**ReceiverActivity**

1. Register a receiver

> 

```java
LocalBroadcastManager.getInstance(this).registerReceiver(aLBReceiver,
              new IntentFilter("anEvent"));

```




1. A concrete object for performing action when the receiver is called

> 

```java
private BroadcastReceiver aLBReceiver = new BroadcastReceiver() {
    @Override 
    public void onReceive(Context context, Intent intent) {
        // perform action here.
    } 
};

```




1. unregister when the view is not visible any longer.

> 

```java
@Override 
protected void onPause() { 
    // Unregister since the activity is about to be closed. 
    LocalBroadcastManager.getInstance(this).unregisterReceiver(aLBReceiver);
    super.onDestroy(); 
}

```






## Communicate two activities through custom Broadcast receiver


You can communicate two activities so that Activity A can be notified of an event happening in Activity B.

**Activity A**

```java
final String eventName = "your.package.goes.here.EVENT";

@Override
protected void onCreate(Bundle savedInstanceState) {
    registerEventReceiver();
    super.onCreate(savedInstanceState);
}

@Override
protected void onDestroy() {
    unregisterEventReceiver(eventReceiver);
    super.onDestroy();
}

private void registerEventReceiver() {
    IntentFilter eventFilter = new IntentFilter();
    eventFilter.addAction(eventName);
    registerReceiver(eventReceiver, eventFilter);
}

private BroadcastReceiver eventReceiver = new BroadcastReceiver() {
    @Override
    public void onReceive(Context context, Intent intent) {
         //This code will be executed when the broadcast in activity B is launched
    }
};

```

**Activity B**

```java
final String eventName = "your.package.goes.here.EVENT";

private void launchEvent() {
    Intent eventIntent = new Intent(eventName);
    this.sendBroadcast(eventIntent);
}

```

Of course you can add more information to the broadcast adding extras to the Intent that is passed between the activities. Not added to keep the example as simple as possible.



## Android stopped state


Starting with Android 3.1 all applications, upon installation, are placed in a stopped state. While in stopped state, the application will not run for any reason, except by a manual launch of an activity, or an **explicit** intent that addresses an activity ,service or broadcast.

When writing system app that installs APKs directly, please take into account that the newly installed APP won't receive any broadcasts until moved into a non stopped state.

An easy way to to activate an app is to sent a explicit broadcast to this app. as most apps implement `INSTALL_REFERRER`, we can use it as a hooking point

Scan the manifest of the installed app, and send an explicit broadcast to to each receiver:

```java
Intent intent = new Intent();
intent.addFlags(Intent.FLAG_INCLUDE_STOPPED_PACKAGES);
intent.setComponent(new ComponentName(packageName, fullClassName));
sendBroadcast(intent);

```

