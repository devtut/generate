---
metaTitle: "Android - AlarmManager"
description: "How to Cancel an Alarm, Creating exact alarms on all Android versions, API23+ Doze mode interferes with AlarmManager, Run an intent at a later time"
---

# AlarmManager



## How to Cancel an Alarm


If you want to cancel an alarm, and you don't have a reference to the original PendingIntent used to set the alarm, you need to recreate a PendingIntent exactly as it was when it was originally created.

An Intent is [considered equal by the AlarmManager](https://developer.android.com/reference/android/content/Intent.html#filterEquals(android.content.Intent)):

> 
if their action, data, type, class, and categories are the same. This does not compare any extra data included in the intents.


Usually the request code for each alarm is defined as a constant:

```java
public static final int requestCode = 9999;

```

So, for a simple alarm set up like this:

```java
Intent intent  = new Intent(this, AlarmReceiver.class);
intent.setAction("SomeAction");
PendingIntent pendingIntent = PendingIntent.getBroadcast(this, requestCode, intent, PendingIntent.FLAG_UPDATE_CURRENT);
AlarmManager alarmManager = (AlarmManager)getSystemService(Context.ALARM_SERVICE);
alarmManager.setExact(AlarmManager.RTC_WAKEUP, targetTimeInMillis, pendingIntent);

```

Here is how you would create a new PendingIntent reference that you can use to cancel the alarm with a new AlarmManager reference:

```java
Intent intent  = new Intent(this, AlarmReceiver.class);
intent.setAction("SomeAction");
PendingIntent pendingIntent = PendingIntent.getBroadcast(this, requestCode, intent, PendingIntent.FLAG_NO_CREATE);
AlarmManager alarmManager = (AlarmManager)getSystemService(Context.ALARM_SERVICE);
if(pendingIntent != null) {
    alarmManager.cancel(pendingIntent);
}

```



## Creating exact alarms on all Android versions


With more and more battery optimizations being put into the Android system over time, the methods of the `AlarmManager` have also significantly changed (to allow for more lenient timing). However, for some applications it is still required to be as exact as possible on all Android versions. The following helper uses the most accurate method available on all platforms to schedule a `PendingIntent`:

```java
public static void setExactAndAllowWhileIdle(AlarmManager alarmManager, int type, long triggerAtMillis, PendingIntent operation) {
    if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.M){
        alarmManager.setExactAndAllowWhileIdle(type, triggerAtMillis, operation);
    } else if (android.os.Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT){
        alarmManager.setExact(type, triggerAtMillis, operation);
    } else {
        alarmManager.set(type, triggerAtMillis, operation);
    }
}

```



## API23+ Doze mode interferes with AlarmManager


Android 6 (API23) introduced Doze mode which interferes with AlarmManager. It uses certain maintenance windows to handle alarms, so even if you used `setExactAndAllowWhileIdle()` you cannot make sure that your alarm fires at the desired point of time.

You can turn this behavior off for your app using your phone's settings (`Settings/General/Battery & power saving/Battery usage/Ignore optimizations` or similar)

Inside your app you can check this setting ...

```java
String packageName = getPackageName();
PowerManager pm = (PowerManager) getSystemService(Context.POWER_SERVICE);
if (pm.isIgnoringBatteryOptimizations(packageName)) {
   // your app is ignoring Doze battery optimization
}

```

... and eventually show the respective settings dialog:

```java
Intent intent = new Intent();
String packageName = getPackageName();
PowerManager pm = (PowerManager) getSystemService(Context.POWER_SERVICE);
intent.setAction(Settings.ACTION_REQUEST_IGNORE_BATTERY_OPTIMIZATIONS);
intent.setData(Uri.parse("package:" + packageName));
startActivity(intent);

```



## Run an intent at a later time


1. Create a receiver. This class will receive the intent and handle it how you wish.

```java
public class AlarmReceiver extends BroadcastReceiver
{
    @Override
    public void onReceive(Context context, Intent intent)
    {
        // Handle intent
        int reqCode = intent.getExtras().getInt("requestCode");
        ...
    }
}

```


1. Give an intent to AlarmManager. This example will trigger the intent to be sent to AlarmReceiver after 1 minute.

```java
final int requestCode = 1337;
AlarmManager am = (AlarmManager) context.getSystemService(Context.ALARM_SERVICE);
Intent intent = new Intent(context, AlarmReceiver.class);
PendingIntent pendingIntent = PendingIntent.getBroadcast(context, requestCode, intent, PendingIntent.FLAG_UPDATE_CURRENT);
am.set( AlarmManager.RTC_WAKEUP, System.currentTimeMillis() + 60000 , pendingIntent );

```

