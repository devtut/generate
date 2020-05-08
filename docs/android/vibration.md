---
metaTitle: "Android - Vibration"
description: "Getting Started with Vibration , Vibrate Indefinitely, Vibration Patterns, Stop Vibrate, Vibrate for one time"
---

# Vibration



## Getting Started with Vibration 


**Grant Vibration Permission**

before you start implement code, you have to add permission in android manifest :

```java
<uses-permission android:name="android.permission.VIBRATE"/>

```

**Import Vibration Library**

```java
import android.os.Vibrator;

```

**Get instance of Vibrator from Context**

```java
Vibrator vibrator = (Vibrator) getSystemService(Context.VIBRATOR_SERVICE);

```

**Check device has vibrator**

```java
void boolean isHaveVibrate(){
    if (vibrator.hasVibrator()) {
        return true;
    }
    return false;
}

```



## Vibrate Indefinitely


using the **vibrate(long[] pattern, int repeat)**

```java
Vibrator vibrator = (Vibrator) getSystemService(Context.VIBRATOR_SERVICE);

// Start time delay 
// Vibrate for 500 milliseconds 
// Sleep for 1000 milliseconds 
long[] pattern = {0, 500, 1000};

// 0 meaning is repeat indefinitely 
vibrator.vibrate(pattern, 0);

```



## Vibration Patterns


You can create vibration patterns by passing in an array of longs, each of which represents a duration in milliseconds. The first number is start time delay. Each array entry then alternates between vibrate, sleep, vibrate, sleep, etc.

The following example demonstrates this pattern:

- vibrate 100 milliseconds and sleep 1000 milliseconds
- vibrate 200 milliseconds and sleep 2000 milliseconds

```java
long[] pattern = {0, 100, 1000, 200, 2000};

```

To cause the pattern to repeat, pass in the index into the pattern array at which to start the repeat, or `-1` to disable repeating.

```java
Vibrator vibrator = (Vibrator) getSystemService(Context.VIBRATOR_SERVICE);
vibrator.vibrate(pattern, -1); // does not repeat
vibrator.vibrate(pattern,  0); // repeats forever

```



## Stop Vibrate


If you want stop vibrate please call :

```java
vibrator.cancel();

```



## Vibrate for one time


using the **vibrate(long milliseconds)**

```java
Vibrator vibrator = (Vibrator) getSystemService(Context.VIBRATOR_SERVICE);
vibrator.vibrate(500);

```

