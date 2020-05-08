---
metaTitle: "Android - Notification Channel Android O"
description: "Notification Channel"
---

# Notification Channel Android O


Notification channels enable us app developers to group our notifications into groups—channels—with the user having the ability to modify notification settings for the entire channel at once.In Android O this feature is introduced.Right now it is available developers preview.



## Notification Channel


**What Are Notification Channels?**

Notification channels enable us app developers to group our notifications into groups—channels—with the user having the ability to modify notification settings for the entire channel at once. For example, for each channel, users can completely block all notifications, override the importance level, or allow a notification badge to be shown. This new feature helps in greatly improving the user experience of an app.

**Create Notification Channels**

```

import android.app.Notification;
import android.app.NotificationChannel;
import android.app.NotificationManager;
import android.content.Context;
import android.content.ContextWrapper;
import android.graphics.Color;
 
public class NotificationUtils extends ContextWrapper {


private NotificationManager mManager;
public static final String ANDROID_CHANNEL_ID = "com.sai.ANDROID";
public static final String IOS_CHANNEL_ID = "com.sai.IOS";
public static final String ANDROID_CHANNEL_NAME = "ANDROID CHANNEL";
public static final String IOS_CHANNEL_NAME = "IOS CHANNEL";

public NotificationUtils(Context base) {
    super(base);
    createChannels();
}

public void createChannels() {

    // create android channel
    NotificationChannel androidChannel = new NotificationChannel(ANDROID_CHANNEL_ID,
            ANDROID_CHANNEL_NAME, NotificationManager.IMPORTANCE_DEFAULT);
    // Sets whether notifications posted to this channel should display notification lights
    androidChannel.enableLights(true);
    // Sets whether notification posted to this channel should vibrate.
    androidChannel.enableVibration(true);
    // Sets the notification light color for notifications posted to this channel
    androidChannel.setLightColor(Color.BLUE);
    // Sets whether notifications posted to this channel appear on the lockscreen or not
    androidChannel.setLockscreenVisibility(Notification.VISIBILITY_PRIVATE);

    getManager().createNotificationChannel(androidChannel);

    // create ios channel
    NotificationChannel iosChannel = new NotificationChannel(IOS_CHANNEL_ID,
            IOS_CHANNEL_NAME, NotificationManager.IMPORTANCE_HIGH);
    iosChannel.enableLights(true);
    iosChannel.enableVibration(true);
    iosChannel.setLightColor(Color.GRAY);
    iosChannel.setLockscreenVisibility(Notification.VISIBILITY_PUBLIC);
    getManager().createNotificationChannel(iosChannel);


  }


private NotificationManager getManager() {
    if (mManager == null) {
        mManager = (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
    }
    return mManager;
}}

```

In the code above, we created two instances of the NotificationChannel, passing  uniqueid a channel name, and also an importance level in its constructor. For each notification channel, we applied following characteristics.

1. Sound
1. Lights
1. Vibration
1. Notification to show on lock screen.

Finally, we got the NotificationManager from the system and then registered the channel by calling the method createNotificationChannel(), passing the channel we have created.

We can create multiple notification channels all at once with createNotificationChannels(), passing a Java list of NotificationChannel instances. You can get all notification channels for an app with getNotificationChannels() and get a specific channel with getNotificationChannel(), passing only the channel id as an argument.

**Importance Level in Notification Channels**

|Method|Description
|---|---|---|---|---|---|---|---|---|---
|IMPORTANCE_MAX|unused
|[IMPORTANCE_HIGH](https://developer.android.com/reference/android/app/NotificationManager.html#IMPORTANCE_HIGH)|shows everywhere, makes noise and peeks
|[IMPORTANCE_DEFAULT](https://developer.android.com/reference/android/app/NotificationManager.html#IMPORTANCE_DEFAULT)|shows everywhere, makes noise, but does not visually intrude
|IMPORTANCE_LOW|shows everywhere, but is not intrusive,value is 0
|IMPORTANCE_MIN|only shows in the shade, below the fold
|IMPORTANCE_NONE|a notification with no importance; does not show in the shade

**Create Notification and Post to channel**

We have created two notification one using NotificationUtils another using NotificationBuilder.

```java
public Notification.Builder getAndroidChannelNotification(String title, String body) {
    return new Notification.Builder(getApplicationContext(), ANDROID_CHANNEL_ID)
            .setContentTitle(title)
            .setContentText(body)
            .setSmallIcon(android.R.drawable.stat_notify_more)
            .setAutoCancel(true);
}
 
public Notification.Builder getIosChannelNotification(String title, String body) {
    return new Notification.Builder(getApplicationContext(), IOS_CHANNEL_ID)
            .setContentTitle(title)
            .setContentText(body)
            .setSmallIcon(android.R.drawable.stat_notify_more)
            .setAutoCancel(true);
}

```

We can also set NotificationChannel Using Notification.Builder().For that we can use ****setChannel(String channelId)****.

**Update Notification Channel Settings**

Once you create a notification channel, the user is in charge of its settings and behavior. You can call createNotificationChannel() again to rename an existing notification channel, or update its description. The following sample code describes how you can redirect a user to the settings for a notification channel by creating an intent to start an activity. In this case, the intent requires extended data including the ID of the notification channel and the package name of your app.

```java
@Override
protected void onCreate(Bundle savedInstanceState) {
    //...
    Button buttonAndroidNotifSettings = (Button) findViewById(R.id.btn_android_notif_settings);
    buttonAndroidNotifSettings.setOnClickListener(new View.OnClickListener() {
     
        @Override
        public void onClick(View view) {
            Intent i = new Intent(Settings.ACTION_CHANNEL_NOTIFICATION_SETTINGS);
            i.putExtra(Settings.EXTRA_APP_PACKAGE, getPackageName());
            i.putExtra(Settings.EXTRA_CHANNEL_ID, NotificationUtils.ANDROID_CHANNEL_ID);
            startActivity(i);
        }
    });
}

```

XML file:

```java
<!--...-->
<Button
    android:id="@+id/btn_android_notif_settings"
    android:layout_width="match_parent"
    android:layout_height="wrap_content"
    android:text="Notification Settings"/>
<!--...-->

```

**Deleting Notification Channel**

You can delete notification channels by calling deleteNotificationChannel().

```java
NotificationManager mNotificationManager =
        (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
// The id of the channel.
String id = "my_channel_01";
NotificationChannel mChannel = mNotificationManager.getNotificationChannel(id);
mNotificationManager.deleteNotificationChannel(mChannel);

```

**Now Create MainActivity and xml**

activity_main.xml

```java
<?xml version="1.0" encoding="utf-8"?>
<LinearLayout
        xmlns:android="http://schemas.android.com/apk/res/android"
        xmlns:app="http://schemas.android.com/apk/res-auto"
        xmlns:tools="http://schemas.android.com/tools"
        android:id="@+id/activity_main"
        android:layout_width="match_parent"
        android:layout_height="match_parent"
        android:orientation="vertical"
        android:layout_margin="16dp"
        tools:context="com.chikeandroid.tutsplusalerts.MainActivity">
 
    <LinearLayout
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:orientation="vertical">
 
        <TextView
                android:layout_width="wrap_content"
                android:layout_height="wrap_content"
                android:text="Tuts+ Android Channel"
                android:layout_gravity="center_horizontal"
                android:textAppearance="@style/TextAppearance.AppCompat.Title"/>
 
        <EditText
                android:id="@+id/et_android_title"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:hint="Title"/>
 
        <EditText
                android:id="@+id/et_android_author"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:hint="Author"/>
         
        <Button
                android:id="@+id/btn_send_android"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:text="Send"/>
    </LinearLayout>
 
    <LinearLayout
            android:layout_width="match_parent"
            android:layout_height="wrap_content"
            android:orientation="vertical"
            android:layout_marginTop="20dp">
 
        <TextView
                android:layout_width="wrap_content"
                android:layout_height="wrap_content"
                android:text="Tuts+ IOS Channel"
                android:layout_gravity="center_horizontal"
                android:textAppearance="@style/TextAppearance.AppCompat.Title"/>
 
        <EditText
                android:id="@+id/et_ios_title"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:hint="Title"
                />
 
        <EditText
                android:id="@+id/et_ios_author"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:hint="Author"/>
        <Button
                android:id="@+id/btn_send_ios"
                android:layout_width="match_parent"
                android:layout_height="wrap_content"
                android:text="Send"/>
    </LinearLayout>
     
</LinearLayout>

```

****MainActivity.java****

we are going to edit our MainActivity so that we can get the title and author from the EditText components and then send these to the Android channel. We get the Notification.Builder for the Android channel we created in our NotificationUtils, and then notify the NotificationManager.

import android.app.Notification;
import android.os.Bundle;
import android.support.v7.app.AppCompatActivity;
import android.text.TextUtils;
import android.view.View;
import android.widget.Button;
import android.widget.EditText;

```java
public class MainActivity extends AppCompatActivity {
 
    private NotificationUtils mNotificationUtils;
 
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
 
        mNotificationUtils = new NotificationUtils(this);
 
        final EditText editTextTitleAndroid = (EditText) findViewById(R.id.et_android_title);
        final EditText editTextAuthorAndroid = (EditText) findViewById(R.id.et_android_author);
        Button buttonAndroid = (Button) findViewById(R.id.btn_send_android);
 
        buttonAndroid.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View view) {
                String title = editTextTitleAndroid.getText().toString();
                String author = editTextAuthorAndroid.getText().toString();
 
                if(!TextUtils.isEmpty(title) && !TextUtils.isEmpty(author)) {
                    Notification.Builder nb = mNotificationUtils.
                            getAndroidChannelNotification(title, "By " + author);
 
                    mNotificationUtils.getManager().notify(107, nb.build());
                }
            }
        });
    }
}

```



#### Syntax


1. class NotificationUtils{} //For create notification channel
1. createChannel()//Generic method for creating notification



#### Parameters


|Method|Description
|---|---|---|---|---|---|---|---|---|---
|IMPORTANCE_MAX|unused
|IMPORTANCE_HIGH|shows everywhere, makes noise and peeks
|IMPORTANCE_DEFAULT|shows everywhere, makes noise, but does not visually intrude
|IMPORTANCE_LOW|shows everywhere, but is not intrusive
|IMPORTANCE_MIN|only shows in the shade, below the fold
|IMPORTANCE_NONE|a notification with no importance; does not show in the shade

