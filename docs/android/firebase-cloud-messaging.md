---
metaTitle: "Android - Firebase Cloud Messaging"
description: "Set Up a Firebase Cloud Messaging Client App on Android, Receive Messages, Registration token, This code that i have implemnted in my app for pushing image,message and also link for opening in your webView, Subscribe to a topic"
---

# Firebase Cloud Messaging


Firebase Cloud Messaging (FCM) is a cross-platform messaging solution that lets you reliably deliver messages at no cost.

Using FCM, you can notify a client app that new email or other data is available to sync. You can send notification messages to drive user reengagement and retention. For use cases such as instant messaging, a message can transfer a payload of up to `4KB` to a client app.



## Set Up a Firebase Cloud Messaging Client App on Android


<li>
<p>Complete the [Installation and setup part](http://stackoverflow.com/documentation/android/3843/firebase/19049/add-firebase-to-your-android-project#t=201608180547233218048) to connect your app to Firebase.<br />
This will create the project in Firebase.</p>
</li>
<li>
Add the dependency for Firebase Cloud Messaging to your module-level `build.gradle` file:
</li>

```java
dependencies {
     compile 'com.google.firebase:firebase-messaging:10.2.1'
}

```

Now you are ready to work with the FCM in Android.

FCM clients require devices running `Android 2.3` or higher that also have the Google Play Store app installed, or an emulator running Android 2.3 with Google APIs.

Edit your `AndroidManifest.xml` file

```java
<service
    android:name=".MyFirebaseMessagingService">
    <intent-filter>
        <action android:name="com.google.firebase.MESSAGING_EVENT"/>
    </intent-filter>
</service>

<service
    android:name=".MyFirebaseInstanceIDService">
    <intent-filter>
        <action android:name="com.google.firebase.INSTANCE_ID_EVENT"/>
    </intent-filter>
</service>

```



## Receive Messages


To receive messages, use a service that extends `FirebaseMessagingService` and override the `onMessageReceived` method.

```java
public class MyFcmListenerService extends FirebaseMessagingService {
    
    /**
     * Called when message is received.
     *
     * @param remoteMessage Object representing the message received from Firebase Cloud Messaging.
     */
    @Override
    public void onMessageReceived(RemoteMessage message) {
        String from = message.getFrom();

        // Check if message contains a data payload.
        if (remoteMessage.getData().size() > 0) {
            Log.d(TAG, "Message data payload: " + remoteMessage.getData());
            Map<String, String> data = message.getData();
        }

        // Check if message contains a notification payload.
        if (remoteMessage.getNotification() != null) {
            Log.d(TAG, "Message Notification Body: " + remoteMessage.getNotification().getBody());
        }

        //.....
    }

```

When the app is in the background, Android directs notification messages to the system tray. A user tap on the notification opens the app launcher by default.

This includes messages that contain both notification and data payload (and all messages sent from the Notifications console). In these cases, the notification is delivered to the device's system tray, and the data payload is delivered in the extras of the intent of your launcher Activity.

Here a short recap:

|App state|Notification|Data|Both
|---|---|---|---|---|---|---|---|---|---
|**Foreground**|`onMessageReceived`|`onMessageReceived`|`onMessageReceived`
|**Background**|System tray|`onMessageReceived`|Notification: system tray
||||Data: in extras of the intent.



## Registration token


On initial startup of your app, the FCM SDK generates a registration token for the client app instance.<br />
If you want to target single devices or create device groups, you'll need to access this token by extending `FirebaseInstanceIdService`.

The `onTokenRefresh` callback fires whenever a new token is generated and you can use the method `FirebaseInstanceID.getToken()` to retrieve the current token.

Example:

```java
public class MyFirebaseInstanceIDService extends FirebaseInstanceIdService {

   /**
     * Called if InstanceID token is updated. This may occur if the security of
     * the previous token had been compromised. Note that this is called when the InstanceID token
     * is initially generated so this is where you would retrieve the token.
     */

    @Override
    public void onTokenRefresh() {
        // Get updated InstanceID token.
        String refreshedToken = FirebaseInstanceId.getInstance().getToken();
        Log.d(TAG, "Refreshed token: " + refreshedToken);

    }

}

```



## This code that i have implemnted in my app for pushing image,message and also link for opening in your webView


This is my FirebaseMessagingService

```

   public class MyFirebaseMessagingService extends FirebaseMessagingService { 
Bitmap bitmap;
@Override 
public void onMessageReceived(RemoteMessage remoteMessage) {
    String message = remoteMessage.getData().get("message");
    //imageUri will contain URL of the image to be displayed with Notification 
    String imageUri = remoteMessage.getData().get("image");
    String link=remoteMessage.getData().get("link");
 
    //To get a Bitmap image from the URL received 
    bitmap = getBitmapfromUrl(imageUri);
    sendNotification(message, bitmap,link);
 
} 
 
 
/** 
 * Create and show a simple notification containing the received FCM message. 
 */ 
 
private void sendNotification(String messageBody, Bitmap image, String link) {
    Intent intent = new Intent(this, NewsListActivity.class);
    intent.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
    intent.putExtra("LINK",link);
    PendingIntent pendingIntent = PendingIntent.getActivity(this, 0 /* Request code */, intent,
            PendingIntent.FLAG_ONE_SHOT);
    Uri defaultSoundUri = RingtoneManager.getDefaultUri(RingtoneManager.TYPE_NOTIFICATION);
    NotificationCompat.Builder notificationBuilder = new NotificationCompat.Builder(this)
            .setLargeIcon(image)/*Notification icon image*/
            .setSmallIcon(R.drawable.hindi)
            .setContentTitle(messageBody)
            .setStyle(new NotificationCompat.BigPictureStyle()
                    .bigPicture(image))/*Notification with Image*/
            .setAutoCancel(true) 
            .setSound(defaultSoundUri)
            .setContentIntent(pendingIntent);
    NotificationManager notificationManager =
            (NotificationManager) getSystemService(Context.NOTIFICATION_SERVICE);
 
    notificationManager.notify(0 /* ID of notification */, notificationBuilder.build());
} 
public Bitmap getBitmapfromUrl(String imageUrl) {
    try { 
        URL url = new URL(imageUrl);
        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setDoInput(true);
        connection.connect();
        InputStream input = connection.getInputStream();
        Bitmap bitmap = BitmapFactory.decodeStream(input);
        return bitmap;
 
    } catch (Exception e) {
        // TODO Auto-generated catch block 
        e.printStackTrace();
        return null; 
 
    } 
}} 

```

And this is MainActivity to open link in my WebView or other browser depand on your requirement through intents.

```java
if (getIntent().getExtras() != null) {
    if (getIntent().getStringExtra("LINK")!=null) {
        Intent i=new Intent(this,BrowserActivity.class);
        i.putExtra("link",getIntent().getStringExtra("LINK"));
        i.putExtra("PUSH","yes");
        NewsListActivity.this.startActivity(i);
        finish();
    }} 

```



## Subscribe to a topic


Client apps can subscribe to any existing topic, or they can create a new topic. When a client app subscribes to a new topic name, a new topic of that name is created in FCM and any client can subsequently subscribe to it.

To subscribe to a topic use the `subscribeToTopic()` method specifying the topic name:

```java
FirebaseMessaging.getInstance().subscribeToTopic("myTopic");

```

