---
metaTitle: "Android - Service"
description: "Lifecycle of a Service, Defining the process of a service, Creating an unbound service, Starting a Service, Creating Bound Service with help of Binder , Creating Remote Service (via AIDL)"
---

# Service


A Service runs in **background** to perform long-running operations or to perform work for remote processes. A service does not provide any user interface it runs only in background with User’s input. For example a service can play music in the background while the user is in a different App, or it might download data from the internet without blocking user’s interaction with the Android device.



## Lifecycle of a Service


The services lifecycle has the following callbacks

- `onCreate()` :

Executed when the service is first created in order to set up the initial configurations you might need. This method is executed only if the service is not already running.

- `onStartCommand()` :

Executed every time `startService()` is invoked by another component, like an Activity or a BroadcastReceiver. When you use this method, the Service will run until you call `stopSelf()` or `stopService()`. Note that regardless of how many times you call `onStartCommand()`, the methods `stopSelf()` and `stopService()` must be invoked only once in order to stop the service.

- `onBind()` :

Executed when a component calls `bindService()` and returns an instance of IBInder, providing a communication channel to the Service. A call to `bindService()` will keep the service running as long as there are clients bound to it.

- `onDestroy()` :

Executed when the service is no longer in use and allows for disposal of resources that have been allocated.

It is important to note that during the lifecycle of a service other callbacks might be invoked such as `onConfigurationChanged()` and `onLowMemory()`

[https://developer.android.com/guide/components/services.html](https://developer.android.com/guide/components/services.html)

[<img src="http://i.stack.imgur.com/RfqYC.png" alt="enter image description here" />](http://i.stack.imgur.com/RfqYC.png)



## Defining the process of a service


The `android:process` field defines the name of the process where the service is to run. Normally, all components of an application run in the default process created for the application. However, a component can override the default with its own process attribute, allowing you to spread your application across multiple processes.

If the name assigned to this attribute begins with a colon (':'), the service will run in its own separate process.

```java
<service
  android:name="com.example.appName"
  android:process=":externalProcess" />

```

If the process name begins with a lowercase character, the service will run in a global process of that name, provided that it has permission to do so. This allows components in different applications to share a process, reducing resource usage.



## Creating an unbound service


The first thing to do is to add the service to `AndroidManifest.xml`, inside the `<application>` tag:

```java
<application ...>

    ...        

    <service
        android:name=".RecordingService"
        <!--"enabled" tag specifies Whether or not the service can be instantiated by the system — "true" -->
        <!--if it can be, and "false" if not. The default value is "true".-->
        android:enabled="true"
        <!--exported tag specifies Whether or not components of other applications can invoke the -->
        <!--service or interact with it — "true" if they can, and "false" if not. When the value-->
        <!--is "false", only components of the same application or applications with the same user -->
        <!--ID can start the service or bind to it.-->
        android:exported="false" />

</application>

```

If your intend to manage your service class in a separate package (eg: .AllServices.RecordingService) then you will need to specify where your service is located. So, in above case we will modify:

```java
android:name=".RecordingService"

```

to

```java
android:name=".AllServices.RecordingService"

```

or the easiest way of doing so is to specify the full package name.

Then we create the actual service class:

```java
public class RecordingService extends Service {
    private int NOTIFICATION = 1; // Unique identifier for our notification

    public static boolean isRunning = false;
    public static RecordingService instance = null;


    private NotificationManager notificationManager = null;


    @Override
    public IBinder onBind(Intent intent) {
        return null;
    }

    @Override
    public void onCreate(){
        instance = this;
        isRunning = true;

        notificationManager = (NotificationManager) getSystemService(NOTIFICATION_SERVICE);

        super.onCreate();
    }

    @Override
    public int onStartCommand(Intent intent, int flags, int startId){
        // The PendingIntent to launch our activity if the user selects this notification
        PendingIntent contentIntent = PendingIntent.getActivity(this, 0, new Intent(this, MainActivity.class), 0);

        // Set the info for the views that show in the notification panel.
        Notification notification = new NotificationCompat.Builder(this)
                .setSmallIcon(R.mipmap.ic_launcher)        // the status icon
                .setTicker("Service running...")           // the status text
                .setWhen(System.currentTimeMillis())       // the time stamp
                .setContentTitle("My App")                 // the label of the entry
                .setContentText("Service running...")      // the content of the entry
                .setContentIntent(contentIntent)           // the intent to send when the entry is clicked
                .setOngoing(true)                          // make persistent (disable swipe-away)
                .build();

        // Start service in foreground mode
        startForeground(NOTIFICATION, notification);

        return START_STICKY;
    }


    @Override
    public void onDestroy(){
        isRunning = false;
        instance = null;

        notificationManager.cancel(NOTIFICATION); // Remove notification

        super.onDestroy();
    }


    public void doSomething(){
        Toast.makeText(getApplicationContext(), "Doing stuff from service...", Toast.LENGTH_SHORT).show();
    }

}

```

All this service does is show a notification when it's running, and it can display toasts when its `doSomething()` method is called.

As you'll notice, it's implemented as a [singleton](https://en.wikipedia.org/wiki/Singleton_pattern), keeping track of its own instance - but without the usual static singleton factory method because services are naturally singletons and are created by intents. The instance is useful to the outside to get a "handle" to the service when it's running.

Last, we need to start and stop the service from an activity:

```java
public void startOrStopService(){
    if( RecordingService.isRunning ){
        // Stop service
        Intent intent = new Intent(this, RecordingService.class);
        stopService(intent);
    }
    else {
        // Start service
        Intent intent = new Intent(this, RecordingService.class);
        startService(intent);
    }
}

```

In this example, the service is started and stopped by the same method, depending on it's current state.

We can also invoke the `doSomething()` method from our activity:

```java
public void makeServiceDoSomething(){
    if( RecordingService.isRunning )
        RecordingService.instance.doSomething();
}

```



## Starting a Service


Starting a service is very easy, just call `startService` with an intent, from within an Activity:

```java
Intent intent = new Intent(this, MyService.class);  //substitute MyService with the name of your service
intent.putExtra(Intent.EXTRA_TEXT, "Some text"); //add any extra data to pass to the service

startService(intent); //Call startService to start the service.

```



## Creating Bound Service with help of Binder 


Create a class which extends `Service` class and in overridden method `onBind` return your local binder instance:

```java
public class LocalService extends Service {
    // Binder given to clients
    private final IBinder mBinder = new LocalBinder();

    /**
     * Class used for the client Binder.  Because we know this service always
     * runs in the same process as its clients, we don't need to deal with IPC.
     */
    public class LocalBinder extends Binder {
        LocalService getService() {
            // Return this instance of LocalService so clients can call public methods
            return LocalService.this;
        }
    }

    @Override
    public IBinder onBind(Intent intent) {
        return mBinder;
    }
}

```

Then in your activity bind to service in `onStart` callback, using `ServiceConnection` instance and unbind from it in `onStop`:

```java
public class BindingActivity extends Activity {
    LocalService mService;
    boolean mBound = false;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.main);
    }

    @Override
    protected void onStart() {
        super.onStart();
        // Bind to LocalService
        Intent intent = new Intent(this, LocalService.class);
        bindService(intent, mConnection, Context.BIND_AUTO_CREATE);
    }

    @Override
    protected void onStop() {
        super.onStop();
        // Unbind from the service
        if (mBound) {
            unbindService(mConnection);
            mBound = false;
        }
    }

    /** Defines callbacks for service binding, passed to bindService() */
    private ServiceConnection mConnection = new ServiceConnection() {

        @Override
        public void onServiceConnected(ComponentName className,
                IBinder service) {
            // We've bound to LocalService, cast the IBinder and get LocalService instance
            LocalBinder binder = (LocalBinder) service;
            mService = binder.getService();
            mBound = true;
        }

        @Override
        public void onServiceDisconnected(ComponentName arg0) {
            mBound = false;
        }
    };
}

```



## Creating Remote Service (via AIDL)


Describe your service access interface through `.aidl` file:

```java
// IRemoteService.aidl
package com.example.android;

// Declare any non-default types here with import statements

/** Example service interface */
interface IRemoteService {
    /** Request the process ID of this service, to do evil things with it. */
    int getPid();
}

```

Now after build application, sdk tools will generate appropriate `.java` file. This file will contain `Stub` class which implements our aidl interface, and which we need to extend:

```java
public class RemoteService extends Service {

    private final IRemoteService.Stub binder = new IRemoteService.Stub() {
        @Override
        public int getPid() throws RemoteException {
            return Process.myPid();
        }
    };

    @Nullable
    @Override
    public IBinder onBind(Intent intent) {
        return binder;
    }
}

```

Then in activity:

```java
public class MainActivity extends AppCompatActivity {
    private final ServiceConnection connection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName componentName, IBinder iBinder) {
            IRemoteService service = IRemoteService.Stub.asInterface(iBinder);
            Toast.makeText(this, "Activity process: " + Process.myPid + ", Service process: " + getRemotePid(service), LENGTH_SHORT).show();
        }

        @Override
        public void onServiceDisconnected(ComponentName componentName) {}
    };

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
    }

    @Override
    protected void onStart() {
        super.onStart();
        Intent intent = new Intent(this, RemoteService.class);
        bindService(intent, connection, Context.BIND_AUTO_CREATE);
    }

    @Override
    protected void onStop() {
        super.onStop();
        unbindService(connection);
    }

    private int getRemotePid(IRemoteService service) {
        int result = -1;

        try {
            result = service.getPid();
        } catch (RemoteException e) {
            e.printStackTrace();
        }

        return result;
    }
}

```



#### Remarks


If you have not defined your service in your AndroidManifest.xml, you will receive a `ServiceNotFoundException` when attempting to start it.

**Note:**

For info on `IntentService`, see here: [IntentService Example](http://stackoverflow.com/documentation/android/5319/intentservice#t=201610241904035833932)

