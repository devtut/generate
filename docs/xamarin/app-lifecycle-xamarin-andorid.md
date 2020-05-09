---
metaTitle: "Xamarin - App lifecycle - Xamarin.Andorid"
description: "Application lifecycle, Activity lifecycle, Fragment lifecycle, Full sample on GitHub"
---

# App lifecycle - Xamarin.Andorid


Xamarin.Android application lifecycle is the same as normal Android app.
When talking about lifecycle we need to talk about: Application lifecycle, Activity lifecycle and Fragment lifecycle.

In the below I'll try to provide a good description and way of using them. I obtained this documentation from the official Android and Xamarin documentation and many helpful web resources provided in remarks section below.



## Application lifecycle


First of all you should know that you can extend Android.Application class so you can access two important methods related with app lifecycle:

<li>
OnCreate - Called when the application is starting, before any other application objects have been created (like MainActivity).
</li>
<li>
<p>OnTerminate -
This method is for use in emulated process environments.
It will never be called on a production Android device, where processes are removed by simply killing them;
No user code (including this callback) is executed when doing so.
From the documentation: [https://developer.android.com/reference/android/app/Application.html#onTerminate()](https://developer.android.com/reference/android/app/Application.html#onTerminate())</p>
</li>

In Xamarin.Android application you can extend Application class in the way presented below. Add new class called "MyApplication.cs" to your project:

```cs
[Application]
public class MyApplication : Application
{
    public MyApplication(IntPtr handle, JniHandleOwnership ownerShip) : base(handle, ownerShip)
    {
    }

    public override void OnCreate()
    {
        base.OnCreate();
    }

    public override void OnTerminate()
    {
        base.OnTerminate();
    }
}

```

As you wrote above you can use OnCreate method. You can for instance initialize local database here or setup some additional configuration.

There is also more methods which can be overridden like: OnConfigurationChanged or OnLowMemory.



## Activity lifecycle


Activity lifecycle is quite more complex. As you know Activity is single page in the Android app where user can perform interaction with it.

On the diagram below you can see how Android Activity lifecycle looks like:

[<img src="https://i.stack.imgur.com/Oi0PS.png" alt="enter image description here" />](https://i.stack.imgur.com/Oi0PS.png)

As you can see there is specific flow of Activity lifecycle. In the mobile application you have of course methods in each Activity class that handle specific lifecycle fragment:

```cs
[Activity(Label = "LifecycleApp", MainLauncher = true, Icon = "@mipmap/icon")]
public class MainActivity : Activity
{
    protected override void OnCreate(Bundle savedInstanceState)
    {
        base.OnCreate(savedInstanceState);
        Log.Debug("OnCreate", "OnCreate called, Activity components are being created");

        // Set our view from the "main" layout resource
        SetContentView(Resource.Layout.MainActivity);
    }

    protected override void OnStart()
    {
        Log.Debug("OnStart", "OnStart called, App is Active");
        base.OnStart();
    }

    protected override void OnResume()
    {
        Log.Debug("OnResume", "OnResume called, app is ready to interact with the user");
        base.OnResume();
    }

    protected override void OnPause()
    {
        Log.Debug("OnPause", "OnPause called, App is moving to background");
        base.OnPause();
    }

    protected override void OnStop()
    {
        Log.Debug("OnStop", "OnStop called, App is in the background");
        base.OnStop();
    }

    protected override void OnDestroy()
    {
        base.OnDestroy();
        Log.Debug("OnDestroy", "OnDestroy called, App is Terminating");
    }
}

```

There is good description in the official Android documentation:

<li>
The entire lifetime of an activity happens between the first call to onCreate(Bundle) through to a single final call to onDestroy(). An activity will do all setup of "global" state in onCreate(), and release all remaining resources in onDestroy(). For example, if it has a thread running in the background to download data from the network, it may create that thread in onCreate() and then stop the thread in onDestroy().
</li>
<li>
The visible lifetime of an activity happens between a call to onStart() until a corresponding call to onStop(). During this time the user can see the activity on-screen, though it may not be in the foreground and interacting with the user. Between these two methods you can maintain resources that are needed to show the activity to the user. For example, you can register a BroadcastReceiver in onStart() to monitor for changes that impact your UI, and unregister it in onStop() when the user no longer sees what you are displaying. The onStart() and onStop() methods can be called multiple times, as the activity becomes visible and hidden to the user.
</li>
<li>
The foreground lifetime of an activity happens between a call to onResume() until a corresponding call to onPause(). During this time the activity is in front of all other activities and interacting with the user. An activity can frequently go between the resumed and paused states -- for example when the device goes to sleep, when an activity result is delivered, when a new intent is delivered -- so the code in these methods should be fairly lightweight.
</li>



## Fragment lifecycle


As you know you can have one activity but different fragments embedded in it. That is why fragment lifecycle is also important for developers.

On the diagram below you can see how Android fragment lifecycle looks like:

[<img src="https://i.stack.imgur.com/wKcPI.png" alt="enter image description here" />](https://i.stack.imgur.com/wKcPI.png)

As described in the official Android documentation you should implement at least below three methods:

<li>
OnCreate - the system calls this when creating the fragment. Within your implementation, you should initialize essential components of the fragment that you want to retain when the fragment is paused or stopped, then resumed.
</li>
<li>
OnCreateView - The system calls this when it's time for the fragment to draw its user interface for the first time. To draw a UI for your fragment, you must return a View from this method that is the root of your fragment's layout. You can return null if the fragment does not provide a UI.
</li>
<li>
OnPause - The system calls this method as the first indication that the user is leaving the fragment (though it does not always mean the fragment is being destroyed). This is usually where you should commit any changes that should be persisted beyond the current user session (because the user might not come back).
</li>

Here is sample implementation in Xamarin.Android:

```cs
public class MainFragment : Fragment
{
    public override void OnCreate(Bundle savedInstanceState)
    {
        base.OnCreate(savedInstanceState);

        // Create your fragment here
        // You should initialize essential components of the fragment
        // that you want to retain when the fragment is paused or stopped, then resumed.
    }

    public override View OnCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState)
    {
        // Use this to return your custom view for this Fragment
        // The system calls this when it's time for the fragment to draw its user interface for the first time.

        var mainView = inflater.Inflate(Resource.Layout.MainFragment, container, false);
        return mainView;
    }

    public override void OnPause()
    {
        // The system calls this method as the first indication that the user is leaving the fragment 

        base.OnPause();
    }
}

```

Of course you can add additional methods here if you want to handle different states.



## Full sample on GitHub


If you would like to get base project with methods described below you can download Xamarin.Android application template from my GitHub. You can find examples for:

- Application lifecycle methods
- Activity lifecycle methods
- Fragment lifecycle methods

[https://github.com/Daniel-Krzyczkowski/XamarinAndroid/tree/master/AndroidLifecycle/LifecycleApp](https://github.com/Daniel-Krzyczkowski/XamarinAndroid/tree/master/AndroidLifecycle/LifecycleApp)



#### Remarks


Interesting links to broad your knowledge about Android application lifecycle:

[https://developer.android.com/reference/android/app/Activity.html](https://developer.android.com/reference/android/app/Activity.html)

[http://www.vogella.com/tutorials/AndroidLifeCycle/article.html](http://www.vogella.com/tutorials/AndroidLifeCycle/article.html)

[https://github.com/xxv/android-lifecycle](https://github.com/xxv/android-lifecycle)

[https://developer.android.com/guide/components/fragments.html](https://developer.android.com/guide/components/fragments.html)

[https://developer.xamarin.com/guides/android/platform_features/fragments/part_1_-_creating_a_fragment/](https://developer.xamarin.com/guides/android/platform_features/fragments/part_1_-_creating_a_fragment/)

[https://developer.android.com/guide/components/activities/activity-lifecycle.html](https://developer.android.com/guide/components/activities/activity-lifecycle.html)

