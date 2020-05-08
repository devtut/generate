---
metaTitle: "Android - Activity"
description: "Activity launchMode, Exclude an activity from back-stack history, Android Activity LifeCycle Explained, End Application with exclude from Recents, Presenting UI with setContentView, Clear your current Activity stack and launch a new Activity, Up Navigation for Activities"
---

# Activity


An Activity represents a single screen with a user **interface(UI)**. An Android App may have more than one Activity, for example, An email App can have one activity to list all the emails, another activity to show email contents, yet another activity to compose new email. All the activities in an App work together to create perfect user experience.



## Activity launchMode


Launch mode defines the behaviour of new or existing activity in the task.<br/>
There are possible launch modes:

- standard
- singleTop
- singleTask
- singleInstance

It should be defined in android manifest in `<activity/>` element as `android:launchMode` attribute.

```java
<activity
    android:launchMode=["standard" | "singleTop" | "singleTask" | "singleInstance"] />

```

### Standard:

Default value. If this mode set, new activity will always be created for each new intent. So it's possible to get many activities of same type. New activity will be placed on the top of the task. There is some difference for different android version: if activity is starting from another application, on androids <= 4.4 it will be placed on same task as starter application, but on >= 5.0 new task will be created.

### SingleTop:

This mode is almost the same as `standard`. Many instances of singleTop activity could be created. The difference is, if an instance of activity already exists on the top of the current stack, `onNewIntent()` will be called instead of creating new instance.

### SingleTask:

Activity with this launch mode can have only one instance **in the system**. New task for activity will be created, if it doesn't exist. Otherwise, task with activity will be moved to front and `onNewIntent` will be called.

### SingleInstance:

This mode is similar to `singleTask`. The difference is task that holds an activity with `singleInstance` could have only this activity and nothing more. When `singleInstance` activity create another activity, new task will be created to place that activity.



## Exclude an activity from back-stack history


Let there be Activity `B` that can be opened, and can further start more Activities. But, user should not encounter it when navigating back in task activities.

[<img src="http://i.stack.imgur.com/aaHri.jpg" alt="Activity stack behavior" />](http://i.stack.imgur.com/aaHri.jpg)

The simplest solution is to set the attribute `noHistory` to `true` for that `<activity>` tag in `AndroidManifest.xml`:

```java
<activity
     android:name=".B"
     android:noHistory="true">

```

This same behavior is also possible from code if `B` calls `finish()` before starting the next activity:

```java
finish();
startActivity(new Intent(context, C.class));

```

Typical usage of `noHistory` flag is with "Splash Screen" or Login Activities.



## Android Activity LifeCycle Explained


Assume an application with a MainActivity which can call the Next Activity using a button click.

```java
public class MainActivity extends AppCompatActivity {

    private final String LOG_TAG = MainActivity.class.getSimpleName();
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        Log.d(LOG_TAG, "calling onCreate from MainActivity");
    }
    @Override
    protected void onStart() {
        super.onStart();
        Log.d(LOG_TAG, "calling onStart from MainActivity");
    }
    @Override
    protected void onResume() {
        super.onResume();
        Log.d(LOG_TAG, "calling onResume  from MainActivity");
    }

    @Override
    protected void onPause() {
        super.onPause();
        Log.d(LOG_TAG, "calling onPause  from MainActivity");
    }

    @Override
    protected void onStop() {
        super.onStop();
        Log.d(LOG_TAG, "calling onStop  from MainActivity");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        Log.d(LOG_TAG, "calling onDestroy  from MainActivity");
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        Log.d(LOG_TAG, "calling onRestart  from MainActivity");
    }
    public void toNextActivity(){
        Log.d(LOG_TAG, "calling Next Activity");
        Intent intent = new Intent(this, NextActivity.class);
        startActivity(intent);
    } }

```

and

```java
public class NextActivity extends AppCompatActivity {
    private final String LOG_TAG = NextActivity.class.getSimpleName();
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_next);
        Log.d(LOG_TAG, "calling onCreate from Next Activity");
    }
    @Override
    protected void onStart() {
        super.onStart();
        Log.d(LOG_TAG, "calling onStart from Next Activity");
    }
    @Override
    protected void onResume() {
        super.onResume();
        Log.d(LOG_TAG, "calling onResume  from Next Activity");
    }

    @Override
    protected void onPause() {
        super.onPause();
        Log.d(LOG_TAG, "calling onPause  from Next Activity");
    }

    @Override
    protected void onStop() {
        super.onStop();
        Log.d(LOG_TAG, "calling onStop  from Next Activity");
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        Log.d(LOG_TAG, "calling onDestroy  from Next Activity");
    }

    @Override
    protected void onRestart() {
        super.onRestart();
        Log.d(LOG_TAG, "calling onRestart  from Next Activity");
    } }

```

**When app is first created**<br />
D/MainActivity: calling onCreate from MainActivity<br />
D/MainActivity: calling onStart from MainActivity<br />
D/MainActivity: calling onResume  from MainActivity<br />
are called

**When screen sleeps**<br />
08:11:03.142 D/MainActivity: calling onPause  from MainActivity<br />
08:11:03.192 D/MainActivity: calling onStop  from MainActivity<br />
are called. And again when it wakes up<br />
08:11:55.922 D/MainActivity: calling onRestart  from MainActivity<br />
08:11:55.962 D/MainActivity: calling onStart from MainActivity<br />
08:11:55.962 D/MainActivity: calling onResume  from MainActivity<br />
are called

****Case1:****
When Next Activity is called from Main Activity<br />
D/MainActivity: calling Next Activity<br />
D/MainActivity: calling onPause  from MainActivity<br />
D/NextActivity: calling onCreate from Next Activity<br />
D/NextActivity: calling onStart from Next Activity<br />
D/NextActivity: calling onResume  from Next Activity<br />
D/MainActivity: calling onStop  from MainActivity

When Returning back to the Main Activity from Next Activity using back button<br />
D/NextActivity: calling onPause  from Next Activity<br />
D/MainActivity: calling onRestart  from MainActivity<br />
D/MainActivity: calling onStart from MainActivity<br />
D/MainActivity: calling onResume  from MainActivity<br />
D/NextActivity: calling onStop  from Next Activity<br />
D/NextActivity: calling onDestroy  from Next Activity

****Case2:****
When Activity is partially obscured (When overview button is pressed) or When app goes to background and another app completely obscures it<br />
D/MainActivity: calling onPause  from MainActivity<br />
D/MainActivity: calling onStop  from MainActivity<br />
and when the app is back in the foreground ready to accept User inputs,<br />
D/MainActivity: calling onRestart  from MainActivity<br />
D/MainActivity: calling onStart from MainActivity<br />
D/MainActivity: calling onResume  from MainActivity<br />
are called

****Case3:****
When an activity is called to fulfill implicit intent and user has make a selection. For eg., when share button is pressed and user has to select an app from the list of applications shown<br />
D/MainActivity: calling onPause  from MainActivity

The activity is visible but not active now. When the selection is done and app is active<br />
D/MainActivity: calling onResume  from MainActivity<br />
is called

****Case4:****<br />
When the app is killed in the background(to free resources for another foreground app), **onPause**(for pre-honeycomb device) or **onStop**(for since honeycomb device) will be the last to be called before the app is terminated.

onCreate and onDestroy will be called utmost once each time the application is run. But the onPause, onStop, onRestart, onStart, onResume maybe called many times during the lifecycle.



## End Application with exclude from Recents


First define an ExitActivity in the AndroidManifest.xml

```java
<activity
        android:name="com.your_example_app.activities.ExitActivity"
        android:autoRemoveFromRecents="true"
        android:theme="@android:style/Theme.NoDisplay" />

```

Afterwards the ExitActivity-class

```java
/**
 * Activity to exit Application without staying in the stack of last opened applications
 */
public class ExitActivity extends Activity {
    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        if (Utils.hasLollipop()) {
            finishAndRemoveTask();
        } else if (Utils.hasJellyBean()) {
            finishAffinity();
        } else {
            finish();
        }
    }

   /**
    * Exit Application and Exclude from Recents
    *
    * @param context Context to use
    */
    public static void exitApplication(ApplicationContext context) {
        Intent intent = new Intent(context, ExitActivity.class);
        intent.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK | Intent.FLAG_ACTIVITY_NO_ANIMATION | Intent.FLAG_ACTIVITY_EXCLUDE_FROM_RECENTS);
        context.startActivity(intent);
    }
}

```



## Presenting UI with setContentView


Activity class takes care of creating a window for you in which you can place your UI with `setContentView`.<br/>
There are three `setContentView` methods:

- `setContentView(int layoutResID)` - Set the activity content from a layout resource.
- `setContentView(View view)` - Set the activity content to an explicit view.
- `setContentView(View view, ViewGroup.LayoutParams params)` - Set the activity content to an explicit view with provided params.

When `setContentView` is called, this view is placed directly into the activity's view hierarchy. It can itself be a complex view hierarchy.

### Examples

### Set content from resource file:

```java
<?xml version="1.0" encoding="utf-8"?>
<FrameLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="match_parent"
    android:layout_height="match_parent" >

    <TextView android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:text="Hello" /> 

</FrameLayout>

```

Set it as content in activity:

```java
public final class MainActivity extends Activity {

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // The resource will be inflated, 
        // adding all top-level views to the activity.
        setContentView(R.layout.main);
    }
}

```

<h3>Set content to an explicit view:</h3>

```java
public final class MainActivity extends Activity {

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        // Creating view with container
        final FrameLayout root = new FrameLayout(this);
        final TextView text = new TextView(this);
        text.setText("Hello");
        root.addView(text);

        // Set container as content view
        setContentView(root);
    }
}

```



## Clear your current Activity stack and launch a new Activity


If you want to clear your current Activity stack and launch a new Activity (for example, logging out of the app and launching a log in Activity), there appears to be two approaches.

**1. Target (API >= 16)**

Calling `finishAffinity()` from an Activity

**2. Target (11 <= API < 16)**

```java
Intent intent = new Intent(this, LoginActivity.class);
intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK | Intent.FLAG_ACTIVITY_CLEAR_TASK |Intent.FLAG_ACTIVITY_CLEAR_TOP);
startActivity(intent);
finish();

```



## Up Navigation for Activities


Up navigation is done in android by adding `android:parentActivityName=""` in Manifest.xml to the activity tag. Basically with this tag you tell the system about the parent activity of a activity.

**How is it done?**



```java
<uses-permission android:name="android.permission.INTERNET" />

<application
    android:name=".SkillSchoolApplication"
    android:allowBackup="true"
    android:icon="@mipmap/ic_launcher"
    android:label="@string/app_name"
    android:supportsRtl="true"
    android:theme="@style/AppTheme">
    <activity
        android:name=".ui.activities.SplashActivity"
        android:theme="@style/SplashTheme">
        <intent-filter>
            <action android:name="android.intent.action.MAIN" />

            <category android:name="android.intent.category.LAUNCHER" />
        </intent-filter>
    </activity>
    <activity android:name=".ui.activities.MainActivity" />
    <activity android:name=".ui.activities.HomeActivity"
     android:parentActivityName=".ui.activities.MainActivity/> // HERE I JUST TOLD THE SYSTEM THAT MainActivity is the parent of HomeActivity
</application>

```

Now when i will click on the arrow inside the toolbar of HomeActivity it will take me back to the parent activity.

**Java Code**

Here i will write the appropriate java code required for this functionality.

```java
public class HomeActivity extends AppCompatActivity {
    @BindView(R.id.toolbar)
    Toolbar toolbar;    

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_home);
        ButterKnife.bind(this);
        //Since i am using custom tool bar i am setting refernce of that toolbar to Actionbar. If you are not using custom then you can simple leave this and move to next line
        setSupportActionBar(toolbar); 
        getSupportActionBar.setDisplayHomeAsUpEnabled(true); // this will show the back arrow in the tool bar.
}
}

```

If you run this code you will see when you press back button it will take you back to MainActivity. For futher understanding of Up Navigation i would recommend reading [docs](https://developer.android.com/training/implementing-navigation/ancestral.html)

You can more customize this behaviour upon your needs by overriding

```java
@Override
public boolean onOptionsItemSelected(MenuItem item) {
    switch (item.getItemId()) {
    // Respond to the action bar's Up/Home button
    case android.R.id.home:
        NavUtils.navigateUpFromSameTask(this); // Here you will write your logic for handling up navigation
        return true;
    }
    return super.onOptionsItemSelected(item);
}

```

**Simple Hack**

This is simple hack which is mostly used to navigate to parent activity if parent is in backstack. By calling `onBackPressed()` if id is equal to `android.R.id.home`

```java
@Override
    public boolean onOptionsItemSelected(MenuItem item) {
        int id = item.getItemId();
        switch (id) {
            case android.R.id.home:
                onBackPressed();
                return true;
        }
        return super.onOptionsItemSelected(item);
    }

```



#### Syntax


<li>
void onCreate(Bundle savedInstanceState) // Called when the activity is starting.
</li>
<li>
void onPostCreate(Bundle savedInstanceState) // Called when activity start-up is complete (after onStart() and onRestoreInstanceState(Bundle) have been called).
</li>
<li>
void onStart() // Called after onCreate(Bundle) — or after onRestart() when the activity had been stopped, but is now again being displayed to the user.
</li>
<li>
void onResume() // Called after onRestoreInstanceState(Bundle), onRestart(), or onPause(), for your activity to start interacting with the user.
</li>
<li>
void onPostResume() // Called when activity resume is complete (after onResume() has been called).
</li>
<li>
void onRestart() // Called after onStop() when the current activity is being re-displayed to the user (the user has navigated back to it).
</li>
<li>
void onPause() // Called as part of the activity lifecycle when an activity is going into the background, but has not (yet) been killed.
</li>
<li>
void onStop() // Called when you are no longer visible to the user.
</li>
<li>
void onDestroy() // Perform any final cleanup before an activity is destroyed.
</li>
<li>
void onNewIntent(Intent intent) // This is called for activities that set launchMode to "singleTop" in their package, or if a client used the FLAG_ACTIVITY_SINGLE_TOP flag when calling startActivity(Intent).
</li>
<li>
void onSaveInstanceState(Bundle outState) // Called to retrieve per-instance state from an activity before being killed so that the state can be restored in onCreate(Bundle) or onRestoreInstanceState(Bundle) (the Bundle populated by this method will be passed to both).
</li>
<li>
void onRestoreInstanceState(Bundle savedInstanceState) // This method is called after onStart() when the activity is being re-initialized from a previously saved state, given here in savedInstanceState.
</li>



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|[Intent](https://stackoverflow.com/documentation/android/103/intent#t=201607261640268639605)|Can be used with [startActivity](https://developer.android.com/reference/android/content/Context.html#startActivity(android.content.Intent)) to launch an Activity
|[Bundle](https://developer.android.com/reference/android/os/Bundle.html)|A mapping from String keys to various [Parcelable](https://developer.android.com/reference/android/os/Parcelable.html) values.
|[Context](https://developer.android.com/reference/android/content/Context.html)|Interface to global information about an application environment.



#### Remarks


An [Activity](https://developer.android.com/reference/android/app/Activity.html) is an application component that provides a screen with which users can interact in order to do something, such as dial the phone, take a photo, send an email, or view a map. Each activity is given a window in which to draw its user interface. The window typically fills the screen, but may be smaller than the screen and float on top of other windows.

