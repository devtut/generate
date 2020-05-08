---
metaTitle: "Android - Orientation Changes"
description: "Saving and Restoring Activity State, Retaining Fragments, Manually Managing Configuration Changes, Handling AsyncTask, Saving and Restoring Fragment State, Lock Screen's rotation programmatically, Locking Screen Orientation"
---

# Orientation Changes



## Saving and Restoring Activity State


As your activity begins to stop, the system calls `onSaveInstanceState()` so your activity can save state information with a collection of key-value pairs. The default implementation of this method automatically saves information about the state of the activity's view hierarchy, such as the text in an `EditText` widget or the scroll position of a `ListView`.

To save additional state information for your activity, you must implement `onSaveInstanceState()` and add key-value pairs to the Bundle object. For example:

```java
public class MainActivity extends Activity {
    static final String SOME_VALUE = "int_value";
    static final String SOME_OTHER_VALUE = "string_value";

    @Override
    protected void onSaveInstanceState(Bundle savedInstanceState) {
        // Save custom values into the bundle
        savedInstanceState.putInt(SOME_VALUE, someIntValue);
        savedInstanceState.putString(SOME_OTHER_VALUE, someStringValue);
        // Always call the superclass so it can save the view hierarchy state
        super.onSaveInstanceState(savedInstanceState);
    }
}

```

The system will call that method before an Activity is destroyed. Then later the system will call `onRestoreInstanceState` where we can restore state from the bundle:

```java
@Override
protected void onRestoreInstanceState(Bundle savedInstanceState) {
    // Always call the superclass so it can restore the view hierarchy
    super.onRestoreInstanceState(savedInstanceState);
    // Restore state members from saved instance
    someIntValue = savedInstanceState.getInt(SOME_VALUE);
    someStringValue = savedInstanceState.getString(SOME_OTHER_VALUE);
}

```

Instance state can also be restored in the standard Activity#onCreate method but it is convenient to do it in `onRestoreInstanceState` which ensures all of the initialization has been done and allows subclasses to decide whether to use the default implementation. Read this [stackoverflow post](http://stackoverflow.com/questions/12683779/are-oncreate-and-onrestoreinstancestate-mutually-exclusive/14676555#14676555) for details.

Note that `onSaveInstanceState` and `onRestoreInstanceState` are not guaranteed to be called together. Android invokes `onSaveInstanceState()` when there's a chance the activity might be destroyed. However, there are cases where `onSaveInstanceState` is called but the activity is not destroyed and as a result `onRestoreInstanceState` is not invoked.



## Retaining Fragments


In many cases, we can avoid problems when an Activity is re-created by simply using fragments. If your views and state are within a fragment, we can easily have the fragment be retained when the activity is re-created:

```java
public class RetainedFragment extends Fragment {
    // data object we want to retain
    private MyDataObject data;

    // this method is only called once for this fragment
    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        // retain this fragment when activity is re-initialized
        setRetainInstance(true);
    }

    public void setData(MyDataObject data) {
        this.data = data;
    }

    public MyDataObject getData() {
        return data;
    }
}

```

This approach keeps the fragment from being destroyed during the activity lifecycle. They are instead retained inside the Fragment Manager. See the Android official docs for more [information](http://developer.android.com/guide/topics/resources/runtime-changes.html#RetainingAnObject).

Now you can check to see if the fragment already exists by tag before creating one and the fragment will retain it's state across configuration changes. See the Handling Runtime Changes guide for [more details](http://developer.android.com/guide/topics/resources/runtime-changes.html#RetainingAnObject).



## Manually Managing Configuration Changes


If your application doesn't need to update resources during a specific configuration change and you have a performance limitation that requires you to avoid the activity restart, then you can declare that your activity handles the configuration change itself, which prevents the system from restarting your activity.

However, this technique should be considered a last resort when you must avoid restarts due to a configuration change and is not recommended for most applications. To take this approach, we must add the `android:configChanges` node to the activity within the **AndroidManifest.xml**:

```java
<activity android:name=".MyActivity"
          android:configChanges="orientation|screenSize|keyboardHidden"
          android:label="@string/app_name">

```

Now, when one of these configurations change, the activity does not restart but instead receives a call to `onConfigurationChanged()`:

```java
// Within the activity which receives these changes
// Checks the current device orientation, and toasts accordingly
@Override
public void onConfigurationChanged(Configuration newConfig) {
    super.onConfigurationChanged(newConfig);

    // Checks the orientation of the screen
    if (newConfig.orientation == Configuration.ORIENTATION_LANDSCAPE) {
        Toast.makeText(this, "landscape", Toast.LENGTH_SHORT).show();
    } else if (newConfig.orientation == Configuration.ORIENTATION_PORTRAIT){
        Toast.makeText(this, "portrait", Toast.LENGTH_SHORT).show();
    }
}

```

See the [Handling the Change](http://developer.android.com/guide/topics/resources/runtime-changes.html#HandlingTheChange) docs. For more about which configuration changes you can handle in your activity, see the [android:configChanges](http://developer.android.com/guide/topics/manifest/activity-element.html#config) documentation and the [Configuration](http://developer.android.com/reference/android/content/res/Configuration.html) class.



## Handling AsyncTask


### Problem:

- If after the `AsyncTask` starts there is a screen rotation the owning activity is destroyed and recreated.
- When the `AsyncTask` finishes it wants to update the UI that may not valid anymore.

### Solution:

Using [Loaders](https://developer.android.com/guide/components/loaders.html), one can easily overcome the activity destruction/recreation.

### Example:

### MainActivity:

```java
public class MainActivity extends AppCompatActivity 
        implements LoaderManager.LoaderCallbacks<Bitmap> {

    //Unique id for the loader
    private static final int MY_LOADER = 0; 

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        LoaderManager loaderManager = getSupportLoaderManager();

        if(loaderManager.getLoader(MY_LOADER) == null) {
            loaderManager.initLoader(MY_LOADER, null, this).forceLoad();
        }
    }

    @Override
    public Loader<Bitmap> onCreateLoader(int id, Bundle args) {
        //Create a new instance of your Loader<Bitmap>
        MyLoader loader = new MyLoader(MainActivity.this);
        return loader;
    }

    @Override
    public void onLoadFinished(Loader<Bitmap> loader, Bitmap data) {
        // do something in the parent activity/service
        // i.e. display the downloaded image
        Log.d("MyAsyncTask", "Received result: ");
    }

    @Override
    public void onLoaderReset(Loader<Bitmap> loader) {

    }
}

```

### AsyncTaskLoader:

```java
public class MyLoader extends AsyncTaskLoader<Bitmap> {
    private WeakReference<Activity> motherActivity;

    public MyLoader(Activity activity) {
        super(activity);
        //We don't use this, but if you want you can use it, but remember, WeakReference
        motherActivity = new WeakReference<>(activity);
    }

    @Override
    public Bitmap loadInBackground() {
        // Do work. I.e download an image from internet to be displayed in gui.
        // i.e. return the downloaded gui
        return result;
    }
}

```

### Note:

It is important to use either the v4 compatibility library or not, but do not use part of one and part of the other, as it will lead to compilation errors. To check you can look at the imports for `android.support.v4.content` and `android.content` (you shouldn't have both).



## Saving and Restoring Fragment State


Fragments also have a `onSaveInstanceState()` method which is called when their state needs to be saved:

```java
public class MySimpleFragment extends Fragment {
    private int someStateValue;
    private final String SOME_VALUE_KEY = "someValueToSave";
   
    // Fires when a configuration change occurs and fragment needs to save state
    @Override
    protected void onSaveInstanceState(Bundle outState) {
        outState.putInt(SOME_VALUE_KEY, someStateValue);
        super.onSaveInstanceState(outState);
    }
}

```

Then we can pull data out of this saved state in `onCreateView`:

```java
public class MySimpleFragment extends Fragment {
   // ...

   // Inflate the view for the fragment based on layout XML
   @Override
   public View onCreateView(LayoutInflater inflater, ViewGroup container, Bundle savedInstanceState) {
        View view = inflater.inflate(R.layout.my_simple_fragment, container, false);
        if (savedInstanceState != null) {
            someStateValue = savedInstanceState.getInt(SOME_VALUE_KEY);
            // Do something with value if needed
        }
        return view;
   }
}

```

For the fragment state to be saved properly, we need to be sure that we aren't unnecessarily recreating the fragment on configuration changes. This means being careful not to reinitialize existing fragments when they already exist. Any fragments being initialized in an Activity need to be looked up by tag after a configuration change:

```java
public class ParentActivity extends AppCompatActivity {
    private MySimpleFragment fragmentSimple;
    private final String SIMPLE_FRAGMENT_TAG = "myfragmenttag";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        if (savedInstanceState != null) { // saved instance state, fragment may exist
           // look up the instance that already exists by tag
           fragmentSimple = (MySimpleFragment)  
              getSupportFragmentManager().findFragmentByTag(SIMPLE_FRAGMENT_TAG);
        } else if (fragmentSimple == null) { 
           // only create fragment if they haven't been instantiated already
           fragmentSimple = new MySimpleFragment();
        }
    }
}

```

This requires us to be careful to include a tag for lookup whenever putting a fragment into the activity within a transaction:

```java
public class ParentActivity extends AppCompatActivity {
    private MySimpleFragment fragmentSimple;
    private final String SIMPLE_FRAGMENT_TAG = "myfragmenttag";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        // ... fragment lookup or instantation from above...
        // Always add a tag to a fragment being inserted into container
        if (!fragmentSimple.isInLayout()) {
            getSupportFragmentManager()
                .beginTransaction()
                .replace(R.id.container, fragmentSimple, SIMPLE_FRAGMENT_TAG)
                .commit();
        }
    }
}

```

With this simple pattern, we can properly re-use fragments and restore their state across configuration changes.



## Lock Screen's rotation programmatically


It is very common that during development, one may find **very useful to lock/unlock the device screen during specific parts of the code**.

**For instance, while showing a Dialog with information, the developer might want to **lock** the screen's rotation to prevent the dialog from being dismissed and the current activity from being rebuilt to **unlock** it again when the dialog is dismissed.**

Even though we can achieve rotation locking from the manifest by doing :

```java
<activity
    android:name=".TheActivity"
    android:screenOrientation="portrait"
    android:label="@string/app_name" >
</activity>

```

One can do it programmatically as well by doing the following :

```java
public void lockDeviceRotation(boolean value) {
    if (value) {
        int currentOrientation = getResources().getConfiguration().orientation;
        if (currentOrientation == Configuration.ORIENTATION_LANDSCAPE) {
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_SENSOR_LANDSCAPE);
        } else {
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_SENSOR_PORTRAIT);
        }
    } else {
        getWindow().clearFlags(WindowManager.LayoutParams.FLAG_NOT_TOUCHABLE);
        if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.JELLY_BEAN_MR2) {
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_FULL_USER);
        } else {
            setRequestedOrientation(ActivityInfo.SCREEN_ORIENTATION_FULL_SENSOR);
        }
    }
}

```

And then calling the following, to respectively lock and unlock the device rotation

```java
lockDeviceRotation(true)

```

and

```java
lockDeviceRotation(false) 

```



## Locking Screen Orientation


If you want to lock the screen orientation change of any screen (activity) of your android application you just need to set the `android:screenOrientation` property of an `<activity>` within the **AndroidManifest.xml**:

```java
<activity
    android:name="com.techblogon.screenorientationexample.MainActivity"
    android:screenOrientation="portrait"
    android:label="@string/app_name" >
    <!-- ... -->
</activity>

```

Now that activity is forced to always be displayed in "**portrait**" mode.



#### Remarks


Reference : [https://guides.codepath.com/android/Handling-Configuration-Changes#references](https://guides.codepath.com/android/Handling-Configuration-Changes#references)

