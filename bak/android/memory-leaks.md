---
metaTitle: "Android - Memory Leaks"
description: "Avoid leaking Activities with AsyncTask, Common memory leaks and how to fix them, Detect memory leaks with the LeakCanary library, Anonymous callback in activities, Activity Context in static classes, Avoid leaking Activities with Listeners, Avoid memory leaks with Anonymous Class, Handler, Timer Task, Thread"
---

# Memory Leaks




## Avoid leaking Activities with AsyncTask


> 
**A word of caution**: AsyncTask has [many gotcha's](https://www.youtube.com/watch?v=jtlRNNhane0) apart from the memory leak described here. So be careful with this API, or avoid it altogether if you don't fully understand the implications. There are many alternatives (Thread, EventBus, RxAndroid, etc).


One common mistake with `AsyncTask` is to capture a strong reference to the host `Activity` (or `Fragment`):

```java
class MyActivity extends Activity {
  private AsyncTask<Void, Void, Void> myTask = new AsyncTask<Void, Void, Void>() {
    // Don't do this! Inner classes implicitly keep a pointer to their
    // parent, which in this case is the Activity!
  }
}

```

This is a problem because `AsyncTask` can easily outlive the parent `Activity`, for example if a configuration change happens while the task is running.

The right way to do this is to make your task a `static` class, which does not capture the parent, and holding a [weak reference](https://docs.oracle.com/javase/7/docs/api/java/lang/ref/WeakReference.html) to the host `Activity`:

```java
class MyActivity extends Activity {
  static class MyTask extends AsyncTask<Void, Void, Void> {
    // Weak references will still allow the Activity to be garbage-collected
    private final WeakReference<MyActivity> weakActivity;

    MyTask(MyActivity myActivity) {
      this.weakActivity = new WeakReference<>(myActivity);
    }

    @Override
    public Void doInBackground(Void... params) {
      // do async stuff here
    }

    @Override
    public void onPostExecute(Void result) {
      // Re-acquire a strong reference to the activity, and verify
      // that it still exists and is active.
      MyActivity activity = weakActivity.get();
      if (activity == null
          || activity.isFinishing()
          || activity.isDestroyed()) {
        // activity is no longer valid, don't do anything!
        return;
      }

      // The activity is still valid, do main-thread stuff here
    }
  }
}

```



## Common memory leaks and how to fix them


### 1. Fix your contexts:

Try using the appropriate context: For example since a Toast can be seen in many activities instead of in just one, use `getApplicationContext()` for toasts, and since services can keep running even though an activity has ended start a service with:

```java
Intent myService = new Intent(getApplicationContext(), MyService.class);

```

Use this table as a quick guide for what context is appropriate:
[<img src="https://i.stack.imgur.com/1o5MI.png" alt="enter image description here" />](https://i.stack.imgur.com/1o5MI.png)

Original [article on context here](https://possiblemobile.com/2013/06/context/).

### 2. Static reference to Context

A serious memory leak mistake is keeping a static reference to `View`. Every `View` has an inner reference to the `Context`. Which means an old Activity with its whole view hierarchy will not be garbage collected until the app is terminated. You will have your app twice in memory when rotating the screen.

Make sure there is absolutely no static reference to View, Context or any of their descendants.

### 3. Check that you're actually finishing your services.

For example, I have an intentService that uses the Google location service API. And I forgot to call `googleApiClient.disconnect();`:

```java
//Disconnect from API onDestroy()
if (googleApiClient.isConnected()) {
    LocationServices.FusedLocationApi.removeLocationUpdates(googleApiClient, GoogleLocationService.this);
    googleApiClient.disconnect();
}

```

### 4. Check image and bitmaps usage:

If you are using Square's **library Picasso** I found I was leaking memory by not using the `.fit()`, that drastically reduced my memory footprint from 50MB in average to less than 19MB:

```java
Picasso.with(ActivityExample.this)                   //Activity context
                .load(object.getImageUrl())           
                .fit()                                //This avoided the OutOfMemoryError
                .centerCrop()                         //makes image to not stretch
                .into(imageView);

```

### 5. If you are using broadcast receivers unregister them.

### 6. If you are using `java.util.Observer` (Observer pattern):

Make sure to use `deleteObserver(observer);`



## Detect memory leaks with the LeakCanary library


[LeakCanary](https://github.com/square/leakcanary) is an Open Source Java library to detect memory leaks in your debug builds.

Just add the dependencies in the `build.gradle`:

```

dependencies {
   debugCompile 'com.squareup.leakcanary:leakcanary-android:1.5.1'
   releaseCompile 'com.squareup.leakcanary:leakcanary-android-no-op:1.5.1'
   testCompile 'com.squareup.leakcanary:leakcanary-android-no-op:1.5.1'
 }

```

Then in your `Application` class:

```java
public class ExampleApplication extends Application {

  @Override public void onCreate() {
    super.onCreate();
    
    if (LeakCanary.isInAnalyzerProcess(this)) {
      // This process is dedicated to LeakCanary for heap analysis.
      // You should not init your app in this process.
      return;
    }

    LeakCanary.install(this);
  }
}

```

Now LeakCanary will automatically show a notification when an activity memory leak is detected in your **debug** build.

NOTE: Release code will contain no reference to LeakCanary other than the two empty classes that exist in the `leakcanary-android-no-op` dependency.



## Anonymous callback in activities


Every Time you create an anonymous class, it retains an implicit reference to its parent class. So when you write:

```java
public class LeakyActivity extends Activity
{

...

    foo.registerCallback(new BarCallback() 
    {            
        @Override
        public void onBar() 
        {
            // do something                
        }            
    });
}

```

You are in fact sending a reference to your LeakyActivity instance to foo. When the user navigates away from your LeakyActivity, this reference can prevent the LeakyActivity instance from being garbage collected. This is a serious leak as activities hold a reference to their entire view hierarchy and are therefore rather large objects in memory.

How to avoid this leak:

You can of course avoid using anonymous callbacks in activities entirely. You can also unregister all of your callbacks with respect to the activity lifecycle. like so:

```java
public class NonLeakyActivity extends Activity
{
    private final BarCallback mBarCallback = new BarCallback() 
    {            
        @Override
        public void onBar() 
        {
            // do something                
        }            
    });

    @Override
    protected void onResume()
    {
        super.onResume();
        foo.registerCallback(mBarCallback);
    }

    @Override
    protected void onPause()
    {
        super.onPause();
        foo.unregisterCallback(mBarCallback);
    }
}

```



## Activity Context in static classes


Often you will want to wrap some of Android's classes in easier to use utility classes. Those utility classes often require a context to access the android OS or your apps' resources. A common example of this is a wrapper for the SharedPreferences class. In order to access Androids shared preferences one must write:

`context.getSharedPreferences(prefsName, mode);`

And so one may be tempted to create the following class:

```java
public class LeakySharedPrefsWrapper
{
    private static Context sContext;

    public static void init(Context context)
    {
        sContext = context;
    }

    public int getInt(String name,int defValue)
    {
        return sContext.getSharedPreferences("a name", Context.MODE_PRIVATE).getInt(name,defValue);
    }
}

```

now, if you call `init()` with your activity context, the LeakySharedPrefsWrapper will retain a reference to your activity, preventing it from being garbage collected.

How to avoid:

When calling static helper functions, you can send in the application context using `context.getApplicationContext();`

When creating static helper functions, you can extract the application context from the context you are given (Calling getApplicationContext() on the application context returns the application context). So the fix to our wrapper is simple:

```java
public static void init(Context context)
{
    sContext = context.getApplicationContext();
}

```

If the application context is not appropriate for your use case, you can include a Context parameter in each utility function, you should avoid keeping references to these context parameters. In this case the solution would look like so:

```java
public int getInt(Context context,String name,int defValue)
{
    // do not keep a reference of context to avoid potential leaks.
    return context.getSharedPreferences("a name", Context.MODE_PRIVATE).getInt(name,defValue);
}

```



## Avoid leaking Activities with Listeners


If you implement or create a listener in an Activity, always pay attention to the lifecycle of the object that has the listener registered.

Consider an application where we have several different activities/fragments interested in when a user is logged in or out. One way of doing this would be to have a singleton instance of a `UserController` that can be subscribed to in order to get notified when the state of the user changes:

```java
public class UserController {
    private static UserController instance;
    private List<StateListener> listeners;

    public static synchronized UserController getInstance() {
        if (instance == null) {
            instance = new UserController();
        }
        return instance;
    }

    private UserController() {
        // Init
    }

    public void registerUserStateChangeListener(StateListener listener) {
        listeners.add(listener);
    }

    public void logout() {
        for (StateListener listener : listeners) {
            listener.userLoggedOut();
        }
    }

    public void login() {
        for (StateListener listener : listeners) {
            listener.userLoggedIn();
        }
    }

    public interface StateListener {
        void userLoggedIn();
        void userLoggedOut();
    }
}

```

Then there are two activities, `SignInActivity`:

```java
public class SignInActivity extends Activity implements UserController.StateListener{

    UserController userController;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        this.userController = UserController.getInstance();
        this.userController.registerUserStateChangeListener(this);
    }

    @Override
    public void userLoggedIn() {
        startMainActivity();
    }

    @Override
    public void userLoggedOut() {
        showLoginForm();
    }

    ...

    public void onLoginClicked(View v) {
        userController.login();
    }
}

```

And `MainActivity`:

```java
public class MainActivity extends Activity implements UserController.StateListener{
    UserController userController;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);

        this.userController = UserController.getInstance();
        this.userController.registerUserStateChangeListener(this);
    }

    @Override
    public void userLoggedIn() {
        showUserAccount();
    }

    @Override
    public void userLoggedOut() {
        finish();
    }

    ...

    public void onLogoutClicked(View v) {
        userController.logout();
    }
}

```

What happens with this example is that every time the user logs in and then logs out again, a `MainActivity` instance is leaked. The leak occurs because there is a reference to the activity in `UserController#listeners`.

**Please note:** Even if we use an anonymous inner class as a listener, the activity would still leak:

```java
...
this.userController.registerUserStateChangeListener(new UserController.StateListener() {
    @Override
    public void userLoggedIn() {
        showUserAccount();
    }

    @Override
    public void userLoggedOut() {
        finish();
    }
});    
...

```

The activity would still leak, because the anonymous inner class has an implicit reference to the outer class (in this case the activity). This is why it is possible to call instance methods in the outer class from the inner class. In fact, the only type of inner classes that do not have a reference to the outer class are **static** inner classes.

In short, all instances of non-static inner classes hold an implicit reference to the instance of the outer class that created them.

There are two main approaches to solving this, either by adding a method to remove a listener from `UserController#listeners` or using a [`WeakReference`](https://developer.android.com/reference/java/lang/ref/WeakReference.html) to hold the reference of the listeners.

### Alternative 1: Removing listeners

Let us start by creating a new method `removeUserStateChangeListener(StateListener listener)`:

```java
public class UserController {

    ...

    public void registerUserStateChangeListener(StateListener listener) {
        listeners.add(listener);
    }

    public void removeUserStateChangeListener(StateListener listener) {
        listeners.remove(listener);
    }

    ...
}

```

Then let us call this method in the activity's `onDestroy` method:

```java
public class MainActivity extends Activity implements UserController.StateListener{
    ...

    @Override
    protected void onDestroy() {
        super.onDestroy();
        userController.removeUserStateChangeListener(this);
    }
}

```

With this modification the instances of `MainActivity` are no longer leaked when the user logs in and out. However, if the documentation isn't clear, chances are that the next developer that starts using `UserController` might miss that it is required to unregister the listener when the activity is destroyed, which leads us to the second method of avoiding these types of leaks.

### Alternative 2: Using weak references

First off, let us start by explaining what a weak reference is. A weak reference, as the name suggests, holds a weak reference to an object. Compared to a normal instance field, which is a strong reference, a weak references does not stop the garbage collector, GC, from removing the objects. In the example above this would allow `MainActivity` to be garbage-collected after it has been destroyed if the `UserController` used [`WeakReference`](https://developer.android.com/reference/java/lang/ref/WeakReference.html) to the reference the listeners.

In short, a weak reference is telling the GC that if no one else has a strong reference to this object, go ahead and remove it.

Let us modify the `UserController` to use a list of [`WeakReference`](https://developer.android.com/reference/java/lang/ref/WeakReference.html) to keep track of it's listeners:

```java
public class UserController {

    ...
    private List<WeakReference<StateListener>> listeners;
    ...

    public void registerUserStateChangeListener(StateListener listener) {
        listeners.add(new WeakReference<>(listener));
    }

    public void removeUserStateChangeListener(StateListener listenerToRemove) {
        WeakReference referencesToRemove = null;
        for (WeakReference<StateListener> listenerRef : listeners) {
            StateListener listener = listenerRef.get();
            if (listener != null && listener == listenerToRemove) {
                referencesToRemove = listenerRef;
                break;
            }
        }
        listeners.remove(referencesToRemove);
    }

    public void logout() {
        List referencesToRemove = new LinkedList();
        for (WeakReference<StateListener> listenerRef : listeners) {
            StateListener listener = listenerRef.get();
            if (listener != null) {
                listener.userLoggedOut();
            } else {
                referencesToRemove.add(listenerRef);
            }
        }
    }

    public void login() {
        List referencesToRemove = new LinkedList();
        for (WeakReference<StateListener> listenerRef : listeners) {
            StateListener listener = listenerRef.get();
            if (listener != null) {
                listener.userLoggedIn();
            } else {
                referencesToRemove.add(listenerRef);
            }
        }
    }
    ...    
}

```

With this modification it doesn't matter whether or not the listeners are removed, since `UserController` holds no strong references to any of the listeners. However, writing this boilerplate code every time is cumbersome. Therefore, let us create a generic class called `WeakCollection`:

```java
public class WeakCollection<T> {
    private LinkedList<WeakReference<T>> list;

    public WeakCollection() {
        this.list = new LinkedList<>();
    }
    public void put(T item){
        //Make sure that we don't re add an item if we already have the reference.
        List<T> currentList = get();
        for(T oldItem : currentList){
            if(item == oldItem){
                return;
            }
        }
        list.add(new WeakReference<T>(item));
    }

    public List<T> get() {
        List<T> ret = new ArrayList<>(list.size());
        List<WeakReference<T>> itemsToRemove = new LinkedList<>();
        for (WeakReference<T> ref : list) {
            T item = ref.get();
            if (item == null) {
                itemsToRemove.add(ref);
            } else {
                ret.add(item);
            }
        }
        for (WeakReference ref : itemsToRemove) {
            this.list.remove(ref);
        }
        return ret;
    }

    public void remove(T listener) {
        WeakReference<T> refToRemove = null;
        for (WeakReference<T> ref : list) {
            T item = ref.get();
            if (item == listener) {
                refToRemove = ref;
            }
        }
        if(refToRemove != null){
            list.remove(refToRemove);
        }
    }
}

```

Now let us re-write `UserController` to use `WeakCollection<T>` instead:

```java
public class UserController {
    ...
    private WeakCollection<StateListener> listenerRefs;
    ...

    public void registerUserStateChangeListener(StateListener listener) {
        listenerRefs.put(listener);
    }

    public void removeUserStateChangeListener(StateListener listenerToRemove) {
        listenerRefs.remove(listenerToRemove);
    }

    public void logout() {
        for (StateListener listener : listenerRefs.get()) {
            listener.userLoggedOut();
        }
    }

    public void login() {
        for (StateListener listener : listenerRefs.get()) {
            listener.userLoggedIn();
        }
    }

    ...
}

```

As shown in the code example above, the `WeakCollection<T>` removes all of the boilerplate code needed to use [`WeakReference`](https://developer.android.com/reference/java/lang/ref/WeakReference.html) instead of a normal list. To top it all off: If a call to `UserController#removeUserStateChangeListener(StateListener)` is missed, the listener, and all the objects it is referencing, will not leak.



## Avoid memory leaks with Anonymous Class, Handler, Timer Task, Thread


<br>In android, every developer uses `Anonymous Class` (Runnable) at least once in a project. Any `Anonymous Class` has a reference to its parent (activity). If we perform a long-running task, the parent activity will not be destroyed until the task is ended.
<br>Example uses handler and Anonymous `Runnable` class. The memory will be leak when we quit the activity before the `Runnable` is finished.

```

 new Handler().postDelayed(new Runnable() {
        @Override
        public void run() {
            // do abc long 5s or so
        }
    }, 10000); // run "do abc" after 10s. It same as timer, thread...

```

How do we solve it?

1. Dont do any long operating with `Anonymous Class` or we need a `Static class` for it and pass `WeakReference` into it (such as activity, view...). `Thread` is the same with `Anonymous Class`.
1. Cancel the `Handler`, `Timer` when activity is destroyed.

