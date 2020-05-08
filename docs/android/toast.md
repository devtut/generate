---
metaTitle: "Android - Toast"
description: "Creating a custom Toast, Set position of a Toast, Showing a Toast Message, Show Toast Message Above Soft Keyboard, Thread safe way of displaying Toast (Application Wide), Thread safe way of displaying a Toast Message (For AsyncTask)"
---

# Toast


A [Toast](https://developer.android.com/guide/topics/ui/notifiers/toasts.html) provides simple feedback about an operation in a small popup and automatically disappears after a timeout. It only fills the amount of space required for the message and the current activity remains visible and interactive.



## Creating a custom Toast


If you don't want to use the default Toast view, you can provide your own using the `setView(View)` method on a `Toast` object.

First, create the XML layout you would like to use in your Toast.

```java
<LinearLayout xmlns:android="http://schemas.android.com/apk/res/android"
    android:id="@+id/toast_layout_root"
    android:orientation="vertical"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:padding="8dp"
    android:background="#111">

    <TextView android:id="@+id/title"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:textColor="#FFF"/>

    <TextView android:id="@+id/description"
        android:layout_width="wrap_content"
        android:layout_height="wrap_content"
        android:textColor="#FFF"/>

</LinearLayout>

```

Then, when creating your Toast, inflate your custom View from XML, and call `setView`

```java
// Inflate the custom view from XML
LayoutInflater inflater = getLayoutInflater();
View layout = inflater.inflate(R.layout.custom_toast_layout,
                               (ViewGroup) findViewById(R.id.toast_layout_root));

// Set the title and description TextViews from our custom layout
TextView title = (TextView) layout.findViewById(R.id.title);
title.setText("Toast Title");

TextView description = (TextView) layout.findViewById(R.id.description);
description.setText("Toast Description");

// Create and show the Toast object

Toast toast = new Toast(getApplicationContext());
toast.setGravity(Gravity.CENTER, 0, 0);
toast.setDuration(Toast.LENGTH_LONG);
toast.setView(layout);
toast.show();

```



## Set position of a Toast


A standard toast notification appears at the bottom of the screen aligned in horizontal centre. You can change this position with the `setGravity(int, int, int)`. This accepts three parameters: a Gravity constant, an x-position offset, and a y-position offset.

For example, if you decide that the toast should appear in the top-left corner, you can set the gravity like this:

```java
toast.setGravity(Gravity.TOP|Gravity.LEFT, 0, 0);

```



## Showing a Toast Message


In Android, a Toast is a simple UI element that can be used to give contextual feedback to a user.

To display a simple Toast message, we can do the following.

```java
// Declare the parameters to use for the Toast

Context context = getApplicationContext(); 
// in an Activity, you may also use "this"
// in a fragment, you can use getActivity()

CharSequence message = "I'm an Android Toast!";
int duration = Toast.LENGTH_LONG; // Toast.LENGTH_SHORT is the other option

// Create the Toast object, and show it!
Toast myToast = Toast.makeText(context, message, duration);
myToast.show();

```

Or, to show a Toast inline, without holding on to the Toast object you can:

```java
Toast.makeText(context, "Ding! Your Toast is ready.", Toast.LENGTH_SHORT).show();

```

**IMPORTANT:** Make sure that the [`show()`](https://developer.android.com/reference/android/widget/Toast.html#show()) method is called from the UI thread. If you're trying to show a `Toast` from a different thread you can e.g. use [`runOnUiThread`](https://developer.android.com/reference/android/app/Activity.html#runOnUiThread(java.lang.Runnable)) method of an [`Activity`](https://developer.android.com/reference/android/app/Activity.html).

Failing to do so, meaning trying to modify the UI by creating a Toast, will throw a `RuntimeException` which will look like this:

```java
java.lang.RuntimeException: Can't create handler inside thread that has not called Looper.prepare()

```

The simplest way of handling this exception is just by using runOnUiThread: syntax is shown below.

```

  runOnUiThread(new Runnable() {
        @Override
        public void run() {
            // Your code here
        }
    });

```



## Show Toast Message Above Soft Keyboard


By default, Android will display Toast messages at the bottom of the screen even if the keyboard is showing. This will show a Toast message just above the keyboard.

```java
public void showMessage(final String message, final int length) {
    View root = findViewById(android.R.id.content);
    Toast toast = Toast.makeText(this, message, length);
    int yOffset = Math.max(0, root.getHeight() - toast.getYOffset());
    toast.setGravity(Gravity.TOP | Gravity.CENTER_HORIZONTAL, 0, yOffset);
    toast.show();
}

```



## Thread safe way of displaying Toast (Application Wide)


```java
public class MainApplication extends Application {
    
    private static Context context; //application context

    private Handler mainThreadHandler;
    private Toast toast;

    public Handler getMainThreadHandler() {
        if (mainThreadHandler == null) {
            mainThreadHandler = new Handler(Looper.getMainLooper());
        }
        return mainThreadHandler;
    }

    @Override public void onCreate() {
        super.onCreate();
        context = this;
    }
       
    public static MainApplication getApp(){
        return (MainApplication) context;
    }

    /**
     * Thread safe way of displaying toast.
     * @param message
     * @param duration
     */
    public void showToast(final String message, final int duration) {
        getMainThreadHandler().post(new Runnable() {
            @Override
            public void run() {
                if (!TextUtils.isEmpty(message)) {
                    if (toast != null) {
                        toast.cancel(); //dismiss current toast if visible
                        toast.setText(message);
                    } else {
                        toast = Toast.makeText(App.this, message, duration);
                    }
                    toast.show();
                }
            }
        });
    }

```

Remember to add `MainApplication` in `manifest`.

Now call it from any thread to display a toast message.

```java
MainApplication.getApp().showToast("Some message", Toast.LENGTH_LONG);

```



## Thread safe way of displaying a Toast Message (For AsyncTask)


If you don't want to extend Application and keep your toast messages thread safe, make sure you show them in the post execute section of your AsyncTasks.

```java
public class MyAsyncTask extends AsyncTask <Void, Void, Void> {

    @Override
    protected Void doInBackground(Void... params) {
        // Do your background work here
    }

    @Override
    protected void onPostExecute(Void aVoid) {
        // Show toast messages here
        Toast.makeText(context, "Ding! Your Toast is ready.",   Toast.LENGTH_SHORT).show();
    }
    
}

```



#### Syntax


<li>Toast makeText (Context context,
CharSequence text,
int duration)</li>
<li>Toast makeText (Context context,
int resId,
int duration)</li>
- void setGravity(int gravity, int xOffset, int yOffset)
- void show()



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|context|The context to display your Toast in. `this` is commonly used in an Activity and `getActivity()` is commonly used in a Fragment
|text|A CharSequence that specifies what text will be shown in the Toast. Any object that implements CharSequence can be used, including a String
|resId|A resource ID that can be used to provide a resource String to display in the Toast
|duration|Integer flag representing how long the Toast will show. Options are `Toast.LENGTH_SHORT` and `Toast.LENGTH_LONG`
|gravity|Integer specifying the position, or "gravity" of the Toast. See options [here](https://developer.android.com/reference/android/view/Gravity.html)
|xOffset|Specifies the horizontal offset for the Toast position
|yOffset|Specifies the vertical offset for the Toast position



#### Remarks


A toast provides simple feedback about an operation in a small popup. It only fills the amount of space required for the message and the current activity remains visible and interactive.

A more recent alternative to Toast is SnackBar. SnackBar offers an updated visual style and allows the user to dismiss the message or take further action. See the [SnackBar](http://stackoverflow.com/documentation/android/1500/snackbar#t=201607221136212178305) documentation for details.

### Official Documentation:

[https://developer.android.com/reference/android/widget/Toast.html](https://developer.android.com/reference/android/widget/Toast.html)

