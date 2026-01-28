---
metaTitle: "Android - Exceptions"
description: "ActivityNotFoundException, NetworkOnMainThreadException, OutOfMemoryError, DexException, UncaughtException, Registering own Handler for unexpected exceptions"
---

# Exceptions



## ActivityNotFoundException


This is a very common `Exception`. It causes your application to stop during the start or execution of your app. In the `LogCat` you see the message:

```java
android.content.ActivityNotFoundException : Unable to find explicit activity class; 
have you declared this activity in your AndroidManifest.xml?

```

In this case, check if you have declared your activity in the `AndroidManifest.xml` file.

The simplest way to declare your `Activity` in `AndroidManifest.xml` is:

```java
<activity  android:name="com.yourdomain.YourStoppedActivity" />           

```



## NetworkOnMainThreadException


From [the documentation](https://developer.android.com/reference/android/os/NetworkOnMainThreadException.html):

> 
The exception that is thrown when an application attempts to perform a networking operation on its main thread.
This is only thrown for applications targeting the Honeycomb SDK or higher. Applications targeting earlier SDK versions are allowed to do networking on their main event loop threads, but it's heavily discouraged.


Here's an example of a code fragment that may cause that exception:

```java
public class MainActivity extends AppCompatActivity {

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        Uri.Builder builder = new Uri.Builder().scheme("http").authority("www.google.com");
        HttpURLConnection urlConnection = null;
        BufferedReader reader = null;
        URL url;
        try {
            url = new URL(builder.build().toString());
            urlConnection = (HttpURLConnection) url.openConnection();
            urlConnection.setRequestMethod("GET");
            urlConnection.connect();
        } catch (IOException e) {
            Log.e("TAG","Connection error", e);
        } finally{
            if (urlConnection != null) {
                urlConnection.disconnect();
            }
            if (reader != null) {
                try {
                    reader.close();
                } catch (final IOException e) {
                    Log.e("TAG", "Error closing stream", e);
                }
            }
        }
    }
}  

```

Above code will throw `NetworkOnMainThreadException` for applications targeting Honeycomb SDK (Android v3.0) or higher as the application is trying to perform a network operation on the main thread.

To avoid this exception, your network operations must always run in a background task via an `AsyncTask`, `Thread`, `IntentService`, etc.

```java
private class MyAsyncTask extends AsyncTask<String, Integer, Void> {

    @Override
    protected Void doInBackground(String[] params) {
        Uri.Builder builder = new Uri.Builder().scheme("http").authority("www.google.com");
        HttpURLConnection urlConnection = null;
        BufferedReader reader = null;
        URL url;
        try {
            url = new URL(builder.build().toString());
            urlConnection = (HttpURLConnection) url.openConnection();
            urlConnection.setRequestMethod("GET");
            urlConnection.connect();
        } catch (IOException e) {
            Log.e("TAG","Connection error", e);
        } finally{
            if (urlConnection != null) {
                urlConnection.disconnect();
            }
            if (reader != null) {
                try {
                    reader.close();
                } catch (final IOException e) {
                    Log.e("TAG", "Error closing stream", e);
                }
            }
        }

        return null;
    }
} 

```



## OutOfMemoryError


This is a runtime error that happens when you request a large amount of memory on the heap. This is common when loading a Bitmap into an ImageView.

You have some options:

1. Use a large application heap

Add the "largeHeap" option to the application tag in your AndroidManifest.xml. This will make more memory available to your app but will likely not fix the root issue.

```java
<application largeHeap="true" ... >

```


1. Recycle your bitmaps

After loading a bitmap, be sure to recycle it and free up memory:

```

   if (bitmap != null && !bitmap.isRecycled())
       bitmap.recycle();

```


1. Load sampled bitmaps into memory

Avoid loading the entire bitmap into memory at once by sampling a reduced size, using BitmapOptions and inSampleSize.

See [Android documentation](https://developer.android.com/training/displaying-bitmaps/load-bitmap.html#load-bitmap) for example



## DexException


```java
com.android.dex.DexException: Multiple dex files define Lcom/example/lib/Class;

```

This error occurs because the app, when packaging, finds two `.dex` files that define the same set of methods.

Usually this happens because the app has accidentally acquired 2 separate dependencies on the same library.

For instance, say you have a project, and you want to rely on two libraries: `A` and `B`, which each have their own dependencies. If library `B` already has a dependency on library `A`, this error will be thrown if library `A` is added to the project by itself. Compiling library `B` already gave the set of code from `A`, so when the compiler goes to bundle library `A`, it finds library `A`'s methods already packaged.

To resolve, make sure that none of your dependencies could accidentally be added twice in such a manner



## UncaughtException


If you want to handle uncaught exceptions try to catch them all in onCreate method of you Application class:

```java
public class MyApp extends Application {
    @Override
    public void onCreate() {
        super.onCreate();
        try {
            Thread
                .setDefaultUncaughtExceptionHandler(
                            new Thread.UncaughtExceptionHandler() {

                @Override
                public void uncaughtException(Thread thread, Throwable e) {
                    Log.e(TAG, 
                            "Uncaught Exception thread: "+thread.getName()+"
                             "+e.getStackTrace());
                    handleUncaughtException (thread, e);
                }
            });
        } catch (SecurityException e) {
            Log.e(TAG, 
                    "Could not set the Default Uncaught Exception Handler:"
                    +e.getStackTrace());
        }
    }

    private void handleUncaughtException (Thread thread, Throwable e){
        Log.e(TAG, "uncaughtException:");
        e.printStackTrace();
    }
}

```



## Registering own Handler for unexpected exceptions


This is how you can react to exceptions which have not been catched, similar to the system's standard "Application XYZ has crashed"

```java
import android.app.Application;
import android.util.Log;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Locale;

/**
 * Application class writing unexpected exceptions to a crash file before crashing.
 */
public class MyApplication extends Application {
    private static final String TAG = "ExceptionHandler";

    @Override
    public void onCreate() {
        super.onCreate();

        // Setup handler for uncaught exceptions.
        final Thread.UncaughtExceptionHandler defaultHandler = Thread.getDefaultUncaughtExceptionHandler();
        Thread.setDefaultUncaughtExceptionHandler(new Thread.UncaughtExceptionHandler() {
            @Override
            public void uncaughtException(Thread thread, Throwable e) {
                try {
                    handleUncaughtException(e);
                    System.exit(1);
                } catch (Throwable e2) {
                    Log.e(TAG, "Exception in custom exception handler", e2);
                    defaultHandler.uncaughtException(thread, e);
                }
            }
        });
    }

    private void handleUncaughtException(Throwable e) throws IOException {
        Log.e(TAG, "Uncaught exception logged to local file", e);

        // Create a new unique file
        final DateFormat dateFormat =  new SimpleDateFormat("yyyy-MM-dd_HH-mm-ss", Locale.US);
        String timestamp;
        File file = null;
        while (file == null || file.exists()) {
            timestamp = dateFormat.format(new Date());
            file = new File(getFilesDir(), "crashLog_" + timestamp + ".txt");
        }
        Log.i(TAG, "Trying to create log file " + file.getPath());
        file.createNewFile();

        // Write the stacktrace to the file
        FileWriter writer = null;
        try {
            writer = new FileWriter(file, true);
            for (StackTraceElement element : e.getStackTrace()) {
                writer.write(element.toString());
            }
        } finally {
            if (writer != null) writer.close();
        }

        // You can (and probably should) also display a dialog to notify the user
    }
}

```

Then register this Application class in your AndroidManifest.xml:

```java
<application android:name="de.ioxp.arkmobile.MyApplication" >

```

