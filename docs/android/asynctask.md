---
metaTitle: "Android - AsyncTask"
description: "Basic Usage, Pass Activity as WeakReference to avoid memory leaks, Download Image using AsyncTask in Android, Canceling AsyncTask, AsyncTask: Serial Execution and Parallel Execution of Task, Publishing progress, Order of execution"
---

# AsyncTask




## Basic Usage


In Android [Activities](https://developer.android.com/reference/android/app/Activity.html) and [Services](https://developer.android.com/reference/android/app/Service.html), most callbacks are run on the [main thread](https://developer.android.com/guide/components/processes-and-threads.html#Threads). This makes it simple to update the UI, but running processor- or I/O-heavy tasks on the main thread can cause your UI to pause and become unresponsive ([official documentation](https://developer.android.com/training/articles/perf-anr.html) on what then happens).

You can remedy this by putting these heavier tasks on a background thread.

One way to do this is using an [AsyncTask](https://developer.android.com/reference/android/os/AsyncTask.html), which provides a framework to facilitate easy usage of a background Thread, and also perform UI Thread tasks before, during, and after the background Thread has completed its work.

**Methods that can be overridden when extending `AsyncTask`:**

- `onPreExecute()` : invoked on the **UI thread** before the task is executed
- `doInBackground()`: invoked on **the background thread** immediately after `onPreExecute()` finishes executing.
- `onProgressUpdate()`:  invoked on the **UI thread** after a call to `publishProgress(Progress...)`.
- `onPostExecute()`: invoked on the **UI thread** after the background computation finishes

### Example

```java
public class MyCustomAsyncTask extends AsyncTask<File, Void, String> {

    
    @Override
    protected void onPreExecute(){
        // This runs on the UI thread before the background thread executes.
        super.onPreExecute();
        // Do pre-thread tasks such as initializing variables. 
        Log.v("myBackgroundTask", "Starting Background Task");  
    }

    @Override
    protected String doInBackground(File... params) {
        // Disk-intensive work. This runs on a background thread.
        // Search through a file for the first line that contains "Hello", and return
        // that line.
        try (Scanner scanner = new Scanner(params[0])) {
            while (scanner.hasNextLine()) {
                final String line = scanner.nextLine();
                publishProgress(); // tell the UI thread we made progress

                if (line.contains("Hello")) {
                    return line;
                }
            }
            return null;
        }
    }

    @Override
    protected void onProgressUpdate(Void...p) {
        // Runs on the UI thread after publishProgress is invoked
        Log.v("Read another line!")
    }        

    @Override
    protected void onPostExecute(String s) {
        // This runs on the UI thread after complete execution of the doInBackground() method
        // This function receives result(String s) returned from the doInBackground() method.
        // Update UI with the found string.
        TextView view = (TextView) findViewById(R.id.found_string);
        if (s != null) {
            view.setText(s);
        } else {
            view.setText("Match not found.");
        }
    }

}

```

### Usage:

```java
MyCustomAsyncTask asyncTask = new MyCustomAsyncTask<File, Void, String>();
// Run the task with a user supplied filename.
asyncTask.execute(userSuppliedFilename);

```

or simply:

```java
new MyCustomAsyncTask().execute(userSuppliedFilename);

```

### Note

When defining an `AsyncTask` we can pass three types between `< >` brackets.<br />
Defined as `<Params, Progress, Result>` (see **Parameters section**)

In the previous example we've used types `<File, Void, String>`:

```java
AsyncTask<File, Void, String>
// Params has type File
// Progress has unused type
// Result has type String

```

[`Void`](https://developer.android.com/reference/java/lang/Void.html) is used when you want to mark a type as unused.

Note that you can't pass primitive types (i.e. `int`, `float` and 6 others) as parameters. In such cases, you should pass their [wrapper classes](https://en.wikipedia.org/wiki/Primitive_wrapper_class), e.g. `Integer` instead of `int`, or `Float` instead of `float`.

**The AsyncTask and Activity life cycle**

AsyncTasks don't follow Activity instances' life cycle. If you start an AsyncTask inside an Activity and you rotate the device, the Activity will be destroyed and a new instance will be created. But the AsyncTask will not die. It will go on living until it completes.

**Solution: AsyncTaskLoader**

One subclass of [Loaders](https://developer.android.com/reference/android/content/Loader.html) is the AsyncTaskLoader. This class performs the same function as the AsyncTask, but much better. It can handle Activity configuration changes more easily, and it behaves within the life cycles of Fragments and Activities. The nice thing is that the AsyncTaskLoader can be used in any situation that the AsyncTask is being used. Anytime data needs to be loaded into memory for the Activity/Fragment to handle, The AsyncTaskLoader can do the job better.



## Pass Activity as WeakReference to avoid memory leaks


It is common for an AsyncTask to require a reference to the Activity that called it.

If the AsyncTask is an inner class of the Activity, then you can reference it and any member variables/methods directly.

If, however, the AsyncTask is not an inner class of the Activity, you will need to pass an Activity reference to the AsyncTask.
When you do this, one potential problem that may occur is that the AsyncTask will keep the reference of the Activity until the AsyncTask has completed its work in its background thread. If the Activity is finished or killed before the AsyncTask's background thread work is done, the AsyncTask will still have its reference to the Activity, and therefore it cannot be garbage collected.

As a result, this will cause a memory leak.

In order to prevent this from happening, make use of a [WeakReference](https://developer.android.com/reference/java/lang/ref/WeakReference.html) in the AsyncTask instead of having a direct reference to the Activity.

Here is an example AsyncTask that utilizes a WeakReference:

```java
private class MyAsyncTask extends AsyncTask<String, Void, Void> {

    private WeakReference<Activity> mActivity;

    public MyAsyncTask(Activity activity) {
        mActivity = new WeakReference<Activity>(activity);
    }

    @Override
    protected void onPreExecute() {
        final Activity activity = mActivity.get();
        if (activity != null) {
            ....
        }
    }

    @Override
    protected Void doInBackground(String... params) {
        //Do something
        String param1 = params[0];
        String param2 = params[1];
        return null;
    }

    @Override
    protected void onPostExecute(Void result) {
        final Activity activity = mActivity.get();
        if (activity != null) {
            activity.updateUI();
        }
    }
} 

```

**Calling the AsyncTask from an Activity:**

```java
new MyAsyncTask(this).execute("param1", "param2");

```

**Calling the AsyncTask from a Fragment:**

```java
new MyAsyncTask(getActivity()).execute("param1", "param2");

```



## Download Image using AsyncTask in Android


This tutorial explains how to download Image using AsyncTask in Android. The example below download image while showing progress bar while during download.
<h3>Understanding Android AsyncTask</h3>
Async task enables you to implement MultiThreading without get Hands dirty into threads. AsyncTask enables proper and easy use of the UI thread. It allows performing background operations and passing the results on the UI thread. If you are doing something isolated related to UI, for example downloading data to present in a list, go ahead and use AsyncTask.

     - AsyncTasks should ideally be used for short operations (a few seconds at the most.)
     - An asynchronous task is defined by 3 generic types, called Params, Progress and Result, and 4 steps, called `onPreExecute()`, `doInBackground()`, `onProgressUpdate()` and `onPostExecute()`.
     - In `onPreExecute()` you can define code, which need to be executed before background processing starts.
     - doInBackground have code which needs to be executed in background, here in `doInBackground()` we can send results to multiple times to event thread by publishProgress() method, to notify background processing has been completed we can return results simply.
     - `onProgressUpdate()` method receives progress updates from `doInBackground()` method, which is published via `publishProgress()` method, and this method can use this progress update to update event thread
     - `onPostExecute()` method handles results returned by `doInBackground()` method.
     <li>The generic types used are
<ul>
     - Params, the type of the parameters sent to the task upon execution
     - Progress, the type of the progress units published during the background computation.
     - Result, the type of the result of the background computation.

### Downloading image using Android AsyncTask

**your .xml layout**

```java
<?xml version="1.0" encoding="utf-8"?>

<LinearLayout 
    xmlns:android="http://schemas.android.com/apk/res/android"
    android:layout_width="fill_parent"
    android:layout_height="fill_parent"
    android:orientation="vertical" >

<Button
    android:id="@+id/downloadButton"
    android:layout_width="match_parent"
    android:layout_height="wrap_content"
    android:text="Click Here to Download" />

<ImageView
    android:id="@+id/imageView"
    android:layout_width="match_parent"
    android:layout_height="match_parent"
    android:contentDescription="Your image will appear here" />

</LinearLayout>

```

**.java class**

```java
package com.javatechig.droid;

import java.io.InputStream;
import org.apache.http.HttpEntity;
import org.apache.http.HttpResponse;
import org.apache.http.HttpStatus;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.impl.client.DefaultHttpClient;
import android.app.Activity;
import android.app.ProgressDialog;
import android.graphics.Bitmap;
import android.graphics.BitmapFactory;
import android.os.AsyncTask;
import android.os.Bundle;
import android.util.Log;
import android.view.View;
import android.view.View.OnClickListener;
import android.widget.Button;
import android.widget.ImageView;

public class ImageDownladerActivity extends Activity {

    private ImageView downloadedImg;
    private ProgressDialog simpleWaitDialog;
    private String downloadUrl = "http://www.9ori.com/store/media/images/8ab579a656.jpg";

    @Override
    public void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.asynch);
        Button imageDownloaderBtn = (Button) findViewById(R.id.downloadButton);

        downloadedImg = (ImageView) findViewById(R.id.imageView);

        imageDownloaderBtn.setOnClickListener(new OnClickListener() {

            @Override
            public void onClick(View v) {
                // TODO Auto-generated method stub
                new ImageDownloader().execute(downloadUrl);
            }

        });
    }

    private class ImageDownloader extends AsyncTask {

        @Override
        protected Bitmap doInBackground(String... param) {
            // TODO Auto-generated method stub
            return downloadBitmap(param[0]);
        }

        @Override
        protected void onPreExecute() {
            Log.i("Async-Example", "onPreExecute Called");
            simpleWaitDialog = ProgressDialog.show(ImageDownladerActivity.this,
                    "Wait", "Downloading Image");

        }

        @Override
        protected void onPostExecute(Bitmap result) {
            Log.i("Async-Example", "onPostExecute Called");
            downloadedImg.setImageBitmap(result);
            simpleWaitDialog.dismiss();

        }

        private Bitmap downloadBitmap(String url) {
            // initilize the default HTTP client object
            final DefaultHttpClient client = new DefaultHttpClient();

            //forming a HttpGet request 
            final HttpGet getRequest = new HttpGet(url);
            try {

                HttpResponse response = client.execute(getRequest);

                //check 200 OK for success
                final int statusCode = response.getStatusLine().getStatusCode();

                if (statusCode != HttpStatus.SC_OK) {
                    Log.w("ImageDownloader", "Error " + statusCode + 
                            " while retrieving bitmap from " + url);
                    return null;

                }

                final HttpEntity entity = response.getEntity();
                if (entity != null) {
                    InputStream inputStream = null;
                    try {
                        // getting contents from the stream 
                        inputStream = entity.getContent();

                        // decoding stream data back into image Bitmap that android understands
                        final Bitmap bitmap = BitmapFactory.decodeStream(inputStream);

                        return bitmap;
                    } finally {
                        if (inputStream != null) {
                            inputStream.close();
                        }
                        entity.consumeContent();
                    }
                }
            } catch (Exception e) {
                // You Could provide a more explicit error message for IOException
                getRequest.abort();
                Log.e("ImageDownloader", "Something went wrong while" +
                        " retrieving bitmap from " + url + e.toString());
            } 

            return null;
        }
    }
}

```

Since there is currently no comment field for examples (or I haven't found it or I haven't permission for it) here is some comment about this:

This is a good example what can be done with AsyncTask.

However the example currently has problems with

- possible memory leaks
- app crash if there was a screen rotation shortly before the async task finished.

For details see:

- [Pass Activity as WeakReference to avoid memory leaks](http://stackoverflow.com/documentation/android/117/asynctask/14069/pass-activity-as-weakreference-to-avoid-memory-leaks)
- [http://stackoverflow.com/documentation/android/117/asynctask/5377/possible-problems-with-inner-async-tasks](http://stackoverflow.com/documentation/android/117/asynctask/5377/possible-problems-with-inner-async-tasks)
- [Avoid leaking Activities with AsyncTask](http://stackoverflow.com/documentation/android/2687/common-memory-leaks-and-how-to-avoid-them/9248/avoid-leaking-activities-with-asynctask)



## Canceling AsyncTask


```java
YourAsyncTask task = new YourAsyncTask();
task.execute();
task.cancel();

```

This doesn't stop your task if it was in progress, it just sets the cancelled flag which can be checked by checking the return value of `isCancelled()` (assuming your code is currently running) by doing this:

```java
class YourAsyncTask extends AsyncTask<Void, Void, Void> {
    @Override
    protected Void doInBackground(Void... params) {
        while(!isCancelled()) {
            ... doing long task stuff
            //Do something, you need, upload part of file, for example
            if (isCancelled()) {    
                return null; // Task was detected as canceled
            }
            if (yourTaskCompleted) {
                return null;
            }
        }
    }
}

```

### Note

If an AsyncTask is canceled while `doInBackground(Params... params)` is still executing then the method `onPostExecute(Result result)` will **NOT** be called after `doInBackground(Params... params)` returns. The AsyncTask will instead call the `onCancelled(Result result)` to indicate that the task was cancelled during execution.



## AsyncTask: Serial Execution and Parallel Execution of Task


AsyncTask is an abstract Class and does not inherit the `Thread` class. It has an **abstract** method `doInBackground(Params... params)`, which is overridden to perform the task. This method is called from `AsyncTask.call()`.

Executor are part of `java.util.concurrent` package.

Moreover, AsyncTask contains 2 `Executor`s

### `THREAD_POOL_EXECUTOR`

It uses worker threads to execute the tasks parallelly.

```java
public static final Executor THREAD_POOL_EXECUTOR = new ThreadPoolExecutor(CORE_POOL_SIZE, MAXIMUM_POOL_SIZE, KEEP_ALIVE, TimeUnit.SECONDS, sPoolWorkQueue, sThreadFactory);

```

### `SERIAL_EXECUTOR`

It executes the task serially, i.e. one by one.

```java
private static class SerialExecutor implements Executor { }

```

Both `Executor`s are **static**, hence only one `THREAD_POOL_EXECUTOR` and one `SerialExecutor` objects exist, but you can create several `AsyncTask` objects.

Therefore, if you try to do multiple background task with the default Executor (`SerialExecutor`), these task will be queue and executed serially.

If you try to do multiple background task with `THREAD_POOL_EXECUTOR`, then they will be executed parallelly.

Example:

```java
public class MainActivity extends Activity {
    private Button bt;
    private int CountTask = 0;
    private static final String TAG = "AsyncTaskExample";

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        bt = (Button) findViewById(R.id.button);
        bt.setOnClickListener(new View.OnClickListener() {
            @Override
            public void onClick(View v) {
                BackgroundTask backgroundTask = new BackgroundTask ();
                Integer data[] = { ++CountTask, null, null };

                // Task Executed in thread pool ( 1 )
                backgroundTask.executeOnExecutor(AsyncTask.THREAD_POOL_EXECUTOR, data);

                // Task executed Serially ( 2 )
                // Uncomment the below code and comment the above code of Thread
                // pool Executor and check
                // backgroundTask.execute(data);
                Log.d(TAG, "Task = " + (int) CountTask + " Task Queued");

            }
        });

    }

    private class BackgroundTask extends AsyncTask<Integer, Integer, Integer> {
        int taskNumber;

        @Override
        protected Integer doInBackground(Integer... integers) {
            taskNumber = integers[0];

            try {
                Thread.sleep(1000);
            } catch (InterruptedException e) {
                // TODO Auto-generated catch block
                e.printStackTrace();
            }

            Log.d(TAG, "Task = " + taskNumber + " Task Running in Background");

            publishProgress(taskNumber);
            return null;
        }

        @Override
        protected void onPreExecute() {
            super.onPreExecute();
        }

        @Override
        protected void onPostExecute(Integer aLong) {
            super.onPostExecute(aLong);
        }

        @Override
        protected void onProgressUpdate(Integer... values) {
            super.onProgressUpdate(values);
            Log.d(TAG, "Task = " + (int) values[0]
                    + " Task Execution Completed");
        }
    }
}

```

Perform Click on button several times to start a task and see the result.

### Task Executed in thread pool(1)

Each task takes 1000 ms to complete.

At t=36s, tasks 2, 3 and 4 are queued and started executing also because they are executing parallelly.

```java
08-02 19:48:35.815: D/AsyncTaskExample(11693): Task = 1 Task Queued
08-02 19:48:35.815: D/AsyncTaskExample(11693): Task = 1 Task Running in Background
08-02 19:48:**36.025**: D/AsyncTaskExample(11693): Task = 2 Task Queued
08-02 19:48:**36.025**: D/AsyncTaskExample(11693): Task = 2 Task Running in Background
08-02 19:48:**36.165**: D/AsyncTaskExample(11693): Task = 3 Task Queued
08-02 19:48:**36.165**: D/AsyncTaskExample(11693): Task = 3 Task Running in Background
08-02 19:48:**36.325**: D/AsyncTaskExample(11693): Task = 4 Task Queued
08-02 19:48:**36.325**: D/AsyncTaskExample(11693): Task = 4 Task Running in Background
08-02 19:48:**36.815**: D/AsyncTaskExample(11693): Task = 1 Task Execution Completed
08-02 19:48:**36.915**: D/AsyncTaskExample(11693): Task = 5 Task Queued
08-02 19:48:**36.915**: D/AsyncTaskExample(11693): Task = 5 Task Running in Background
08-02 19:48:37.025: D/AsyncTaskExample(11693): Task = 2 Task Execution Completed
08-02 19:48:37.165: D/AsyncTaskExample(11693): Task = 3 Task Execution Completed
----------

```

Comment `Task Executed in thread pool` (1) and uncomment `Task executed Serially` (2).

Perform Click on button several times to start a task and see the result.

It is executing the task serially hence every task is started after the current task completed execution.
Hence when Task 1's execution completes, only Task 2 starts running in background. Vice versa.

```java
08-02 19:42:57.505: D/AsyncTaskExample(10299): Task = 1 Task Queued
08-02 19:42:57.505: D/AsyncTaskExample(10299): Task = 1 Task Running in Background
08-02 19:42:57.675: D/AsyncTaskExample(10299): Task = 2 Task Queued
08-02 19:42:57.835: D/AsyncTaskExample(10299): Task = 3 Task Queued
08-02 19:42:58.005: D/AsyncTaskExample(10299): Task = 4 Task Queued
08-02 19:42:58.155: D/AsyncTaskExample(10299): Task = 5 Task Queued
08-02 19:42:58.505: D/AsyncTaskExample(10299): Task = 1 Task Execution Completed
08-02 19:42:58.505: D/AsyncTaskExample(10299): Task = 2 Task Running in Background
08-02 19:42:58.755: D/AsyncTaskExample(10299): Task = 6 Task Queued
08-02 19:42:59.295: D/AsyncTaskExample(10299): Task = 7 Task Queued
08-02 19:42:59.505: D/AsyncTaskExample(10299): Task = 2 Task Execution Completed
08-02 19:42:59.505: D/AsyncTaskExample(10299): Task = 3 Task Running in Background
08-02 19:43:00.035: D/AsyncTaskExample(10299): Task = 8 Task Queued
08-02 19:43:00.505: D/AsyncTaskExample(10299): Task = 3 Task Execution Completed
08-02 19:43:**00.505**: D/AsyncTaskExample(10299): Task = 4 Task Running in Background
08-02 19:43:**01.505**: D/AsyncTaskExample(10299): Task = 4 Task Execution Completed
08-02 19:43:**01.515**: D/AsyncTaskExample(10299): Task = 5 Task Running in Background
08-02 19:43:**02.515**: D/AsyncTaskExample(10299): Task = 5 Task Execution Completed
08-02 19:43:**02.515**: D/AsyncTaskExample(10299): Task = 6 Task Running in Background
08-02 19:43:**03.515**: D/AsyncTaskExample(10299): Task = 7 Task Running in Background
08-02 19:43:**03.515**: D/AsyncTaskExample(10299): Task = 6 Task Execution Completed
08-02 19:43:04.515: D/AsyncTaskExample(10299): Task = 8 Task Running in Background
08-02 19:43:**04.515**: D/AsyncTaskExample(10299): Task = 7 Task Execution Completed

```



## Publishing progress


Sometimes, we need to update the progress of the computation done by an `AsyncTask`. This progress could be represented by a string, an integer, etc. To do this, we have to use two functions. First, we need to set the `onProgressUpdate` function whose parameter type is the same as the second type parameter of our `AsyncTask`.

```java
class YourAsyncTask extends AsyncTask<URL, Integer, Long> {
    @Override
    protected void onProgressUpdate(Integer... args) {
        setProgressPercent(args[0])
    }
}

```

Second, we have to use the function `publishProgress` necessarily on the `doInBackground` function, and that is all, the previous method will do all the job.

```java
protected Long doInBackground(URL... urls) {
     int count = urls.length;
     long totalSize = 0;
     for (int i = 0; i < count; i++) {
         totalSize += Downloader.downloadFile(urls[i]);
         publishProgress((int) ((i / (float) count) * 100));
     }
     return totalSize;
 }

```



## Order of execution


When first introduced, `AsyncTasks` were executed serially on a single background thread. Starting with `DONUT`, this was changed to a pool of threads allowing multiple tasks to operate in parallel. Starting with `HONEYCOMB`, tasks are executed on a single thread to avoid common application errors caused by parallel execution.

If you truly want parallel execution, you can invoke `executeOnExecutor(java.util.concurrent.Executor, Object[])` with `THREAD_POOL_EXECUTOR`.

> 
<p>SERIAL_EXECUTOR -> An Executor that executes tasks one at a time in
serial order.</p>
<p>THREAD_POOL_EXECUTOR -> An Executor that can be used to execute tasks in
parallel.</p>


**sample :**

```java
Task task = new Task();
if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.HONEYCOMB)
    task.executeOnExecutor(AsyncTask.SERIAL_EXECUTOR, data);
else
    task.execute(data);

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|Params|the type of the parameters sent to the task upon execution.
|Progress|the type of the progress units published during the background computation
|Result|the type of the result of the background computation.

