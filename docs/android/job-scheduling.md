---
metaTitle: "Android - Job Scheduling"
description: "Basic usage"
---

# Job Scheduling



## Basic usage


### Create a new JobService

This is done by extending the `JobService` class and implementing/overriding the required methods `onStartJob()` and `onStopJob()`.

```java
public class MyJobService extends JobService
{
    final String TAG = getClass().getSimpleName();
    
    @Override
    public boolean onStartJob(JobParameters jobParameters) {
        Log.i(TAG, "Job started");

        // ... your code here ...
        
        jobFinished(jobParameters, false);  // signal that we're done and don't want to reschedule the job
        return false;                       // finished: no more work to be done
    }

    @Override
    public boolean onStopJob(JobParameters jobParameters) {
        Log.w(TAG, "Job stopped");
        return false;
    }
}

```

### Add the new JobService to your AndroidManifest.xml

The following step is **mandatory**, otherwise you won't be able to run your job:

Declare your `MyJobService` class as a new `<service>` element between `<application> </application>` in your **AndroidManifest.xml**.

```java
<?xml version="1.0" encoding="utf-8"?>
<manifest xmlns:android="http://schemas.android.com/apk/res/android" package="com.example">
    <application
        android:allowBackup="true"
        android:icon="@mipmap/ic_launcher"
        android:label="@string/app_name"
        android:supportsRtl="true"
        android:theme="@style/AppTheme">
        <activity android:name=".MainActivity">
            <intent-filter>
                <action android:name="android.intent.action.MAIN" />
                <category android:name="android.intent.category.LAUNCHER" />
            </intent-filter>
        </activity>

        <service
            android:name=".MyJobService"
            android:permission="android.permission.BIND_JOB_SERVICE" />
    </application>
</manifest>

```

### Setup and run the job

After you implemented a new JobService and added it to your **AndroidManifest.xml**, you can continue with the final steps.

- `onButtonClick_startJob()` prepares and runs a periodical job. Besides periodic jobs, `JobInfo.Builder` allows to specify many other settings and constraints. For example you can define that a **plugged in charger** or a **network connection** is required to run the job.
- `onButtonClick_stopJob()` cancels all running jobs

```java
public class MainActivity extends AppCompatActivity
{
    final String TAG = getClass().getSimpleName();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
    }

    public void onButtonClick_startJob(View v) {
        // get the jobScheduler instance from current context
        JobScheduler jobScheduler = (JobScheduler) getSystemService(JOB_SCHEDULER_SERVICE);

        // MyJobService provides the implementation for the job
        ComponentName jobService = new ComponentName(getApplicationContext(), MyJobService.class);

        // define that the job will run periodically in intervals of 10 seconds
        JobInfo jobInfo = new JobInfo.Builder(1, jobService).setPeriodic(10 * 1000).build();

        // schedule/start the job
        int result = jobScheduler.schedule(jobInfo);
        if (result == JobScheduler.RESULT_SUCCESS)
            Log.d(TAG, "Successfully scheduled job: " + result);
        else
            Log.e(TAG, "RESULT_FAILURE: " + result);
    }

    public void onButtonClick_stopJob(View v) {
        JobScheduler jobScheduler = (JobScheduler) getSystemService(JOB_SCHEDULER_SERVICE);
        Log.d(TAG, "Stopping all jobs...");
        jobScheduler.cancelAll(); // cancel all potentially running jobs
    }
}

```

After calling `onButtonClick_startJob()`, the job will approximately run in intervals of 10 seconds, even when the app is in the **paused** state (user pressed home button and app is no longer visible).

Instead of cancelling all running jobs inside `onButtonClick_stopJob()`, you can also call `jobScheduler.cancel()` to cancel a specific job based on it's job ID.



#### Remarks


Beware of running lots of code or doing heavy work inside your `JobService`, for example in `onStartJob()`. The code will run on the **main/UI** thread and therefore can lead to a blocked UI, no longer responding app or even a crash of your app!

Because of that, you must offload the work, for example by using a `Thread` or `AsyncTask`.

