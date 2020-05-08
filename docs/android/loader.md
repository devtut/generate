---
metaTitle: "Android - Loader"
description: "Basic AsyncTaskLoader, AsyncTaskLoader with cache, Reloading, Pass parameters using a Bundle"
---

# Loader


Loader is good choice for prevent memory leak if you want to load data in background when oncreate method is called.
For example when we execute Asynctask in oncreate method and we rotate the screen  so the activity will recreate which will execute another AsyncTask again, so probably two Asyntask running in parallel together rather than like loader which will continue the background process we executed before.



## Basic AsyncTaskLoader


`AsyncTaskLoader` is an abstract `Loader` that provides an `AsyncTask` to do the work.

Here some basic implementation:

```java
final class BasicLoader extends AsyncTaskLoader<String> {

    public BasicLoader(Context context) {
        super(context);
    }

    @Override
    public String loadInBackground() {
        // Some work, e.g. load something from internet
        return "OK";
    }

    @Override
    public void deliverResult(String data) {
        if (isStarted()) {
            // Deliver result if loader is currently started
            super.deliverResult(data);
        }
    }

    @Override
    protected void onStartLoading() {
        // Start loading
        forceLoad();
    }

    @Override
    protected void onStopLoading() {
        cancelLoad();
    }

    @Override
    protected void onReset() {
        super.onReset();

        // Ensure the loader is stopped
        onStopLoading();
    }
}

```

Typically `Loader` is initialized within the activity's `onCreate()` method, or within the fragment's `onActivityCreated()`. Also usually activity or fragment implements `LoaderManager.LoaderCallbacks` interface:

```java
public class MainActivity extends Activity implements LoaderManager.LoaderCallbacks<String> {

    // Unique id for loader
    private static final int LDR_BASIC_ID = 1;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        // Initialize loader; Some data can be passed as second param instead of Bundle.Empty
        getLoaderManager().initLoader(LDR_BASIC_ID, Bundle.EMPTY, this);
    }

    @Override
    public Loader<String> onCreateLoader(int id, Bundle args) {
        return new BasicLoader(this);
    }

    @Override
    public void onLoadFinished(Loader<String> loader, String data) {
        Toast.makeText(this, data, Toast.LENGTH_LONG).show();
    }

    @Override
    public void onLoaderReset(Loader<String> loader) {
    }
}

```

In this example, when loader completed, toast with result will be shown.



## AsyncTaskLoader with cache


It's a good practice to cache loaded result to avoid multiple loading of same data.<br/> To invalidate cache `onContentChanged()` should be called. If loader has been already started, `forceLoad()` will be called, otherwise (if loader in stopped state) loader will be able to understand content change with `takeContentChanged()` check.

**Remark: `onContentChanged()` must be called from the process's main thread.**

**Javadocs says about takeContentChanged():**

> 
<p>Take the current flag indicating whether the loader's content had
changed while it was stopped.  If it had, true is returned and the
flag is cleared.</p>


```java
public abstract class BaseLoader<T> extends AsyncTaskLoader<T> {

    // Cached result saved here
    private final AtomicReference<T> cache = new AtomicReference<>();

    public BaseLoader(@NonNull final Context context) {
        super(context);
    }

    @Override
    public final void deliverResult(final T data) {
        if (!isReset()) {
            // Save loaded result
            cache.set(data);
            if (isStarted()) {
                super.deliverResult(data);
            }
        }
    }

    @Override
    protected final void onStartLoading() {            
        // Register observers
        registerObserver();

        final T cached = cache.get();    
        // Start new loading if content changed in background
        // or if we never loaded any data
        if (takeContentChanged() || cached == null) {
            forceLoad();
        } else {
            deliverResult(cached);
        }
    }

    @Override
    public final void onStopLoading() {
        cancelLoad();
    }

    @Override
    protected final void onReset() {
        super.onReset();
        onStopLoading();
        // Clear cache and remove observers
        cache.set(null);
        unregisterObserver();
    }

    /* virtual */
    protected void registerObserver() {
        // Register observers here, call onContentChanged() to invalidate cache
    }

    /* virtual */
    protected void unregisterObserver() {
        // Remove observers
    }
}

```



## Reloading


To invalidate your old data and restart existing loader you can use [`restartLoader()`](https://developer.android.com/reference/android/app/LoaderManager.html#restartLoader(int,%20android.os.Bundle,%20android.app.LoaderManager.LoaderCallbacks%3CD%3E)) method:

```java
private void reload() {
    getLoaderManager().reastartLoader(LOADER_ID, Bundle.EMPTY, this);
}

```



## Pass parameters using a Bundle


You can pass parameters by Bundle:

```java
Bundle myBundle = new Bundle();
myBundle.putString(MY_KEY, myValue);

```

Get the value in onCreateLoader:

```java
@Override
public Loader<String> onCreateLoader(int id, final Bundle args) {
    final String myParam = args.getString(MY_KEY);
    ...
}

```



#### Parameters


|Class|Description
|---|---|---|---|---|---|---|---|---|---
|[LoaderManager](https://developer.android.com/reference/android/app/LoaderManager.html)|An abstract class associated with an [Activity](https://developer.android.com/reference/android/app/Activity.html) or [Fragment](https://developer.android.com/reference/android/app/Fragment.html) for managing one or more Loader instances.
|[LoaderManager.LoaderCallbacks](https://developer.android.com/reference/android/app/LoaderManager.LoaderCallbacks.html)|A callback interface for a client to interact with the LoaderManager.
|[Loader](https://developer.android.com/reference/android/content/Loader.html)|An abstract class that performs asynchronous loading of data.
|[AsyncTaskLoader](https://developer.android.com/reference/android/content/AsyncTaskLoader.html)|Abstract loader that provides an [AsyncTask](https://developer.android.com/reference/android/os/AsyncTask.html) to do the work.
|[CursorLoader](https://developer.android.com/reference/android/content/CursorLoader.html)|A subclass of AsyncTaskLoader that queries the ContentResolver and returns a Cursor.



#### Remarks


Introduced in Android 3.0, loaders make it easy to asynchronously load data in an activity or fragment. Loaders have these characteristics:

- They are available to every [Activity](https://developer.android.com/reference/android/app/Activity.html) and [Fragment](https://developer.android.com/reference/android/app/Fragment.html).
- They provide asynchronous loading of data.
- They monitor the source of their data and deliver new results when the content changes.
- They automatically reconnect to the last loader's cursor when being recreated after a configuration change. Thus, they don't need to re-query their data.

### When not to use Loaders

