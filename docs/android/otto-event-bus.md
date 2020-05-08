---
metaTitle: "Android - Otto Event Bus"
description: "Passing an event, Receiving an event"
---

# Otto Event Bus



## Passing an event


This example describes passing an event using the [Otto Event Bus](http://square.github.io/otto/).

To use the Otto Event Bus in **Android Studio** you have to insert the following statement in your modules gradle file:

```java
dependencies {
    compile 'com.squareup:otto:1.3.8'
}

```

The event we'd like to pass is a simple Java object:

```java
public class DatabaseContentChangedEvent {
    public String message;

    public DatabaseContentChangedEvent(String message) {
        this.message = message;
    }
}

```

We need a Bus to send events. This is typically a singleton:

```java
import com.squareup.otto.Bus;

public final class BusProvider {
    private static final Bus mBus = new Bus();

    public static Bus getInstance() {
        return mBus;
    }

    private BusProvider() {
    }
}

```

To send an event we only need our BusProvider and it's `post` method. Here we send an event if the action of an AsyncTask is completed:

```java
public abstract class ContentChangingTask extends AsyncTask<Object, Void, Void> {

    ...

    @Override
    protected void onPostExecute(Void param) {
        BusProvider.getInstance().post(
            new DatabaseContentChangedEvent("Content changed")
        );
    }
}

```



## Receiving an event


To receive an event it is necessary to implement a method with the event type as parameter and annotate it using `@Subscribe`. Furthermore you have to register/unregister the instance of your object at the `BusProvider` (see example **Sending an event**):

```java
public class MyFragment extends Fragment {
    private final static String TAG = "MyFragment";

    ...

    @Override
    public void onResume() {
        super.onResume();
        BusProvider.getInstance().register(this);
    }

    @Override
    public void onPause() {
        super.onPause();
        BusProvider.getInstance().unregister(this);
    }

    @Subscribe
    public void onDatabaseContentChanged(DatabaseContentChangedEvent event) {
        Log.i(TAG, "onDatabaseContentChanged: "+event.message);
    }
}

```

**Important:** In order to receive that event an instance of the class has to exist. This is usually not the case when you want to send a result from one activity to another activity. So check your use case for the event bus.



#### Remarks


**Otto** is [deprecated](https://github.com/square/otto#deprecated) in favor of `RxJava` and `RxAndroid`. These projects permit the same event-driven programming model as Otto, but they’re more capable and offer better control of threading.

