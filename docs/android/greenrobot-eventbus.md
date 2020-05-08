---
metaTitle: "Android - GreenRobot EventBus"
description: "Passing a Simple Event, Receiving Events, Sending Events, Creating an Event object"
---

# GreenRobot EventBus



## Passing a Simple Event


The first thing we need to do it add EventBus to our module's gradle file:

```java
dependencies {
    ...
    compile 'org.greenrobot:eventbus:3.0.0'
    ...
}

```

Now we need to create a model for our event. It can contain anything we want to pass along. For now we'll just make an empty class.

```java
public class DeviceConnectedEvent
{
}

```

Now we can add the code to our `Activity` that will register with EventBus and subscribe to the event.

```java
public class MainActivity extends AppCompatActivity
{
    private EventBus _eventBus;

    @Override
    protected void onCreate (Bundle savedInstanceState)
    {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        _eventBus = EventBus.getDefault();
    }

    @Override
    protected void onStart ()
    {
        super.onStart();
        _eventBus.register(this);
    }

    @Override
    protected void onStop ()
    {
        _eventBus.unregister(this);
        super.onStop();
    }

    @Subscribe(threadMode = ThreadMode.MAIN)
    public void onDeviceConnected (final DeviceConnectedEvent event)
    {
        // Process event and update UI
    }
}

```

In this `Activity` we get an instance of `EventBus` in the `onCreate()` method. We register / unregister for events in `onStart()` / `onStop()`. It's important to remember to unregister when your listener loses scope or you could leak your `Activity`.

Finally we define the method that we want called with the event. The `@Subscribe` annotation tells EventBus which methods it can look for to handle events. You have to have at least one methods annotated with `@Subscribe` to register with EventBus or it will throw an exception. In the annotation we define the thread mode. This tells EventBus which thread to call the method on. It is a very handy way of passing information from a background thread to the UI thread! That's exactly what we're doing here. `ThreadMode.MAIN` means that this method will be called on Android's main UI thread so it's safe to do any UI manipulations here that you need. The name of the method doesn't matter. The only think, other that the `@Subscribe` annotation, that EventBus is looking for is the type of the argument. As long as the type matches it will be called when an event is posted.

The last thing we need to do it to post an event. This code will be in our `Service`.

```java
EventBus.getDefault().post(new DeviceConnectedEvent());

```

That's all there is to it! EventBus will take that DeviceConnectedEvent and look through its registered listeners, look through the methods that they've subscribed and find the ones that take a DeviceConnectedEvent as an argument and call them on the thread that they want to be called on.



## Receiving Events


For receiving events you need to register your class on the `EventBus`.

```java
@Override
public void onStart() {
    super.onStart();
    EventBus.getDefault().register(this);
}

@Override
public void onStop() {
   EventBus.getDefault().unregister(this);
   super.onStop();
}

```

And then subscribe to the events.

```java
@Subscribe(threadMode = ThreadMode.MAIN)
public void handleEvent(ArbitaryEvent event) {
    Toast.makeText(getActivity(), "Event type: "+event.getEventType(), Toast.LENGTH_SHORT).show();
}

```



## Sending Events


Sending events is as easy as creating the Event object and then posting it.

```java
EventBus.getDefault().post(new ArbitaryEvent(ArbitaryEvent.TYPE_1));

```



## Creating an Event object


For sending and receiving events we first need an Event object. Event objects are actually simple POJOs.

```java
public class ArbitaryEvent{
    public static final int TYPE_1 = 1;
    public static final int TYPE_2 = 2;
    private int eventType;
    public ArbitaryEvent(int eventType){
        this.eventType = eventType;
    }
    
    public int getEventType(){
        return eventType;
    }
}

```



#### Syntax


<li>@Subscribe(threadMode = ThreadMode.POSTING)
public void onEvent(EventClass event)
{
}</li>



#### Parameters


|Thread Mode|Description
|---|---|---|---|---|---|---|---|---|---
|`ThreadMode.POSTING`|Will be called on the same thread that the event was posted on. This is the default mode.
|`ThreadMode.MAIN`|Will be called on the main UI thread.
|`ThreadMode.BACKGROUND`|Will be called on a background thread. If the posting thread isn't the main thread it will be used. If posted on the main thread `EventBus` has a single background thread that it will use.
|`ThreadMode.ASYNC`|Will be called on its own thread.

