---
metaTitle: "Android - IntentService"
description: "Creating an IntentService, Basic IntentService Example, Sample Intent Service"
---

# IntentService




## Creating an IntentService


To create an IntentService, create a class which extends `IntentService`, and within it, a method which overrides `onHandleIntent`:

```java
package com.example.myapp;
public class MyIntentService extends IntentService {
    @Override
     protected void onHandleIntent (Intent workIntent) {
         //Do something in the background, based on the contents of workIntent.
     }
}

```



## Basic IntentService Example


The abstract class [`IntentService`](https://developer.android.com/reference/android/app/IntentService.html) is a base class for services, which run in the background without any user interface. Therefore, in order to update the UI, we have to make use of a receiver, which may be either a [`BroadcastReceiver`](https://developer.android.com/reference/android/content/BroadcastReceiver.html) or a [`ResultReceiver`](https://developer.android.com/reference/android/os/ResultReceiver.html):

- A `BroadcastReceiver` should be used if your service needs to communicate with multiple components that want to listen for communication.
- A `ResultReceiver`: should be used if your service needs to communicate with only the parent application (i.e. your application).

Within the `IntentService`, we have one key method, `onHandleIntent()`, in which we will do all actions, for example, preparing notifications, creating alarms, etc.

If you want to use you own `IntentService`, you have to extend it as follows:

```java
public class YourIntentService extends IntentService {
    public YourIntentService () {
        super("YourIntentService ");
    }

    @Override
    protected void onHandleIntent(Intent intent) {
        // TODO: Write your own code here.
    }
}    

```

Calling/starting the activity can be done as follows:

```java
Intent i = new Intent(this, YourIntentService.class);
startService(i);  // For the service.
startActivity(i); // For the activity; ignore this for now.

```

Similar to any activity, you can pass extra information such as bundle data to it as follows:

```java
Intent passDataIntent = new Intent(this, YourIntentService.class);
msgIntent.putExtra("foo","bar");
startService(passDataIntent);

```

Now assume that we passed some data to the `YourIntentService` class. Based on this data, an action can be performed as follows:

```java
public class YourIntentService extends IntentService {
    private String actvityValue="bar";
    String retrivedValue=intent.getStringExtra("foo");

    public YourIntentService () {
        super("YourIntentService ");
    }

    @Override
    protected void onHandleIntent(Intent intent) {
        if(retrivedValue.equals(actvityValue)){
            // Send the notification to foo.
        } else {
            // Retrieving data failed.
        }    
    }
}

```

The code above also shows how to handle constraints in the `OnHandleIntent()` method.



## Sample Intent Service


Here is an example of an `IntentService` that pretends to load images in the background. All you need to do to implement an `IntentService` is to provide a constructor that calls the `super(String)` constructor, and you need to implement the `onHandleIntent(Intent)` method.

```java
public class ImageLoaderIntentService extends IntentService {

    public static final String IMAGE_URL = "url";

    /**
     * Define a constructor and call the super(String) constructor, in order to name the worker
     * thread - this is important if you want to debug and know the name of the thread upon 
     * which this Service is operating its jobs.
     */
    public ImageLoaderIntentService() {
        super("Example");
    }

    @Override
    protected void onHandleIntent(Intent intent) {
        // This is where you do all your logic - this code is executed on a background thread

        String imageUrl = intent.getStringExtra(IMAGE_URL);

        if (!TextUtils.isEmpty(imageUrl)) {
            Drawable image = HttpUtils.loadImage(imageUrl); // HttpUtils is made-up for the example
        }

        // Send your drawable back to the UI now, so that you can use it - there are many ways
        // to achieve this, but they are out of reach for this example
    }
}

```

In order to start an `IntentService`, you need to send an `Intent` to it. You can do so from an `Activity`, for an example. Of course, you're not limited to that. Here is an example of how you would summon your new `Service` from an `Activity` class.

```java
Intent serviceIntent = new Intent(this, ImageLoaderIntentService.class); // you can use 'this' as the first parameter if your class is a Context (i.e. an Activity, another Service, etc.), otherwise, supply the context differently
serviceIntent.putExtra(IMAGE_URL, "http://www.example-site.org/some/path/to/an/image");
startService(serviceIntent); // if you are not using 'this' in the first line, you also have to put the call to the Context object before startService(Intent) here

```

The `IntentService` processes the data from its `Intent`s sequentially, so that you can send multiple `Intent`s without worrying whether they will collide with each other. Only one `Intent` at a time is processed, the rest go in a queue. When all the jobs are complete, the `IntentService` will shut itself down automatically.



#### Syntax


1. <service android:name=".UploadS3IntentService"android:exported="false" />



#### Remarks


An `IntentService` provides a simple way to offload work on a background thread. It handles everything about receiving requests, putting them in a queue, stopping itself, etc. for you. It is also easy to implement, making it the perfect thing to use when you have time-consuming operations to do that don't belong on the Main (UI) thread.

