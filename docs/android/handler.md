---
metaTitle: "Android - Handler"
description: "HandlerThreads and communication between Threads, Use Handler to create a Timer (similar to javax.swing.Timer), Using a Handler to execute code after a delayed amount of time, Stop handler from execution "
---

# Handler



## HandlerThreads and communication between Threads


As `Handler`s are used to send `Message`s and `Runnable`s to a Thread's message queue it's easy to implement event based communication between multiple Threads. Every Thread that has a `Looper` is able to receive and process messages. A `HandlerThread` is a Thread that implements such a `Looper`, for example the main Thread (UI Thread) implements the features of a `HandlerThread`.

### Creating a Handler for the current Thread

```java
Handler handler = new Handler();

```

### Creating a Handler for the main Thread (UI Thread)

```java
Handler handler = new Handler(Looper.getMainLooper());

```

### Send a Runnable from another Thread to the main Thread

```java
new Thread(new Runnable() {
    public void run() {
        // this is executed on another Thread

        // create a Handler associated with the main Thread
        Handler handler = new Handler(Looper.getMainLooper());

        // post a Runnable to the main Thread
        handler.post(new Runnable() {
            public void run() {
                // this is executed on the main Thread
            }
        });
    }
}).start();

```

### Creating a Handler for another HandlerThread and sending events to it

```java
// create another Thread
HandlerThread otherThread = new HandlerThread("name");

// create a Handler associated with the other Thread
Handler handler = new Handler(otherThread.getLooper());

// post an event to the other Thread
handler.post(new Runnable() {
    public void run() {
        // this is executed on the other Thread
    }
});

```



## Use Handler to create a Timer (similar to javax.swing.Timer)


This can be useful if you're writing a game or something that needs to execute a piece of code every a few seconds.

```java
import android.os.Handler;

public class Timer {
    private Handler handler;
    private boolean paused;

    private int interval;

    private Runnable task = new Runnable () {
        @Override
        public void run() {
            if (!paused) {
                runnable.run ();
                Timer.this.handler.postDelayed (this, interval);
            }
        }
    };

    private Runnable runnable;

    public int getInterval() {
        return interval;
    }

    public void setInterval(int interval) {
        this.interval = interval;
    }

    public void startTimer () {
        paused = false;
        handler.postDelayed (task, interval);
    }

    public void stopTimer () {
        paused = true;
    }

    public Timer (Runnable runnable, int interval, boolean started) {
        handler = new Handler ();
        this.runnable = runnable;
        this.interval = interval;
        if (started)
            startTimer ();
    }
}

```

Example usage:

```java
Timer timer = new Timer(new Runnable() {
    public void run() {
        System.out.println("Hello");
    }
}, 1000, true)

```

This code will print "Hello" every second.



## Using a Handler to execute code after a delayed amount of time


**Executing code after 1.5 seconds:**

```java
Handler handler = new Handler();
handler.postDelayed(new Runnable() {
    @Override
    public void run() {
        //The code you want to run after the time is up
    }
}, 1500); //the time you want to delay in milliseconds

```

**Executing code repeatedly every 1 second:**

```java
Handler handler = new Handler();
handler.postDelayed(new Runnable() {
    @Override
    public void run() {
        handler.postDelayed(this, 1000);
    }
}, 1000); //the time you want to delay in milliseconds

```



## Stop handler from execution 


To stop the Handler from execution remove the callback attached to it using the runnable running inside it:

```java
Runnable my_runnable = new Runnable() {
    @Override
    public void run() {
        // your code here
    }
};

public Handler handler = new Handler(); // use 'new Handler(Looper.getMainLooper());' if you want this handler to control something in the UI
// to start the handler
public void start() {
    handler.postDelayed(my_runnable, 10000);
}

// to stop the handler
public void stop() {
    handler.removeCallbacks(my_runnable);
}

// to reset the handler
public void restart() {
    handler.removeCallbacks(my_runnable);
    handler.postDelayed(my_runnable, 10000);
}

```



#### Remarks


A Handler can be easily used to execute code after a delayed amount of time. It is also useful for executing code repeatedly after a specified amount of time by calling the Handler.postDelayed() method again from within the Runnable's run() method.

