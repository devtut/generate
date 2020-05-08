---
metaTitle: "Android - Thread"
description: "Thread Example with its description, Updating the UI from a Background Thread"
---

# Thread



## Thread Example with its description


While launching an application firstly main thread is executed. This Main thread handles all the UI concept of application. If we want to run long the task in which we don't need the UI then we use thread for running that task in background.

**Here is the example of Thread which describes blow:**

```java
new Thread(new Runnable() {
    public void run() {
        for(int i = 1; i < 5;i++) {  
            System.out.println(i);  
        }
    }
}).start();

```

We can create thread by creating the object of Thread which have `Thread.run()` method for running the thread.Here, `run()` method is called by the `start()` method.

We can also run the the multiple threads independently, which is known as MultiThreading. This thread also have the functionality of sleep by which the currently executing thread to sleep (temporarily cease execution) for the specified number of time. But sleep throws the InterruptedException So, we have to handle it by using try/catch like this.

```java
try{Thread.sleep(500);}catch(InterruptedException e){System.out.println(e);}

```



## Updating the UI from a Background Thread


It is common to use a background Thread for doing network operations or long running tasks, and then update the UI with the results when needed.

This poses a problem, as only the main thread can update the UI.

The solution is to use the [`runOnUiThread()`](https://developer.android.com/reference/android/app/Activity.html#runOnUiThread(java.lang.Runnable)) method, as it allows you to initiate code execution on the UI thread from a background Thread.

In this simple example, a Thread is started when the Activity is created, runs until the magic number of `42` is randomly generated, and then uses the `runOnUiThread()` method to update the UI once this condition is met.

```java
public class MainActivity extends AppCompatActivity {

    TextView mTextView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);
        mTextView = (TextView) findViewById(R.id.my_text_view);

        new Thread(new Runnable() {
            @Override
            public void run() {
                while (true) {
                    //do stuff....
                    Random r = new Random();
                    if (r.nextInt(100) == 42) {
                       break;
                    }
                }

                runOnUiThread(new Runnable() {
                    @Override
                    public void run() {
                        mTextView.setText("Ready Player One");
                    }
                });
            }
        }).start();
    }
}

```

