---
metaTitle: "Android - UI Lifecycle"
description: "Saving data on memory trimming"
---

# UI Lifecycle



## Saving data on memory trimming


```java
public class ExampleActivity extends Activity {
    
    private final static String EXAMPLE_ARG = "example_arg";
    private int mArg;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_example);
        
        if(savedInstanceState != null) {
            mArg = savedInstanceState.getInt(EXAMPLE_ARG);
        }
    }

    @Override
    public void onSaveInstanceState(Bundle outState) {
        super.onSaveInstanceState(outState);
        outState.putInt(EXAMPLE_ARG, mArg);
    }
}

```

**Explanation**

So, what is happening here?

The Android system will always strive to clear as much memory as it can. So, if your activity is down to the background, and another foreground activity is demanding its share, the Android system will call `onTrimMemory()` on your activity.

But that doesn't mean that all your properties should vanish. What you should do is to save them into a Bundle object. Bundle object are much better handled memory wise. Inside a bundle every object is identified by unique text sequence - in the example above integer value variable `mArg` is hold under reference name `EXAMPLE_ARG`. And when the activity is recreated extract your old values from the Bundle object instead of recreating them from scratch

