---
metaTitle: "Android - Getting Calculated View Dimensions"
description: "Calculating initial View dimensions in an Activity"
---

# Getting Calculated View Dimensions



## Calculating initial View dimensions in an Activity


```java
package com.example;

import android.os.Bundle;
import android.support.annotation.Nullable;
import android.util.Log;
import android.view.View;
import android.view.ViewTreeObserver;

public class ExampleActivity extends Activity {

    @Override
    protected void onCreate(@Nullable final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_example);

        final View viewToMeasure = findViewById(R.id.view_to_measure);

        // viewToMeasure dimensions are not known at this point.
        // viewToMeasure.getWidth() and viewToMeasure.getHeight() both return 0,
        // regardless of on-screen size.

        viewToMeasure.getViewTreeObserver().addOnPreDrawListener(new ViewTreeObserver.OnPreDrawListener() {
            @Override
            public boolean onPreDraw() {
                // viewToMeasure is now measured and laid out, and displayed dimensions are known.
                logComputedViewDimensions(viewToMeasure.getWidth(), viewToMeasure.getHeight());
                
                // Remove this listener, as we have now successfully calculated the desired dimensions.
                viewToMeasure.getViewTreeObserver().removeOnPreDrawListener(this);

                // Always return true to continue drawing.
                return true; 
            }
        });
    }

    private void logComputedViewDimensions(final int width, final int height) {
        Log.d("example", "viewToMeasure has width " + width);
        Log.d("example", "viewToMeasure has height " + height);
    }

}

```



#### Remarks


Note that a `ViewTreeObserver` instance associated with a `View` instance can become invalid while that `View` is still alive. From the `View.getViewTreeObserver` javadocs:

```java
// The returned ViewTreeObserver observer is not guaranteed to remain
// valid for the lifetime of this View. If the caller of this method keeps
// a long-lived reference to ViewTreeObserver, it should always check for
// the return value of {@link ViewTreeObserver#isAlive()}.

```

Thus, if you have previously added a listener to a `ViewTreeObserver` instance and now wish to remove it, it is easiest to call `getViewTreeObserver` on the corresponding `View` instance again to receive a fresh `ViewTreeObserver` instance. (Checking `isAlive` on an existing instance is more work for little benefit; if the `ViewTreeObserver` is no longer alive, you'll be fetching that fresh reference anyway!)

