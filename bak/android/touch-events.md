---
metaTitle: "Android - Touch Events"
description: "How to vary between child and parent view group touch events"
---

# Touch Events



## How to vary between child and parent view group touch events


1. The `onTouchEvents()` for nested view groups can be managed by the `boolean` [onInterceptTouchEvent](http://developer.android.com/reference/android/view/ViewGroup.html#onInterceptTouchEvent(android.view.MotionEvent)).

The default value for the `OnInterceptTouchEvent` is false.

The parent's `onTouchEvent` is received before the child's. If the `OnInterceptTouchEvent` returns false, it sends the motion event down the chain to the child's `OnTouchEvent` handler. If it returns true the parent's will handle the touch event.

However there may be instances when we want some child elements to manage `OnTouchEvent`s and some to be managed by the parent view (or possibly the parent of the parent).

This can be managed in more than one way.

1. One way a child element can be protected from the parent's `OnInterceptTouchEvent` is by implementing the [requestDisallowInterceptTouchEvent](http://developer.android.com/reference/android/view/ViewGroup.html#requestDisallowInterceptTouchEvent(boolean)).

> 
<p>public void requestDisallowInterceptTouchEvent (boolean
disallowIntercept)</p>


This prevents any of the parent views from managing the `OnTouchEvent` for this element, if the element has event handlers enabled.

1. 

If the `OnInterceptTouchEvent` is false, the child element's `OnTouchEvent` will be evaluated. If you have a methods within the child elements handling the various touch events, any related event handlers that are disabled will return the OnTouchEvent to the parent.

This answer:<br />
A visualisation of how the propagation of touch events passes through:<br />
`parent -> child|parent -> child|parent -> child views.`

[<img src="http://i.stack.imgur.com/unTyh.png" alt="enter image description here" />](http://i.stack.imgur.com/unTyh.png)<sub>[Courtesy from here](http://stackoverflow.com/a/13540006/3956566)</sub>

1. Another way is returning varying values from the `OnInterceptTouchEvent` for the parent.

This example taken from [Managing Touch Events in a ViewGroup](http://developer.android.com/training/gestures/viewgroup.html)  and demonstrates how to intercept the child's `OnTouchEvent` when the user is scrolling.

4a.

```java
@Override
public boolean onInterceptTouchEvent(MotionEvent ev) {
    /*
     * This method JUST determines whether we want to intercept the motion.
     * If we return true, onTouchEvent will be called and we do the actual
     * scrolling there.
     */


    final int action = MotionEventCompat.getActionMasked(ev);

    // Always handle the case of the touch gesture being complete.
    if (action == MotionEvent.ACTION_CANCEL || action == MotionEvent.ACTION_UP) {
        // Release the scroll.
        mIsScrolling = false;
        return false; // Do not intercept touch event, let the child handle it
    }

    switch (action) {
        case MotionEvent.ACTION_MOVE: {
            if (mIsScrolling) {
                // We're currently scrolling, so yes, intercept the 
                // touch event!
                return true;
            }

            // If the user has dragged her finger horizontally more than 
            // the touch slop, start the scroll

            // left as an exercise for the reader
            final int xDiff = calculateDistanceX(ev); 

            // Touch slop should be calculated using ViewConfiguration 
            // constants.
            if (xDiff > mTouchSlop) { 
                // Start scrolling!
                mIsScrolling = true;
                return true;
            }
            break;
        }
        ...
    }

    // In general, we don't want to intercept touch events. They should be 
    // handled by the child view.
    return false;
}

```

This is some code from the same link showing how to create the parameters of the rectangle around your element:

4b.

```java
// The hit rectangle for the ImageButton
myButton.getHitRect(delegateArea);
        
// Extend the touch area of the ImageButton beyond its bounds
// on the right and bottom.
delegateArea.right += 100;
delegateArea.bottom += 100;
        
// Instantiate a TouchDelegate.
// "delegateArea" is the bounds in local coordinates of 
// the containing view to be mapped to the delegate view.
// "myButton" is the child view that should receive motion
// events.
TouchDelegate touchDelegate = new TouchDelegate(delegateArea, myButton);
 
// Sets the TouchDelegate on the parent view, such that touches 
// within the touch delegate bounds are routed to the child.
if (View.class.isInstance(myButton.getParent())) {
    ((View) myButton.getParent()).setTouchDelegate(touchDelegate);
}

```

