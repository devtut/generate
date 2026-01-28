---
metaTitle: "Android - Keyboard"
description: "Hide keyboard when user taps anywhere else on the screen, Register a callback for keyboard open and close"
---

# Keyboard



## Hide keyboard when user taps anywhere else on the screen


Add code in your **Activity**.

This would work for **Fragment** also, **no need** to add this code in **Fragment**.

```java
@Override
public boolean dispatchTouchEvent(MotionEvent ev) {
    View view = getCurrentFocus();
    if (view != null && (ev.getAction() == MotionEvent.ACTION_UP || ev.getAction() == MotionEvent.ACTION_MOVE) && view instanceof EditText && !view.getClass().getName().startsWith("android.webkit.")) {
        int scrcoords[] = new int[2];
        view.getLocationOnScreen(scrcoords);
        float x = ev.getRawX() + view.getLeft() - scrcoords[0];
        float y = ev.getRawY() + view.getTop() - scrcoords[1];
        if (x < view.getLeft() || x > view.getRight() || y < view.getTop() || y > view.getBottom())
        ((InputMethodManager)this.getSystemService(Context.INPUT_METHOD_SERVICE)).hideSoftInputFromWindow((this.getWindow().getDecorView().getApplicationWindowToken()), 0);
    }
    return super.dispatchTouchEvent(ev);
}

```



## Register a callback for keyboard open and close


The idea is to measure a layout before and after each change and if there is a significant change you can be somewhat certain that its the softkeyboard.

```java
// A variable to hold the last content layout hight
private int mLastContentHeight = 0;

private ViewTreeObserver.OnGlobalLayoutListener keyboardLayoutListener = new ViewTreeObserver.OnGlobalLayoutListener() {
    @Override public void onGlobalLayout() {
        int currentContentHeight = findViewById(Window.ID_ANDROID_CONTENT).getHeight();

        if (mLastContentHeight > currentContentHeight + 100) {
            Timber.d("onGlobalLayout: Keyboard is open");
            mLastContentHeight = currentContentHeight;
        } else if (currentContentHeight > mLastContentHeight + 100) {
            Timber.d("onGlobalLayout: Keyboard is closed");
            mLastContentHeight = currentContentHeight;
        }
    }
};

```

then in our `onCreate` set the initial value for `mLastContentHeight`

```java
mLastContentHeight = findViewById(Window.ID_ANDROID_CONTENT).getHeight();

```

and add the listener

```java
rootView.getViewTreeObserver().addOnGlobalLayoutListener(keyboardLayoutListener);

```

don't forget to remove the listener on `destroy`

```java
rootView.getViewTreeObserver().removeOnGlobalLayoutListener(keyboardLayoutListener);

```

