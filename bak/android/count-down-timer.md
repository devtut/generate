---
metaTitle: "Android - Count Down Timer"
description: "Creating a simple countdown timer, A More Complex Example"
---

# Count Down Timer



## Creating a simple countdown timer


CountDownTimer is useful for repeatedly performing an action in a steady interval for a set duration. In this example, we will update a text view every second for 30 seconds telling how much time is remaining. Then when the timer finishes, we will set the TextView to say "Done."

```java
TextView textView = (TextView)findViewById(R.id.text_view);

CountDownTimer countDownTimer = new CountDownTimer(30000, 1000) {
    public void onTick(long millisUntilFinished) {
        textView.setText(String.format(Locale.getDefault(), "%d sec.", millisUntilFinished / 1000L));
    }

    public void onFinish() {
        textView.setText("Done.");
    }
}.start();

```



## A More Complex Example


In this example, we will pause/resume the CountDownTimer based off of the Activity lifecycle.

```java
private static final long TIMER_DURATION = 60000L;
private static final long TIMER_INTERVAL = 1000L;

private CountDownTimer mCountDownTimer;
private TextView textView;

private long mTimeRemaining;

@Override
protected void onCreate(Bundle savedInstanceState) {
    super.onCreate(savedInstanceState);
    setContentView(R.layout.activity_main);

    textView = (TextView)findViewById(R.id.text_view); // Define in xml layout.

    mCountDownTimer = new CountDownTimer(TIMER_DURATION, TIMER_INTERVAL) {
        
        @Override
        public void onTick(long millisUntilFinished) {
            textView.setText(String.format(Locale.getDefault(), "%d sec.", millisUntilFinished / 1000L));
            mTimeRemaining = millisUntilFinished; // Saving timeRemaining in Activity for pause/resume of CountDownTimer.
        }

        @Override
        public void onFinish() {
            textView.setText("Done.");
        }
    }.start();
}


@Override
protected void onResume() {
    super.onResume();
    
    if (mCountDownTimer == null) { // Timer was paused, re-create with saved time.
        mCountDownTimer = new CountDownTimer(timeRemaining, INTERVAL) {
            @Override
            public void onTick(long millisUntilFinished) {
                textView.setText(String.format(Locale.getDefault(), "%d sec.", millisUntilFinished / 1000L));
                timeRemaining = millisUntilFinished;
            }

            @Override
            public void onFinish() {
                textView.setText("Done.");
            }
        }.start();
    }
}

@Override
protected void onPause() {
    super.onPause();
    mCountDownTimer.cancel();
    mCountDownTimer = null;
}

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|`long millisInFuture`|The total duration the timer will run for, a.k.a how far in the future you want the timer to end. In milliseconds.
|`long countDownInterval`|The interval at which you would like to receive timer updates. In milliseconds.
|`long millisUntilFinished`|A parameter provided in `onTick()` that tells how long the CountDownTimer has remaining. In milliseconds



#### Remarks


CountDownTimer is a pretty lean class - it does one thing very well. Since you can only start/cancel a CountDownTimer, you have to implement pause/resume functionality as shown in the second example. For more complex functionality, or to specify a timer that should run indefinitely, use the [Timer](https://developer.android.com/reference/java/util/Timer.html) object.

