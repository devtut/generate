---
metaTitle: "Android - Detect Shake Event in Android"
description: "Shake Detector in Android Example, Using Seismic shake detection"
---

# Detect Shake Event in Android



## Shake Detector in Android Example


```java
public class ShakeDetector implements SensorEventListener {
 

    private static final float SHAKE_THRESHOLD_GRAVITY = 2.7F;
    private static final int SHAKE_SLOP_TIME_MS = 500;
    private static final int SHAKE_COUNT_RESET_TIME_MS = 3000;
 
    private OnShakeListener mListener;
    private long mShakeTimestamp;
    private int mShakeCount;
 
    public void setOnShakeListener(OnShakeListener listener) {
        this.mListener = listener;
    }
 
    public interface OnShakeListener {
        public void onShake(int count);
    }
 
    @Override
    public void onAccuracyChanged(Sensor sensor, int accuracy) {
        // ignore
    }
 
    @Override
    public void onSensorChanged(SensorEvent event) {
 
        if (mListener != null) {
            float x = event.values[0];
            float y = event.values[1];
            float z = event.values[2];
 
            float gX = x / SensorManager.GRAVITY_EARTH;
            float gY = y / SensorManager.GRAVITY_EARTH;
            float gZ = z / SensorManager.GRAVITY_EARTH;
 
            // gForce will be close to 1 when there is no movement.
            float gForce = FloatMath.sqrt(gX * gX + gY * gY + gZ * gZ);
 
            if (gForce > SHAKE_THRESHOLD_GRAVITY) {
                final long now = System.currentTimeMillis();
                // ignore shake events too close to each other (500ms)
                if (mShakeTimestamp + SHAKE_SLOP_TIME_MS > now) {
                    return;
                }
 
                // reset the shake count after 3 seconds of no shakes
                if (mShakeTimestamp + SHAKE_COUNT_RESET_TIME_MS < now) {
                    mShakeCount = 0;
                }
 
                mShakeTimestamp = now;
                mShakeCount++;
 
                mListener.onShake(mShakeCount);
            }
        }
    }
}

```



## Using Seismic shake detection


[**Seismic**](https://github.com/square/seismic) is an Android device shake detection library by Square. To use it just start listening to the shake events emitted by it.

```java
@Override
protected void onCreate(Bundle savedInstanceState) {
    sm = (SensorManager) getSystemService(SENSOR_SERVICE);
    sd = new ShakeDetector(() -> { /* react to detected shake */ });
}

@Override
protected void onResume() {
    sd.start(sm);
}

@Override
protected void onPause() {
    sd.stop();
}

```

To define the a different acceleration threshold use `sd.setSensitivity(sensitivity)` with a `sensitivity` of `SENSITIVITY_LIGHT`, `SENSITIVITY_MEDIUM`, `SENSITIVITY_HARD` or any other reasonable integer value. The given default values range from **11** to **15**.

### Installation

```java
compile 'com.squareup:seismic:1.0.2'

```

