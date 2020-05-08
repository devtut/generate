---
metaTitle: "Android - Android Things"
description: "Controlling a Servo Motor"
---

# Android Things




## Controlling a Servo Motor


This example assumes you have a servo with the following characteristics, which happen to be typical:

- movement between 0 and 180 degrees
- pulse period of 20 ms
- minimum pulse length of 0.5 ms
- maximum pulse length of 2.5 ms

You need to check if those values match your hardware, since forcing it to go outside its specified operating range can damage the servo. A damaged servo in turn has the potential to damage your Android Things device. The example `ServoController` class consists of two methods, `setup()` and `setPosition()`:

```java
public class ServoController {
    private double periodMs, maxTimeMs, minTimeMs;
    private Pwm pin;

    public void setup(String pinName) throws IOException {
        periodMs  = 20;
        maxTimeMs = 2.5;
        minTimeMs = 0.5;

        PeripheralManagerService service = new PeripheralManagerService();
        pin = service.openPwm(pinName);

        pin.setPwmFrequencyHz(1000.0d / periodMs);
        setPosition(90);
        pin.setEnabled(true);
    }

    public void setPosition(double degrees) {
        double pulseLengthMs = (degrees / 180.0 * (maxTimeMs - minTimeMs)) + minTimeMs;

        if (pulseLengthMs < minTimeMs) {
            pulseLengthMs = minTimeMs;
        } else if (pulseLengthMs > maxTimeMs) {
            pulseLengthMs = maxTimeMs;
        }

        double dutyCycle = pulseLengthMs / periodMs * 100.0;

        Log.i(TAG, "Duty cycle = " + dutyCycle + " pulse length = " + pulseLengthMs);

        try {
            pin.setPwmDutyCycle(dutyCycle);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}

```

You can discover pin names that support PWM on your device as follows:

```java
PeripheralManagerService service = new PeripheralManagerService();

for (String pinName : service.getPwmList() ) {
    Log.i("ServoControlled","Pwm pin found: " + pinName);
}

```

In order to make your servo swinging forever between 80 degrees and 100 degrees, you can simply use the following code:

```java
final ServoController servoController = new ServoController(pinName);

Thread th = new Thread(new Runnable() {
    @Override
    public void run() {
        while (true) {
            try {
                servoController.setPosition(80);
                Thread.sleep(500);
                servoController.setPosition(100);
                Thread.sleep(500);
            } catch (InterruptedException e) {
                e.printStackTrace();
            }
        }
    }
});
th.start();

```

You can compile and deploy all of the above code without actually hooking any servo motors to the computing device. For the wiring, refer to your computing device pinout chart (e.g. a Raspberry Pi 3 pinout chart is available [here](https://pinout.xyz/)).

Then you need to hook your servo to Vcc, Gnd, and signal.

