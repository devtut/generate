---
metaTitle: "Python - IoT Programming with Python and Raspberry PI"
description: "Example - Temperature sensor"
---

# IoT Programming with Python and Raspberry PI




## Example - Temperature sensor


Interfacing of DS18B20 with Raspberry pi

**Connection of DS18B20 with Raspberry pi**

[<img src="https://i.stack.imgur.com/OBA2X.png" alt="enter image description here" />](https://i.stack.imgur.com/OBA2X.png)

You can see there are three terminal

1. Vcc
1. Gnd
1. Data (One wire protocol)

[<img src="https://i.stack.imgur.com/dSFFQ.png" alt="enter image description here" />](https://i.stack.imgur.com/dSFFQ.png)

**R1 is 4.7k ohm resistance for pulling up the voltage level**

1. **Vcc** should be connected to any of the 5v or 3.3v pins of Raspberry pi (PIN : 01, 02, 04, 17).
1. **Gnd** should be connected to any of the Gnd pins of Raspberry pi (PIN : 06, 09, 14, 20, 25).
1. **DATA** should be connected to (PIN : 07)

**Enabling the one-wire interface from the RPi side**

<li>
Login to Raspberry pi using putty or any other linux/unix terminal.
</li>
<li>
After login, open the /boot/config.txt file in your favourite browser.
nano /boot/config.txt
</li>
<li>
Now add the this line `dtoverlay=w1–gpio` to the end of the file.
</li>
<li>
Now reboot the Raspberry pi `sudo reboot`.
</li>
<li>
Log in to Raspberry pi, and run `sudo modprobe g1-gpio`
</li>
<li>
Then run `sudo modprobe w1-therm`
</li>
<li>
Now go to the directory /sys/bus/w1/devices `cd /sys/bus/w1/devices`
</li>
<li>
Now you will found out a virtual directory created of your temperature sensor starting from 28-********.
</li>
<li>
Go to this directory `cd 28-********`
</li>
<li>
Now there is a file name **w1-slave**, This file contains the temperature and other information like CRC. `cat w1-slave`.
</li>

**Now write a module in python to read the temperature**

```py
import glob
import time

RATE = 30
sensor_dirs = glob.glob("/sys/bus/w1/devices/28*")

if len(sensor_dirs) != 0:
    while True:
        time.sleep(RATE)
        for directories in sensor_dirs:
            temperature_file = open(directories + "/w1_slave")
            # Reading the files
            text = temperature_file.read()
            temperature_file.close()
            # Split the text with new lines (\n) and select the second line.
            second_line = text.split("\n")[1]
            # Split the line into words, and select the 10th word
            temperature_data = second_line.split(" ")[9]
            # We will read after ignoring first two character.
            temperature = float(temperature_data[2:])
            # Now normalise the temperature by dividing 1000.
            temperature = temperature / 1000
            print 'Address : '+str(directories.split('/')[-1])+', Temperature : '+str(temperature)

```

Above python module will print the temperature vs address for infinite time. RATE parameter is defined to change or adjust the frequency of temperature query from the sensor.

GPIO pin diagram

1. [[https://www.element14.com/community/servlet/JiveServlet/previewBody/73950-102-11-339300/pi3_gpio.png][3]](https://www.element14.com/community/servlet/JiveServlet/previewBody/73950-102-11-339300/pi3_gpio.png%5D%5B3%5D)

