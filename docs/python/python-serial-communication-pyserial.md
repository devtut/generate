---
metaTitle Python Serial Communication (pyserial)
description Initialize serial device, Read from serial port, Check what serial ports are available on your machine
---

# Python Serial Communication (pyserial)



## Initialize serial device


```
import serial
#Serial takes these two parameters: serial device and baudrate
ser = serial.Serial('/dev/ttyUSB0', 9600)

```



## Read from serial port


Initialize serial device

```
import serial
#Serial takes two parameters: serial device and baudrate
ser = serial.Serial('/dev/ttyUSB0', 9600)

```

to read single byte from serial device

```
data = ser.read()

```

to read given number of bytes from the serial device

```
data = ser.read(size=5)

```

to read one line from serial device.

```
data = ser.readline()

```

to read the data from serial device while something is being written over it.

```
#for python2.7
data = ser.read(ser.inWaiting())

#for python3
ser.read(ser.inWaiting)

```



## Check what serial ports are available on your machine


To get a list of available serial ports use

```
python -m serial.tools.list_ports

```

at a command prompt or

```
from serial.tools import list_ports
list_ports.comports()  # Outputs list of available serial ports

```

from the Python shell.



#### Syntax


<li>
ser.read(size=1)
</li>
<li>
ser.readline()
</li>
<li>
ser.write()
</li>



#### Parameters


|parameter|details
|------
|port|Device name e.g. /dev/ttyUSB0 on GNU/Linux or COM3 on Windows.
|baudrate|baudrate type: int default: 9600 standard values: 50, 75, 110, 134, 150, 200, 300, 600, 1200, 1800, 2400, 4800, 9600, 19200, 38400, 57600, 115200



#### Remarks


For more details check out [pyserial documentation](https://pythonhosted.org/pyserial/index.html)

