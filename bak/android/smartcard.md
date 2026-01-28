---
metaTitle: "Android - Smartcard"
description: "Smart card send and receive"
---

# Smartcard




## Smart card send and receive


For connection, here is a snippet to help you understand:

```java
//Allows you to enumerate and communicate with connected USB devices.
UsbManager mUsbManager = (UsbManager) getSystemService(Context.USB_SERVICE);
//Explicitly asking for permission
final String ACTION_USB_PERMISSION = "com.android.example.USB_PERMISSION";
PendingIntent mPermissionIntent = PendingIntent.getBroadcast(this, 0, new Intent(ACTION_USB_PERMISSION), 0);
HashMap<String, UsbDevice> deviceList = mUsbManager.getDeviceList();

UsbDevice device = deviceList.get("//the device you want to work with");
if (device != null) {
    mUsbManager.requestPermission(device, mPermissionIntent);
}

```

Now you have to understand that in java the communication takes place using package javax.smarcard which is not available for Android so take a look here for getting an idea as to how you can communicate or send/receive APDU (smartcard command).

Now as told in the answer mentioned above

You cannot simply send an APDU (smartcard command) over the bulk-out endpoint and expect to receive a response APDU over the bulk-in endpoint.
For getting the endpoints see the code snippet below :

```java
UsbEndpoint epOut = null, epIn = null;
UsbInterface usbInterface;

UsbDeviceConnection connection = mUsbManager.openDevice(device);

    for (int i = 0; i < device.getInterfaceCount(); i++) {
        usbInterface = device.getInterface(i);
        connection.claimInterface(usbInterface, true);

        for (int j = 0; j < usbInterface.getEndpointCount(); j++) {
            UsbEndpoint ep = usbInterface.getEndpoint(j);

            if (ep.getType() == UsbConstants.USB_ENDPOINT_XFER_BULK) {
                if (ep.getDirection() == UsbConstants.USB_DIR_OUT) {
                    // from host to device
                    epOut = ep;

                } else if (ep.getDirection() == UsbConstants.USB_DIR_IN) {
                    // from device to host
                    epIn = ep;
                }
            }
        }
    }

```

Now you have the bulk-in and bulk-out endpoints to send and receive APDU command and APDU response blocks:

For sending commands, see the code snippet below:

```java
public void write(UsbDeviceConnection connection, UsbEndpoint epOut, byte[] command) {
    result = new StringBuilder();
    connection.bulkTransfer(epOut, command, command.length, TIMEOUT);
    //For Printing logs you can use result variable
    for (byte bb : command) {
        result.append(String.format(" %02X ", bb));
    }
}

```

And for receive/ read a response see the code snippet below :

```java
public int read(UsbDeviceConnection connection, UsbEndpoint epIn) {
result = new StringBuilder();
final byte[] buffer = new byte[epIn.getMaxPacketSize()];
int byteCount = 0;
byteCount = connection.bulkTransfer(epIn, buffer, buffer.length, TIMEOUT);

//For Printing logs you can use result variable
if (byteCount >= 0) {
    for (byte bb : buffer) {
        result.append(String.format(" %02X ", bb));
    }

    //Buffer received was : result.toString()
} else {
    //Something went wrong as count was : " + byteCount
}

return byteCount;
}

```

Now if you see this answer here the 1st command to be sent is :

PC_to_RDR_IccPowerOn command to activate the card.

which you can create by reading section 6.1.1 of the USB Device Class Specifications doc here.

Now let's take an example of this command like the one here: `62000000000000000000` How you can send this is :

```java
write(connection, epOut, "62000000000000000000");

```

Now after you have successfully sent the APDU command, you can read the response using :

```java
read(connection, epIn);

```

And receive something like

```java
80 18000000 00 00 00 00 00 3BBF11008131FE45455041000000000000000000000000F1

```

Now the response received in the code here will be in the `result` variable of `read()` method from code

