---
metaTitle: "Android - Bluetooth Low Energy"
description: "Finding BLE Devices, Connecting to a GATT Server, Writing and Reading from Characteristics, Subscribing to Notifications from the Gatt Server, Advertising a BLE Device, Using a Gatt Server"
---

# Bluetooth Low Energy


This documentation is meant as an enhancement over the [original documentation](https://developer.android.com/guide/topics/connectivity/bluetooth-le.html) and it will focus on the latest Bluetooth LE API introduced in Android 5.0 (API 21). Both Central and Peripheral roles will be covered as well as how to start scanning and advertising operations.



## Finding BLE Devices


The following permissions are required to use the Bluetooth APIs:

```java
BluetoothManager bluetoothManager = (BluetoothManager) context.getSystemService(Context.BLUETOOTH_SERVICE);
bluetoothAdapter = bluetoothManager.getAdapter();

```

The `startScan (ScanCallback callback)`method of the BluetoothLeScanner class is the most basic way to start a scanning operation. A `ScanCallback` object is required to receive results:

```java
bluetoothAdapter.getBluetoothLeScanner().startScan(new ScanCallback() {
     @Override
     public void onScanResult(int callbackType, ScanResult result) {
     super.onScanResult(callbackType, result);
     Log.i(TAG, "Remote device name: " + result.getDevice().getName());
       }
    });

```



## Connecting to a GATT Server


Once you have discovered a desired BluetoothDevice object, you can connect to it by using its `connectGatt()` method which takes as parameters a Context object, a boolean indicating whether to automatically connect to the BLE device and a BluetoothGattCallback reference where connection events and client operations results will be delivered:

```

if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.M) {
        device.connectGatt(context, false, bluetoothGattCallback, BluetoothDevice.TRANSPORT_AUTO);
    } else {
        device.connectGatt(context, false, bluetoothGattCallback);
    }

```

Override `onConnectionStateChange` in BluetoothGattCallback to receive connection an disconnection events:

```

   BluetoothGattCallback bluetoothGattCallback =
        new BluetoothGattCallback() {
    @Override
    public void onConnectionStateChange(BluetoothGatt gatt, int status,
            int newState) {
        if (newState == BluetoothProfile.STATE_CONNECTED) {
            Log.i(TAG, "Connected to GATT server.");

        } else if (newState == BluetoothProfile.STATE_DISCONNECTED) {
            
            Log.i(TAG, "Disconnected from GATT server.");
        }
    }
   };

```



## Writing and Reading from Characteristics


Once you are connected to a Gatt Server, you're going to be interacting with it by writing and reading from the server's characteristics. To do this, first you have to discover what services are available on this server and which characteristics are avaiable in each service:

```

@Override
 public void onConnectionStateChange(BluetoothGatt gatt, int status,
        int newState) {
    if (newState == BluetoothProfile.STATE_CONNECTED) {
        Log.i(TAG, "Connected to GATT server.");
        gatt.discoverServices();

    }
. . .

 @Override
    public void onServicesDiscovered(BluetoothGatt gatt, int status) {
        if (status == BluetoothGatt.GATT_SUCCESS) {
             List<BluetoothGattService> services = gatt.getServices();
                for (BluetoothGattService service : services) {
                    List<BluetoothGattCharacteristic> characteristics = service.getCharacteristics();
                    for (BluetoothGattCharacteristic characteristic : characteristics) { 
                        ///Once you have a characteristic object, you can perform read/write
                        //operations with it         
                    }
                 }
              }
            }

```

A basic write operation goes like this:

```java
characteristic.setValue(newValue);
characteristic.setWriteType(BluetoothGattCharacteristic.WRITE_TYPE_DEFAULT);
gatt.writeCharacteristic(characteristic);

```

When the write process has finished, the `onCharacteristicWrite` method of your `BluetoothGattCallback` will be called:

```

   @Override
    public void onCharacteristicWrite(BluetoothGatt gatt, BluetoothGattCharacteristic characteristic, int status) {
        super.onCharacteristicWrite(gatt, characteristic, status);
        Log.d(TAG, "Characteristic " + characteristic.getUuid() + " written);
    }

```

A basic write operation goes like this:

```java
gatt.readCharacteristic(characteristic);

```

When the write process has finished, the `onCharacteristicRead` method of your `BluetoothGattCallback` will be called:

```java
@Override
public void onCharacteristicRead(BluetoothGatt gatt, BluetoothGattCharacteristic characteristic, int status) {
  super.onCharacteristicRead(gatt, characteristic, status);
  byte[] value = characteristic.getValue();
  }

```



## Subscribing to Notifications from the Gatt Server


You can request to be notified from the Gatt Server when the value of a characteristic has been changed:

```java
gatt.setCharacteristicNotification(characteristic, true);
BluetoothGattDescriptor descriptor = characteristic.getDescriptor(
    UUID.fromString("00002902-0000-1000-8000-00805f9b34fb");
descriptor.setValue(BluetoothGattDescriptor.ENABLE_NOTIFICATION_VALUE);
mBluetoothGatt.writeDescriptor(descriptor);

```

All notifications from the server will be received in the `onCharacteristicChanged` method of your BluetoothGattCallback:

```

   @Override
    public void onCharacteristicChanged(BluetoothGatt gatt, BluetoothGattCharacteristic characteristic) {
        super.onCharacteristicChanged(gatt, characteristic);
        byte[] newValue = characteristic.getValue();
}

```



## Advertising a BLE Device


You can use Bluetooth LE Advertising to broadcast data packages to all nearby devices without having to establish a connection first. Bear in mind that there's a strict limit of 31 bytes of advertisement data. Advertising your device is also the first step towards letting other users connect to you.

Since not all devices support Bluetooth LE Advertising, the first step is to check that your device has all the necessary requirements to support it. Afterwards, you can initialize a `BluetoothLeAdvertiser` object and with it, you can start advertising operations:

```

       if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP && bluetoothAdapter.isMultipleAdvertisementSupported())
        {
            BluetoothLeAdvertiser advertiser = bluetoothAdapter.getBluetoothLeAdvertiser();

            AdvertiseData.Builder dataBuilder = new AdvertiseData.Builder();
           //Define a service UUID according to your needs                
           dataBuilder.addServiceUuid(SERVICE_UUID);
            dataBuilder.setIncludeDeviceName(true);


             AdvertiseSettings.Builder settingsBuilder = new AdvertiseSettings.Builder();
             settingsBuilder.setAdvertiseMode(AdvertiseSettings.ADVERTISE_MODE_LOW_POWER);
             settingsBuilder.setTimeout(0);

             //Use the connectable flag if you intend on opening a Gatt Server
             //to allow remote connections to your device.
             settingsBuilder.setConnectable(true);

            AdvertiseCallback advertiseCallback=new AdvertiseCallback() {
            @Override
            public void onStartSuccess(AdvertiseSettings settingsInEffect) {
                super.onStartSuccess(settingsInEffect);
                Log.i(TAG, "onStartSuccess: ");
            }

            @Override
            public void onStartFailure(int errorCode) {
                super.onStartFailure(errorCode);
                Log.e(TAG, "onStartFailure: "+errorCode );
            }
          };
advertising.startAdvertising(settingsBuilder.build(),dataBuilder.build(),advertiseCallback);
        }

```



## Using a Gatt Server


In order for your device to act as a peripheral, first you need to open a `BluetoothGattServer` and populate it with at least one `BluetoothGattService` and one `BluetoothGattCharacteristic`:

```java
BluetoothGattServer server=bluetoothManager.openGattServer(context, bluetoothGattServerCallback);

BluetoothGattService service = new BluetoothGattService(SERVICE_UUID, BluetoothGattService.SERVICE_TYPE_PRIMARY);

```

This is an example of a BluetoothGattCharacteristic with full write,read and notify permissions. According to your needs, you might want to fine tune the permissions that you grant this characteristic:

```java
BluetoothGattCharacteristic characteristic = new BluetoothGattCharacteristic(CHARACTERISTIC_UUID,
                    BluetoothGattCharacteristic.PROPERTY_READ | BluetoothGattCharacteristic.PROPERTY_WRITE |
                            BluetoothGattCharacteristic.PROPERTY_NOTIFY,
                    BluetoothGattCharacteristic.PERMISSION_READ | BluetoothGattCharacteristic.PERMISSION_WRITE);

characteristic.addDescriptor(new BluetoothGattDescriptor(UUID.fromString("00002902-0000-1000-8000-00805f9b34fb"), BluetoothGattCharacteristic.PERMISSION_WRITE));

service.addCharacteristic(characteristic);

server.addService(service);

```

The `BluetoothGattServerCallback` is responsible for receiving all events related to your `BluetoothGattServer`:

```java
BluetoothGattServerCallback bluetoothGattServerCallback= new BluetoothGattServerCallback() {
                @Override
                public void onConnectionStateChange(BluetoothDevice device, int status, int newState) {
                    super.onConnectionStateChange(device, status, newState);
                }

                @Override
                public void onCharacteristicReadRequest(BluetoothDevice device, int requestId, int offset, BluetoothGattCharacteristic characteristic) {
                    super.onCharacteristicReadRequest(device, requestId, offset, characteristic);
                }

                @Override
                public void onCharacteristicWriteRequest(BluetoothDevice device, int requestId, BluetoothGattCharacteristic characteristic, boolean preparedWrite, boolean responseNeeded, int offset, byte[] value) {
                    super.onCharacteristicWriteRequest(device, requestId, characteristic, preparedWrite, responseNeeded, offset, value);
                }

                @Override
                public void onDescriptorReadRequest(BluetoothDevice device, int requestId, int offset, BluetoothGattDescriptor descriptor) {
                    super.onDescriptorReadRequest(device, requestId, offset, descriptor);
                }

                @Override
                public void onDescriptorWriteRequest(BluetoothDevice device, int requestId, BluetoothGattDescriptor descriptor, boolean preparedWrite, boolean responseNeeded, int offset, byte[] value) {
                    super.onDescriptorWriteRequest(device, requestId, descriptor, preparedWrite, responseNeeded, offset, value);
                }

```

Whenever you receive a request for a write/read to a characteristic or descriptor you must send a response to it in order for the request to be completed succesfully :

```java
@Override
 public void onCharacteristicReadRequest(BluetoothDevice device, int requestId, int offset, BluetoothGattCharacteristic characteristic) {
    super.onCharacteristicReadRequest(device, requestId, offset, characteristic);
    server.sendResponse(device, requestId, BluetoothGatt.GATT_SUCCESS, offset, YOUR_RESPONSE);
}

```

