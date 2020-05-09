---
metaTitle: "iOS - iBeacon"
description: "iBeacon Basic Operation, Scanning specific Beacons, Ranging iBeacons"
---

# iBeacon




## iBeacon Basic Operation


1. Setup monitoring beacons

```swift
func initiateRegion(ref:BeaconHandler){
    let uuid: NSUUID = NSUUID(UUIDString: "<UUID>")
    let beacon = CLBeaconRegion(proximityUUID: uuid, identifier: "")
    locationManager?.requestAlwaysAuthorization()    //cllocation manager obj.
    beacon?.notifyOnEntry = true
    beacon?.notifyOnExit = true
    beacon?.notifyEntryStateOnDisplay = true
    locationManager?.startMonitoringForRegion(beacon!)
    locationManager?.delegate = self;
    // Check if beacon monitoring is available for this device
    if (!CLLocationManager.isMonitoringAvailableForClass(CLBeaconRegion)) {
        print("error")
    }
    locationManager!.startRangingBeaconsInRegion(self.beacon!)
}

```


1. Location manager enter and exit region

```swift
func locationManager(manager: CLLocationManager, didEnterRegion region: CLRegion) {
    if(region.isKindOfClass(CLBeaconRegion)) {
        locationManager!.startRangingBeaconsInRegion(self.beacon!)
    }
}
    
func locationManager(manager: CLLocationManager, didExitRegion region: CLRegion) {
    if(region.isKindOfClass(CLBeaconRegion)) {
        locationManager!.stopRangingBeaconsInRegion(self.beacon!)
    }
}

```


1. Location manager range beacon

```swift
func locationManager(manager: CLLocationManager, didRangeBeacons beacons: [CLBeacon], inRegion region: CLBeaconRegion) {
    print(beacons.first.major)
}

```



## Scanning specific Beacons


```

beacon = CLBeaconRegion(proximityUUID: <#NSUUID#>, major: <#CLBeaconMajorValue#>, identifier: <#String#>) // listening to all beacons with given UUID and major value
 beacon =    CLBeaconRegion(proximityUUID: <##NSUUID#>, major: <##CLBeaconMajorValue#>, minor: <##CLBeaconMinorValue#>, identifier: <##String#>) // listening to all beacons with given UUID and major and minor value

```



## Ranging iBeacons


First, you have to request authorization of location services

```swift
let locationManager = CLLocationManager()
locationManager.delegate = self
locationManager.requestWhenInUseAuthorization()
// OR locationManager.requestAlwaysAuthorization()

```

Then you can get all iBeacons' information inside `didRangeBeacons`

```swift
func locationManager(manager: CLLocationManager, didRangeBeacons beacons: [CLBeacon], inRegion region: CLBeaconRegion) {
    for beacon in beacons {
        print(beacon.major)
        print(beacon.minor)
    }
}

```



#### Parameters


|Parameters|Details
|---|---|---|---|---|---|---|---|---|---
|manager|CLLocationManager reference
|region|CLRegion   could be circular region (geofence or beacon region)
|beacons|Array of  CLBeacon contains all ranged beacons



#### Remarks


Beacons are IOT objects. We are focusing on those which conform to iBeacon protocol a Apple standard. Each beacon is a one way device which transmits 3 things

1. UUID
1. Major
1. Minor

We can scan iBeacons by setting up our CLLocation manager object to scan for beacons for particular UUID. All beacons with the given UUID will be scanned.

CLLocation manager also gives call on enter and exit of beacon region.

