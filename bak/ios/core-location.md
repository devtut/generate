---
metaTitle: "iOS - Core Location"
description: "Request Permission to Use Location Services, Link CoreLocation Framework, Add own custom location using GPX file, Location Services in the Background"
---

# Core Location




## Request Permission to Use Location Services


Check the app's authorization status with:

```swift
//Swift
let status: CLAuthorizationStatus = CLLocationManager.authorizationStatus()

//Objective-C
CLAuthorizationStatus status = [CLLocationManager authorizationStatus];

```

Test the status against the follow constants:

```swift
//Swift
switch status {
case .NotDetermined:
    // Do stuff
case .AuthorizedAlways:
    // Do stuff
case .AuthorizedWhenInUse:
    // Do stuff
case .Restricted:
    // Do stuff
case .Denied:
    // Do stuff
}

//Objective-C
switch (status) {
    case kCLAuthorizationStatusNotDetermined:
        
        //The user hasn't yet chosen whether your app can use location services or not.
        
        break;
        
    case kCLAuthorizationStatusAuthorizedAlways:
        
        //The user has let your app use location services all the time, even if the app is in the background.
        
        break;
        
    case kCLAuthorizationStatusAuthorizedWhenInUse:
        
        //The user has let your app use location services only when the app is in the foreground.
        
        break;
        
    case kCLAuthorizationStatusRestricted:
        
        //The user can't choose whether or not your app can use location services or not, this could be due to parental controls for example.
        
        break;
        
    case kCLAuthorizationStatusDenied:
        
        //The user has chosen to not let your app use location services.
        
        break;
        
    default:
        break;
}

```

### Getting Location Service Permission While App is in Use

[<img src="http://i.stack.imgur.com/wT57a.png" alt="Location When In Use Usage Dialog" />](http://i.stack.imgur.com/wT57a.png)

Simplest method is to initialize the location manager as a property of your root view controller and place the permission request in its `viewDidLoad`.

This brings up the alert controller that asks for permission:

```swift
//Swift
let locationManager = CLLocationManager()
locationManager.requestWhenInUseAuthorization()

//Objective-C
CLLocationManager *locationManager = [[CLLocationManager alloc] init];
[locationManager requestWhenInUseAuthorization];

```

Add the **NSLocationWhenInUseUsageDescription** key to your **Info.plist**. The value will be used in the alert controller's `message` label.

[<img src="http://i.stack.imgur.com/evvkE.png" alt="enter image description here" />](http://i.stack.imgur.com/evvkE.png)

### Getting Location Service Permission Always

[<img src="http://i.stack.imgur.com/T8wjR.png" alt="enter image description here" />](http://i.stack.imgur.com/T8wjR.png)

To ask for permission to use location services even when the app is not active, use the following call instead:

```swift
//Swift
locationManager.requestAlwaysAuthorization()

//Objective-C
[locationManager requestAlwaysAuthorization];

```

Then add the **NSLocationAlwaysUsageDescription** key to your **Info.plist**. Again, the value will be used in the alert controller's `message` label.

[<img src="http://i.stack.imgur.com/HRtxQ.png" alt="enter image description here" />](http://i.stack.imgur.com/HRtxQ.png)



## Link CoreLocation Framework


[<img src="http://i.stack.imgur.com/bcsu4.png" alt="Linking CoreLocation Framework" />](http://i.stack.imgur.com/bcsu4.png)

Import the CoreLocation module in your classes that use CoreLocation functionality.

```swift
//Swift
import CoreLocation

//Objective-C
#import <CoreLocation/CoreLocation.h>

```



## Add own custom location using GPX file


To check for location services we need real device but for testing purpose we can also use simulator and add our own location by following below steps:

- add new GPX file into your project.
- in GPX file add waypoints like

```swift
<?xml version="1.0"?>
<gpx version="1.1" creator="Xcode"> 
<!--
        Provide one or more waypoints containing a latitude/longitude pair. If you provide one
        waypoint, Xcode will simulate that specific location. If you provide multiple waypoints,
        Xcode will simulate a route visitng each waypoint.
 -->
<wpt lat="52.599878" lon="4.702029">
     <name>location name (eg. Florida)</name>
</wpt>

```


- then go to product-->Scheme-->Edit Scheme and into RUN set default location as your GPX file name.



## Location Services in the Background


To use standard location services while the application is in the background you need first turn on `Background Modes` in the Capabilities tab of the Target settings, and select `Location updates`.

[<img src="http://cdn2.raywenderlich.com/wp-content/uploads/2014/12/background_modes.png" alt="Background Modes" />](http://cdn2.raywenderlich.com/wp-content/uploads/2014/12/background_modes.png)

Or, add it directly to the Info.plist.

```swift
<key>NSLocationAlwaysUsageDescription</key>
<string>I want to get your location Information in background</string>

<key>UIBackgroundModes</key>
<array>
    <string>location</string>
</array>

```

Then you need to setup the CLLocationManager

**Objective C**

```swift
//The Location Manager must have a strong reference to it.
_locationManager = [[CLLocationManager alloc] init];
_locationManager.delegate = self;

//Request Always authorization (iOS8+)
if ([_locationManager respondsToSelector:@selector(requestAlwaysAuthorization)]) {
    [_locationManager requestAlwaysAuthorization];
}

//Allow location updates in the background (iOS9+)
if ([_locationManager respondsToSelector:@selector(allowsBackgroundLocationUpdates)]) {
    _locationManager.allowsBackgroundLocationUpdates = YES;
}

[_locationManager startUpdatingLocation];

```

**Swift**

```swift
self.locationManager.delegate = self

if #available (iOS 8.0,*) {
    self.locationManager.requestAlwaysAuthorization()
}

if #available (iOS 9.0,*) {
    self.locationManager.allowsBackgroundLocationUpdates = true
}

self.locationManager.startUpdatingLocation()

```



#### Syntax


1. desiredAccuracy
1. distanceFilter
1. requestLocation()
1. startUpdatingLocation()
1. allowDeferredLocationUpdates(untilTraveled:timeout:)
1. startMonitoringSignificantLocationChanges()
1. allowDeferredLocationUpdates(untilTraveled:timeout:)
1. authorizedAlways
1. authorizedWhenInUse
1. locationManager(_:didChangeAuthorization:)



#### Remarks


### [Simulate a Location at Runtime](https://developer.apple.com/library/ios/recipes/xcode_help-debugger/articles/simulating_locations.html)

1. Run the app from Xcode.
1. In the debug bar, click the "Simulate location" button.
1. Choose a location from the menu.

[<img src="https://i.stack.imgur.com/e02nU.png" alt="Simulate location - debug" />](https://i.stack.imgur.com/e02nU.png)

