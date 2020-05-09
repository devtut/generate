---
metaTitle: "iOS - CLLocation"
description: " Distance Filter using , Get User Location Using CLLocationManager"
---

# CLLocation



##  Distance Filter using 


Example :

```

  CLLocationManager *locationManager = [[CLLocationManager alloc] init];
   locationManager.delegate = self;
   locationManager.desiredAccuracy = kCLLocationAccuracyBest;
   locationManager.distanceFilter = 5;

```

E.g. In the above  example code above, location changes of less than 5 metres won’t be sent to the callback, but instead be ignored.



## Get User Location Using CLLocationManager


1 - Include the CoreLocation.framework in your project; this is accomplished by clicking on:

```swift
root directory -> build phases -> Link Binary With Libraries

```

Click on the (+) button, look for CoreLocation.framework and click add.

2- Modify the info.plist file to ask for permission to use user location by opening it as source code. Add either of the following key:value pair under the  tag to ask for usage of user's location while the application is in use:

```swift
<key>NSLocationWhenInUseUsageDescription</key>
<string>message to display when asking for permission</string>

```

3- import CoreLocation to the ViewController that will be using it.

```swift
import CoreLocation

```

4- Make sure your ViewController conforms to the CLLocationManagerDelagate protocol

```swift
class ViewController: UIViewController,CLLocationManagerDelegate {}

```

After these steps, we can create a CLLocationManager object as instance variable and use it in the ViewController.

```swift
var manager:CLLocationManager!

```

We do not use 'let' here because we will modify the manager to specify its delegate, minimum distance before update event, and its accuracy

```swift
//initialize the manager
manager = CLLocationManager()

//specify delegate
manager.delegate = self

//set the minimum distance the phone needs to move before an update event is triggered (for example:  100 meters)
manager.distanceFilter = 100

//set Accuracy to any of the following depending on your use case

//let kCLLocationAccuracyBestForNavigation: CLLocationAccuracy
//let kCLLocationAccuracyBest: CLLocationAccuracy
//let kCLLocationAccuracyNearestTenMeters: CLLocationAccuracy
//let kCLLocationAccuracyHundredMeters: CLLocationAccuracy
//let kCLLocationAccuracyKilometer: CLLocationAccuracy
//let kCLLocationAccuracyThreeKilometers: CLLocationAccuracy

manager.desiredAccuracy = kCLLocationAccuracyBest

//ask the user for permission
manager.requestWhenInUseAuthorization()

//Start collecting location information
if #available(iOS 9.0, *) {
            
   manager.requestLocation()
            
 } else {
  
   manager.startUpdatingLocation()
  
  }

```

Now to get access to the location updates, we can implement the function below which is called overtime the distanceFilter is reached.

```swift
func locationManager(manager: CLLocationManager, didUpdateLocations locations: [CLLocation]) {}

```

The locations parameter is an array of CLLocation objects that represent the actual location of the device.From these objects, one can get access to the following attributes: `coordinate,altitude, floor, horizontalAccuracy, verticalAccuracy, timestamp, description, course, speed`, and a function `distance(from:)` that measures the distance between two locations.

Note: While requesting permission for location, there are two different types of authorization.

> 
<p>"When In Use" authorization only gives the app permission to receive
your location when the app is in use or foreground.</p>
<p>“Always" authorization, gives the app background
permissions which may lead to decrease battery life in case your app is closed.</p>
Plist file should be adjusted as necessary.


