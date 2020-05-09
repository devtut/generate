---
metaTitle: "iOS - iOS Google Places API"
description: "Getting Nearby Places from Current Location"
---

# iOS Google Places API



## Getting Nearby Places from Current Location


Prerequisites

1. Install pods in your project
1. Install the GooglePlaces SDK
1. Enable location services

First we need to get the users location by getting their current longitude and latitude.

1. Import GooglePlaces and GooglePlacePicker

```swift
import GooglePlaces
import GooglePlacePicker

```


1. Add the `CLLOcationManagerDelegate` protocol

```swift
class ViewController: UIViewController, CLLocationManagerDelegate {
    
}

```


1. create your CLLocationManager()

```swift
var currentLocation = CLLocationManager()

```


1. Request authorization

```swift
currentLocation = CLLocationManager()
currentLocation.requetAlwayAuthorization()

```


1. Create a button to call the GooglePlacePicker method

@IBAction func placePickerAction(sender: AnyObject) {

```swift
if CLLOcationManager.authorizationStatues() == .AuthorizedAlways {

        let center = CLLocationCoordinate2DMake((currentLocation.location?.coordinate.latitude)!, (currentLocation.location?.coordinate.longitude)!)
        let northEast = CLLocationCoordinate2DMake(center.latitude + 0.001, center.longitude + 0.001)
        let southWest = CLLocationCoordinate2DMake(center.latitude - 0.001, center.longitude - 0.001)
        let viewport = GMSCoordinateBounds(coordinate: northEast, coordinate: southWest)
        let config = GMSPlacePickerConfig(viewport: viewport)
        placePicker = GMSPlacePicker(config: config)
        
        placePicker?.pickPlaceWithCallback({ (place: GMSPlace?, error: NSError?) -> Void in
            if let error = error {
                print("Pick Place error: \(error.localizedDescription)")
                return
            }
            
            if let place = place {
               print("Place name: \(place.name)")
                print("Address: \(place.formattedAddress)")
                
            } else {
               print("Place name: nil")
                print("Address: nil")
            }
        })
    }        
}

```

