---
metaTitle: "Xamarin - Xamarin iOS Google Places Autocomplete"
description: "Add an autocomplete UI control with results controller."
---

# Xamarin iOS Google Places Autocomplete


Since starting to work with Xamarin there have been many things I wish someone else had already documented. Here I explain how to use 1 aspect of the google places autocompletion, the UI control making use of a results controller.  While this code is based on googles own examples converted to C# from Swift I have tried my best to make sure it is a fully working example.  I hope very much this documentation will help others.



## Add an autocomplete UI control with results controller.


The autocomplete UI control is a search dialog with built-in autocomplete functionality. As a user enters search terms, the control presents a list of predicted places to choose from. When the user makes a selection, a GMSPlace (Place in Xamarin) instance is returned, which your app can then use to get details about the selected place.

As mentioned above this example uses a results controller which allows for more control over the text input UI. The results controller will dynamically toggle the visibility of the results list based on input UI focus.

The aim of this code is to display a screen just like the below:

[<img src="https://i.stack.imgur.com/3SxJY.png" alt="enter image description here" />](https://i.stack.imgur.com/3SxJY.png)

This will autocomplete your address when you start typing, like the image below:

[<img src="https://i.stack.imgur.com/djLDx.png" alt="enter image description here" />](https://i.stack.imgur.com/djLDx.png)

**Instructions:**

<li>
First we need to add the google maps API to our Visual Studio, It's available through Nuget, just search for Xamarin.Google.iOS.Maps, add it to your iOS project, alternatively you can download it from Xamarin [Xamarin Google Maps iOS SDK](https://components.xamarin.com/view/googleiosmaps)
</li>
<li>
We need something like a button to trigger the google autocomplete view controller.  In this example I do this with a story board and have added a button named GoogleButton, you could trigger it with code it doesn't really matter.
</li>
<li>
Under ViewDidLoad in your view controllers class add the following code. In my example below I am not using the mobile devices actual location, my final solution would of course do this, but this was a test and I didn't want to implement extra code until I had proved this worked or dilute what I am trying to show you:
</li>

// Code to bring up the google places auto complete view controller.

```cs
GoogleButton.TouchUpInside += (sender, ea) =>
{

    var FakeCoordinates = new CLLocationCoordinate2D()
    {
        Latitude = 54.135364,
        Longitude = -0.797888
    };

    var north = LocationWithBearing(45, 3000, FakeCoordinates);
    var east = LocationWithBearing(225, 3000, FakeCoordinates);

    var autocompleteController = new AutocompleteViewController();
    autocompleteController.Delegate = new AutoCompleteDelegate();
    autocompleteController.AutocompleteBounds = new CoordinateBounds(north, east);
    PresentViewController(autocompleteController, true, null);
                
};

```


1. This is optional, but I have added a function to calculate a local bounds, I am passing in 3000 this number is in metres, so if you want a bigger initial bounds feel free to adjust, please note that google search will still find any address in the world, it just weights the initial results to this local areas bounds first. This function was borrowed from a stack overflow post, I have converted it from Swift to C# for our purposes:

```cs
public CLLocationCoordinate2D LocationWithBearing(Double bearing, Double distanceMeters, CLLocationCoordinate2D origin)
{
    var distRadians = distanceMeters/(6372797.6);

    var rbearing = bearing*Math.PI/180.0;

    var lat1 = origin.Latitude*Math.PI/180;
    var lon1 = origin.Longitude*Math.PI/180;

    var lat2 = Math.Asin(Math.Sin(lat1)*Math.Cos(distRadians) + Math.Cos(lat1)*Math.Sin(distRadians)*Math.Cos(rbearing));
    var lon2 = lon1 + Math.Atan2(Math.Sin(rbearing)*Math.Sin(distRadians)*Math.Cos(lat1),
                    Math.Cos(distRadians) - Math.Sin(lat1)*Math.Sin(lat2));

    return new CLLocationCoordinate2D(latitude: lat2*180/ Math.PI, longitude: lon2*180/Math.PI);
}

```


1. This final code snippet is the delegate for the autocomplete, we need this delegate to handle all that google will return to us:

```cs
public  class AutoCompleteDelegate : AutocompleteViewControllerDelegate
{

    public override void DidFailAutocomplete(AutocompleteViewController viewController, NSError error)
    {
        // TODO: handle the error.
        Debug.Print("Error: " + error.Description);
    }


    public override void DidAutocomplete(AutocompleteViewController viewController, Place place)
    {
        Debug.Print(place.Name);
        Debug.Print(place.FormattedAddress);

        viewController.DismissViewController(true, null);
    }

    public override void DidRequestAutocompletePredictions(AutocompleteViewController viewController)
    {
        UIApplication.SharedApplication.NetworkActivityIndicatorVisible = true;
    }
        
    public override void DidUpdateAutocompletePredictions(AutocompleteViewController viewController)
    {
        UIApplication.SharedApplication.NetworkActivityIndicatorVisible = true;
    }
        
    public override void WasCancelled(AutocompleteViewController viewController)
    {
        viewController.DismissViewController(true, null);
    }

}

```

Run your project and it should work perfectly, this example is quite focussed, but hopefully it till give you a basic example of how any of the google autocomplete UI controls would need to work. Thanks!

