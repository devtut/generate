---
metaTitle: "Xamarin - Gestures"
description: "Make an Image tappable by adding a TapGestureRecognizer, Zoom an Image with the Pinch gesture, Show all of the zoomed Image content with the PanGestureRecognizer, Place a pin where the user touched the screen with MR.Gestures"
---

# Gestures



## Make an Image tappable by adding a TapGestureRecognizer


There are a couple of default recognizers available in Xamarin.Forms, one of them is the `TapGestureRecognizer`.

You can add them to virtually any visual element. Have a look at a simple implementation which binds to an `Image`. Here is how to do it in code.

```cs
var tappedCommand = new Command(() =>
{
    //handle the tap
});

var tapGestureRecognizer = new TapGestureRecognizer { Command = tappedCommand };
image.GestureRecognizers.Add(tapGestureRecognizer);

```

Or in XAML:

```cs
<Image Source="tapped.jpg">
    <Image.GestureRecognizers>
        <TapGestureRecognizer
                Command="{Binding TappedCommand}"
                NumberOfTapsRequired="2" />
  </Image.GestureRecognizers>
</Image>

```

Here the command is set by using data binding.
As you can see you can also set the `NumberOfTapsRequired` to enable it for more taps before it takes action. The default value is 1 tap.

Other gestures are Pinch and Pan.



## Zoom an Image with the Pinch gesture


In order to make an `Image` (or any other visual element) zoomable we have to add a `PinchGestureRecognizer` to it. Here is how to do it in code:

```cs
var pinchGesture = new PinchGestureRecognizer();
pinchGesture.PinchUpdated += (s, e) => {
// Handle the pinch
};
    
image.GestureRecognizers.Add(pinchGesture);

```

But it can also be done from XAML:

```cs
<Image Source="waterfront.jpg">
  <Image.GestureRecognizers>
    <PinchGestureRecognizer PinchUpdated="OnPinchUpdated" />
  </Image.GestureRecognizers>
</Image>

```

In the accompanied event handler you should provide the code to zoom your image. Of course other uses can be implement as well.

```cs
void OnPinchUpdated (object sender, PinchGestureUpdatedEventArgs e)
{
    // ... code here
}

```

Other gestures are Tap and Pan.



## Show all of the zoomed Image content with the PanGestureRecognizer


When you have a zoomed `Image` (or other content) you may want to drag around the `Image` to show all of its content in the zoomed in state.

This can be achieved by implementing the PanGestureRecognizer.
From code this looks like so:

```cs
var panGesture = new PanGestureRecognizer();
panGesture.PanUpdated += (s, e) => {
  // Handle the pan
};

image.GestureRecognizers.Add(panGesture);

```

This can also be done from XAML:

```cs
<Image Source="MonoMonkey.jpg">
  <Image.GestureRecognizers>
    <PanGestureRecognizer PanUpdated="OnPanUpdated" />
  </Image.GestureRecognizers>
</Image>

```

In the code-behind event you can now handle the panning accordingly. Use this method signature to handle it:

```cs
void OnPanUpdated (object sender, PanUpdatedEventArgs e)
{
  // Handle the pan
}

```



## Place a pin where the user touched the screen with MR.Gestures


Xamarins built in gesture recognizers provide only very basic touch handling. E.g. there is no way to get the position of a touching finger.
MR.Gestures is a component which adds 14 different touch handling events. The position of the touching fingers is part of the `EventArgs` passed to all MR.Gestures events.

If you want to place a pin anywhere on the screen, the easiest way is to use an `MR.Gestures.AbsoluteLayout` which handles the `Tapping` event.

```cs
<mr:AbsoluteLayout x:Name="MainLayout" Tapping="OnTapping">
    ...
</mr:AbsoluteLayout>

```

As you can see the `Tapping="OnTapping"` also feels more like .NET than Xamarins syntax with the nested `GestureRecognizers`. That syntax was copied from iOS and it smells a bit for .NET developers.

In your code behind you could add the `OnTapping` handler like this:

```cs
private void OnTapping(object sender, MR.Gestures.TapEventArgs e)
{
    if (e.Touches?.Length > 0)
    {
        Point touch = e.Touches[0];
        var image = new Image() { Source = "pin" };
        MainLayout.Children.Add(image, touch);
    }
}

```

Instead of the `Tapping` event, you could also use the `TappingCommand` and bind to your ViewModel, but that would complicate things in this simple example.

More samples for MR.Gestures can be found in the [GestureSample app on GitHub](https://github.com/MichaelRumpler/GestureSample) and on the [MR.Gestures website](http://www.mrgestures.com/). These also show how to use all the other touch events with event handlers, commands, MVVM, ...

