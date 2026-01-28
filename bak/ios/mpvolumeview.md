---
metaTitle: "iOS - MPVolumeView"
description: "Adding a MPVolumeView"
---

# MPVolumeView


The MPVolumeView class is volume view to presents the user with a slider control for setting the system audio output volume, and a button for choosing the audio output route.



## Adding a MPVolumeView


```swift
// Add MPVolumeView in a holder view
let mpVolumeHolderView = UIView(frame: CGRect(x: 0, y: view.bounds.midY, width: view.bounds.width, height: view.bounds.height))
// Set the holder view's background color to transparent
mpVolumeHolderView.backgroundColor = .clear
let mpVolume = MPVolumeView(frame: mpVolumeHolderView.bounds)
mpVolume.showsRouteButton = true
mpVolumeHolderView.addSubview(mpVolume)
view.addSubview(mpVolumeHolderView)
// the volume view is white, set the parent background to black to show it better in this example
view.backgroundColor = .black

```

`!!!` A very important note is that the MPVolumeView only works on an actual device and not on a simulator.

[<img src="https://i.stack.imgur.com/mBNOt.png" alt="MPVolumeView with slider and route button in device" />](https://i.stack.imgur.com/mBNOt.png)



#### Remarks


MPVolumeView only shows up when building and running on an actual iOS device and will not work in a simulator.

