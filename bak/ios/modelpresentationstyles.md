---
metaTitle: "iOS - ModelPresentationStyles"
description: "Exploring ModalPresentationStyle using Interface Builder"
---

# ModelPresentationStyles


Modal Presentation styles are used when you are transitioning from one view controller to another. There are 2 ways of achieving this customization. One is through code and another through Interface Builder(using segues). This effect is achieved by setting`modalPresentationStyle` variable to an instance of `UIModalPresentationStyle` enum. `modalPresentationStyle` property is a class variable of `UIViewController` and is used to specify how a `ViewController` is presented on screen.



## Exploring ModalPresentationStyle using Interface Builder


This will be a very basic app which will illustrate different `ModalpresentationStyle` in iOS. According to documentation found [here](https://developer.apple.com/reference/uikit/uimodalpresentationstyle), There are 9 different values for `UIModalPresentationStyle` which are as follows,

1. `fullScreen`
1. `pageSheet`
1. `formSheet`
1. `currentContext`
1. `custom`
1. `overFullScreen`
1. `overCurrentContext`
1. `popover`
1. `none`

To setup a project, just create a normal iOS project and add 2 `ViewControllers`. Put a `UIButton` in you initial `ViewController` and connect it to 2nd `ViewController` via a `Target -> Action` mechanism. To distinguish both `ViewControllers`, set background property of `UIView` in `ViewController` some other color. If all goes well, your Interface Builder should look something this,
[<img src="https://i.stack.imgur.com/u3T2N.png" alt="Initial Interface builder" />](https://i.stack.imgur.com/u3T2N.png)

Make sure you build this project and run it on **iPad** (For details on why iPad, refer to Remarks section). Once you are done setting up your project, select the segue and go to the `attributes inspector`. You should be able to see something like this,
[<img src="https://i.stack.imgur.com/dkaNm.png" alt="enter image description here" />](https://i.stack.imgur.com/dkaNm.png)

Set the kind property to `Present Modally`.

Now, we won't see all of the effects in this example as some of them requires little bit of code.

Let's start with `fullscreen`. This effect is selected by default when you select `Present Modally` in `Kind` tab. When you build and run, the 2nd `ViewController` would occupy the full screen of you iPad.

[<img src="https://i.stack.imgur.com/nImSM.png" alt="enter image description here" />](https://i.stack.imgur.com/nImSM.png)

Next is `pageSheet`. You can select this option from `Presentation` tab. In this option, when device is in portrait mode, the 2nd `ViewController` is similar to full screen but in landscape mode, 2nd `ViewController` is much narrow the device width. Also, any content not covered by 2nd `ViewController` will be dimmed.

[<img src="https://i.stack.imgur.com/guMIT.png" alt="enter image description here" />](https://i.stack.imgur.com/guMIT.png)

For `formSheet` style, the 2nd `ViewController` is placed in center of device and the size is smaller to that of device. Also when device is in landscape mode and keyboard is visible position of view is adjusted upwards to show the `ViewController`.

[<img src="https://i.stack.imgur.com/ZKu3t.png" alt="enter image description here" />](https://i.stack.imgur.com/ZKu3t.png)

Last style which we are going to try is `popover`. To select this style, select `Present as Popover` in `Kind` tab. The 2nd `ViewController` is presented as a small popover(size can be set). The background content is dimmed. Any tap outside the popover would dismiss the popover. Your `Attributes Inspector` should look something like this,

[<img src="https://i.stack.imgur.com/XTOSj.png" alt="enter image description here" />](https://i.stack.imgur.com/XTOSj.png)

`Anchor` is the UI element to which you want your popover arrow to point.
`Directions` are the directions you allow your popover `Anchor` to point in.

[<img src="https://i.stack.imgur.com/5DKN1.png" alt="enter image description here" />](https://i.stack.imgur.com/5DKN1.png)

There more then these basic Modal Presentation Styles but they are little complicated to achieve and require some code. More details can be found in the Apple Documentation.



#### Remarks


Always remember the following mention from Apple.

> 
In a horizontally compact environment, modal view controllers are always presented full-screen. In a horizontally regular environment, there are several different presentation options.


