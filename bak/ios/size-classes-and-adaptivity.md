---
metaTitle: "iOS - Size Classes and Adaptivity"
description: "Size Classes and Adaptivity through Storyboard"
---

# Size Classes and Adaptivity



## Size Classes and Adaptivity through Storyboard


We can add adaptivity to any subclass of `UIView` which we add on view controller in nib file.<br />
Lets take an example of adding adaptivity using size classes to a view.

1. Add a view on view controller as:

[<img src="http://i.stack.imgur.com/Wvis9.png" alt="enter image description here" />](http://i.stack.imgur.com/Wvis9.png)

1. Now we need to pin this view to it's superview for fixing it's size and position using **constraints** as:

[<img src="http://i.stack.imgur.com/ocSWe.png" alt="enter image description here" />](http://i.stack.imgur.com/ocSWe.png)

1. We can see the added constraints as:

[<img src="http://i.stack.imgur.com/ttiC2.png" alt="enter image description here" />](http://i.stack.imgur.com/ttiC2.png)

These constraints defines that the added view will be placed in it's superview as

```swift
CGRect(20, 0, superview.width - 20, superview.height - 20)

```


1. To see the preview on screen of these added constraints we can use **Assistant Editor** as;

[<img src="http://i.stack.imgur.com/l7W4L.png" alt="enter image description here" />](http://i.stack.imgur.com/l7W4L.png)

1. We can add more screen to see preview like:

[<img src="http://i.stack.imgur.com/vEbj2.png" alt="enter image description here" />](http://i.stack.imgur.com/vEbj2.png)

We can also see the preview with landscape mode by moving mouse on the name of device and clicking the rotation button as:

[<img src="http://i.stack.imgur.com/XCRdo.png" alt="enter image description here" />](http://i.stack.imgur.com/XCRdo.png)



#### Remarks


For more details ( Size Classes and Adaptivity through Storyboard) of using auto layout for adaptivity in iOS, we can follow the [apple developer site link](https://developer.apple.com/library/ios/documentation/UserExperience/Conceptual/AutolayoutPG/).

We can also add constraints **Programatically** using **Visual Format Language** as described [here at apple developer site](https://developer.apple.com/library/prerelease/content/documentation/UserExperience/Conceptual/AutolayoutPG/ProgrammaticallyCreatingConstraints.html).

