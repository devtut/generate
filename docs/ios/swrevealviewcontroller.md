---
metaTitle: "iOS - SWRevealViewController"
description: "Setting up a basic app with SWRevealViewController"
---

# SWRevealViewController



## Setting up a basic app with SWRevealViewController


Create a basic application with single view application template with swift as language

Add `SWRevealViewController.h` and `SWRevealViewController.m`

then click on Create Bridging Header button

[<img src="http://i.stack.imgur.com/nqQZM.png" alt="enter image description here" />](http://i.stack.imgur.com/nqQZM.png)

and add

```swift
#import "SWRevealViewController.h"

```

on the Bridging header

Then select viewController on storyboard and change class to `SWRevealViewController`

[<img src="http://i.stack.imgur.com/vP10G.png" alt="enter image description here" />](http://i.stack.imgur.com/vP10G.png)

Then rename the viewController on files to MainViewController and add new ViewController with RightViewController name

[<img src="http://i.stack.imgur.com/4wiPd.png" alt="enter image description here" />](http://i.stack.imgur.com/4wiPd.png)

then we add two segues from SWRevealViewController to MainViewController and from SWRevealViewController to RightViewController, then we need to select the first (from SWRevealViewController to MainViewController) and edit properties

on identifier set `sw_front`
on Class set `SWRevealViewControllerSegueSetController`

[<img src="http://i.stack.imgur.com/019Uz.png" alt="enter image description here" />](http://i.stack.imgur.com/019Uz.png)

after this we need to do the same with the segue (from SWRevealViewController to RightViewController)

on identifier set `sw_rear`
on Class set `SWRevealViewControllerSegueSetController`

[<img src="http://i.stack.imgur.com/lBSc6.png" alt="enter image description here" />](http://i.stack.imgur.com/lBSc6.png)

then on MainViewController add this line on `viewDidLoad` method

```swift
self.view.addGestureRecognizer(self.revealViewController().panGestureRecognizer());

```

And this is all, you have a basic app with SWRevealViewController integrated, you can swipe to right to show `RightViewController` as lateral menu



#### Remarks


Using the SWRevealViewController class as the main navigation might not always result in the best user experience. If the sidebar contains only 5 or less entries (or the content can be compressed into 5 or less entries), you should consider using the default tab bar.

The tab bar is intuitive and allows the user to quickly change between views/contexts. On the other hand, the sidebar navigation can perform more actions than switching the view/context and uses less space when collapsed.

For more information check out Apple's [iOS Human Interface Guidelines](https://developer.apple.com/ios/human-interface-guidelines/).

