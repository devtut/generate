---
metaTitle: "Xamarin - Controlling the Screenshot in the iOS Multitasking Switcher"
description: "Show an Image for Snapshot"
---

# Controlling the Screenshot in the iOS Multitasking Switcher


In the [App Programming Guide for iOS](https://developer.apple.com/library/content/documentation/iPhone/Conceptual/iPhoneOSProgrammingGuide/BackgroundExecution/BackgroundExecution.html#//apple_ref/doc/uid/TP40007072-CH4-SW8):

Remove sensitive information from views before moving to the background.

When an app transitions to the background, the system takes a snapshot of the app’s main window, which it then presents briefly when transitioning your app back to the foreground.



## Show an Image for Snapshot


```cs
public override void DidEnterBackground(UIApplication application)
 {
    //to add the background image in place of 'active' image
    var backgroundImage = new UIImageView();
    backgroundImage.Tag = 1234;
    backgroundImage.Image = UIImage.FromBundle("Background");
    backgroundImage.Frame = this.window.Frame;
    this.window.AddSubview(backgroundImage);
    this.window.BringSubviewToFront(backgroundImage);
}

public override void WillEnterForeground(UIApplication application)
{
    //remove 'background' image
    var backgroundView = this.window.ViewWithTag(1234);
    if(null != backgroundView)
        backgroundView.RemoveFromSuperview();
}

```



#### Remarks


Adapted from actual StackOverflow Question [Controlling the Screenshot in the iOS7 Multitasking Switcher](http://stackoverflow.com/questions/18959411/controlling-the-screenshot-in-the-ios-7-multitasking-switcher/41409351#41409351) and answer [Obj-c Answer](http://stackoverflow.com/questions/18959411/controlling-the-screenshot-in-the-ios-7-multitasking-switcher/20040270#20040270)

