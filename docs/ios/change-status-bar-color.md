---
metaTitle: "iOS - Change Status Bar Color"
description: "For non-UINavigationBar status bars, For UINavigationBar status bars, If you cannot change ViewController's code, For ViewController containment, Changing the status bar style for the entire application"
---

# Change Status Bar Color




## For non-UINavigationBar status bars


1. In info.plist set `View controller-based status bar appearance` to `YES`
1. In view controllers not contained by `UINavigationController` implement this method.

**In Objective-C:**

```swift
- (UIStatusBarStyle)preferredStatusBarStyle
{ 
    return UIStatusBarStyleLightContent; 
}

```

**In Swift:**

```swift
override func preferredStatusBarStyle() -> UIStatusBarStyle {
    return UIStatusBarStyle.LightContent
}

```



## For UINavigationBar status bars


Subclass UINavigationController and then override these methods:

In Objective-C:

```swift
- (UIStatusBarStyle)preferredStatusBarStyle
{ 
    return UIStatusBarStyleLightContent; 
}

```

In Swift:

```swift
override func preferredStatusBarStyle() -> UIStatusBarStyle {
    return .lightContent
}

```

Alternatively, you can set `barStyle` on the `UINavigationBar` instance:

Objective C:

```swift
// e.g. in your view controller's viewDidLoad method:
self.navigationController.navigationBar.barStyle = UIBarStyleBlack;  // this will give you a white status bar

```

Swift

```swift
// e.g. in your view controller's viewDidLoad method:
navigationController?.navigationBar.barStyle = .black // this will give you a white status bar

```

`UIBarStyle` options are `default`, `black`, `blackOpaque`, `blackTranslucent`. The latter 3 should all give you a status bar with white text, just the last two specify the opacity of the bar.

Note: you can still change the appearance of your navigation bar as you like.



## If you cannot change ViewController's code


If you are using library that contains (for example) AwesomeViewController with a wrong status bar color you can try this:

```

 let awesomeViewController = AwesomeViewController()
  awesomeViewController.navigationBar.barStyle = .blackTranslucent // or other style

```



## For ViewController containment


If you are using `UIViewControllerContainment` there are a few other methods that are worth looking at.

When you want a child viewController to control the presentation of the status bar (i.e. if the child is positioned at the top of the screen

in Swift

```swift
class RootViewController: UIViewController {

    private let messageBarViewController = MessageBarViewController()        

    override func childViewControllerForStatusBarStyle() -> UIViewController? {
        return messageBarViewController
    }

    override func viewDidLoad() {
        super.viewDidLoad()
        
        //add child vc code here...

        setNeedsStatusBarAppearanceUpdate()
    }
}

class MessageBarViewController: UIViewController {
    
    override func preferredStatusBarStyle() -> UIStatusBarStyle {
        return .Default 
    }
}

```



## Changing the status bar style for the entire application


### <br>**SWIFT:**

### Step 1:

In your **Info.plist** add the following attribute:

```swift
View controller-based status bar appearance

```

and set its value to

```swift
NO

```

as described in the image below:

[<img src="https://i.stack.imgur.com/4EF5C.png" alt="enter image description here" />](https://i.stack.imgur.com/4EF5C.png)<br><br>

### Step 2:

In your **AppDelegate.swift** file, in `didFinishLaunchingWithOptions` method, add this code:

```swift
UIApplication.shared.statusBarStyle = .lightContent

```

**or**

```swift
UIApplication.shared.statusBarStyle = .default

```


<li>
<p>The **.lightContent** option will set the colour of the **statusBar**
to white, for the entire app.</p>
</li>
<li>
<p>The **.default** option will set the colour of the **statusBar** to
the original black colour, for the entire app.<br><br></p>
</li>

### **OBJECTIVE-C:**

<br> Follow the first step from the **SWIFT** Section.
Then add this code to the **AppDelegate.m** file:

```swift
[[UIApplication sharedApplication] setStatusBarStyle:UIStatusBarStyleLightContent];

```

**or**

```swift
[[UIApplication sharedApplication] setStatusBarStyle:UIStatusBarStyleDefault];

```

