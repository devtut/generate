---
metaTitle: "iOS - UIViewController"
description: "Subclassing, Access the container view controller, Create an instance, Set the view programmatically, Instantiate from a Storyboard, Adding/removing a child view controller"
---

# UIViewController



## Subclassing


Subclassing `UIControl` gives us access to the following methods:

- `beginTrackingWithTouch` is called when the finger first touches down within the control's bounds.
- `continueTrackingWithTouch` is called repeatedly as the finger slides across the control and even outside of the control's bounds.
- `endTrackingWithTouch` is called when the finger lifts off the screen.

**MyCustomControl.swift**

```swift
import UIKit

// These are out self-defined rules for how we will communicate with other classes
protocol ViewControllerCommunicationDelegate: class {
    func myTrackingBegan()
    func myTrackingContinuing(location: CGPoint)
    func myTrackingEnded()
}

class MyCustomControl: UIControl {

    // whichever class wants to be notified of the touch events must set the delegate to itself
    weak var delegate: ViewControllerCommunicationDelegate?
    
    override func beginTrackingWithTouch(touch: UITouch, withEvent event: UIEvent?) -> Bool {
        
        // notify the delegate (i.e. the view controller)
        delegate?.myTrackingBegan()
        
        // returning true means that future events (like continueTrackingWithTouch and endTrackingWithTouch) will continue to be fired
        return true
    }
    
    override func continueTrackingWithTouch(touch: UITouch, withEvent event: UIEvent?) -> Bool {
        
        // get the touch location in our custom control's own coordinate system
        let point = touch.locationInView(self)
        
        // Update the delegate (i.e. the view controller) with the new coordinate point
        delegate?.myTrackingContinuing(point)
        
        // returning true means that future events will continue to be fired
        return true
    }
    
    override func endTrackingWithTouch(touch: UITouch?, withEvent event: UIEvent?) {
        
        // notify the delegate (i.e. the view controller)
        delegate?.myTrackingEnded()
    }
}

```

**ViewController.swift**

This is how the view controller is set up to be the delegate and respond to touch events from our custom control.

```swift
import UIKit
class ViewController: UIViewController, ViewControllerCommunicationDelegate {
    
    @IBOutlet weak var myCustomControl: MyCustomControl!
    @IBOutlet weak var trackingBeganLabel: UILabel!
    @IBOutlet weak var trackingEndedLabel: UILabel!
    @IBOutlet weak var xLabel: UILabel!
    @IBOutlet weak var yLabel: UILabel!
    
    override func viewDidLoad() {
        super.viewDidLoad()
        myCustomControl.delegate = self
    }

    func myTrackingBegan() {
        trackingBeganLabel.text = "Tracking began"
    }
    
    func myTrackingContinuing(location: CGPoint) {
        xLabel.text = "x: \(location.x)"
        yLabel.text = "y: \(location.y)"
    }
    
    func myTrackingEnded() {
        trackingEndedLabel.text = "Tracking ended"
    }
}

```

**Notes**

<li>
Alternate methods of achieving the same result without subclassing include adding a target or using a gesture recognizer.
</li>
<li>
It is not necessary to use a delegate with these methods if they are only being used within the custom control itself. We could have just added a `print` statement to show how the events are being called. In that case, the code would be simplified to

```swift
  import UIKit
  class MyCustomControl: UIControl {

      override func beginTrackingWithTouch(touch: UITouch, withEvent event: UIEvent?) -> Bool {
          print("Began tracking")
          return true
      }
  
      override func continueTrackingWithTouch(touch: UITouch, withEvent event: UIEvent?) -> Bool {
          let point = touch.locationInView(self)
          print("x: \(point.x), y: \(point.y)")
          return true
      }
  
      override func endTrackingWithTouch(touch: UITouch?, withEvent event: UIEvent?) {
          print("Ended tracking")
      }
  }

```


</li>



## Access the container view controller


When the view controller is presented within a tab bar controller, you can access the tab bar controller like this:

**Swift**

```swift
let tabBarController = viewController.tabBarController

```

**Objective-C**

```swift
UITabBarController *tabBarController = self.tabBarController;

```

When the view controller is part on an navigation stack, you can access the navigation controller like this:

**Swift**

```swift
let navigationController = viewController.navigationController

```

**Objective-C**

```swift
UINavigationController *navigationController = self.navigationController;

```



## Create an instance


**Swift**

```swift
let viewController = UIViewController()

```

**Objective-C**

```swift
UIViewController *viewController = [UIViewController new];

```



## Set the view programmatically


Swift

```swift
class FooViewController: UIViewController {

  override func loadView() {
    view = FooView()
  }

}

```



## Instantiate from a Storyboard


```swift
UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:nil];

```

**With an Identifier**:

Give the scene a Storyboard ID within the identity inspector of the storyboard.

[<img src="http://i.stack.imgur.com/aZyXo.png" alt="enter image description here" />](http://i.stack.imgur.com/aZyXo.png)

Instantiate in code:

```swift
UIViewController *controller = [storyboard instantiateViewControllerWithIdentifier:@"myIdentifier"];

```

**Instantiate an initial viewcontroller**:

Within the storyboard select the view controller, then select the attribute inspector, check the "Is Initial View Controller" box.

[<img src="http://i.stack.imgur.com/rc7Wi.png" alt="enter image description here" />](http://i.stack.imgur.com/rc7Wi.png)

```swift
UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:nil];
UIViewController *controller = [storyboard instantiateInitialViewController];

```



## Adding/removing a child view controller


To add a child view controller:

```swift
- (void)displayContentController:(UIViewController *)vc {
   [self addChildViewController:vc];
   vc.view.frame = self.view.frame;
   [self.view addSubview:vc.view];
   [vc didMoveToParentViewController:self];
}

```

To remove a child view controller:

```swift
- (void)hideContentController:(UIViewController *)vc {
   [vc willMoveToParentViewController:nil];
   [vc.view removeFromSuperview];
   [vc removeFromParentViewController];
}

```

