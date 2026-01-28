---
metaTitle: "iOS - UIAlertController"
description: "AlertViews with UIAlertController, Action Sheets with UIAlertController, Adding Text Field in  UIAlertController like a prompt Box, Highlighting an action button, Displaying and handling alerts, Temporary toast-like pop up"
---

# UIAlertController




## AlertViews with UIAlertController


`UIAlertView` and `UIActionSheet` are Deprecated in `iOS 8` and Later. So Apple introduced a new controller for `AlertView` and `ActionSheet` called `UIAlertController` , changing the `preferredStyle`, you can switch between `AlertView` and `ActionSheet`. There is no delegate method for it because all button events are handled in their blocks.

**Simple AlertView**

****Swift:****

```swift
let alert = UIAlertController(title: "Simple", message: "Simple alertView demo with Cancel and OK.", preferredStyle: .alert)

alert.addAction(UIAlertAction(title: "Cancel", style: .cancel) { _ in
        print("Cancel")
})
alert.addAction(UIAlertAction(title: "OK", style: .default) { _ in
        print("OK")
})

present(alert, animated: true)

```

****Objective-C:****

```

UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Simple" message:@"Simple alertView demo with Cancel and OK." preferredStyle:UIAlertControllerStyleAlert];
    UIAlertAction *cancelAction = [UIAlertAction actionWithTitle:@"Cancel" style:UIAlertActionStyleCancel handler:^(UIAlertAction * action) {
        NSLog(@"Cancel");
    }];
    UIAlertAction *okAction = [UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:^(UIAlertAction * action) {
        NSLog(@"OK");
    }];
    
    [alertController addAction:cancelAction];
    [alertController addAction:okAction];
    [self presentViewController:alertController animated: YES completion: nil];

```

[<img src="https://i.stack.imgur.com/Pyu1U.png" alt="enter image description here" />](https://i.stack.imgur.com/Pyu1U.png)

**Destructive AlertView**

****Swift:****

```swift
let alert = UIAlertController(title: "Simple", message: "Simple alertView demo with Cancel and OK.", preferredStyle: .alert)

alert.addAction(UIAlertAction(title: "Destructive", style: .destructive) { _ in
        print("Destructive")
})
alert.addAction(UIAlertAction(title: "OK", style: .default) { _ in
        print("OK")
})

present(alert, animated: true)

```

****Objective-C:****

```swift
UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Destructive" message:@"Simple alertView demo with Destructive and OK." preferredStyle:UIAlertControllerStyleAlert];
    UIAlertAction *destructiveAction = [UIAlertAction actionWithTitle:@"Cancel" style:UIAlertActionStyleDestructive handler:^(UIAlertAction * action) {
        NSLog(@"Destructive");
    }];
    UIAlertAction *okAction = [UIAlertAction actionWithTitle:@"OK" style:UIAlertActionStyleDefault handler:^(UIAlertAction * action) {
        NSLog(@"OK");
    }];
    
    [alertController addAction:destructiveAction];
    [alertController addAction:okAction];
    [self presentViewController:alertController animated: YES completion: nil];

```

[<img src="https://i.stack.imgur.com/nQDRl.png" alt="enter image description here" />](https://i.stack.imgur.com/nQDRl.png)



## Action Sheets with UIAlertController


With `UIAlertController`, action sheets like the deprecated `UIActionSheet` are created with the same API as you use for AlertViews.

### Simple Action Sheet with two buttons

### Swift

```swift
let alertController = UIAlertController(title: "Demo", message: "A demo with two buttons", preferredStyle: UIAlertControllerStyle.actionSheet)

```

### Objective-C

```swift
UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Demo" message:@"A demo with two buttons" preferredStyle:UIAlertControllerStyleActionSheet];

```

Create the buttons "Cancel" and "Okay"

### Swift

```swift
let cancelAction = UIAlertAction(title: "Cancel", style: .cancel) { (result : UIAlertAction) -> Void in
    //action when pressed button
}
let okAction = UIAlertAction(title: "Okay", style: .default) { (result : UIAlertAction) -> Void in
    //action when pressed button
}

```

### Objective-C

```swift
UIAlertAction *cancelAction = [UIAlertAction actionWithTitle:@"Cancel" style:UIAlertActionStyleCancel handler:^(UIAlertAction * action) {
        //action when pressed button
    }];

UIAlertAction * okAction = [UIAlertAction actionWithTitle:@"Okay" style:UIAlertActionStyleDefault handler:^(UIAlertAction * action) {
        //action when pressed button
    }];

```

And add them to the action sheet:

### Swift

```swift
alertController.addAction(cancelAction)
alertController.addAction(okAction)

```

### Objective-C

```swift
[alertController addAction:cancelAction];
[alertController addAction:okAction];

```

Now present the `UIAlertController`:

### Swift

```swift
self.present(alertController, animated: true, completion: nil)

```

### Objective-C

```swift
[self presentViewController:alertController animated: YES completion: nil];

```

This should be the result:

[<img src="http://i.stack.imgur.com/M0uT4.png" alt="UIAlertController Action Sheet example" />](http://i.stack.imgur.com/M0uT4.png)

### Action Sheet with destructive button

Using the `UIAlertActionStyle` `.destructive` for an `UIAlertAction` will create a button with red tint color.

[<img src="http://i.stack.imgur.com/yfT3b.png" alt="Destructive button" />](http://i.stack.imgur.com/yfT3b.png)

For this example, the `okAction` from above was replaced by this `UIAlertAction`:

### Swift

```swift
let destructiveAction = UIAlertAction(title: "Delete", style: .destructive) { (result : UIAlertAction) -> Void in
    //action when pressed button
}

```

### Objective-C

```swift
UIAlertAction * destructiveAction = [UIAlertAction actionWithTitle:@"Delete" style:UIAlertActionStyleDestructive handler:^(UIAlertAction * action) {
            //action when pressed button
        }];

```



## Adding Text Field in  UIAlertController like a prompt Box


### Swift

```swift
let alert = UIAlertController(title: "Hello",
                              message: "Welcome to the world of iOS",
                              preferredStyle: UIAlertControllerStyle.alert)
let defaultAction = UIAlertAction(title: "OK", style: UIAlertActionStyle.default) { (action) in
        
}
defaultAction.isEnabled = false
alert.addAction(defaultAction)
    
alert.addTextFieldWithConfigurationHandler { (textField) in
     textField.delegate = self
}
    
present(alert, animated: true, completion: nil)

```

### Objective-C

```swift
UIAlertController* alert = [UIAlertController alertControllerWithTitle:@"Hello"
                                                               message:@"Welcome to the world of iOS"
                                                        preferredStyle:UIAlertControllerStyleAlert];
    
UIAlertAction* defaultAction = [UIAlertAction actionWithTitle:@"OK" 
                                                        style:UIAlertActionStyleDefault
                                                      handler:^(UIAlertAction * action) {}];
    
defaultAction.enabled = NO;
[alert addAction:defaultAction];    
    
[alert addTextFieldWithConfigurationHandler:^(UITextField *textField) {
     textField.delegate = self;
}];        

[self presentViewController:alert animated:YES completion:nil];

```

[<img src="https://i.stack.imgur.com/ga7am.png" alt="enter image description here" />](https://i.stack.imgur.com/ga7am.png)



## Highlighting an action button


Alert controller has a property which is used to put emphases on an action added in the alert controller. This property can be used to highlight a particular action for user attention.For objective C;

```swift
@property(nonatomic, strong) UIAlertAction *preferredAction

```

An action **which is already added in alert controller** can be assigned to this property.The Alert Controller will highlight this action.

**This property can only be used with UIAlertControllerStyleAlert.**

Following example shows how to use it.

```swift
UIAlertController *alertController = [UIAlertController alertControllerWithTitle:@"Cancel edit" message:@"Are you really want to cancel your edit?" preferredStyle:UIAlertControllerStyleAlert];

UIAlertAction *cancel = [UIAlertAction actionWithTitle:@"Cancel" style:UIAlertActionStyleCancel handler:^(UIAlertAction * action) {
    NSLog(@"Cancel");
}];

UIAlertAction *no = [UIAlertAction actionWithTitle:@"NO" style:UIAlertActionStyleDefault handler:^(UIAlertAction * action) {
    NSLog(@"Highlighted button is pressed.");
}];

[alertController addAction:cancel];
[alertController addAction:no];

//add no action to preffered action.
//Note
//the action should already be added to alert controller
alertController.preferredAction = no;

[self presentViewController:alertController animated: YES completion: nil];

```

Alert Controller with **preferred action set**.The **NO** button is highlighted.

[<img src="http://i.stack.imgur.com/C7LVT.png" alt="enter image description here" />](http://i.stack.imgur.com/C7LVT.png)

Alert Controller with **preferred action not set**.The **NO** button is not highlighted.

[<img src="http://i.stack.imgur.com/WLLu0.png" alt="enter image description here" />](http://i.stack.imgur.com/WLLu0.png)



## Displaying and handling alerts


### One Button

[<img src="http://i.stack.imgur.com/x5V0q.png" alt="enter image description here" />](http://i.stack.imgur.com/x5V0q.png)

### Swift

```swift
class ViewController: UIViewController {

    @IBAction func showAlertButtonTapped(sender: UIButton) {
        
        // create the alert
        let alert = UIAlertController(title: "My Title", message: "This is my message.", preferredStyle: UIAlertControllerStyle.Alert)
        
        // add an action (button)
        alert.addAction(UIAlertAction(title: "OK", style: UIAlertActionStyle.Default, handler: nil))

        // show the alert
        self.presentViewController(alert, animated: true, completion: nil)
    }
}

```

### Two Buttons

[<img src="http://i.stack.imgur.com/3Rq2U.png" alt="enter image description here" />](http://i.stack.imgur.com/3Rq2U.png)

### Swift

```swift
class ViewController: UIViewController {

    @IBAction func showAlertButtonTapped(sender: UIButton) {
        
        // create the alert
        let alert = UIAlertController(title: "UIAlertController", message: "Would you like to continue learning how to use iOS alerts?", preferredStyle: UIAlertControllerStyle.Alert)
        
        // add the actions (buttons)
        alert.addAction(UIAlertAction(title: "Continue", style: UIAlertActionStyle.Default, handler: nil))
        alert.addAction(UIAlertAction(title: "Cancel", style: UIAlertActionStyle.Cancel, handler: nil))
        
        // show the alert
        self.presentViewController(alert, animated: true, completion: nil)
    }
}

```

### Three Buttons

[<img src="http://i.stack.imgur.com/IG91U.png" alt="enter image description here" />](http://i.stack.imgur.com/IG91U.png)

### Swift

```swift
class ViewController: UIViewController {

    @IBAction func showAlertButtonTapped(sender: UIButton) {
        
        // create the alert
        let alert = UIAlertController(title: "Notice", message: "Lauching this missile will destroy the entire universe. Is this what you intended to do?", preferredStyle: UIAlertControllerStyle.Alert)
        
        // add the actions (buttons)
        alert.addAction(UIAlertAction(title: "Remind Me Tomorrow", style: UIAlertActionStyle.Default, handler: nil))
        alert.addAction(UIAlertAction(title: "Cancel", style: UIAlertActionStyle.Cancel, handler: nil))
        alert.addAction(UIAlertAction(title: "Launch the Missile", style: UIAlertActionStyle.Destructive, handler: nil))
        
        // show the alert
        self.presentViewController(alert, animated: true, completion: nil)
    }
}

```

### Handling Button Taps

The `handler` was `nil` in the above examples. You can replace `nil` with a [closure](https://developer.apple.com/library/ios/documentation/Swift/Conceptual/Swift_Programming_Language/Closures.html) to do something when the user taps a button, like the example below:

### Swift

```swift
alert.addAction(UIAlertAction(title: "Launch the Missile", style: UIAlertActionStyle.Destructive, handler: { action in
    
    // do something like...
    self.launchMissile()
    
}))

```

### Notes

- Multiple buttons do not necessarily need to use different `UIAlertActionStyle` types. They could all be `.Default`.
- For more than three buttons consider using an Action Sheet. The setup is very similar. [Here is an example.](http://stackoverflow.com/a/32991999/3681880)



## Temporary toast-like pop up


Good for quick notifications that don't require interaction.

### Swift

```swift
let alert = UIAlertController(title: "Toast", message: "Hello World", preferredStyle: .Alert)

presentViewController(alert, animated: true) {
     let delay_s:Double = 2
     let delayTime = dispatch_time(DISPATCH_TIME_NOW, Int64(delay_s * Double(NSEC_PER_SEC)))
     dispatch_after(delayTime, dispatch_get_main_queue()) {
          alert.dismissViewControllerAnimated(true, completion: nil)
     }
}

```



#### Remarks


> 
<p>A `UIAlertController` object displays an alert message to the user.
This class replaces the `UIActionSheet` and `UIAlertView` classes for
displaying alerts. After configuring the alert controller with the
actions and style you want, present it using the
`presentViewController:animated:completion:` method.</p>


From [Apple's Documentation](https://developer.apple.com/library/ios/documentation/UIKit/Reference/UIAlertController_class/)

[UIAlertController in Swift](https://iosdevcenters.blogspot.com/2016/03/uialertcontroller-in-swift.html)

