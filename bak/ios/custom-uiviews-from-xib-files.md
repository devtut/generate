---
metaTitle: "iOS - Custom UIViews from XIB files"
description: "Wiring elements, How to make custom reusable UIView using XIB"
---

# Custom UIViews from XIB files




## Wiring elements


> 
Create a XIB file


Xcode Menu Bar > File > New > File.<br />
Select iOS, User Interface and then "View":

[<img src="https://i.stack.imgur.com/RSkzu.png" alt="First step" />](https://i.stack.imgur.com/RSkzu.png)

Give your XIB a name (yes, we are doing a Pokemon example 👾).<br />
**Remember to check your target** and hit "Create".

[<img src="https://i.stack.imgur.com/oJ1s1.png" alt="Second step" />](https://i.stack.imgur.com/oJ1s1.png)

> 
Design your view


To make things easier, set:

- Size: Freeform
- Status Bar: None
- Top Bar: None
- Bottom Bar: None

[<img src="https://i.stack.imgur.com/Gy0KD.png" alt="Third step" />](https://i.stack.imgur.com/Gy0KD.png)

Click on the Size Inspector and resize the view.<br />
For this example we'll be using width 321 and height 256.

[<img src="https://i.stack.imgur.com/PNIek.png" alt="Fourth step" />](https://i.stack.imgur.com/PNIek.png)

Drop some elements into your XIB file like shown below.<br />
Here we'll be adding an **Image View** (256x256) and a **Switch**.

[<img src="https://i.stack.imgur.com/6u3pd.png" alt="Fifth step" />](https://i.stack.imgur.com/6u3pd.png)

Add Auto-Layout constraints by clicking on "Resolve Auto Layout Issues" (bottom-right) and selecting "Add Missing Constraints" under "All Views".

[<img src="https://i.stack.imgur.com/esZhQ.png" alt="Sixth step" />](https://i.stack.imgur.com/esZhQ.png)

Preview the changes you made by clicking on "Show the Assistant Editor" (top-right), then "Preview".<br />
You can add iPhone screens by clicking on the "Plus" button.<br />
The preview should look like this:

[<img src="https://i.stack.imgur.com/D3rac.png" alt="Seventh step" />](https://i.stack.imgur.com/D3rac.png)

> 
Subclass UIView


Create the class that is going to manage the XIB file.<br />
Xcode Menu Bar > File > New > File.<br />
Select iOS / Source / Cocoa Touch Class. Hit "Next".

[<img src="https://i.stack.imgur.com/0EyHy.png" alt="Eighth step" />](https://i.stack.imgur.com/0EyHy.png)

Give the class a name, which must be the same name as the XIB file (Pokemon).<br />
Select UIView as the subclass type, then hit "Next".

[<img src="https://i.stack.imgur.com/qbo3s.png" alt="Nineth step" />](https://i.stack.imgur.com/qbo3s.png)

On the next window, select your target and hit "Create".

[<img src="https://i.stack.imgur.com/Ks9Tu.png" alt="Tenth step" />](https://i.stack.imgur.com/Ks9Tu.png)

> 
Connect Pokemon.xib to Pokemon.swift via "File’s Owner" attribute


Click on the Pokemon.xib file in Xcode.<br />
Click on the "File's Owner" outlet.<br />
On the "Identity inspector" (top-right), set the Class to our recently created Pokemon.swift file.

[<img src="https://i.stack.imgur.com/4YT3i.png" alt="Eleventh step" />](https://i.stack.imgur.com/4YT3i.png)

> 
POKEMONS!!!


Yes! Drag and drop some Pokemons into your project to finish up our "infrastructure".<br />
Here we are adding two PGN files, 256x256, transparent.

[<img src="https://i.stack.imgur.com/ibqxZ.png" alt="Twelfth step" />](https://i.stack.imgur.com/ibqxZ.png)

> 
Show me code already.


All right, all right.<br />
Time to add some code to our Pokemon.swift class.

It's actually pretty simple:

1. Implement required initializers
1. Load the XIB file
1. Configure the view that will display the XIB file
1. Show the above view

Add the following code to the Pokemon.swift class:

```swift
import UIKit

class Pokemon: UIView {
    
    // MARK: - Initializers
    
    override init(frame: CGRect) {
        super.init(frame: frame)
        setupView()
    }
    
    required init?(coder aDecoder: NSCoder) {
        super.init(coder: aDecoder)
        setupView()
    }
    
    // MARK: - Private Helper Methods
    
    // Performs the initial setup.
    private func setupView() {
        let view = viewFromNibForClass()
        view.frame = bounds

        // Auto-layout stuff.
        view.autoresizingMask = [
            UIViewAutoresizing.flexibleWidth,
            UIViewAutoresizing.flexibleHeight
        ]

        // Show the view.
        addSubview(view)
    }
    
    // Loads a XIB file into a view and returns this view.
    private func viewFromNibForClass() -> UIView {
        
        let bundle = Bundle(for: type(of: self))
        let nib = UINib(nibName: String(describing: type(of: self)), bundle: bundle)
        let view = nib.instantiate(withOwner: self, options: nil).first as! UIView
        
        /* Usage for swift < 3.x
        let bundle = NSBundle(forClass: self.dynamicType)
        let nib = UINib(nibName: String(self.dynamicType), bundle: bundle)
        let view = nib.instantiateWithOwner(self, options: nil)[0] as! UIView
        */

        return view
    }
}

```

> 
@IBDesignable and @IBInspectable


By adding `@IBDesignable` to your class, you make possible for it to live-render in Interface Builder.<br />
By adding `@IBInspectable` to the properties of your class, you can see your custom views changing in Interface Builder as soon as you modify those properties.

Let's make the `Image View` of our custom view "Inspectable".

First, hook up the `Image View` from the Pokemon.xib file to the Pokemon.swift class.

[<img src="https://i.stack.imgur.com/Dxyv2.png" alt="Thirteenth step" />](https://i.stack.imgur.com/Dxyv2.png)

Call the outlet `imageView` and then add the following code (notice the `@IBDesignable` before the class name):

```swift
@IBDesignable class Pokemon: UIView {
    
    // MARK: - Properties
    
    @IBOutlet weak var imageView: UIImageView!
    
    @IBInspectable var image: UIImage? {
        get {
            return imageView.image
        }
        set(image) {
            imageView.image = image
        }
    }

    // MARK: - Initializers
    ...

```

> 
Using your Custom Views


Got to your Main storyboard file, drag a UIView into it.<br />
Resize the view to, say 200x200. Centralize.<br />
Go to the Identity inspector (top-right) and set the Class to Pokemon.

[<img src="https://i.stack.imgur.com/Iah3Y.png" alt="Fourteenth steps" />](https://i.stack.imgur.com/Iah3Y.png)

To select a Pokemon, go to the Attribute Inspector (top-right) and select one of the Pokemon images you previously added using the awesome `@IBInspectable` image property.

[<img src="https://i.stack.imgur.com/727mm.png" alt="Fifteenth step" />](https://i.stack.imgur.com/727mm.png)

Now duplicate your custom Pokemon view.<br />
Give it a different size, say 150x150.<br />
Choose another Pokemon image, observe:

[<img src="https://i.stack.imgur.com/xrTXw.png" alt="Sixteenth step" />](https://i.stack.imgur.com/xrTXw.png)

Now we are going to add more logic to that self-containing custom UI element.<br />
The button will allow Pokemons to be enabled/disabled.

Create an `IBAction` from the Switch button to the Pokemon.swift class.<br />
Call the action something like `switchTapped`.<br />
Add the following code to it:

```swift
// MARK: - Actions

@IBAction func switchTapped(sender: UISwitch) {
    imageView.alpha = sender.on ? 1.0 : 0.2
}

// MARK: - Initializers
...

```

Final result:

[<img src="https://i.stack.imgur.com/DQJvO.gif" alt="Final" />](https://i.stack.imgur.com/DQJvO.gif)

You are done!<br />
Now you can create complex custom views and reuse them anywhere you want.<br />
This will increase productivity while isolating code into self-contained UI elements.

[The final project can be cloned in Github.](https://github.com/singledev/custom-uiviews-from-xib)<br />
(**Updated to Swift 3.1**)



## How to make custom reusable UIView using XIB


Following example shows steps involved in initializing a view from XIB.

This is not a complex operation but exact steps need to be followed in order to do it right way first time, avoiding exceptions.

[How does loadNibNamed Works](http://stackoverflow.com/questions/20323393/how-does-loadnibnamed-work-uiview-outlets-not-initializing-using-loadnibnamed)

Main steps are:

1. Create XIB
1. Create class .h and .m
1. Define outlets in .h
1. Connect outlets between .h and XIB

See attached screenshot:

[<img src="http://i.stack.imgur.com/rSBw6.png" alt="Label Object connected to myLabel IBOutlet UILabel variable" />](http://i.stack.imgur.com/rSBw6.png)

1. Invoke loadNibNamed inside initWithCoder function of .m file. This is needed to ensure you can directly place UIView object into storyboard / Parent UIView XIB file and define it as your custom view. No other initialization code is needed once you load the storyboard / parent XIB. Your custom view can be added to other views just like other built-in Objective C view objects given in XCode.



#### Remarks


[From Apple: Creating a Custom View That Renders in Interface Builder](https://developer.apple.com/library/ios/recipes/xcode_help-IB_objects_media/Chapters/CreatingaLiveViewofaCustomObject.html)

- Note: Keep in mind that if you'd use fancy 'custom'  fonts in your XIB elements (such UILabel, UITextField etc) then the initial loading time of your XIB will be longer depending on the font chosen and system version.

