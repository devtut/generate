---
metaTitle: "iOS - IBOutlets"
description: "Using an IBOutlet in a UI Element"
---

# IBOutlets



## Using an IBOutlet in a UI Element


In general, IBOutlets are used to connect an user interface object to another object, in this case a UIViewController. The connection serves to allow for the object to be affected my code or events programmatically. This can be done simply by using the assistant from a storyboard and control-clicking from the element to the view controller's .h property section, but it can also be done programmatically and manually connecting the IBOutlet code to the "connections" tab of the object the utility bar on the right. Here is an objective-c example of a UIViewController with a label outlet:

```swift
//ViewController.h
#import <UIKit/UIKit.h>

@interface ViewController : UIViewController

//This is the declaration of the outlet
@property (nonatomic, weak) IBOutlet UILabel *myLabel;

@end

//ViewController.m
#import "ViewController.h"

@implementation ViewController

@synthesize myLabel;

-(void) viewDidLoad {

    [super viewDidLoad];
    //Editing the properties of the outlet
    myLabel.text = @"TextHere";
    
}

@end

```

And swift:

```swift
import UIKit
class ViewController: UIViewController {
    //This is the declaration of the outlet
    @IBOutlet weak var myLabel: UILabel!


    override func viewDidLoad() {
        super.viewDidLoad()
        //Editing the properties of the outlet
        myLabel.text = "TextHere"
    }
}

```

The connection between the storyboard object, and the programmed object can be verified as connected if the dot to the left of the declaration of the outlet in the .h is filled. An empty circle implied an incomplete connection.



#### Remarks


IBOutlet is neither a reserved word nor a variable or class, is syntactic sugar for Interface Builder. After the Objective-C source code is pre-processed it is resolved to nothing.

In Swift it's resolved as nil.

It's declared in `<UIKit/UINibDeclarations.h>`  as

```swift
#ifndef IBOutlet
#define IBOutlet
#endif

```

