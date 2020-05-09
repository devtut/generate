---
metaTitle: "iOS - Managing the Keyboard"
description: "Create a custom in-app keyboard, Dismiss a keyboard with tap on view, Managing the Keyboard Using a Singleton + Delegate, Scrolling a UIScrollView/UITableView When Displaying the Keyboard, Moving view up or down when keyboard is present"
---

# Managing the Keyboard




## Create a custom in-app keyboard


[<img src="http://i.stack.imgur.com/10lPa.gif" alt="custom keyboard animated gif" />](http://i.stack.imgur.com/10lPa.gif)

This is a basic in-app keyboard. The same method could be used to make just about any keyboard layout. Here are the main things that need to be done:

- Create the keyboard layout in an .xib file, whose owner is a Swift or Objective-C class that is a `UIView` subclass.
- Tell the `UITextField` to use the custom keyboard.
- Use a delegate to communicate between the keyboard and the main view controller.

### Create the .xib keyboard layout file

- In Xcode go to **File > New > File... > iOS > User Interface > View** to create the .xib file.
- I called mine Keyboard.xib
- Add the buttons that you need.
- Use auto layout constraints so that no matter what size the keyboard is, the buttons will resize accordingly.
- Set the File's Owner (not the root view) to be the `Keyboard` class. This is a common source of error. You'll create this class in the next step. See the note at the end.

### Create the .swift UIView subclass keyboard file

<li>
In Xcode go to **File > New > File... > iOS > Source > Cocoa Touch Class** to create the Swift or Objective-C class. Choose `UIView` as a superclass for newly created class
</li>
<li>
I called mine `Keyboard.swift` (`Keyboard` class in Objective-C)
</li>
<li>
Add the following code for Swift:

```swift
  import UIKit

  // The view controller will adopt this protocol (delegate)
  // and thus must contain the keyWasTapped method
  protocol KeyboardDelegate: class {
      func keyWasTapped(character: String)
  }

  class Keyboard: UIView {
  
      // This variable will be set as the view controller so that 
      // the keyboard can send messages to the view controller.
      weak var delegate: KeyboardDelegate?

      // MARK:- keyboard initialization
  
      required init?(coder aDecoder: NSCoder) {
          super.init(coder: aDecoder)
          initializeSubviews()
      }
  
      override init(frame: CGRect) {
          super.init(frame: frame)
          initializeSubviews()
      }
  
      func initializeSubviews() {
          let xibFileName = "Keyboard" // xib extention not included
          let view = NSBundle.mainBundle().loadNibNamed(xibFileName, owner: self, options: nil)[0] as! UIView
          self.addSubview(view)
          view.frame = self.bounds
      }
  
      // MARK:- Button actions from .xib file
  
      @IBAction func keyTapped(sender: UIButton) {
          // When a button is tapped, send that information to the 
          // delegate (ie, the view controller)
          self.delegate?.keyWasTapped(sender.titleLabel!.text!) // could alternatively send a tag value
      }
  
  }

```


</li>
<li>
Add the following code for Objective-C:
**Keyboard.h File**

```swift
#import <UIKit/UIKit.h>

// The view controller will adopt this protocol (delegate)
// and thus must contain the keyWasTapped method
@protocol KeyboardDelegate<NSObject>
- (void)keyWasTapped:(NSString *)character;
@end

@interface Keyboard : UIView
@property (nonatomic, weak) id<KeyboardDelegate> delegate;  
@end

```


**Keyboard.m File**

```swift
#import "Keyboard.h"

@implementation Keyboard

- (id)initWithCoder:(NSCoder *)aDecoder {
    self = [super initWithCoder:aDecoder];
    [self initializeSubviews];
    return self;
}

- (id)initWithFrame:(CGRect)frame {
    self = [super initWithFrame:frame];
    [self initializeSubviews];
    return self;
}

- (void)initializeSubviews {
    NSString *xibFileName = @"Keyboard"; // xib extention not included
    UIView *view = [[[NSBundle mainBundle] loadNibNamed:xibFileName owner:self options:nil] firstObject];
    [self addSubview:view];
    view.frame = self.bounds;
}

// MARK:- Button actions from .xib file

-(IBAction)keyTapped:(UIButton *)sender {
    // When a button is tapped, send that information to the
    // delegate (ie, the view controller)
    [self.delegate keyWasTapped:sender.titleLabel.text]; // could alternatively send a tag value
}

@end

```


</li>

- Control drag actions from the buttons to button callback in the .xib file to the `@IBAction` method in the Swift or Objective-C owner to hook them all up.
- Note that the protocol and delegate code. See [this answer](http://stackoverflow.com/a/33549729/3681880) for a simple explanation about how delegates work.

### Set up the View Controller

<li>
Add a `UITextField` to your main storyboard and connect it to your view controller with an `IBOutlet`. Call it `textField`.
</li>
<li>
Use the following code for the View Controller in Swift:

```swift
  import UIKit

  class ViewController: UIViewController, KeyboardDelegate {
  
      @IBOutlet weak var textField: UITextField!
  
      override func viewDidLoad() {
          super.viewDidLoad()
      
          // initialize custom keyboard
          let keyboardView = Keyboard(frame: CGRect(x: 0, y: 0, width: 0, height: 300))
          keyboardView.delegate = self // the view controller will be notified by the keyboard whenever a key is tapped
      
          // replace system keyboard with custom keyboard
          textField.inputView = keyboardView
      }
  
      // required method for keyboard delegate protocol
      func keyWasTapped(character: String) {
          textField.insertText(character)
      }
  }

```


</li>
<li>
Use the following code for Objective-C:
**.h File**

```swift
#import <UIKit/UIKit.h>

@interface ViewController : UIViewController

@end

```


**.m File**

```swift
#import "ViewController.h"
#import "Keyboard.h"

@interface ViewController ()<KeyboardDelegate>

@property (nonatomic, weak) IBOutlet UITextField *textField;

@end

@implementation ViewController

- (void)viewDidLoad {
    [super viewDidLoad];
    // Do any additional setup after loading the view, typically from a nib.

    // initialize custom keyboard
    Keyboard *keyboardView = [[Keyboard alloc] initWithFrame:CGRectMake(0, 0, 0, 300)];
    keyboardView.delegate = self; // the view controller will be notified by the keyboard whenever a key is tapped

    // replace system keyboard with custom keyboard
    self.textField.inputView = keyboardView;
}

- (void)keyWasTapped:(NSString *)character {
    [self.textField insertText:character];
}

@end

```


</li>

- Note that the view controller adopts the `KeyboardDelegate` protocol that we defined above.

### Common error

If you are getting an `EXC_BAD_ACCESS` error, it is probably because you set the view's custom class as `Keyboard` rather than do this for the nib File's Owner.

Select `Keyboard.nib` and then choose File's Owner.

[<img src="http://i.stack.imgur.com/Q9O0Y.png" alt="file's owner screenshot" />](http://i.stack.imgur.com/Q9O0Y.png)

Make sure that the custom class for the root view is blank.

[<img src="http://i.stack.imgur.com/WQDv8.png" alt="blank root view screenshot" />](http://i.stack.imgur.com/WQDv8.png)

### Notes

This example comes originally from [this Stack Overflow answer](http://stackoverflow.com/a/33692231/3681880).



## Dismiss a keyboard with tap on view


If you want to hide a keyboard by tap outside of it, it's possible to use this hacky trick (works only with Objective-C):

```swift
- (void)viewDidLoad {
    [super viewDidLoad];

    // dismiss keyboard when tap outside a text field
    UITapGestureRecognizer *tapGestureRecognizer = [[UITapGestureRecognizer alloc] initWithTarget:self.view action:@selector(endEditing:)];
    [tapGestureRecognizer setCancelsTouchesInView:NO];
    [self.view addGestureRecognizer:tapGestureRecognizer];
}

```

for Swift there will be a bit more code:

```swift
override func viewDidLoad() {
    super.viewDidLoad()

    // dismiss keyboard when tap outside a text field 
    let tapGestureRecognizer: UITapGestureRecognizer = UITapGestureRecognizer(target: self, action: #selector(YourVCName.dismissKeyboard))
    view.addGestureRecognizer(tapGestureRecognizer)
}

//Calls this function when the tap is recognized.
func dismissKeyboard() {
    //Causes the view (or one of its embedded text fields) to resign the first responder status.
    view.endEditing(true)
}

```

Another Swift 3/iOS 10 example

```swift
class vc: UIViewController {
    override func viewDidLoad() {
        super.viewDidLoad()
        // Do any additional setup after loading the view, typically from a nib.
        
        txtSomeField.delegate = self
    }
}

extension vc: UITextFieldDelegate {
    //Hide the keyboard for any text field when the UI is touched outside of the keyboard.
    override func touchesBegan(_ touches: Set<UITouch>, with event: UIEvent?)
    {
        self.view.endEditing(true) //Hide the keyboard
    }
}

```



## Managing the Keyboard Using a Singleton + Delegate


When I first started managing the keyboard I would use separate Notifications in each ViewController.

Notification Method (Using NSNotification):

```swift
class ViewController: UIViewController {
    override func viewDidLoad() {
        super.viewDidLoad()
        NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(ViewController.keyboardNotification(_:)), name: UIKeyboardWillChangeFrameNotification, object: nil)
    }

    func keyboardNotification(notification: NSNotification) {
        guard let userInfo = notification.userInfo else { return }
    
        let endFrame = (userInfo[UIKeyboardFrameEndUserInfoKey] as? NSValue)?.CGRectValue()
        let duration: NSTimeInterval = (userInfo[UIKeyboardAnimationDurationUserInfoKey] as? NSNumber)?.doubleValue ?? 0
        let animationCurveRawNSN = userInfo[UIKeyboardAnimationCurveUserInfoKey] as? NSNumber
        let animationCurveRaw = animationCurveRawNSN?.unsignedLongValue ?? UIViewAnimationOptions.CurveEaseOut.rawValue
        let animationCurve: UIViewAnimationOptions = UIViewAnimationOptions(rawValue: animationCurveRaw)
    
        if endFrame?.origin.y >= UIScreen.mainScreen().bounds.size.height {
            lowerViewBottomConstraint.constant = 0
        } else {
            lowerViewBottomConstraint.constant = endFrame?.size.height ?? 0.0
        }
        view.animateConstraintWithDuration(duration, delay: NSTimeInterval(0), options: animationCurve, completion: nil)
    }
}

```

My problem was that I found myself writing this code again and again for every single ViewController. After experimenting a bit I found using a Singleton + Delegate pattern allowed me to reuse a bunch of code and organize all of the Keyboard Management in a single place!

Singleton + Delegate Method:

```swift
protocol KeyboardManagerDelegate: class {
    func keyboardWillChangeFrame(endFrame: CGRect?, duration: NSTimeInterval, animationCurve: UIViewAnimationOptions)
}

class KeyboardManager {

    weak var delegate: KeyboardManagerDelegate?

    class var sharedInstance: KeyboardManager {
        struct Singleton {
            static let instance = KeyboardManager()
        }
        return Singleton.instance
    }

    init() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(KeyboardManager.keyboardWillChangeFrameNotification(_:)), name: UIKeyboardWillChangeFrameNotification, object: nil)
    }

    @objc func keyboardWillChangeFrameNotification(notification: NSNotification) {
        guard let userInfo = notification.userInfo else { return }
    
        let endFrame = (userInfo[UIKeyboardFrameEndUserInfoKey] as? NSValue)?.CGRectValue()
        let duration: NSTimeInterval = (userInfo[UIKeyboardAnimationDurationUserInfoKey] as? NSNumber)?.doubleValue ?? 0
        let animationCurveRawNSN = userInfo[UIKeyboardAnimationCurveUserInfoKey] as? NSNumber
        let animationCurveRaw = animationCurveRawNSN?.unsignedLongValue ?? UIViewAnimationOptions.CurveEaseOut.rawValue
        let animationCurve: UIViewAnimationOptions = UIViewAnimationOptions(rawValue: animationCurveRaw)
    
        delegate?.keyboardWillChangeFrame(endFrame, duration: duration, animationCurve: animationCurve)
    }
}

```

Now when I want to manage the keyboard from a ViewController all I need to do is set the delegate to that ViewController and implement any delegate methods.

```swift
class ViewController: UIViewController {
    override func viewWillAppear(animated: Bool) {
        super.viewWillAppear(animated)
        KeyboardManager.sharedInstance.delegate = self
    }
}

// MARK: - Keyboard Manager

extension ViewController: KeyboardManagerDelegate {
    func keyboardWillChangeFrame(endFrame: CGRect?, duration: NSTimeInterval, animationCurve: UIViewAnimationOptions) {
        if endFrame?.origin.y >= UIScreen.mainScreen().bounds.size.height {
           lowerViewBottomConstraint.constant = 0
        } else {
            lowerViewBottomConstraint.constant = (endFrame?.size.height ?? 0.0)
        }
        view.animateConstraintWithDuration(duration, delay: NSTimeInterval(0), options: animationCurve, completion: nil)
    }
}

```

This method is very customizable too! Say we want to add functionality for `UIKeyboardWillHideNotification`. This is as easy as adding a method to our `KeyboardManagerDelegate`.

`KeyboardManagerDelegate` with `UIKeyboardWillHideNotification`:

```swift
protocol KeyboardManagerDelegate: class {
    func keyboardWillChangeFrame(endFrame: CGRect?, duration: NSTimeInterval, animationCurve: UIViewAnimationOptions)
    func keyboardWillHide(notificationUserInfo: [NSObject: AnyObject])
}

class KeyboardManager {
    init() {
        NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(KeyboardManager.keyboardWillChangeFrameNotification(_:)), name: UIKeyboardWillChangeFrameNotification, object: nil)
        NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(KeyboardManager.keyboardWillHide(_:)), name: UIKeyboardWillHideNotification, object: nil)
    }

    func keyboardWillHide(notification: NSNotification) {
        guard let userInfo = notification.userInfo else { return }
        delegate?.keyboardWillHide(userInfo)
    }
}

```

Say we only want to implement `func keyboardWillHide(notificationUserInfo: [NSObject: AnyObject])` in one ViewController. We can also make this method optional.

```swift
typealias KeyboardManagerDelegate = protocol<KeyboardManagerModel, KeyboardManagerConfigureable>

protocol KeyboardManagerModel: class {
    func keyboardWillChangeFrame(endFrame: CGRect?, duration: NSTimeInterval, animationCurve: UIViewAnimationOptions)
}

@objc protocol KeyboardManagerConfigureable {
    optional func keyboardWillHide(userInfo: [NSObject: AnyObject])
}

```

*Note this pattern helps avoid overuse of `@objc`. See [http://www.jessesquires.com/avoiding-objc-in-swift/](http://www.jessesquires.com/avoiding-objc-in-swift/) for more details!

In summary, I've found using a Singleton + Delegate to manage the keyboard is both more efficient and easier to use than using Notifications



## Scrolling a UIScrollView/UITableView When Displaying the Keyboard


There are few approaches available there:

1. You can subscribe for keyboard appearance events notifications and change offset manually:

```swift
//Swift 2.0+
override func viewDidLoad() {
    super.viewDidLoad()

    NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(YourVCClassName.keyboardWillShow(_:)), name: UIKeyboardWillShowNotification, object: nil)
    NSNotificationCenter.defaultCenter().addObserver(self, selector: #selector(YourVCClassName.keyboardWillHide(_:)), name: UIKeyboardWillHideNotification, object: nil)
}

func keyboardWillShow(notification: NSNotification) {
    if let userInfo = notification.userInfo {
        if let keyboardHeight = userInfo[UIKeyboardFrameEndUserInfoKey]?.CGRectValue.size.height {
            tableView.contentInset = UIEdgeInsetsMake(0, 0, keyboardHeight, 0)
        }
    }
}
    
func keyboardWillHide(notification: NSNotification) {
    tableView.contentInset = UIEdgeInsetsMake(0, 0, 0, 0)
}

//Objective-C
- (void)viewDidLoad {

   [super viewDidLoad];


    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardWillShow:) name:UIKeyboardWillShowNotification object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardWillHide:) name:UIKeyboardWillHideNotification object:nil];

}

- (void)keyboardWillShow:(NSNotification *)notification {

    NSDictionary *userInfo = [notification userInfo];

    if (userInfo) {
    
        CGRect keyboardEndFrame;
        [[userInfo objectForKey:UIKeyboardFrameEndUserInfoKey] getValue:&keyboardEndFrame];
        tableView.contentInset = UIEdgeInsetsMake(0, 0, keyboardEndFrame.size.height, 0);
    
    }

}

- (void)keyboardWillHide:(NSNotification *)notification {

    tableView.contentInset = UIEdgeInsetsMake(0, 0, 0, 0);

}

```


<li>Or use ready-made solutions like TPKeyboardAvoidingTableView or TPKeyboardAvoidingScrollView
[https://github.com/michaeltyson/TPKeyboardAvoiding](https://github.com/michaeltyson/TPKeyboardAvoiding)</li>



## Moving view up or down when keyboard is present


### **Note:** This only works for the built-in keyboard provided by iOS

### **SWIFT:**

In order for the view of a **UIViewController** to increase the origin of the frame when it is presented and decrease it when it is hidden, add the following functions to your class:

```swift
func keyboardWillShow(notification: NSNotification) {

    if let keyboardSize = (notification.userInfo?[UIKeyboardFrameBeginUserInfoKey] as? NSValue)?.cgRectValue {
        if self.view.frame.origin.y == 0{
            self.view.frame.origin.y -= keyboardSize.height
        }
    }

}

func keyboardWillHide(notification: NSNotification) {
    if let keyboardSize = (notification.userInfo?[UIKeyboardFrameBeginUserInfoKey] as? NSValue)?.cgRectValue {
        if self.view.frame.origin.y != 0{
            self.view.frame.origin.y += keyboardSize.height
        }
    }
}

```

And in the `viewDidLoad()` method of your class, add the following observers:

```swift
NotificationCenter.default.addObserver(self, selector: #selector(Login.keyboardWillShow), name: NSNotification.Name.UIKeyboardWillShow, object: nil)
NotificationCenter.default.addObserver(self, selector: #selector(Login.keyboardWillHide), name: NSNotification.Name.UIKeyboardWillHide, object: nil) 

```

And this will work for any screen size, using the height property of the keyboard.

### **OBJECTIVE-C:**

To do the same thing in Objective-C, this code can be used:

```swift
- (void)viewWillAppear:(BOOL)animated {
    [super viewWillAppear:animated];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardWillShow:) name:UIKeyboardWillShowNotification object:nil];
    [[NSNotificationCenter defaultCenter] addObserver:self selector:@selector(keyboardWillHide:) name:UIKeyboardWillHideNotification object:nil];
}

- (void)viewWillDisappear:(BOOL)animated {
    [super viewWillDisappear:animated];
    [[NSNotificationCenter defaultCenter] removeObserver:self name:UIKeyboardWillShowNotification object:nil];
    [[NSNotificationCenter defaultCenter] removeObserver:self name:UIKeyboardWillHideNotification object:nil];
}

- (void)keyboardWillShow:(NSNotification *)notification
{
    CGSize keyboardSize = [[[notification userInfo] objectForKey:UIKeyboardFrameBeginUserInfoKey] CGRectValue].size;

    [UIView animateWithDuration:0.3 animations:^{
        CGRect f = self.view.frame;
        f.origin.y = -keyboardSize.height;
        self.view.frame = f;
    }];
}

-(void)keyboardWillHide:(NSNotification *)notification
{
    [UIView animateWithDuration:0.3 animations:^{
        CGRect f = self.view.frame;
        f.origin.y = 0.0f;
        self.view.frame = f;
    }];
}

```

