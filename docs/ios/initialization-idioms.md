---
metaTitle: "iOS - Initialization idioms"
description: "Set to tuples to avoid code repetition, Initialize with positional constants, Initialize attributes in didSet, Group outlets in a custom NSObject, Initialize with then, Factory method with block"
---

# Initialization idioms



## Set to tuples to avoid code repetition


Avoid code repetition in constructors by setting a tuple of variables with a one liner:

```swift
class Contact: UIView
{
    private var message: UILabel
    private var phone: UITextView
    
    required init?(coder aDecoder: NSCoder) {
        (message, phone) = self.dynamicType.setUp()
        super.init(coder: aDecoder)
    }
    
    override func awakeFromNib() {
        (message, phone) = self.dynamicType.setUp()
        super.awakeFromNib()
    }
    
    override init(frame: CGRect) {
        (message, phone) = self.dynamicType.setUp()
        super.init(frame: frame)
    }
    
    private static func setUp(){
        let message = UILabel()  // ...
        let phone = UITextView() // ...
        return (message, phone)
    }
}

```



## Initialize with positional constants


```swift
let mySwitch: UISwitch = {
    view.addSubview($0)
    $0.addTarget(self, action: "action", forControlEvents: .TouchUpInside)
    return $0
}(UISwitch())

```



## Initialize attributes in didSet


```swift
@IBOutlet weak var title: UILabel! {
  didSet {
    label.textColor = UIColor.redColor()
    label.font = UIFont.systemFontOfSize(20)
    label.backgroundColor = UIColor.blueColor()
  }
}

```

It's also possible to both set a value and initialize it:

```swift
private var loginButton = UIButton() {
    didSet(oldValue) {
        loginButton.addTarget(self, action: #selector(LoginController.didClickLogin), forControlEvents: .TouchUpInside)
    }
}

```



## Group outlets in a custom NSObject


Move every outlet to an NSObject. Then drag an Object from the library to the controller scene of the storyboard and hook the elements there.

```swift
class ContactFormStyle: NSObject 
{
    @IBOutlet private weak var message: UILabel! {
      didSet {
        message.font = UIFont.systemFontOfSize(12)
        message.textColor = UIColor.blackColor()
      }
    }
}

class ContactFormVC: UIViewController 
{
    @IBOutlet private var style: ContactFormStyle!
}

```



## Initialize with then


This is similar in syntax to the example that initializes using positional constants, but requires the `Then` extension from [https://github.com/devxoul/Then](https://github.com/devxoul/Then) (attached below).

```swift
let label = UILabel().then {
    $0.textAlignment = .Center
    $0.textColor = UIColor.blackColor(
    $0.text = "Hello, World!"
}

```

The `Then` extension:

```swift
import Foundation

public protocol Then {}

extension Then 
{
    public func then(@noescape block: inout Self -> Void) -> Self {
        var copy = self
        block(&copy)
        return copy
    }
}

extension NSObject: Then {}

```



## Factory method with block


```swift
internal func Init<Type>(value : Type, block: @noescape (object: Type) -> Void) -> Type
{
    block(object: value)
    return value
}

```

Usage:

```swift
Init(UILabel(frame: CGRect.zero)) {
    $0.backgroundColor = UIColor.blackColor()
}

```

