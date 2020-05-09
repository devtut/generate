---
metaTitle: "iOS - UIControl - Event Handling with Blocks"
description: "Introduction"
---

# UIControl - Event Handling with Blocks



## Introduction


Typically, when using `UIControl` or `UIButton`, we add a `selector` as a callback action for when an event occurs on a button or control, such as the user pressing the button or touching the control.

For example, we would do the following:

```swift
import UIKit

class ViewController: UIViewController {
    @IBOutlet weak var button: UIButton!

    override func viewDidLoad() {
        super.viewDidLoad()
        
        let button = UIButton(frame: CGRect(x: 0, y: 0, width: 100, height: 44))
        button.addTarget(self, action: #selector(self.onButtonPress(_:)), for: .touchUpInside)
        self.view.addSubview(button)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
    
    func onButtonPress(_ button: UIButton!) {
        print("PRESSED")
    }
}

```

When it comes to `selector`, the compiler only needs to know that it exists.. This can be done through a `protocol` and not be implemented.

For example, the following would crash your application:

```swift
import UIKit

@objc
protocol ButtonEvent {
    @objc optional func onButtonPress(_ button: UIButton)
}

class ViewController: UIViewController, ButtonEvent {
    @IBOutlet weak var button: UIButton!

    override func viewDidLoad() {
        super.viewDidLoad()
        
        let button = UIButton(frame: CGRect(x: 0, y: 0, width: 100, height: 44))
        button.addTarget(self, action: #selector(ButtonEvent.onButtonPress(_:)), for: .touchUpInside)
        self.view.addSubview(button)
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
}

```

This is because your application does NOT implement the `onButtonPress` function.

Now what if you could do all of this alongside the initialization of the button? What if you didn't have to specify callbacks and could instead specify blocks that can be added and removed at any time? Why worry about implementing selectors?

**Solution**

```swift
import Foundation
import UIKit

protocol RemovableTarget {
    func enable();
    func disable();
}

extension UIControl {
    func addEventHandler(event: UIControlEvents, runnable: (control: UIControl) -> Void) -> RemovableTarget {
        
        class Target : RemovableTarget {
            private var event: UIControlEvents
            private weak var control: UIControl?
            private var runnable: (control: UIControl) -> Void
            
            private init(event: UIControlEvents, control: UIControl, runnable: (control: UIControl) -> Void) {
                self.event = event
                self.control = control
                self.runnable = runnable
            }
            
            @objc
            private func run(_ control: UIControl) {
                runnable(control: control)
            }
            
            private func enable() {
                control?.addTarget(self, action: #selector(Target.run(_:)), for: event)
                objc_setAssociatedObject(self, unsafeAddress(of: self), self, .OBJC_ASSOCIATION_RETAIN)
            }
            
            private func disable() {
                control?.removeTarget(self, action: #selector(Target.run(_:)), for: self.event)
                objc_setAssociatedObject(self, unsafeAddress(of: self), nil, .OBJC_ASSOCIATION_ASSIGN)
            }
        }
        
        let target = Target(event: event, control: self, runnable: runnable)
        target.enable()
        return target
    }
}

```

The above is a simple extension on `UIControl`. It adds an inner private class that has a callback `func run(_ control: UIControl)` that is used as the events' action.

Next we use `object association` to add and remove the target because it will not be retained by the `UIControl`.

The event handler function returns a `Protocol` in order to hide the inner workings of the `Target` class but also to allow you to `enable` and `disable` the target at any given time.

**Usage Example:**

```swift
import Foundation
import UIKit

class ViewController: UIViewController {

    override func viewDidLoad() {
        super.viewDidLoad()
        

        //Create a button.
        let button = UIButton(frame: CGRect(x: 0, y: 0, width: 100, height: 44))
        
        //Add an event action block/listener -- Handles Button Press.
        let target = button.addEventHandler(event: .touchUpInside) { (control) in
            print("Pressed")
        }
        

        self.view.addSubview(button)
        
        //Example of enabling/disabling the listener/event-action-block.
        DispatchQueue.main.after(when: DispatchTime.now() + 5) {
            target.disable() //Disable the listener.
            
            DispatchQueue.main.after(when: DispatchTime.now() + 5) {
                target.enable() //Enable the listener.
            }
        }
    }

    override func didReceiveMemoryWarning() {
        super.didReceiveMemoryWarning()
    }
}

```

