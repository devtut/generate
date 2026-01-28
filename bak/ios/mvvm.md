---
metaTitle: "iOS - MVVM"
description: "MVVM Without Reactive Programming"
---

# MVVM




## MVVM Without Reactive Programming


I'll start with a really short explanation what is and why use Model-View-ViewModel (MVVM) design pattern in your iOS apps.
When iOS first appeared, Apple suggested to use MVC (Model-View-Controller) as a design pattern. They showed it in all of their examples and all first developers were happy using it because it nicely separated concerns between business logic and user interface. As applications became larger and more complex a new problem appeared appropriately called Massive View Controllers (MVC). Because all business logic was added in the ViewController, with time they usually became too large and complex.
To avoid MVC issue, a new design pattern was introduced to the world of iOS - Model-View-ViewModel (MVVM) pattern.

[<img src="https://i.stack.imgur.com/jHinj.png" alt="MVVM Diagram" />](https://i.stack.imgur.com/jHinj.png)

The diagram above shows how MVVM looks like. You have a standard ViewController + View (in storyboard, XIB or Code), which acts as MVVM's View (in later text - View will reference MVVM's View). A view has a reference to a ViewModel, where our business logic is. It's important to notice that ViewModel doesn't know anything about the View and never has a reference to the view. ViewModel has a reference to a Model.<br />
This is enough with a theoretical part of the MVVM. More about it can be read [here](https://www.objc.io/issues/13-architecture/mvvm/).

One of the **main issues with MVVM** is how to update View via the ViewModel when ViewModel doesn't have any references and doesn't even know anything about the View.

The main part of this example is to show how to use MVVM (more precisely, how to bind ViewModel and View) without any reactive programming (ReactiveCocoa, ReactiveSwift or RxSwif). Just as a note: if you want to use Reactive programming, even better since MVVM bindings are done really easy using it. But this example is on how to use MVVM without Reactive programming.

Let's create a simple example to demonstrate how to use MVVM.

Our `MVVMExampleViewController` is a simple ViewController with a label and a button. When button is pressed, the label text should be set to 'Hello'. Since deciding what to do on user user interaction is part of the business logic, ViewModel will have to decide what to do when user presses the button. MVVM's View shouldn't do any business logic.

```swift
class MVVMExampleViewController: UIViewController {
    
    @IBOutlet weak var helloLabel: UILabel!
    
    var viewModel: MVVMExampleViewModel?
    
    override func viewDidLoad() {
        super.viewDidLoad()
    }
    
    @IBAction func sayHelloButtonPressed(_ sender: UIButton) {
        viewModel?.userTriggeredSayHelloButton()
    }
}

```

`MVVMExampleViewModel` is a simple ViewModel.

```swift
class MVVMExampleViewModel {
    
    func userTriggeredSayHelloButton() {
        // How to update View's label when there is no reference to the View??
    }
}

```

You might wonder, how to set the ViewModel's reference in the View. I usually do it when ViewController is being initialized or before it will be shown. For this simple example, I would do something like this in the `AppDelegate`:

```swift
func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]?) -> Bool {
        if let rootVC = window?.rootViewController as? MVVMExampleViewController {
            let viewModel = MVVMExampleViewModel()
            rootVC.viewModel = viewModel
        }
        
        return true

```

**The real question now is: how to update View from ViewModel without giving a reference to the View to the ViewModel?** (Remember, we won't use any of the Reactive Programming iOS libraries)

You could think about using KVO, but that would just complicate things too much. Some smart people have thought about the issue and came up with the [Bond library](https://github.com/ReactiveKit/Bond). The library might seem complicated and a little harder to understand at first, so I'll just take one small part of it and make our MVVM fully functional.

Let's introduce the `Dynamic` class which is the core of our simple yet fully functional MVVM pattern.

```swift
class Dynamic<T> {
    typealias Listener = (T) -> Void
    var listener: Listener?
    
    func bind(_ listener: Listener?) {
        self.listener = listener
    }
    
    func bindAndFire(_ listener: Listener?) {
        self.listener = listener
        listener?(value)
    }
    
    var value: T {
        didSet {
            listener?(value)
        }
    }
    
    init(_ v: T) {
        value = v
    }
}

```

`Dynamic` class is using Generics and Closures to bind our ViewModel with our View. I won't go into details about this class, we can do it in the comments (to make this example shorter).
Let's now update our `MVVMExampleViewController` and `MVVMExampleViewModel` to use those classes.

Our updated `MVVMExampleViewController`

```swift
class MVVMExampleViewController: UIViewController {
    
    @IBOutlet weak var helloLabel: UILabel!
    
    var viewModel: MVVMExampleViewModel?
    
    override func viewDidLoad() {
        super.viewDidLoad()
        bindViewModel()
    }
    
    func bindViewModel() {
        if let viewModel = viewModel {
            viewModel.helloText.bind({ (helloText) in
                DispatchQueue.main.async {
                    // When value of the helloText Dynamic variable
                    // is set or changed in the ViewModel, this code will
                    // be executed
                    self.helloLabel.text = helloText
                }
            })
        }
    }
    
    @IBAction func sayHelloButtonPressed(_ sender: UIButton) {
        viewModel?.userTriggeredSayHelloButton()
    }
}

```

Updated `MVVMExampleViewModel`:

```

   class MVVMExampleViewModel {
    
    // we have to initialize the Dynamic var with the
    // data type we want
    var helloText = Dynamic("")
    
    func userTriggeredSayHelloButton() {
        // Setting the value of the Dynamic variable
        // will trigger the closure we defined in the View
        helloText.value = "Hello"
    }
}

```

That is it. Your `ViewModel` is now able to update `View` without it having a reference to the `View`.

This is a really simple example, but I think you have an idea how powerful this can be. I won't go into details about benefits of the MVVM, but once you switch from MVC to MVVM, you won't go back. Just try it and see for yourself.

