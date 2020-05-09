---
metaTitle: "iOS - Swift: Changing the rootViewController in AppDelegate to present main or login/onboarding flow"
description: "Option 1: Swap the Root View Controller (Good), Option 2: Present Alternative Flow Modally (Better)"
---

# Swift: Changing the rootViewController in AppDelegate to present main or login/onboarding flow


It is often useful to present a first-run experience to new users of your App. This could be for any number of reasons, such as prompting them to sign in (if required for your situation), explaining how to use the App, or simply informing them of new features in an update (as Notes, Photos and Music do in iOS11).



## Option 1: Swap the Root View Controller (Good)


There are benefits to switching the root view controller, although the transition options are limited to those supported by `UIViewAnimationOptions`, so depending on how you wish to transition between flows might mean you have to implement a custom transition - which can be cumbersome.

You can show the Onboarding flow by simply setting the `UIApplication.shared.keyWindow.rootViewController`

Dismissal is handled by utilizing `UIView.transition(with:)` and passing the transition style as a `UIViewAnimationOptions`, in this case Cross Dissolve. (Flips and Curls are also supported).

You also have to set the frame of the Main view before you transition back to it, as you're instantiating it for the first time.

```swift
// MARK: - Onboarding

extension AppDelegate {

    func showOnboarding() {
        if let window = UIApplication.shared.keyWindow, let onboardingViewController = UIStoryboard(name: "Onboarding", bundle: nil).instantiateInitialViewController() as? OnboardingViewController {
            onboardingViewController.delegate = self
            window.rootViewController = onboardingViewController
        }
    }

    func hideOnboarding() {
        if let window = UIApplication.shared.keyWindow, let mainViewController = UIStoryboard(name: "Main", bundle: nil).instantiateInitialViewController() {
            mainViewController.view.frame = window.bounds
            UIView.transition(with: window, duration: 0.5, options: .transitionCrossDissolve, animations: {
                window.rootViewController = mainViewController
            }, completion: nil)
        }
    }
}

```



## Option 2: Present Alternative Flow Modally (Better)


In the most straightforward implementation, the Onboarding flow can simply be presented in a modal context, since semantically the User is on a single journey.

> 
[Apple Human Interface Guidelines – Modality][1]:
Consider creating a modal context only when it’s critical to get someone’s attention, when a task must be completed or abandoned to continue using the app, or to save important data.


Presenting modally allows the simple option of dismissal at the end of the journey, with little of the cruft of swapping controllers.

Custom transitions are also supported in the standard way, since this uses the `ViewController.present()` API:

```swift
// MARK: - Onboarding

extension AppDelegate {

    func showOnboarding() {
        if let window = window, let onboardingViewController = UIStoryboard(name: "Onboarding", bundle: nil).instantiateInitialViewController() as? OnboardingViewController {
            onboardingViewController.delegate = self
            window.makeKeyAndVisible()
            window.rootViewController?.present(onboardingViewController, animated: false, completion: nil)
        }
    }

    func hideOnboarding() {
        if let window = UIApplication.shared.keyWindow {
            window.rootViewController?.dismiss(animated: true, completion: nil)
        }
    }
}

```



#### Remarks


Firstly, as you are dealing with multiple flows, this is where Storyboards can be used effectively. By default your Application uses `Main.storyboard` for your primary flow. Your onboarding/alternative flow can be contained in a secondary storyboard, eg. `Onboarding.storyboard`

This has a number of advantages:

- in a team of developers, the work on each user flow can be separated
- clearer source control (git)
- separation of concerns

When your App launches, you can determine which flow should be presented. The logic for this can be contained in your AppDelegate:

```swift
func application(_ application: UIApplication, didFinishLaunchingWithOptions launchOptions: [UIApplicationLaunchOptionsKey: Any]?) -> Bool {
    let isFirstRun = true // logic to determine goes here
    if isFirstRun {
        showOnboarding()
    }
    return true
}

```

In order to show the Onboarding flow, it's worth considering how you'd like to handle the experience of dismissing it once the person using it has completed the journey, and which is semantically correct for what you are trying to create.

### Approaches:

The two main approaches are:

1. Swap the root view controller of the App's main window
1. Present the Onboarding flow as a modal journey, overlapping the Main flow.

The implementation of this should be contained in an extension to AppDelegate.

