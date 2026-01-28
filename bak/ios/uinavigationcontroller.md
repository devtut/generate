---
metaTitle: "iOS - UINavigationController"
description: "Popping in a Navigation Controller, Embed a view controller in a navigation controller programmatically, Purpose, Creating a NavigationController, Pushing a view controller onto the navigation stack"
---

# UINavigationController



## Popping in a Navigation Controller


### To previous view controller

To pop back to the previous page you can do this:

Swift

```swift
navigationController?.popViewControllerAnimated(true)

```

Objective-C

```swift
[self.navigationController popViewControllerAnimated:YES];

```

### To root view controller

To pop to the root of the navigation stack, you can do this:

Swift

```swift
navigationController?.popToRootViewControllerAnimated(true)

```

Objective C

```swift
[self.navigationController popToRootViewControllerAnimated:YES];

```



## Embed a view controller in a navigation controller programmatically


Swift

```swift
//Swift
let viewController = UIViewController()
let navigationController = UINavigationController(rootViewController: viewController)

//Objective-C
UIViewController *viewController = [[UIViewController alloc] init];
UINavigationController *navigationController = [[UINavigationController alloc] initWithRootViewController:viewController];

```



## Purpose


`UINavigationController` is used to form a tree-like hierarchy of view controllers, which is known as a `navigation stack`.

**From developers perspective:**

You can connect independently made controller and get all the benefits of a free hierarchy manager and common UI presenter gratis. `UINavigationController` animates the transition to new controllers and provides the back functionality for you automatically. `UINavigationController`also gives access to all the other controllers in the `navigation stack` which can help access to some functionality or data.

**From user's perspective:**

`UINavigationController` helps to remember where user is at the moment (navigation bar title) and how he can go back (embedded back button) to one of the previous screens.



## Creating a NavigationController


In your storyboard select the ViewController that you want to embed into a Navigation Controller.

Then navigate to Editor > Embed In > Navigation Controller

[<img src="http://i.stack.imgur.com/JKS8j.png" alt="enter image description here" />](http://i.stack.imgur.com/JKS8j.png)

And that will create your navigation controller

[<img src="http://i.stack.imgur.com/0zk9n.png" alt="enter image description here" />](http://i.stack.imgur.com/0zk9n.png)



## Pushing a view controller onto the navigation stack


```swift
//Swift
let fooViewController = UIViewController()
navigationController?.pushViewController(fooViewController, animated: true)

//Objective-C
UIViewController *fooViewController = [[UIViewController alloc] init];
[navigationController pushViewController:fooViewController animated:YES];

```



#### Remarks


From the [documentation](https://developer.apple.com/library/ios/documentation/UIKit/Reference/UINavigationController_Class/):

> 
The UINavigationController class implements a specialized view controller that manages the navigation of hierarchical content. This navigation interface makes it possible to present your data efficiently and makes it easier for the user to navigate that content. You generally use this class as-is but you may also subclass to customize the class behavior.


