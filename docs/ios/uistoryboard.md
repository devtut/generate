---
metaTitle: "iOS - UIStoryboard"
description: "Getting an instance of UIStoryboard programmatically, Open another storyboard"
---

# UIStoryboard


A UIStoryboard object encapsulates the view controller graph stored in an Interface Builder storyboard resource file. This view controller graph represents the view controllers for all or part of your application’s user interface.



## Getting an instance of UIStoryboard programmatically


### <br>**SWIFT:**

Getting an instance of **UIStoryboard** programmatically can be done as follows:

```

   let storyboard = UIStoryboard(name: "Main", bundle: nil)

```

where:

- **name** => the name of the storyboard without the extension
- **bundle** => the bundle containing the storyboard file and its related resources. If you              specify nil, this method looks in the main bundle of the current application.

For example, you can use the instance created above to access a certain **UIViewController** instantiated within that storyboard:

```

  let viewController = storyboard.instantiateViewController(withIdentifier: "yourIdentifier")

```

### **OBJECTIVE-C:**

Getting an instance of **UIStoryboard** in Objective-C can be done as follows:

```

UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"MainStoryboard" bundle:nil];

```

Example of accessing **UIViewController** instantiated within that storyboard:

```swift
MyViewController *myViewController = [storyboard instantiateViewControllerWithIdentifier:@"MyViewControllerIdentifier"];

```



## Open another storyboard


```swift
let storyboard = UIStoryboard(name: "StoryboardName", bundle: nil)
let vc = storyboard.instantiateViewController(withIdentifier: "ViewControllerID") as YourViewController
self.present(vc, animated: true, completion: nil)

```

