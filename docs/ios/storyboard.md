---
metaTitle: "iOS - Storyboard"
description: "Initialize, Fetch Initial ViewController , Fetch ViewController"
---

# Storyboard




## Initialize


```swift
//Swift    
let storyboard = UIStoryboard(name: "Main", bundle: NSBundle.mainBundle()) 

//Objective-c
UIStoryboard *storyboard = [UIStoryboard storyboardWithName:@"Main" bundle:[NSBundle mainBundle]];

```



## Fetch Initial ViewController 


```swift
//Swift
let initialScreen = storyboard.instantiateInitialViewController()

//Objective-c
UIViewController *initailScreen = [storyboard instantiateInitialViewController];

```



## Fetch ViewController


```swift
//Swift    
let viewController = storyboard.instantiateViewControllerWithIdentifier("identifier")

//Objective-c
UIViewController *viewController = [storyboard instantiateViewControllerWithIdentifier:@"identifier"];

```

