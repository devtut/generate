---
metaTitle: "iOS - 3D Touch"
description: "3D Touch with Swift, 3 D Touch Objective-C Example"
---

# 3D Touch




## 3D Touch with Swift


3D touch has been introduced with iPhone 6s Plus. There are two behaviors added with this new interface layer: Peek and Pop.

**Peek and Pop in a nutshell**

Peek - Press hard

Pop - Press really hard

**Checking for 3D support**

You should check if the device has a 3D touch support. You can do this by checking the value of **forceTouchCapability** property of a **UITraitCollection** object. UITraitCollection describes the iOS interface environment for your app.

```swift
if (traitCollection.forceTouchCapability == .Available) {    
    registerForPreviewingWithDelegate(self, sourceView: view)
}

```

**Implementing the delegate**

You need to implement the two methods of **UIViewControllerPreviewingDelegate** in your class. One of the methods is for **peek** and the other one is for **pop** behavior.

The method to be implemented for the **peek** is **previewingContext**.

```swift
func previewingContext(previewingContext: UIViewControllerPreviewing, viewControllerForLocation location: CGPoint) -> UIViewController? {
    
    guard let indexPath = self.tableView.indexPathForRowAtPoint(location), cell = self.tableView.cellForRowAtIndexPath(indexPath) as? <YourTableViewCell> else {
        return nil
    }
    
    guard let datailVC = storyboard?.instantiateViewControllerWithIdentifier("<YourViewControllerIdentifier>") as? <YourViewController> else {
        return nil
    }

    datailVC.peekActive = true
    previewingContext.sourceRect = cell.frame

    // Do the stuff
    
    return datailVC

}

```

The method to be implemented for the **pop** is **previewingContext**. :)

```swift
func previewingContext(previewingContext: UIViewControllerPreviewing, commitViewController viewControllerToCommit: UIViewController) {

    let balanceViewController = viewControllerToCommit as! <YourViewController>

    // Do the stuff

    navigationController?.pushViewController(balanceViewController, animated: true)

}

```

As you can see they are overloaded methods. You can use 3D touch in any way implementing these methods.

**Objective-C**

```swift
//Checking for 3-D Touch availability
if ([self.traitCollection respondsToSelector:@selector(forceTouchCapability)] &&
        (self.traitCollection.forceTouchCapability == UIForceTouchCapabilityAvailable))
    {
        [self registerForPreviewingWithDelegate:self sourceView:self.view];
    }
//Peek
- (UIViewController *)previewingContext:(id<UIViewControllerPreviewing>)previewingContext
              viewControllerForLocation:(CGPoint)location {

    NSIndexPath *indexPath = [self.tableView indexPathForRowAtPoint:location];
    Country *country = [self countryForIndexPath:indexPath];
    if (country) {
        CountryCell *cell = [self.tableView cellForRowAtIndexPath:indexPath];
        if (cell) {
            previewingContext.sourceRect = cell.frame;
            UINavigationController *navController = [self.storyboard instantiateViewControllerWithIdentifier:@"UYLCountryNavController"];
            [self configureNavigationController:navController withCountry:country];
            return navController;
        }
    }
    return nil;
}
//Pop
- (void)previewingContext:(id<UIViewControllerPreviewing>)previewingContext commitViewController:(UIViewController *)viewControllerToCommit {
    
    [self showDetailViewController:viewControllerToCommit sender:self];
}

```



## 3 D Touch Objective-C Example


**Objective-C**

```swift
//Checking for 3-D Touch availability
if ([self.traitCollection respondsToSelector:@selector(forceTouchCapability)] &&
        (self.traitCollection.forceTouchCapability == UIForceTouchCapabilityAvailable))
    {
        [self registerForPreviewingWithDelegate:self sourceView:self.view];
    }
//Peek
- (UIViewController *)previewingContext:(id<UIViewControllerPreviewing>)previewingContext
              viewControllerForLocation:(CGPoint)location {

    NSIndexPath *indexPath = [self.tableView indexPathForRowAtPoint:location];
    Country *country = [self countryForIndexPath:indexPath];
    if (country) {
        CountryCell *cell = [self.tableView cellForRowAtIndexPath:indexPath];
        if (cell) {
            previewingContext.sourceRect = cell.frame;
            UINavigationController *navController = [self.storyboard instantiateViewControllerWithIdentifier:@"UYLCountryNavController"];
            [self configureNavigationController:navController withCountry:country];
            return navController;
        }
    }
    return nil;
}
//Pop
- (void)previewingContext:(id<UIViewControllerPreviewing>)previewingContext commitViewController:(UIViewController *)viewControllerToCommit {
    
    [self showDetailViewController:viewControllerToCommit sender:self];
}

```

