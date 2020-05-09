---
metaTitle: "iOS - UIAppearance"
description: "Set appearance of all instances of the class, Appearance for class when contained in container class"
---

# UIAppearance




## Set appearance of all instances of the class


To customize appearance of all instances of a class, access appearance proxy of the desired class. For example:

**Set UIButton tint color**

**Swift:**

```swift
UIButton.appearance().tintColor = UIColor.greenColor()

```

**Objective-C:**

```swift
[UIButton appearance].tintColor = [UIColor greenColor];

```

**Set UIButton background color**

**Swift:**

```swift
UIButton.appearance().backgroundColor = UIColor.blueColor()

```

**Objective-C:**

```swift
[UIButton appearance].backgroundColor = [UIColor blueColor];

```

**Set UILabel text color**

**Swift:**

```swift
UILabel.appearance().textColor = UIColor.redColor()

```

**Objective-C:**

```swift
[UILabel appearance].textColor = [UIColor redColor];

```

**Set UILabel background color**

**Swift:**

```swift
UILabel.appearance().backgroundColor = UIColor.greenColor()

```

**Objective-C:**

```swift
[UILabel appearance].backgroundColor = [UIColor greenColor];

```

**Set UINavigationBar tint color**

**Swift:**

```swift
UINavigationBar.appearance().tintColor = UIColor.cyanColor()

```

**Objective-C:**

```swift
[UINavigationBar appearance].tintColor = [UIColor cyanColor];

```

**Set UINavigationBar background color**

**Swift:**

```swift
UINavigationBar.appearance().backgroundColor = UIColor.redColor()

```

**Objective-C:**

```swift
[UINavigationBar appearance].backgroundColor = [UIColor redColor];

```



## Appearance for class when contained in container class


Use `appearanceWhenContainedInInstancesOfClasses:` to customize the appearance for instance of a class when contained within an instance of container class. For example customization of `UILabel`'s `textColor` and `backgroundColor` within `ViewController` class will look like this:

**Set UILabel text color**

**Swift:**

```swift
UILabel.appearanceWhenContainedInInstancesOfClasses([ViewController.self]).textColor = UIColor.whiteColor()

```

**Objective-C:**

```swift
[UILabel appearanceWhenContainedInInstancesOfClasses:@[[ViewController class]]].textColor = [UIColor whiteColor];

```

**Set UILabel background color**

**Swift:**

```swift
UILabel.appearanceWhenContainedInInstancesOfClasses([ViewController.self]).backgroundColor = UIColor.blueColor()

```

**Objective-C:**

```swift
[UILabel appearanceWhenContainedInInstancesOfClasses:@[[ViewController class]]].backgroundColor = [UIColor blueColor];

```

