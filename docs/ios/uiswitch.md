---
metaTitle: "iOS - UISwitch"
description: "Set On / Off, Set Image for On/Off state, Set Background Color, Set Tint Color"
---

# UISwitch




## Set On / Off


**Objective-C**

```swift
[mySwitch setOn:YES];
//or
[mySwitch setOn:YES animated:YES];

```

**Swift**

```swift
mySwitch.setOn(false)
//or
mySwitch.setOn(false, animated: false)

```



## Set Image for On/Off state


**Objective-C**

```swift
//set off-image
mySwitch.offImage = [UIImage imageNamed:@"off_image"];
[mySwitch setOffImage:[UIImage imageNamed:@"off_image"]];

//set on-image
mySwitch.onImage = [UIImage imageNamed:@"on_image"];
[mySwitch setOnImage:[UIImage imageNamed:@"on_image"]];

```

**Swift**

```swift
//set off-image
mySwitch.offImage = UIImage(named: "off_image")

//set on-image
mySwitch.onImage = UIImage(named: "on_image")

```



## Set Background Color


**Objective-C**

```swift
mySwitch.backgroundColor = [UIColor yellowColor];
[mySwitch setBackgroundColor: [UIColor yellowColor]];
mySwitch.backgroundColor =[UIColor colorWithRed:255/255.0 green:0/255.0 blue:0/255.0 alpha:1.0];
mySwitch.backgroundColor= [UIColor colorWithWhite: 0.5 alpha: 1.0];
mySwitch.backgroundColor=[UIColor colorWithHue: 0.4 saturation: 0.3 brightness:0.7 alpha: 1.0];

```

**Swift**

```swift
mySwitch.backgroundColor = UIColor.yellow
mySwitch.backgroundColor = UIColor(red: 255.0/255, green: 0.0/255, blue: 0.0/255, alpha: 1.0)
mySwitch.backgroundColor = UIColor(white: 0.5, alpha: 1.0)
mySwitch.backgroundColor = UIColor(hue: 0.4,saturation: 0.3,brightness: 0.7,alpha: 1.0)

```



## Set Tint Color


**Objective-C**

```swift
//for off-state
mySwitch.tintColor = [UIColor blueColor];
[mySwitch setTintColor: [UIColor blueColor]];

//for on-state
mySwitch.onTintColor = [UIColor cyanColor];
[mySwitch setOnTintColor: [UIColor cyanColor]];

```

**Swift**

```swift
//for off-state
mySwitch.tintColor = UIColor.blueColor()

//for on-state
mySwitch.onTintColor = UIColor.cyanColor()

```



#### Syntax


- (instancetype)initWithFrame:(CGRect)frame;
- (void)setOn:(BOOL)on animated:(BOOL)animated;
- (nullable instancetype)initWithCoder:(NSCoder *)aDecoder;



#### Remarks


<h3> 1. UISwitch Reference : [Apple Documentation](https://developer.apple.com/library/ios/documentation/UIKit/Reference/UISwitch_Class/index.html#//apple_ref/occ/cl/UISwitch) </h3>

[<img src="https://i.stack.imgur.com/ZqTs0.png" alt="enter image description here" />](https://i.stack.imgur.com/ZqTs0.png)

<h3> 2. Another Reference given by : [Enoch Huang](http://studyswift.blogspot.in/2016/05/create-uiswitch-programmatically.html) </h3>

[<img src="https://i.stack.imgur.com/zYobJ.gif" alt="enter image description here" />](https://i.stack.imgur.com/zYobJ.gif)

