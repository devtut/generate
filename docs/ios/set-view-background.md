---
metaTitle: "iOS - Set View Background"
description: "Set View background, Fill background Image of a UIView, Set View backround with image, Creating a gradient background view"
---

# Set View Background



## Set View background


Objective C:

```swift
view.backgroundColor = [UIColor redColor];

```

Swift:

```swift
view.backgroundColor! = UIColor.redColor()

```

Swift 3

```swift
view.backgroundColor = UIColor.redColor

```



## Fill background Image of a UIView


**Objective-C**

```

UIGraphicsBeginImageContext(self.view.frame.size);
 [[UIImage imageNamed:@"image.png"] drawInRect:self.view.bounds];
 UIImage *image = UIGraphicsGetImageFromCurrentImageContext();
 UIGraphicsEndImageContext();     
 self.view.backgroundColor = [UIColor colorWithPatternImage:image];

```



## Set View backround with image


```swift
self.view.backgroundColor = [UIColor colorWithPatternImage:[UIImage imageNamed:@"Background.png"]];

```



## Creating a gradient background view


To create a background with a gradient you can use the [CAGradientLayer](https://developer.apple.com/reference/quartzcore/cagradientlayer) class:

Swift 3.1:

```swift
func createGradient() { 
    let caLayer = CAGradientLayer()
    caLayer.colors = [UIColor.white, UIColor.green, UIColor.blue]
    caLayer.locations = [0, 0.5, 1]
    caLayer.bounds = self.bounds
    self.layer.addSublayer(caLayer) 
}

```

This can be called on viewDidLoad() like so:

```swift
override func viewDidLoad() {
    super.viewDidLoad()
    createGradient()
}

```

The CAGradientLayer locations and bounds variables can take multiple values to create a gradient layer with how ever many colors you desire. From the documentation:

> 
By default, the colors are spread uniformly across the layer, but you can optionally specify locations for control over the color positions through the gradient.


