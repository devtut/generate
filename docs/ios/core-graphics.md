---
metaTitle: "iOS - Core Graphics"
description: "Creating a Core Graphics Context, Presenting the Drawn Canvas to User"
---

# Core Graphics



## Creating a Core Graphics Context


> 
<h3>Core Graphics context</h3>
A Core Graphics context is a canvas which we can draw in it and set some properties like the line thickness.


### Making a context

To make a context, we use the `UIGraphicsBeginImageContextWithOptions()` C function. Then, when we are done with drawing, we just call `UIGraphicsEndImageContext()` to end the context:

### Swift

```swift
let size = CGSize(width: 256, height: 256)

UIGraphicsBeginImageContextWithOptions(size, false, 0)

let context = UIGraphicsGetCurrentContext()

// drawing code here

UIGraphicsEndImageContext()

```

### Objective-C

```swift
CGSize size = [CGSize width:256 height:256];

UIGraphicsBeginImageContextWithOptions(size, NO, 0);

CGContext *context = UIGraphicsGetCurrentContext();

// drawing code here

UIGraphicsEndImageContext();

```

In the code above, we passed 3 parameters to the `UIGraphicsBeginImageContextWithOptions()` function:

<li>
A `CGSize` object which stores the whole size of the context (the canvas)
</li>
<li>
A boolean value which if it is true, the context will be opaque
</li>
<li>
An integer value which sets the scale (1 for non-retina, 2 for retina and 3 for retina HD screens). If set to 0, the system automatically handles the scale based on the target device.
</li>



## Presenting the Drawn Canvas to User


### Swift

```swift
let image = UIGraphicsGetImageFromCurrentImageContext()
imageView.image = image //assuming imageView is a valid UIImageView object

```

### Objective-C

```swift
UIImage *image = UIGraphicsGetImageFromCurrentImageContext();
imageView.image = image; //assuming imageView is a valid UIImageView object

```

