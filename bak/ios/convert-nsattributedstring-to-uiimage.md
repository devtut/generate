---
metaTitle: "iOS - Convert NSAttributedString to UIImage"
description: "NSAttributedString to UIImage Conversion"
---

# Convert NSAttributedString to UIImage



## NSAttributedString to UIImage Conversion


**Objective-C**

```

NSMutableAttributedString *str = [[NSMutableAttributedString alloc] initWithString:@"Hello. That is a test attributed string."];
 [str addAttribute:NSBackgroundColorAttributeName value:[UIColor yellowColor] range:NSMakeRange(3,5)];
 [str addAttribute:NSForegroundColorAttributeName value:[UIColor greenColor] range:NSMakeRange(10,7)];
 [str addAttribute:NSFontAttributeName value:[UIFont fontWithName:@"HelveticaNeue-Bold" size:20.0] range:NSMakeRange(20, 10)];
 UIImage *customImage = [self imageFromAttributedString:str];

```

The function `imageFromAttributedString` is as defined below:

```swift
- (UIImage *)imageFromAttributedString:(NSAttributedString *)text
{
    UIGraphicsBeginImageContextWithOptions(text.size, NO, 0.0);
    
    // draw in context
    [text drawAtPoint:CGPointMake(0.0, 0.0)];
    
    // transfer image
    UIImage *image = [UIGraphicsGetImageFromCurrentImageContext() imageWithRenderingMode:UIImageRenderingModeAlwaysOriginal];
    UIGraphicsEndImageContext();
    
    return image;
}

```

