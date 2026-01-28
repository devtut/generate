---
metaTitle: "iOS - Resizing UIImage"
description: "Resize any image by size & quality"
---

# Resizing UIImage



## Resize any image by size & quality


```swift
- (UIImage *)drawImageBySize:(CGSize)size quality:(CGInterpolationQuality)quality
{
    UIGraphicsBeginImageContextWithOptions(size, NO, 0.0);
    CGContextRef context = UIGraphicsGetCurrentContext();
    CGContextSetInterpolationQuality(context, quality);
    [self drawInRect: CGRectMake (0, 0, size.width, size.height)];
    UIImage *resizedImage = UIGraphicsGetImageFromCurrentImageContext();
    UIGraphicsEndImageContext();
    return resizedImage;
}

```



#### Parameters


|CGInterpolationQuality|Levels of interpolation quality for rendering an image.
|---|---|---|---|---|---|---|---|---|---
|Interpolation quality is a graphics state parameter|typedef enum CGInterpolationQuality CGInterpolationQuality;

