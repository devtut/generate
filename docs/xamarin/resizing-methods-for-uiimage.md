---
metaTitle: "Xamarin - Resizing Methods for UIImage"
description: "Resize Image - with Aspect Ratio, Resize Image - without Aspect Ratio, Crop Image without Resize"
---

# Resizing Methods for UIImage



## Resize Image - with Aspect Ratio


```cs
// resize the image to be contained within a maximum width and height, keeping aspect ratio
public static UIImage MaxResizeImage(this UIImage sourceImage, float maxWidth, float maxHeight)
{
    var sourceSize = sourceImage.Size;
    var maxResizeFactor = Math.Min(maxWidth / sourceSize.Width, maxHeight / sourceSize.Height);
    if (maxResizeFactor > 1) return sourceImage;
    var width = maxResizeFactor * sourceSize.Width;
    var height = maxResizeFactor * sourceSize.Height;
    UIGraphics.BeginImageContext(new CGSize(width, height));
    sourceImage.Draw(new CGRect(0, 0, width, height));
    var resultImage = UIGraphics.GetImageFromCurrentImageContext();
    UIGraphics.EndImageContext();
    return resultImage;
}

```



## Resize Image - without Aspect Ratio


```cs
// resize the image (without trying to maintain aspect ratio)
public static UIImage ResizeImage(this UIImage sourceImage, float width, float height)
{
    UIGraphics.BeginImageContext(new SizeF(width, height));
    sourceImage.Draw(new RectangleF(0, 0, width, height));
    var resultImage = UIGraphics.GetImageFromCurrentImageContext();
    UIGraphics.EndImageContext();
    return resultImage;
}

```



## Crop Image without Resize


```cs
// crop the image, without resizing
public static UIImage CropImage(this UIImage sourceImage, int crop_x, int crop_y, int width, int height)
{
    var imgSize = sourceImage.Size;
    UIGraphics.BeginImageContext(new SizeF(width, height));
    var context = UIGraphics.GetCurrentContext();
    var clippedRect = new RectangleF(0, 0, width, height);
    context.ClipToRect(clippedRect);
    var drawRect = new CGRect(-crop_x, -crop_y, imgSize.Width, imgSize.Height);
    sourceImage.Draw(drawRect);
    var modifiedImage = UIGraphics.GetImageFromCurrentImageContext();
    UIGraphics.EndImageContext();
    return modifiedImage;
}

```

