---
metaTitle: "iOS - Content Hugging/Content Compression in Autolayout"
description: "Definition: Intrinsic Content Size"
---

# Content Hugging/Content Compression in Autolayout



## Definition: Intrinsic Content Size


Before Auto Layout, you always had to tell buttons and other controls how big they should be, either by setting their frame or bounds properties or by resizing them in Interface Builder. But it turns out that most controls are perfectly capable of determining how much space they need, based on their content.

A **label** knows how wide and tall it is because it knows the length of the text that has been set on it, as well as the font size for that text. Likewise for a **button**, which might combine the text with a background image and some padding.

The same is true for segmented controls, progress bars, and most other controls, although some may only have a predetermined height but an unknown width.

This is known as the intrinsic content size, and it is an important concept in Auto Layout.  Auto Layout asks your controls how big they need to be and lays out the screen based on that information.

Usually you want to use the `intrinsic content size`, but there are some cases where you may not want to do that. You can prevent this by setting an explicit Width or Height constraint on a control.

Imagine what happens when you set an image on a UIImageView if that image is much larger than the screen. You usually want to give image views a fixed width and height and scale the content, unless you want the view to resize to the dimensions of the image.

Reference: [https://www.raywenderlich.com/115444/auto-layout-tutorial-in-ios-9-part-2-constraints](https://www.raywenderlich.com/115444/auto-layout-tutorial-in-ios-9-part-2-constraints)



#### Remarks


**Content Compression Resistance Priority**

> 
<p>This value determines how resistant a view is to being compressed, or
shrunk. A higher value here means the view will be less likely to be
compressed and more likely to stay the same.</p>


**Content Hugging Priority**

> 
<p>This value determines how resistant a view is to being expanded. You
can imagine “hugging” here to mean “size to fit” – the bounds of the
view will “hug” or be close to the intrinsic content size. A higher
value here means the view will be less likely to grow and more likely
to stay the same.</p>


