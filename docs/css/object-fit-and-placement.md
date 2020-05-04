---
metaTitle: "Object Fit and Placement"
description: "object-fit"
---

# Object Fit and Placement



## object-fit


The **object-fit** property will defines how an element will fit into a box with an established height and width. Usually applied to an image or video, Object-fit accepts the following five values:

**FILL**

```css
object-fit:fill;

```

[<img src="https://i.stack.imgur.com/xIdvn.png" alt="object-fit:fill;" />](https://i.stack.imgur.com/xIdvn.png)

Fill stretches the image to fit the content box without regard to the image's original aspect ratio.

**CONTAIN**

```css
object-fit:contain;

```

[<img src="https://i.stack.imgur.com/qpiUd.png" alt="object-fit:contain;" />](https://i.stack.imgur.com/qpiUd.png)

Contain fits the image in the box's height or width while maintaining the image's aspect ratio.

**COVER**

```css
object-fit:cover;

```

[<img src="https://i.stack.imgur.com/zxl94.png" alt="object-fit:cover;" />](https://i.stack.imgur.com/zxl94.png)

Cover fills the entire box with the image. The image aspect ratio is preserved, but the image is cropped to the dimensions of the box.

**NONE**

```css
object-fit:none;

```

[<img src="https://i.stack.imgur.com/YdXVL.png" alt="object-fit:none;" />](https://i.stack.imgur.com/YdXVL.png)

None ignores the size of the box and is not resized.

**SCALE-DOWN**

```css
object-fit:scale-down;

```

Scale-down either sizes the object as `none` or as `contain`. It displays whichever option results in a smaller image size.
[<img src="https://i.stack.imgur.com/bnDKA.png" alt="object-fit:scale-down;" />](https://i.stack.imgur.com/bnDKA.png)



#### Remarks


The properties `object-fit` and `object-position` are not supported by Internet Explorer.

