---
metaTitle: "Android - 9-Patch Images"
description: "Basic rounded corners, Basic spinner, Optional padding lines"
---

# 9-Patch Images



## Basic rounded corners


The key to correctly stretching is in the top and left border.

The top border controls horizontal stretching and the left border controls vertical stretching.

This example creates rounded corners suitable for a Toast.

[<img src="http://i.stack.imgur.com/YlwtQ.png" alt="9-Patch rounded corners example" />](http://i.stack.imgur.com/YlwtQ.png)

The parts of the image that are below the **top border** and to the right of the **left border** will expand to fill all unused space.

This example will stretch to all combinations of sizes, as shown below:

[<img src="http://i.stack.imgur.com/5F8jt.png" alt="enter image description here" />](http://i.stack.imgur.com/5F8jt.png)



## Basic spinner


The `Spinner` can be reskinned according to your own style requirements using a Nine Patch.

As an example, see this Nine Patch:

[<img src="http://i.stack.imgur.com/kuFzo.png" alt="Spinner Nine Patch Example" />](http://i.stack.imgur.com/kuFzo.png)

As you can see, it has 3 extremely small areas of stretching marked.

The top border has only left of the icon marked. That indicates that I want the left side (complete transparency) of the drawable to fill the `Spinner` view until the icon is reached.

The left border has marked transparent segments at the top and bottom of the icon marked. That indicates that both the top and the bottom will expand to the size of the `Spinner` view. This will leave the icon itself centered vertically.

Using the image without Nine Patch metadata:

[<img src="http://i.stack.imgur.com/lcGMv.png" alt="Spinner Image without Nine Patch" />](http://i.stack.imgur.com/lcGMv.png)

Using the image with Nine Patch metadata:

[<img src="http://i.stack.imgur.com/DlNCX.png" alt="Spinner Image with Nine Patch" />](http://i.stack.imgur.com/DlNCX.png)



## Optional padding lines


Nine-patch images allow optional definition of the padding lines in the image. The padding lines are the lines on the right and at the bottom.

If a View sets the 9-patch image as its background, the padding lines are used to define the space for the View's content (e.g. the text input in an `EditText`). If the padding lines are not defined, the left and top lines are used instead.

[<img src="http://i.stack.imgur.com/VG4Bd.png" alt="enter image description here" />](http://i.stack.imgur.com/VG4Bd.png)

The content area of the stretched image then looks like this:

[<img src="http://i.stack.imgur.com/bFVfx.png" alt="enter image description here" />](http://i.stack.imgur.com/bFVfx.png) [<img src="http://i.stack.imgur.com/wJv9l.png" alt="enter image description here" />](http://i.stack.imgur.com/wJv9l.png) [<img src="http://i.stack.imgur.com/Vc6z3.png" alt="enter image description here" />](http://i.stack.imgur.com/Vc6z3.png)



#### Remarks


A **9-patch** image file is a specially formatted file so that Android knows which areas/portions of the image can or cannot be scaled. It breaks your image into a 3x3 grid. The corners remain unscaled, the sides are scaled in one direction and the center is scaled in both dimensions.

[<img src="https://i.stack.imgur.com/Rm3CR.png" alt="how a 9-patch image scales" />](https://i.stack.imgur.com/Rm3CR.png)

A Nine Patch (9-Patch) image is a bitmap that has a single pixel wide border around the entire image. Ignoring the 4 pixels in the corners of the image. This border provides metadata for the bitmap itself. Bounds are marked by solid black line(s).

A Nine Patch image is stored with the extension `.9.png`.

The top border indicates areas that stretch horizontally. The left border indicates areas that stretch vertically.

The bottom border indicates padding horizontally. The right border indicates padding vertically.

The padding borders are usually used to determine where text is to be drawn.

There is an excellent tool provided by Google that **greatly** simplifies the creation of these files.

Located in the Android SDK: `android-sdk\tools\lib\draw9patch.jar`

