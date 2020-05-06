---
metaTitle: "HTML - Compositing"
description: "Draw behind existing shapes with destination-over, Erase existing shapes with destination-out, Default compositing: New shapes are drawn over Existing shapes, Clip images inside shapes with destination-in, Clip images inside shapes with source-in, Inner shadows with source-atop, Invert or Negate image with difference, Black & White with color, Increase the color contrast with  saturation, Sepia FX with  luminosity, Change opacity with globalAlpha"
---

# Compositing



## Draw behind existing shapes with "destination-over"


```html
context.globalCompositeOperation = "destination-over"

```

"destination-over" compositing places new drawing **under existing drawings**.

```html
context.drawImage(rainy,0,0);
context.globalCompositeOperation='destination-over';  // sunny UNDER rainy
context.drawImage(sunny,0,0);

```

[<img src="http://i.stack.imgur.com/fMvyX.png" alt="enter image description here" />](http://i.stack.imgur.com/fMvyX.png)



## Erase existing shapes with "destination-out"


```html
context.globalCompositeOperation = "destination-out"

```

"destination-out" compositing uses new shapes to erase existing drawings.

The new shape is not actually drawn -- it is just used as a "cookie-cutter" to erase existing pixels.

```html
context.drawImage(apple,0,0);
context.globalCompositeOperation = 'destination-out';   // bitemark erases
context.drawImage(bitemark,100,40);

```

[<img src="http://i.stack.imgur.com/09Qgz.png" alt="enter image description here" />](http://i.stack.imgur.com/09Qgz.png)



## Default compositing: New shapes are drawn over Existing shapes


```html
context.globalCompositeOperation = "source-over"

```

"source-over" compositing **[default]**, places all new drawings over any existing drawings.

```html
context.globalCompositeOperation='source-over';  // the default
context.drawImage(background,0,0);
context.drawImage(parachuter,0,0);

```

[<img src="http://i.stack.imgur.com/C61XF.png" alt="enter image description here" />](http://i.stack.imgur.com/C61XF.png)



## Clip images inside shapes with "destination-in"


```html
context.globalCompositeOperation = "destination-in"

```

"destination-in" compositing clips existing drawings inside a new shape.

**Note: Any part of the existing drawing that falls outside the new drawing is erased.**

```html
context.drawImage(picture,0,0);
context.globalCompositeOperation='destination-in';  // picture clipped inside oval
context.drawImage(oval,0,0);

```

[<img src="http://i.stack.imgur.com/I5DS2.png" alt="enter image description here" />](http://i.stack.imgur.com/I5DS2.png)



## Clip images inside shapes with "source-in"


```html
context.globalCompositeOperation = "source-in";

```

`source-in` compositing clips new drawings inside an existing shape.

**Note: Any part of the new drawing that falls outside the existing drawing is erased.**

```html
context.drawImage(oval,0,0);
context.globalCompositeOperation='source-in';  // picture clipped inside oval
context.drawImage(picture,0,0);

```

[<img src="http://i.stack.imgur.com/KTqsN.png" alt="enter image description here" />](http://i.stack.imgur.com/KTqsN.png)



## Inner shadows with "source-atop"


```html
context.globalCompositeOperation = 'source-atop'

```

`source-atop` compositing clips new image inside an existing shape.

```html
// gold filled rect
ctx.fillStyle='gold';
ctx.fillRect(100,100,100,75);
// shadow
ctx.shadowColor='black';
ctx.shadowBlur=10;
// restrict new draw to cover existing pixels
ctx.globalCompositeOperation='source-atop';
// shadowed stroke
// "source-atop" clips off the undesired outer shadow
ctx.strokeRect(100,100,100,75);
ctx.strokeRect(100,100,100,75);

```

[<img src="http://i.stack.imgur.com/QXUzY.png" alt="enter image description here" />](http://i.stack.imgur.com/QXUzY.png)



## Invert or Negate image with "difference"


Render a white rectangle over an image with the composite operation

```html
ctx.globalCompositeOperation = 'difference';

```

The amount of the effect can be controled with the alpha setting

```html
// Render the image
ctx.globalCompositeOperation='source-atop';
ctx.drawImage(image, 0, 0);

// set the composite operation
ctx.globalCompositeOperation='difference';
ctx.fillStyle = "white";
ctx.globalAlpha = alpha;  // alpha 0 = no effect 1 = full effect
ctx.fillRect(0, 0, image.width, image.height);

```

[<img src="http://i.stack.imgur.com/euaCF.png" alt="enter image description here" />](http://i.stack.imgur.com/euaCF.png)



## Black & White with "color"


Remove color from an image via

```html
ctx.globalCompositeOperation = 'color';

```

The amount of the effect can be controled with the alpha setting

```html
// Render the image
ctx.globalCompositeOperation='source-atop';
ctx.drawImage(image, 0, 0);

// set the composite operation
ctx.globalCompositeOperation='color';
ctx.fillStyle = "white";
ctx.globalAlpha = alpha;  // alpha 0 = no effect 1 = full effect
ctx.fillRect(0, 0, image.width, image.height);

```

[<img src="http://i.stack.imgur.com/9B5Y1.png" alt="enter image description here" />](http://i.stack.imgur.com/9B5Y1.png)



## Increase the color contrast with  "saturation"


Increase the saturation level of an image with

```html
ctx.globalCompositeOperation = 'saturation';

```

The amount of the effect can be controled with the alpha setting or the amount of saturation in the fill overlay

```html
// Render the image
ctx.globalCompositeOperation='source-atop';
ctx.drawImage(image, 0, 0);

// set the composite operation
ctx.globalCompositeOperation ='saturation';
ctx.fillStyle = "red";
ctx.globalAlpha = alpha;  // alpha 0 = no effect 1 = full effect
ctx.fillRect(0, 0, image.width, image.height);

```

[<img src="http://i.stack.imgur.com/9Ldnl.png" alt="enter image description here" />](http://i.stack.imgur.com/9Ldnl.png)



## Sepia FX with  "luminosity"


Create a colored sepia FX with

```html
ctx.globalCompositeOperation = 'luminosity';

```

In this case the sepia colour is rendered first the the image.

The amount of the effect can be controled with the alpha setting or the amount of saturation in the fill overlay

```html
// Render the image
ctx.globalCompositeOperation='source-atop';
ctx.fillStyle = "#F80";  // the color of the sepia FX
ctx.fillRect(0, 0, image.width, image.height);

// set the composite operation
ctx.globalCompositeOperation ='luminosity';

ctx.globalAlpha = alpha;  // alpha 0 = no effect 1 = full effect
ctx.drawImage(image, 0, 0);

```

[<img src="http://i.stack.imgur.com/hDrhq.png" alt="enter image description here" />](http://i.stack.imgur.com/hDrhq.png)



## Change opacity with "globalAlpha"


```html
context.globalAlpha=0.50

```

You can change the opacity of new drawings by setting the `globalAlpha` to a value between 0.00 (fully transparent) and 1.00 (fully opaque).

The default `globalAlpha` is 1.00 (fully opaque).

Existing drawings are not affected by `globalAlpha`.

```html
// draw an opaque rectangle
context.fillRect(10,10,50,50);

// change alpha to 50% -- all new drawings will have 50% opacity
context.globalAlpha=0.50;

// draw a semi-transparent rectangle
context.fillRect(100,10,50,50);

```

