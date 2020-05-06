---
metaTitle: "HTML - Clearing the screen"
description: "Rectangles, Raw image data, Complex shapes, Clear canvas with gradient., Clear canvas using composite operation"
---

# Clearing the screen



## Rectangles


You can use the `clearRect` method to clear any rectangular section of the canvas.

```html
// Clear the entire canvas
ctx.clearRect(0, 0, canvas.width, canvas.height);

```

> 
**Note:** `clearRect` is dependent on the transformation matrix.


To deal with this, it's possible to reset the transformation matrix before you clear the canvas.

```html
ctx.save();                                       // Save the current context state
ctx.setTransform(1, 0, 0, 1, 0, 0);               // Reset the transformation matrix
ctx.clearRect(0, 0, canvas.width, canvas.height); // Clear the canvas
ctx.restore();                                    // Revert context state including 
                                                  // transformation matrix

```

> 
**Note:** `ctx.save` and `ctx.restore` are only requiered if you wish to keep the canvas 2D context state. In some situations save and restore can be be slow and generally should be avoided if not required.




## Raw image data


It's possible to write directly to the rendered image data using `putImageData`. By creating new image data then assigning it to the canvas, you will clear the entire screen.

```html
var imageData = ctx.createImageData(canvas.width, canvas.height);
ctx.putImageData(imageData, 0, 0);

```

**Note: `putImageData` is not affected by any transformations applied to the context. It will write data directly to the rendered pixel region.**



## Complex shapes


It's possible to clear complex shaped regions by changing the `globalCompositeOperation` property.

```html
// All pixels being drawn will be transparent
ctx.globalCompositeOperation = 'destination-out';

// Clear a triangular section
ctx.globalAlpha = 1;    // ensure alpha is 1
ctx.fillStyle = '#000'; // ensure the current fillStyle does not have any transparency
ctx.beginPath();
ctx.moveTo(10, 0);
ctx.lineTo(0, 10);
ctx.lineTo(20, 10);
ctx.fill();

// Begin drawing normally again
ctx.globalCompositeOperation = 'source-over';

```



## Clear canvas with gradient.


Rather than use `clearRect` which makes all pixels transparent you may want a background.

To clear with a gradient

```html
// create the background gradient once
var bgGrad = ctx.createLinearGradient(0,0,0,canvas.height);
bgGrad.addColorStop(0,"#0FF");
bgGrad.addColorStop(1,"#08F");

// Every time you need to clear the canvas
ctx.fillStyle = bgGrad;
ctx.fillRect(0,0,canvas.width,canvas.height);

```

This is about half as quick `0.008ms` as clearRect `0.004ms` but the 4millions of a second should not negatively impact any realtime animation. (Times will vary considerably depending on device, resolution, browser, and browser configuration. Times are for comparison only)



## Clear canvas using composite operation


Clear the canvas using compositing operation. This will clear the canvas independent of transforms but is not as fast as `clearRect()`.

```html
ctx.globalCompositeOperation = 'copy';

```

anything drawn next will clear previous content.



#### Syntax


- void clearRect(x, y, width, height)
- ImageData createImageData(width, height)



#### Remarks


None of these methods will produce transparent pixels if the context was created with the `alpha: false` parameter.

