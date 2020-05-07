---
metaTitle: "HTML - Getting started with html5-canvas"
description: "Detecting mouse position on the canvas, Canvas size and resolution, How to add the Html5 Canvas Element to a webpage, Off screen canvas, Hello World, An index to Html5 Canvas Capabilities & Uses, Rotate, Save canvas to image file"
---

# Getting started with html5-canvas



## Detecting mouse position on the canvas


This example will show how to get the mouse position relative to the canvas, such that `(0,0)` will be the top-left hand corner of the HTML5 Canvas. The `e.clientX` and `e.clientY` will get the mouse positions relative to the top of the document, to change this to be based on the top of the canvas we subtract the `left` and `right` positions of the canvas from the client X and Y.

```js
var canvas = document.getElementById("myCanvas");
var ctx = canvas.getContext("2d");
ctx.font = "16px Arial";

canvas.addEventListener("mousemove", function(e) { 
    var cRect = canvas.getBoundingClientRect();        // Gets CSS pos, and width/height
    var canvasX = Math.round(e.clientX - cRect.left);  // Subtract the 'left' of the canvas 
    var canvasY = Math.round(e.clientY - cRect.top);   // from the X/Y positions to make  
    ctx.clearRect(0, 0, canvas.width, canvas.height);  // (0,0) the top left of the canvas
    ctx.fillText("X: "+canvasX+", Y: "+canvasY, 10, 20);
});

```

[**Runnable Example**](https://jsfiddle.net/ab1exorg/)

The use of `Math.round` is due to ensure the `x,y` positions are integers, as the bounding rectangle of the canvas may not have integer positions.



## Canvas size and resolution


The size of a canvas is the area it occupies on the page and is defined by the CSS width and height properties.

```css
canvas {
   width : 1000px;
   height : 1000px;
}

```

The canvas resolution defines the number of pixels it contains. The resolution is set by setting the canvas element width and height properties. If not specified the canvas defaults to 300 by 150 pixels.

The following canvas will use the above CSS size but as the `width` and `height` is not specified the resolution will be 300 by 150.

```html
<canvas id="my-canvas"></canvas>

```

This will result in each pixel being stretched unevenly. The pixel aspect is 1:2. When the canvas is stretched the browser will use bilinear filtering. This has an effect of blurring out pixels that are stretched.

For the best results when using the canvas ensure that the canvas resolution matches the display size.

Following on from the CSS style above to match the display size add the canvas with the `width` and `height` set to the same pixel count as the style defines.

```html
<canvas id = "my-canvas" width = "1000" height = "1000"></canvas>

```



## How to add the Html5 Canvas Element to a webpage


**Html5-Canvas ...**

- Is an Html5 element.
- Is supported in most modern browsers (Internet Explorer 9+).
- Is a visible element that is transparent by default
- Has a default width of 300px and a default height of 150px.
- Requires JavaScript because all content must be programmatically added to the Canvas.

Example: Create an Html5-Canvas element using both Html5 markup and JavaScript:

```html
<!doctype html>
<html>
<head>
<style>
    body{ background-color:white; }
    #canvasHtml5{border:1px solid red; }
    #canvasJavascript{border:1px solid blue; }
</style>
<script>
window.onload=(function(){

    // add a canvas element using javascript
    var canvas=document.createElement('canvas');
    canvas.id='canvasJavascript'
    document.body.appendChild(canvas);

}); // end $(function(){});
</script>
</head>
<body>

    <!-- add a canvas element using html -->
    <canvas id='canvasHtml5'></canvas>

</body>
</html>

```



## Off screen canvas


Many times when working with the canvas you will need to have a canvas to hold some intrum pixel data. It is easy to create an offscreen canvas, obtain a 2D context. An offscreen canvas will also use the available graphics hardware to render.

The following code simply creates a canvas and fills it with blue pixels.

```js
function createCanvas(width, height){
    var canvas = document.createElement("canvas"); // create a canvas element
    canvas.width = width;
    canvas.height = height;
    return canvas;
}

var myCanvas = createCanvas(256,256); // create a small canvas 256 by 256 pixels
var ctx = myCanvas.getContext("2d");
ctx.fillStyle = "blue";
ctx.fillRect(0,0,256,256);

```

Many times the offscreen canvas will be used for many tasks, and you may have many canvases. To simplify the use of the canvas you can attach the canvas context to the canvas.

```js
function createCanvasCTX(width, height){
    var canvas = document.createElement("canvas"); // create a canvas element
    canvas.width = width;
    canvas.height = height;
    canvas.ctx = canvas.getContext("2d");
    return canvas;
}
var myCanvas = createCanvasCTX(256,256); // create a small canvas 256 by 256 pixels
myCanvas.ctx.fillStyle = "blue";
myCanvas.ctx.fillRect(0,0,256,256);    

```



## Hello World


**HTML**

```html
<canvas id="canvas" width=300 height=100 style="background-color:#808080;">
</canvas>

```

**Javascript**

```js
var canvas = document.getElementById("canvas");
var ctx = canvas.getContext("2d");
ctx.font = "34px serif";
ctx.textAlign = "center";
ctx.textBaseline="middle"; 
ctx.fillStyle = "#FFF";
ctx.fillText("Hello World",150,50);

```

**Result**

[<img src="http://i.stack.imgur.com/IyQEd.png" alt="Hello World with HTML5 canvas" />](http://i.stack.imgur.com/IyQEd.png)



## An index to Html5 Canvas Capabilities & Uses


### Capabilities of the Canvas

Canvas lets you programmatically draw onto your webpage:

- [Images](http://stackoverflow.com/documentation/html5-canvas/3210/images),
- [Texts](http://stackoverflow.com/documentation/html5-canvas/5235/text),
- [Lines and Curves](http://stackoverflow.com/documentation/html5-canvas/3241/path-syntax-only).

Canvas drawings can be extensively styled:

- [stroke width](http://stackoverflow.com/documentation/html5-canvas/3241/path-syntax-only/13473/linewidth-a-path-styling-attribute),
- [stroke color](http://stackoverflow.com/documentation/html5-canvas/3241/path-syntax-only/13471/strokestyle-a-path-styling-attribute),
- [shape fill color](http://stackoverflow.com/documentation/html5-canvas/3241/path-syntax-only/13472/fillstyle-a-path-styling-attribute),
- [opacity](http://stackoverflow.com/documentation/html5-canvas/5547/compositing/20032/change-opacity-with-globalalpha),
- [shadowing](http://stackoverflow.com/documentation/html5-canvas/5322/shadows),
- [linear gradients](http://stackoverflow.com/documentation/html5-canvas/3241/path-syntax-only/13475/createlineargradient-creates-a-path-styling-object) and [radial gradients](http://stackoverflow.com/documentation/html5-canvas/3241/path-syntax-only/13476/createradialgradient-creates-a-path-styling-object),
- [font face](http://stackoverflow.com/documentation/html5-canvas/5235/text/18589/formatting-text),
- [font size](http://stackoverflow.com/documentation/html5-canvas/5235/text/18589/formatting-text),
- [text alignment](http://stackoverflow.com/documentation/html5-canvas/5235/text/18589/formatting-text),
- [text may be stroked, filled or both stroked & filled](http://stackoverflow.com/documentation/html5-canvas/5235/text/18588/drawing-text),
- [image resizing](http://stackoverflow.com/documentation/html5-canvas/3210/images/19169/scaling-image-to-fit-or-fill),
- [image cropping](http://stackoverflow.com/documentation/html5-canvas/3210/images/9061/image-cropping-using-canvas),
- [compositing](http://stackoverflow.com/documentation/html5-canvas/5547/compositing)

### Uses of the Canvas

Drawings can be combined and positioned anywhere on the canvas so it can be used to create:

- Paint / Sketch applications,
- Fast paced interactive games,
- Data analyses like charts, graphs,
- Photoshop-like imaging,
- Flash-like advertising and Flashy web content.

Canvas allows you to manipulate the Red, Green, Blue & Alpha component colors of images. This allows canvas to manipulate images with results similar to Photoshop.

- Recolor any part of an image at the pixel level (if you use HSL you can even recolor an image while retaining the important Lighting & Saturation so the result doesn't look like someone slapped paint on the image),
- "Knockout" the background around a person/item in an image,
- Detect and Floodfill part of an image (eg, change the color of a user-clicked flower petal from green to yellow -- just that clicked petal!),
- Do Perspective warping (e.g. wrap an image around the curve of a cup),
- Examine an image for content (eg. facial recognition),
- Answer questions about an image: Is there a car parked in this image of my parking spot?,
- Apply standard image filters (grayscale, sepia, etc)
- Apply any exotic image filter you can dream up (Sobel Edge Detection),
- Combine images. If dear Grandma Sue couldn't make it to the family reunion, just "photoshop" her into the reunion image. Don't like Cousin Phil -- just "photoshop him out,
- Play a video / Grab a frame from a video,
- Export the canvas content as a .jpg | .png image (you can even optionally crop or annotate the image and export the result as a new image),

About moving and editing canvas drawings (for example to create an action game):

- After something has been drawn on the canvas, that existing drawing cannot be moved or edited. This common misconception that canvas drawings are movable is worth clarifying: **Existing canvas drawings cannot be edited or moved!**
- Canvas draws very, very quickly. Canvas can draw hundreds of images, texts, lines & curves in a fraction of a second. It uses the GPU when available to speed up drawing.
- Canvas creates the illusion of motion by quickly and repeatedly drawing something and then redrawing it in a new position. Like television, this constant redrawing gives the eye the illusion of motion.



## Rotate


The `rotate(r)` method of the 2D context rotates the canvas by the specified amount `r` of **radians** around the origin.

**HTML**

```html
<canvas id="canvas" width=240 height=240 style="background-color:#808080;">
</canvas>

<button type="button" onclick="rotate_ctx();">Rotate context</button>

```

**Javascript**

```js
var canvas = document.getElementById("canvas");
var ctx = canvas.getContext("2d");
var ox = canvas.width / 2;
var oy = canvas.height / 2;
ctx.font = "42px serif";
ctx.textAlign = "center";
ctx.textBaseline = "middle";
ctx.fillStyle = "#FFF";
ctx.fillText("Hello World", ox, oy);

rotate_ctx = function() {
  // translate so that the origin is now (ox, oy) the center of the canvas
  ctx.translate(ox, oy);
  // convert degrees to radians with radians = (Math.PI/180)*degrees. 
  ctx.rotate((Math.PI / 180) * 15);
  ctx.fillText("Hello World", 0, 0);
  // translate back
  ctx.translate(-ox, -oy);
};

```

[Live demo on JSfiddle](https://jsfiddle.net/user2314737/c6on9hat/)



## Save canvas to image file


You can save a canvas to an image file by using the method `canvas.toDataURL()`, that returns the **data URI** for the canvas' image data.

The method can take two optional parameters `canvas.toDataURL(type, encoderOptions)`: `type` is the image format (if omitted the default is `image/png`); `encoderOptions` is a number between 0 and 1 indicating image quality (default is  0.92).

Here we draw a canvas and attach the canvas' data URI to the "Download to myImage.jpg" link.

**HTML**

```html
<canvas id="canvas" width=240 height=240 style="background-color:#808080;">
</canvas>
<p></p>
<a id="download" download="myImage.jpg" href="" onclick="download_img(this);">Download to myImage.jpg</a>

```

**Javascript**

```js
var canvas = document.getElementById("canvas");
var ctx = canvas.getContext("2d");
var ox = canvas.width / 2;
var oy = canvas.height / 2;
ctx.font = "42px serif";
ctx.textAlign = "center";
ctx.textBaseline = "middle";
ctx.fillStyle = "#800";
ctx.fillRect(ox / 2, oy / 2, ox, oy);

download_img = function(el) {
  // get image URI from canvas object
  var imageURI = canvas.toDataURL("image/jpg");
  el.href = imageURI;
};

```

[Live demo](https://jsfiddle.net/user2314737/28wqq1gu/) on JSfiddle.

