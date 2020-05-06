---
metaTitle: "HTML - Canvas"
description: "Basic Example, Drawing two rectangles on a <canvas>"
---

# Canvas




## Basic Example


The `canvas` element was introduced in HTML5 for drawing graphics.

```html
<canvas id="myCanvas">
   Cannot display graphic. Canvas is not supported by your browser (IE<9)
</canvas>

```

The above will create a transparent HTML`<canvas>` element of 300×150 px in size.

You can use the **canvas** element to draw amazing stuff like shapes, graphs, manipulate images, create engaging games etc. with **JavaScript**.<br />
The `canvas`'s 2D **drawable layer** surface Object is referred to as `CanvasRenderingContext2D`; or from a `HTMLCanvasElement` using the `.getContext("2d")` method:

```html
var ctx = document.getElementById("myCanvas").getContext("2d");
// now we can refer to the canvas's 2D layer context using `ctx`

ctx.fillStyle = "#f00";
ctx.fillRect(0, 0, ctx.canvas.width, ctx.canvas.height); // x, y, width, height

ctx.fillStyle = "#000";
ctx.fillText("My red canvas with some black text", 24, 32); // text, x, y

```

[jsFiddle example](https://jsfiddle.net/omvrvhyu/)



## Drawing two rectangles on a <canvas>


```html
<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="utf-8" />
    <title>Draw two rectangles on the canvas</title>
    <style>
      canvas{
          border:1px solid gray;
      }
    </style>
    <script async>
      window.onload = init; // call init() once the window is completely loaded
      function init(){
        // #1 - get reference to <canvas> element
        var canvas = document.querySelector('canvas');
        
        // #2 - get reference to the drawing context and drawing API
        var ctx = canvas.getContext('2d');

        // #3 - all fill operations are now in red
        ctx.fillStyle = 'red'; 

        // #4 - fill a 100x100 rectangle at x=0,y=0
        ctx.fillRect(0,0,100,100);
        
        // #5 - all fill operations are now in green
        ctx.fillStyle = 'green'; 
        
        // #6 - fill a 50x50 rectangle at x=25,y=25
        ctx.fillRect(25,25,50,50);
        
      }
      </script>
</head>
<body>
  <canvas width=300 height=200>Your browser does not support canvas.</canvas>
</body>
</html>

```

This example looks like this:

[<img src="http://i.stack.imgur.com/7QfjB.png" alt="enter image description here" />](http://i.stack.imgur.com/7QfjB.png)



#### Parameters


|Attribute|Description
|---|---|---|---|---|---|---|---|---|---
|height|Specifies the canvas height
|width|Specifies the canvas width



#### Remarks


- This tag is not compatible with versions of Internet Explorer less than 9. Check [caniuse.com](http://caniuse.com/#feat=canvas) for browser compatibility.
- `canvas` is only a container for graphics, and the actual drawing of graphics is done by JavaScript.

