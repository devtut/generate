---
metaTitle: "HTML - Responsive Design"
description: "Creating a responsive full page canvas, Mouse coordinates after resizing (or scrolling), Responsive canvas animations without resize events."
---

# Responsive Design



## Creating a responsive full page canvas


Starter code to create and remove a full page canvas that responds to resize events via javascript.

```js
var canvas;    // Global canvas reference
var ctx;       // Global 2D context reference
// Creates a canvas
function createCanvas () {                
    const canvas = document.createElement("canvas"); 
    canvas.style.position = "absolute"; // Set the style 
    canvas.style.left     = "0px";      // Position in top left
    canvas.style.top      = "0px";
    canvas.style.zIndex   = 1;        
    document.body.appendChild(canvas);  // Add to document
    return canvas;
}
// Resizes canvas. Will create a canvas if it does not exist
function sizeCanvas () {                
    if (canvas === undefined) {         // Check for global canvas reference
        canvas = createCanvas();        // Create a new canvas element
        ctx = canvas.getContext("2d");  // Get the 2D context
    }
    canvas.width  = innerWidth;         // Set the canvas resolution to fill the page
    canvas.height = innerHeight;        
}
// Removes the canvas
function removeCanvas () {
    if (canvas !== undefined) {              // Make sure there is something to remove
        removeEventListener("resize", sizeCanvas); // Remove resize event
        document.body.removeChild(canvas);   // Remove the canvas from the DOM
        ctx = undefined;                     // Dereference the context
        canvas = undefined;                  // Dereference the canvas
     }
}

// Add the resize listener
addEventListener("resize", sizeCanvas); 
// Call sizeCanvas to create and set the canvas resolution
sizeCanvas();
// ctx and canvas are now available for use.

```

If you no longer need the canvas you can remove it by calling `removeCanvas()`

[A demo of this example](https://jsfiddle.net/nzromo1n/) at jsfiddle



## Mouse coordinates after resizing (or scrolling)


Canvas apps often rely heavily on user interaction with the mouse, but when the window is resized, the mouse event coordinates that canvas relies on are likely changed because resizing causes the canvas to be offset in a different position relative to the window. Thus, responsive design requires that the canvas offset position be recalculated when the window is resized -- and also recalculated when the window is scrolled.

This code listens for window resizing events and recalculates the offsets used in mouse event handlers:

```js
// variables holding the current canvas offset position
//    relative to the window
var offsetX,offsetY;

// a function to recalculate the canvas offsets
function reOffset(){
    var BB=canvas.getBoundingClientRect();
    offsetX=BB.left;
    offsetY=BB.top;        
}

// listen for window resizing (and scrolling) events
//     and then recalculate the canvas offsets
window.onscroll=function(e){ reOffset(); }
window.onresize=function(e){ reOffset(); }

// example usage of the offsets in a mouse handler
function handleMouseUp(e){
    // use offsetX & offsetY to get the correct mouse position
    mouseX=parseInt(e.clientX-offsetX);
    mouseY=parseInt(e.clientY-offsetY);
    // ...
}

```



## Responsive canvas animations without resize events.


The window resize events can fire in response to the movement of the user's input device. When you resize a canvas it is automatically cleared and you are forced to re-render the content. For animations you do this every frame via the main loop function called by `requestAnimationFrame` which does its best to keep the rendering in sync with the display hardware.

The problem with the resize event is that when the mouse is used to resize the window the events can be trigger many times quicker than the standard 60fps rate of the browser. When the resize event exits the canvas back buffer is presented to the DOM out of sync with the display device, which can cause shearing and other negative effects. There is also a lot of needless memory allocation and release that can further impact the animation when GC cleans up some time afterwards.

### Debounced resize event

A common way to deal with the high firing rates of the resize event is to debounce the resize event.

```js
// Assume canvas is in scope
 addEventListener.("resize", debouncedResize );

 // debounce timeout handle
 var debounceTimeoutHandle;

 // The debounce time in ms (1/1000th second)
 const DEBOUNCE_TIME = 100; 

 // Resize function 
 function debouncedResize () { 
     clearTimeout(debounceTimeoutHandle);  // Clears any pending debounce events

     // Schedule a canvas resize 
     debounceTimeoutHandle = setTimeout(resizeCanvas, DEBOUNCE_TIME);
 }

 // canvas resize function
 function resizeCanvas () { ... resize and redraw ... }

```

The above example delays the resizing of the canvas until 100ms after the resize event. If in that time further resize events are triggered the existing resize timeout is canceled and a new one scheduled. This effectively consumes most of the resize events.

It still has some problems, the most notable is the delay between resizing and seeing the resized canvas. Reducing the debounce time improves this but the resize is still out of sync with the display device. You also still have the animation main loop rendering to an ill fitting canvas.

More code can reduce the problems! More code also creates its own new problems.

### Simple and the best resize

Having tried many differing ways to smooth out the resizing of the canvas, from the absurdly complex, to just ignoring the problem (who cares anyways?) I fell back to a trusty friend.

**K.I.S.S.** is something most programmers should be aware of ((**K**eep **I**t **S**imple **S**tupid) **The stupid refers to me for not having thought of it years ago.** ) and it turns out the best solution is the simplest of all.

Just resize the canvas from within the main animation loop. It stays in sync with the display device, there is no needless rendering, and the resource management is at the minimum possible while maintaining full frame rate. Nor do you need to add a resize event to the window or any additional resize functions.

You add the resize where you would normally clear the canvas by checking if the canvas size matches the window size. If not resize it.

```js
// Assumes canvas element is in scope as canvas

// Standard main loop function callback from requestAnimationFrame
function mainLoop(time) {

    // Check if the canvas size matches the window size
    if (canvas.width !== innerWidth || canvas.height !== innerHeight) {
        canvas.width = innerWidth;    // resize canvas
        canvas.height = innerHeight;  // also clears the canvas
    } else {
        ctx.clearRect(0, 0, canvas.width, canvas.height); // clear if not resized
    }

    // Animation code as normal.



    requestAnimationFrame(mainLoop);
}

```

