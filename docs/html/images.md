---
metaTitle: "HTML - Images"
description: "Image cropping using canvas, The Tained canvas, Is context.drawImage not displaying the image on the Canvas?, Scaling image to fit or fill."
---

# Images



## Image cropping using canvas


This example shows a simple image cropping function that takes an image and cropping coordinates and returns the cropped image.

```html
function cropImage(image, croppingCoords) {
    var cc = croppingCoords;
    var workCan = document.createElement("canvas"); // create a canvas
    workCan.width = Math.floor(cc.width);  // set the canvas resolution to the cropped image size
    workCan.height = Math.floor(cc.height);
    var ctx = workCan.getContext("2d");    // get a 2D rendering interface
    ctx.drawImage(image, -Math.floor(cc.x), -Math.floor(cc.y)); // draw the image offset to place it correctly on the cropped region
    image.src = workCan.toDataURL();       // set the image source to the canvas as a data URL
    return image;
}

```

To use

```html
var image = new Image();
image.src = "image URL"; // load the image
image.onload = function () {  // when loaded
    cropImage(
        this, {
        x : this.width / 4,     // crop keeping the center
        y : this.height / 4,
        width : this.width / 2,
        height : this.height / 2,
    });
    document.body.appendChild(this);  // Add the image to the DOM
};

```



## The Tained canvas


When adding content from sources outside your domain, or from the local file system the canvas is marked as tainted. Attempt to access the pixel data, or convert to a dataURL will throw a security error.

```html
vr image = new Image();
image.src = "file://myLocalImage.png";
image.onload = function(){
    ctx.drawImage(this,0,0);
    ctx.getImageData(0,0,canvas.width,canvas.height);  // throws a security error
}

```

This example is just a stub to entice someone with a detailed understanding elaborate.



## Is "context.drawImage" not displaying the image on the Canvas?


Make sure your image object is fully loaded before you try to draw it on the canvas with `context.drawImage`. Otherwise the image will silently fail to display.

In JavaScript, images are not loaded immediately. Instead, images are loaded asynchronously and during the time they take to load JavaScript continues executing any code that follows `image.src`. This means `context.drawImage` may be executed with an empty image and therefore will display nothing.

**Example making sure the image is fully loaded before trying to draw it with .drawImage**

```html
var img=new Image();
img.onload=start;
img.onerror=function(){alert(img.src+' failed');} 
img.src="someImage.png";
function start(){
    // start() is called AFTER the image is fully loaded regardless 
    //     of start's position in the code
}

```

**Example loading multiple images before trying to draw with any of them**

**There are more full-functioned image loaders, but this example illustrates how to do it**

```html
// first image
var img1=new Image();
img1.onload=start;
img1.onerror=function(){alert(img1.src+' failed to load.');};
img1.src="imageOne.png";
// second image
var img2=new Image();
img2.onload=start;
img1.onerror=function(){alert(img2.src+' failed to load.');};
img2.src="imageTwo.png";
//
var imgCount=2;
// start is called every time an image loads
function start(){
    // countdown until all images are loaded
    if(--imgCount>0){return;}
    // All the images are now successfully loaded
    // context.drawImage will successfully draw each one
    context.drawImage(img1,0,0);
    context.drawImage(img2,50,0);
}

```



## Scaling image to fit or fill.


**Scaling to fit**

Means that the whole image will be visible but there may be some empty space on the sides or top and bottom if the image is not the same aspect as the canvas. The example shows the image scaled to fit. The blue on the sides is due to the fact that the image is not the same aspect as the canvas.

[<img src="http://i.stack.imgur.com/gl0rU.png" alt="enter image description here" />](http://i.stack.imgur.com/gl0rU.png)

**Scaling to fill**

Means that the image is scaled so that all the canvas pixels will be covered by the image. If the image aspect is not the same as the canvas then some parts of the image will be clipped. The example shows the image scaled to fill. Note how the top and bottom of the image are no longer visible.

[<img src="http://i.stack.imgur.com/jlVCD.png" alt="enter image description here" />](http://i.stack.imgur.com/jlVCD.png)

### Example Scale to fit

```html
var image = new Image();
image.src = "imgURL";
image.onload = function(){
    scaleToFit(this);
}

function scaleToFit(img){
    // get the scale
    var scale = Math.min(canvas.width / img.width, canvas.height / img.height);
    // get the top left position of the image
    var x = (canvas.width / 2) - (img.width / 2) * scale;
    var y = (canvas.height / 2) - (img.height / 2) * scale;
    ctx.drawImage(img, x, y, img.width * scale, img.height * scale);
}

```

### Example Scale to fill

```html
var image = new Image();
image.src = "imgURL";
image.onload = function(){
    scaleToFill(this);
}

function scaleToFill(img){
    // get the scale
    var scale = Math.max(canvas.width / img.width, canvas.height / img.height);
    // get the top left position of the image
    var x = (canvas.width / 2) - (img.width / 2) * scale;
    var y = (canvas.height / 2) - (img.height / 2) * scale;
    ctx.drawImage(img, x, y, img.width * scale, img.height * scale);
}

```

The only differance between the two functions is getting the scale. The fit uses the min fitting scale will the fill uses the max fitting scale.

