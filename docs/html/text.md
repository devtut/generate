---
metaTitle: "HTML - Text"
description: "Drawing Text, Formatting Text, Wrapping text into paragraphs, Draw text paragraphs into irregular shapes, Fill text with an image, Rendering text along an arc., Text on curve, cubic and quadratic beziers, Justified text, Justified paragraphs."
---

# Text



## Drawing Text


Drawing to canvas isn't just limited to shapes and images. You can also draw text to the canvas.

To draw text on the canvas, get a reference to the canvas and then call the `fillText` method on the context.

```js
var canvas = document.getElementById('canvas');
var ctx = canvas.getContext('2d');
ctx.fillText("My text", 0, 0);

```

The three **required** arguments that are passed into `fillText` are:

1. The text that you would like to display
1. The horizontal (x-axis) position
1. The vertical (y-axis) position

Additionally, there is a fourth **optional** argument, which you can use to specify the maximum width of your text in pixels. In the example below the value of `200` restricts the maximum width of the text to 200px:

```js
ctx.fillText("My text", 0, 0, 200);

```

Result:

[<img src="http://i.stack.imgur.com/ugzVs.png" alt="Example result of using the fillText method on canvas" />](http://i.stack.imgur.com/ugzVs.png)

You can also draw text without a fill, and just an outline instead, using the `strokeText` method:

```js
ctx.strokeText("My text", 0, 0);

```

Result:

[<img src="http://i.stack.imgur.com/8Exp1.png" alt="Example result of using the strokeText method on canvas" />](http://i.stack.imgur.com/8Exp1.png)

Without any font formatting properties applied, the canvas renders text at 10px in sans-serif by default, making it hard to see the difference between the result of the `fillText` and `strokeText` methods. See the [Formatting Text example](http://stackoverflow.com/documentation/html5-canvas/5235/text/18589/formatting-text#t=201608091853255094075) for details on how to increase text size and apply other aesthetic changes to text.



## Formatting Text


The default font formatting provided by the `fillText` and `strokeText` methods isn't very aesthetically appealing. Fortunately the canvas API provides properties for formatting text.

Using the `font` property you can specify:

- font-style
- font-variant
- font-weight
- font-size / line-height
- font-family

For example:

```js
ctx.font = "italic small-caps bold 40px Helvetica, Arial, sans-serif";
ctx.fillText("My text", 20, 50);

```

Result:

[<img src="http://i.stack.imgur.com/PtCie.png" alt="Example result of specifying font properties" />](http://i.stack.imgur.com/PtCie.png)

Using the `textAlign` property you can also change text alignment to either:

- left
- center
- right
- end (same as right)
- start (same as left)

For example:

```js
ctx.textAlign = "center";

```



## Wrapping text into paragraphs


Native Canvas API does not have a method to wrap text onto the next line when a desired maximum width is reached. This example wraps text into paragraphs.

```js
function wrapText(text, x, y, maxWidth, fontSize, fontFace){
  var firstY=y;
  var words = text.split(' ');
  var line = '';
  var lineHeight=fontSize*1.286; // a good approx for 10-18px sizes

  ctx.font=fontSize+" "+fontFace;
  ctx.textBaseline='top';

  for(var n = 0; n < words.length; n++) {
    var testLine = line + words[n] + ' ';
    var metrics = ctx.measureText(testLine);
    var testWidth = metrics.width;
    if(testWidth > maxWidth) {
      ctx.fillText(line, x, y);
      if(n<words.length-1){
          line = words[n] + ' ';
          y += lineHeight;
      }
    }
    else {
      line = testLine;
    }
  }
  ctx.fillText(line, x, y);
}

```



## Draw text paragraphs into irregular shapes


This example draws text paragraphs into any portions of the canvas that have opaque pixels.

It works by finding the next block of opaque pixels that is large enough to contain the next specified word and filling that block with the specified word.

The opaque pixels can come from any source: Path drawing commands and /or  images.

[<img src="http://i.stack.imgur.com/IfJpv.png" alt="enter image description here" />](http://i.stack.imgur.com/IfJpv.png)

```html
<!doctype html>
<html>
<head>
<style>
    body{ background-color:white; padding:10px; }
    #canvas{border:1px solid red;}
</style>
<script>
window.onload=(function(){

    var canvas=document.getElementById("canvas");
    var ctx=canvas.getContext("2d");
    var cw=canvas.width;
    var ch=canvas.height;

    var fontsize=12;
    var fontface='verdana';
    var lineHeight=parseInt(fontsize*1.286);

    var text='It was the best of times, it was the worst of times, it was the age of wisdom, it was the age of foolishness, it was the epoch of belief, it was the epoch of incredulity, it was the season of Light, it was the season of Darkness, it was the spring of hope, it was the winter of despair, we had everything before us, we had nothing before us, we were all going direct to Heaven, we were all going direct the other way; in short, the period was so far like the present period, that some of its noisiest authorities insisted on its being received, for good or for evil, in the superlative degree of comparison only.';
    var words=text.split(' ');
    var wordWidths=[];
    ctx.font=fontsize+'px '+fontface;
    for(var i=0;i<words.length;i++){ wordWidths.push(ctx.measureText(words[i]).width); }
    var spaceWidth=ctx.measureText(' ').width;
    var wordIndex=0
    var data=[];

    // Demo: draw Heart 
    // Note: the shape can be ANY opaque drawing -- even an image
    ctx.scale(3,3);
    ctx.beginPath();
    ctx.moveTo(75,40);
    ctx.bezierCurveTo(75,37,70,25,50,25);
    ctx.bezierCurveTo(20,25,20,62.5,20,62.5);
    ctx.bezierCurveTo(20,80,40,102,75,120);
    ctx.bezierCurveTo(110,102,130,80,130,62.5);
    ctx.bezierCurveTo(130,62.5,130,25,100,25);
    ctx.bezierCurveTo(85,25,75,37,75,40);
    ctx.fillStyle='red';
    ctx.fill();
    ctx.setTransform(1,0,0,1,0,0);

    // fill heart with text
    ctx.fillStyle='white';
    var imgDataData=ctx.getImageData(0,0,cw,ch).data;
    for(var i=0;i<imgDataData.length;i+=4){
        data.push(imgDataData[i+3]);
    }
    placeWords();   

    // draw words sequentially into next available block of 
    //    available opaque pixels 
    function placeWords(){
        var sx=0;
        var sy=0;
        var y=0;
        var wordIndex=0;
        ctx.textBaseline='top';
        while(y<ch && wordIndex<words.length){
            sx=0;
            sy=y;
            var startingIndex=wordIndex;
            while(sx<cw && wordIndex<words.length){
                var x=getRect(sx,sy,lineHeight);
                var available=x-sx;
                var spacer=spaceWidth;  // spacer=0 to have no left margin
                var w=spacer+wordWidths[wordIndex];
                while(available>=w){
                    ctx.fillText(words[wordIndex],spacer+sx,sy);
                    sx+=w;
                    available-=w;
                    spacer=spaceWidth;
                    wordIndex++;
                    w=spacer+wordWidths[wordIndex];
                }
                sx=x+1;
            }
            y=(wordIndex>startingIndex)?y+lineHeight:y+1;
        }
    }

    // find a rectangular block of opaque pixels
    function getRect(sx,sy,height){
        var x=sx;
        var y=sy;
        var ok=true;
        while(ok){
            if(data[y*cw+x]<250){ok=false;}
            y++;
            if(y>=sy+height){
                y=sy;
                x++;
                if(x>=cw){ok=false;}
            }
        }
        return(x);
    }

}); // end $(function(){});
</script>
</head>
<body>
    <h4>Note: the shape must be closed and alpha>=250 inside</h4>
    <canvas id="canvas" width=400 height=400></canvas>
</body>
</html>

```



## Fill text with an image


This example fills text with a specified image.

Important! The specified image must be fully loaded before calling this function or the drawing will fail. Use `image.onload` to be sure the image is fully loaded.

[<img src="http://i.stack.imgur.com/58KTX.png" alt="enter image description here" />](http://i.stack.imgur.com/58KTX.png)

```js
function drawImageInsideText(canvas,x,y,img,text,font){
    var c=canvas.cloneNode();
    var ctx=c.getContext('2d');
    ctx.font=font;
    ctx.fillText(text,x,y);
    ctx.globalCompositeOperation='source-atop';
    ctx.drawImage(img,0,0);
    canvas.getContext('2d').drawImage(c,0,0);
}

```



## Rendering text along an arc.


This example shows how to render text along an arc. It includes how you can add functionality to the `CanvasRenderingContext2D` by extending its prototype.

This examples is derived from the stackoverflow answer [Circular Text](http://stackoverflow.com/a/34170195/3877726).

### **Example rendering**

[<img src="http://i.stack.imgur.com/KYlZg.png" alt="Example of circle text" />](http://i.stack.imgur.com/KYlZg.png)

### Example code

The example adds 3 new text rendering functions to the 2D context prototype.

- **ctx.fillCircleText(text, x, y, radius, start, end, forward);**
- **ctx.strokeCircleText(text, x, y, radius, start, end, forward);**
- **ctx.measureCircleText(text, radius);**

```js
(function(){
    const FILL = 0;        // const to indicate filltext render
    const STROKE = 1;
    var renderType = FILL; // used internal to set fill or stroke text
    const multiplyCurrentTransform = true; // if true Use current transform when rendering
                                           // if false use absolute coordinates which is a little quicker
                                           // after render the currentTransform is restored to default transform
                                           
      

    // measure circle text
    // ctx: canvas context
    // text: string of text to measure
    // r: radius in pixels
    //
    // returns the size metrics of the text
    //
    // width: Pixel width of text
    // angularWidth : angular width of text in radians
    // pixelAngularSize : angular width of a pixel in radians
    var measure = function(ctx, text, radius){        
        var textWidth = ctx.measureText(text).width; // get the width of all the text
        return {
            width               : textWidth,
            angularWidth        : (1 / radius) * textWidth,
            pixelAngularSize    : 1 / radius
        };
    }

    // displays text along a circle
    // ctx: canvas context
    // text: string of text to measure
    // x,y: position of circle center
    // r: radius of circle in pixels
    // start: angle in radians to start. 
    // [end]: optional. If included text align is ignored and the text is 
    //        scaled to fit between start and end;
    // [forward]: optional default true. if true text direction is forwards, if false  direction is backward
    var circleText = function (ctx, text, x, y, radius, start, end, forward) {
        var i, textWidth, pA, pAS, a, aw, wScale, aligned, dir, fontSize;
        if(text.trim() === "" || ctx.globalAlpha === 0){ // dont render empty string or transparent
            return;
        }
        if(isNaN(x) || isNaN(y) || isNaN(radius) || isNaN(start) || (end !== undefined && end !== null && isNaN(end))){ // 
            throw TypeError("circle text arguments requires a number for x,y, radius, start, and end.")
        }
        aligned = ctx.textAlign;        // save the current textAlign so that it can be restored at end
        dir = forward ? 1 : forward === false ? -1 : 1;  // set dir if not true or false set forward as true  
        pAS = 1 / radius;               // get the angular size of a pixel in radians
        textWidth = ctx.measureText(text).width; // get the width of all the text
        if (end !== undefined && end !== null) { // if end is supplied then fit text between start and end
            pA = ((end - start) / textWidth) * dir;
            wScale = (pA / pAS) * dir;
        } else {                 // if no end is supplied correct start and end for alignment
            // if forward is not given then swap top of circle text to read the correct direction
            if(forward === null || forward === undefined){
                if(((start % (Math.PI * 2)) + Math.PI * 2) % (Math.PI * 2) > Math.PI){
                    dir = -1;
                }
            }
            pA = -pAS * dir ;
            wScale = -1 * dir;
            switch (aligned) {
            case "center":       // if centered move around half width
                start -= (pA * textWidth )/2;
                end = start + pA * textWidth;
                break;
            case "right":// intentionally falls through to case "end"
            case "end":
                end = start;
                start -= pA * textWidth;
                break;
            case "left":  // intentionally falls through to case "start"
            case "start":
                end = start + pA * textWidth;
            }
        }

        ctx.textAlign = "center";                     // align for rendering
        a = start;                                    // set the start angle
        for (var i = 0; i < text.length; i += 1) {    // for each character
            aw = ctx.measureText(text[i]).width * pA; // get the angular width of the text
            var xDx = Math.cos(a + aw / 2);           // get the yAxies vector from the center x,y out
            var xDy = Math.sin(a + aw / 2);
            if(multiplyCurrentTransform){ // transform multiplying current transform
                ctx.save();
                if (xDy < 0) { // is the text upside down. If it is flip it
                    ctx.transform(-xDy * wScale, xDx * wScale, -xDx, -xDy, xDx * radius + x, xDy * radius + y);
                } else {
                    ctx.transform(-xDy * wScale, xDx * wScale, xDx, xDy, xDx * radius + x, xDy * radius + y);
                }
            }else{
                if (xDy < 0) { // is the text upside down. If it is flip it
                    ctx.setTransform(-xDy * wScale, xDx * wScale, -xDx, -xDy, xDx * radius + x, xDy * radius + y);
                } else {
                    ctx.setTransform(-xDy * wScale, xDx * wScale, xDx, xDy, xDx * radius + x, xDy * radius + y);
                }
            }
            if(renderType === FILL){
                ctx.fillText(text[i], 0, 0);    // render the character
            }else{                    
                ctx.strokeText(text[i], 0, 0);  // render the character
            }
            if(multiplyCurrentTransform){  // restore current transform
                ctx.restore();
            }
            a += aw;                     // step to the next angle
        }
        // all done clean up.
        if(!multiplyCurrentTransform){
            ctx.setTransform(1, 0, 0, 1, 0, 0); // restore the transform
        }
        ctx.textAlign = aligned;            // restore the text alignment
    }
    // define fill text
    var fillCircleText = function(text, x, y, radius, start, end, forward){
        renderType = FILL;
        circleText(this, text, x, y, radius, start, end, forward);
    }
    // define stroke text
    var strokeCircleText = function(text, x, y, radius, start, end, forward){
        renderType = STROKE;
        circleText(this, text, x, y, radius, start, end, forward);
    }
    // define measure text
    var measureCircleTextExt = function(text,radius){
        return measure(this, text, radius);
    }
    // set the prototypes
    CanvasRenderingContext2D.prototype.fillCircleText = fillCircleText;
    CanvasRenderingContext2D.prototype.strokeCircleText = strokeCircleText;
    CanvasRenderingContext2D.prototype.measureCircleText = measureCircleTextExt;  
})();

```

### **Function descriptions**

This example adds 3 functions to the `CanvasRenderingContext2D prototype`.  `fillCircleText`, `strokeCircleText`, and `measureCircleText`

### **CanvasRenderingContext2D.fillCircleText(text, x, y, radius, start, [end, [forward]]);**<br>

### **CanvasRenderingContext2D.strokeCircleText(text, x, y, radius, start, [end, [forward]]);**

- **text:** Text to render as String.
- **x**,**y**: Position of circle center as Numbers.
- **radius:** radius of circle in pixels
- **start:** angle in radians to start.
- **[end]:** optional. If included `ctx.textAlign` is ignored and the text is scaled to fit between start and end.
- **[forward]:** optional default 'true'. if true text direction is forwards, if 'false'  direction is backward.

Both functions use the textBaseline to position the text vertically around the radius. For the best results use `ctx.TextBaseline`.

Functions will throw a `TypeError` is any of the numerical arguments as NaN.

If the `text` argument trims to an empty string or `ctx.globalAlpha = 0` the function just drops through and does nothing.

### **CanvasRenderingContext2D.measureCircleText(text, radius);**

```

- **text:** String of text to measure.
 - **radius:** radius of circle in pixels.

```

Returns a Object containing various size metrics for rendering circular text

```

  - **width:** Pixel width of text as it would normaly be rendered
   - **angularWidth:** angular width of text in radians.
   - **pixelAngularSize:** angular width of a pixel in radians.

```

### **Usage examples**

```js
const rad = canvas.height * 0.4;
const text = "Hello circle TEXT!";
const fontSize = 40;
const centX = canvas.width / 2;
const centY = canvas.height / 2;
ctx.clearRect(0,0,canvas.width,canvas.height)

ctx.font = fontSize + "px verdana";
ctx.textAlign = "center";
ctx.textBaseline = "bottom";
ctx.fillStyle = "#000";
ctx.strokeStyle = "#666";

// Text under stretched from Math.PI to 0 (180 - 0 deg)
ctx.fillCircleText(text, centX, centY, rad, Math.PI, 0);

// text over top centered at Math.PI * 1.5 ( 270 deg)
ctx.fillCircleText(text, centX, centY, rad, Math.PI * 1.5);

// text under top centered at Math.PI * 1.5 ( 270 deg)
ctx.textBaseline = "top";
ctx.fillCircleText(text, centX, centY, rad, Math.PI * 1.5);


// text over top centered at Math.PI * 1.5 ( 270 deg)
ctx.textBaseline = "middle";
ctx.fillCircleText(text, centX, centY, rad, Math.PI * 1.5);


// Use measureCircleText to get angular size
var circleTextMetric = ctx.measureCircleText("Text to measure", rad);
console.log(circleTextMetric.width);            // width of text if rendered normally
console.log(circleTextMetric.angularWidth);     // angular width of text
console.log(circleTextMetric.pixelAngularSize); // angular size of a pixel    



// Use measure text to draw a arc around the text
ctx.textBaseline = "middle";
var width = ctx.measureCircleText(text, rad).angularWidth;    
ctx.fillCircleText(text, centX, centY, rad, Math.PI * 1.5);

// render the arc around the text
ctx.strokeStyle= "red";
ctx.lineWidth = 3;
ctx.beginPath();
ctx.arc(centX, centY, rad + fontSize / 2,Math.PI * 1.5 - width/2,Math.PI*1.5 + width/2);
ctx.arc(centX, centY, rad - fontSize / 2,Math.PI * 1.5 + width/2,Math.PI*1.5 - width/2,true);
ctx.closePath();
ctx.stroke();

```

> 
**NOTE:** The text rendered is only an approximation of circular text. For example if two l's are rendered the two lines will not be parallel, but if you render a "H" the two edges will be parallel. This is because each character is rendered as close as possible to the required direction, rather than each pixel being correctly transformed to create circular text.


> 
<p>**NOTE:** `const multiplyCurrentTransform = true;` defined in this example is used to set the transformation method used. If `false` the transformation for circular text rendering is absolute and does not depend on the current transformation state. The text will not be effected by any previous scale, rotate, or translate transforms. This will increase the performance of the render function, after the function is called the transform will be set to the default `setTransform(1,0,0,1,0,0)` <br><br>
If `multiplyCurrentTransform = true` (set as default in this example) the text will use the current transform so that the text can be scaled translated, skewed, rotated, etc but modifying the current transform befor calling the `fillCircleText` and `strokeCircleText` functions. Depending on the current state of the 2D context this may be somewhat slower then `multiplyCurrentTransform = false`</p>




## Text on curve, cubic and quadratic beziers


[<img src="http://i.stack.imgur.com/Tsyf1.png" alt="enter image description here" />](http://i.stack.imgur.com/Tsyf1.png)

**textOnCurve(text,offset,x1,y1,x2,y2,x3,y3,x4,y4)**

Renders text on quadratic and cubic curves.

- `text` is the text to render
- `offset` distance from start of curve to text >= 0
- `x1,y1` - `x3,y3` points of quadratic curve or
- `x1,y1` - `x4,y4` points of cubic curve or

### Example usage:

```js
textOnCurve("Hello world!",50,100,100,200,200,300,100); // draws text on quadratic curve
                                                        // 50 pixels from start of curve


textOnCurve("Hello world!",50,100,100,200,200,300,100,400,200); 
                                                        // draws text on cubic curve
                                                        // 50 pixels from start of curve

```

The Function and curver helper function

```js
// pass 8 values for cubic bezier
// pass 6 values for quadratic
// Renders text from start of curve
var textOnCurve = function(text,offset,x1,y1,x2,y2,x3,y3,x4,y4){
    ctx.save();
    ctx.textAlign = "center";
    var widths = [];
    for(var i = 0; i < text.length; i ++){
        widths[widths.length] = ctx.measureText(text[i]).width;
    }
    var ch = curveHelper(x1,y1,x2,y2,x3,y3,x4,y4);
    var pos = offset;
    var cpos = 0;

    for(var i = 0; i < text.length; i ++){
        pos += widths[i] / 2;
        cpos = ch.forward(pos);
        ch.tangent(cpos);
        ctx.setTransform(ch.vect.x, ch.vect.y, -ch.vect.y, ch.vect.x, ch.vec.x, ch.vec.y);
        ctx.fillText(text[i],0,0);     

        pos += widths[i] / 2;
    }
    ctx.restore();
}

```

The curve helper function is designed to increase the performance of finding points on the bezier.

```js
// helper function locates points on bezier curves.
function curveHelper(x1, y1, x2, y2, x3, y3, x4, y4){
    var tx1, ty1, tx2, ty2, tx3, ty3, tx4, ty4;
    var a,b,c,u;
    var vec,currentPos,vec1,vect;
    vec = {x:0,y:0};
    vec1 = {x:0,y:0};
    vect = {x:0,y:0};
    quad = false;
    currentPos = 0;
    currentDist = 0;
    if(x4 === undefined || x4 === null){
        quad = true;
        x4 = x3;
        y4 = y3;
    }
    var estLen = Math.sqrt((x4 - x1) * (x4 - x1) + (y4 - y1) * (y4 - y1));
    var onePix = 1 / estLen;
    function posAtC(c){
        tx1 = x1; ty1 = y1;
        tx2 = x2; ty2 = y2;
        tx3 = x3; ty3 = y3;
        tx1 += (tx2 - tx1) * c;
        ty1 += (ty2 - ty1) * c;
        tx2 += (tx3 - tx2) * c;
        ty2 += (ty3 - ty2) * c;
        tx3 += (x4 - tx3) * c;
        ty3 += (y4 - ty3) * c;
        tx1 += (tx2 - tx1) * c;
        ty1 += (ty2 - ty1) * c;
        tx2 += (tx3 - tx2) * c;
        ty2 += (ty3 - ty2) * c;
        vec.x = tx1 + (tx2 - tx1) * c;
        vec.y = ty1 + (ty2 - ty1) * c;    
        return vec;
    }
    function posAtQ(c){
        tx1 = x1; ty1 = y1;
        tx2 = x2; ty2 = y2;
        tx1 += (tx2 - tx1) * c;
        ty1 += (ty2 - ty1) * c;
        tx2 += (x3 - tx2) * c;
        ty2 += (y3 - ty2) * c;
        vec.x = tx1 + (tx2 - tx1) * c;
        vec.y = ty1 + (ty2 - ty1) * c;
        return vec;
    }    
    function forward(dist){
        var step;
        helper.posAt(currentPos);

        while(currentDist < dist){
            vec1.x = vec.x;
            vec1.y = vec.y;            
            currentPos += onePix;
            helper.posAt(currentPos);
            currentDist += step = Math.sqrt((vec.x - vec1.x) * (vec.x - vec1.x) + (vec.y - vec1.y) * (vec.y - vec1.y));

        }
        currentPos -= ((currentDist - dist) / step) * onePix
        currentDist -= step;
        helper.posAt(currentPos);
        currentDist += Math.sqrt((vec.x - vec1.x) * (vec.x - vec1.x) + (vec.y - vec1.y) * (vec.y - vec1.y));
        return currentPos;
    }
    
    function tangentQ(pos){
        a = (1-pos) * 2;
        b = pos * 2;
        vect.x = a * (x2 - x1) + b * (x3 - x2);
        vect.y = a * (y2 - y1) + b * (y3 - y2);       
        u = Math.sqrt(vect.x * vect.x + vect.y * vect.y);
        vect.x /= u;
        vect.y /= u;        
    }
    function tangentC(pos){
        a  = (1-pos)
        b  = 6 * a * pos;       
        a *= 3 * a;                  
        c  = 3 * pos * pos; 
        vect.x  = -x1 * a + x2 * (a - b) + x3 * (b - c) + x4 * c;
        vect.y  = -y1 * a + y2 * (a - b) + y3 * (b - c) + y4 * c;
        u = Math.sqrt(vect.x * vect.x + vect.y * vect.y);
        vect.x /= u;
        vect.y /= u;
    }  
    var helper = {
        vec : vec,
        vect : vect,
        forward : forward,
    }
    if(quad){
        helper.posAt = posAtQ;
        helper.tangent = tangentQ;
    }else{
        helper.posAt = posAtC;
        helper.tangent = tangentC;
    }
    return helper
}

```



## Justified text


This example renders justified text. It adds extra functionality to the `CanvasRenderingContext2D` by extending its prototype or as a global object `justifiedText` (optional see Note A).

### Example rendering.

[<img src="http://i.stack.imgur.com/ndKly.png" alt="enter image description here" />](http://i.stack.imgur.com/ndKly.png)<br>
<sub>**Code to render this image is in the usage examples at the bottom**.</sub>

### The Example

The function as a anonymous immediately invoked function.

```js
(function(){
    const FILL = 0;        // const to indicate filltext render
    const STROKE = 1;
    const MEASURE = 2;
    var renderType = FILL; // used internal to set fill or stroke text
    
    var maxSpaceSize = 3; // Multiplier for max space size. If greater then no justificatoin applied
    var minSpaceSize = 0.5; // Multiplier for minimum space size
    var renderTextJustified = function(ctx,text,x,y,width){
        var words, wordsWidth, count, spaces, spaceWidth, adjSpace, renderer, i, textAlign, useSize, totalWidth;
        textAlign = ctx.textAlign; // get current align settings
        ctx.textAlign = "left";
        wordsWidth = 0;
        words = text.split(" ").map(word => {
            var w = ctx.measureText(word).width;                
            wordsWidth += w;
            return {
                width : w,
                word : word,
            };
        });
        // count = num words, spaces = number spaces, spaceWidth normal space size
        // adjSpace new space size >= min size. useSize Resulting space size used to render
        count = words.length;
        spaces = count - 1;
        spaceWidth = ctx.measureText(" ").width;
        adjSpace = Math.max(spaceWidth * minSpaceSize, (width - wordsWidth) / spaces);
        useSize = adjSpace > spaceWidth * maxSpaceSize ? spaceWidth : adjSpace;
        totalWidth = wordsWidth + useSize * spaces
        if(renderType === MEASURE){ // if measuring return size
            ctx.textAlign = textAlign;
            return totalWidth;
        }
        renderer = renderType === FILL ? ctx.fillText.bind(ctx) : ctx.strokeText.bind(ctx); // fill or stroke
        switch(textAlign){
            case "right":
                x -= totalWidth;
                break;
            case "end":
                x += width - totalWidth;
                break;
            case "center": // intentional fall through to default
                x -= totalWidth / 2;                     
            default:
        }
        if(useSize === spaceWidth){ // if space size unchanged
            renderer(text,x,y);
        } else {
            for(i = 0; i < count; i += 1){
                renderer(words[i].word,x,y);
                x += words[i].width;
                x += useSize;
            }
        }
        ctx.textAlign = textAlign;
    }
    // Parse vet and set settings object.
    var justifiedTextSettings = function(settings){
        var min,max;
        var vetNumber = (num, defaultNum) => {
            num = num !== null && num !== null && !isNaN(num) ? num : defaultNum;
            if(num < 0){
                num = defaultNum;
            }
            return num;
        }
        if(settings === undefined || settings === null){
            return;
        }
        max = vetNumber(settings.maxSpaceSize, maxSpaceSize);
        min = vetNumber(settings.minSpaceSize, minSpaceSize);
        if(min > max){
            return;
        }
        minSpaceSize = min;
        maxSpaceSize = max;
    }
    // define fill text
    var fillJustifyText = function(text, x, y, width, settings){
        justifiedTextSettings(settings);
        renderType = FILL;
        renderTextJustified(this, text, x, y, width);
    }
    // define stroke text
    var strokeJustifyText = function(text, x, y, width, settings){
        justifiedTextSettings(settings);
        renderType = STROKE;
        renderTextJustified(this, text, x, y, width);
    }
    // define measure text
    var measureJustifiedText = function(text, width, settings){
        justifiedTextSettings(settings);
        renderType = MEASURE;
        return renderTextJustified(this, text, 0, 0, width);
    }
    // code point A
    // set the prototypes
    CanvasRenderingContext2D.prototype.fillJustifyText = fillJustifyText;
    CanvasRenderingContext2D.prototype.strokeJustifyText = strokeJustifyText;
    CanvasRenderingContext2D.prototype.measureJustifiedText = measureJustifiedText;  
    // code point B
    
    // optional code if you do not wish to extend the CanvasRenderingContext2D prototype
    /* Uncomment from here to the closing comment
    window.justifiedText = {
        fill : function(ctx, text, x, y, width, settings){
            justifiedTextSettings(settings);
            renderType = FILL;
            renderTextJustified(ctx, text, x, y, width);
        },
        stroke : function(ctx, text, x, y, width, settings){
            justifiedTextSettings(settings);
            renderType = STROKE;
            renderTextJustified(ctx, text, x, y, width);
        },
        measure : function(ctx, text, width, settings){
            justifiedTextSettings(settings);
            renderType = MEASURE;
            return renderTextJustified(ctx, text, 0, 0, width);
        }
    }
    to here*/
})();

```

> 
**Note A:** If you do not wish to extend the `CanvasRenderingContext2D` prototype Remove from the example all code between `// code point A` and `// code point B` and uncomment the code marked `/* Uncomment from here to the closing comment`


### How to use

Three functions are added to the `CanvasRenderingContext2D` and are available to all 2D context objects created.

- ctx.fillJustifyText( text, x, y, width, [settings]);
- ctx.strokeJustifyText( text, x, y, width, [settings]);
- ctx.measureJustifiedText( text, width, [settings]);

Fill and stroke text function fill or stroke text and use the same arguments. `measureJustifiedText` will return the actual width that text would be rendered at. This may be equal, less, or greater than the argument `width` depending on current settings.

> 
**Note:** Arguments inside `[` and `]` are optional.


### Function arguments

<li>
**text:** String containing the text to be rendered.
</li>
<li>
**x, y:** Coordinates to render the text at.
</li>
<li>
**width:** Width of the justified text. Text will increase/decrease spaces between words to fit the width. If the space between words is greater than `maxSpaceSize` (default = 6) times normal spacing will be used and the text will not fill the required width. If the spacing is less than `minSpaceSize` (default = 0.5) time normal spacing then the min space size is used and the text will overrun the width requested
</li>
<li>
**settings:** Optional. Object containing min and max space sizes.
</li>

The `settings` argument is optional and if not included text rendering will use the last setting defined or the default (shown below).

Both min and max are the min and max sizes for the [space] character separating words. The default `maxSpaceSize = 6` means that when the space between characters is > 63 * ctx.measureText(" ").width text will not be justified. If text to be justified has spaces less than `minSpaceSize = 0.5` (default value 0.5) * `ctx.measureText(" ").width` the spacing will be set to `minSpaceSize * ctx.measureText(" ").width` and the resulting text will overrun the justifying width.

The following rules are applied, min and max must be numbers. If not then the associate values will not be changed. If `minSpaceSize` is larger than `maxSpaceSize` both input setting are invalid and min max will not be changed.

Example setting object with defaults

```js
settings = { 
    maxSpaceSize : 6;   // Multiplier for max space size. 
    minSpaceSize : 0.5; // Multiplier for minimum space size
};

```

> 
**NOTE:** These text functions introduce a subtle behaviour change for the `textAlign` property of the 2D context. 'Left', 'right', 'center' and 'start' behave as is expected but 'end' will not align from the right of the function argument `x` but rather from the right of `x + width`


> 
**Note:** settings (min and max space size) are global to all 2D context objects.


### USAGE Examples

```js
var i = 0;
text[i++] = "This text is aligned from the left of the canvas."; 
text[i++] = "This text is near the max spacing size"; 
text[i++] = "This text is way too short."; 
text[i++] = "This text is too long for the space provied and will overflow#";
text[i++] = "This text is aligned using 'end' and starts at x + width"; 
text[i++] = "This text is near the max spacing size"; 
text[i++] = "This text is way too short."; 
text[i++] = "#This text is too long for the space provied and will overflow";
text[i++] = "This is aligned with 'center' and is placed from the center"; 
text[i++] = "This text is near the max spacing size"; 
text[i++] = "This text is way too short."; 
text[i++] = "This text is just too long for the space provied and will overflow";

// ctx is the 2d context
// canvas is the canvas

ctx.clearRect(0,0,w,h);
ctx.font = "25px arial";
ctx.textAlign = "center"
var left = 20;
var center = canvas.width / 2;
var width = canvas.width-left*2;
var y = 40;
var size = 16;
var i = 0;
ctx.fillText("Justified text examples.",center,y);
y+= 40;
ctx.font = "14px arial";
ctx.textAlign = "left"
var ww = ctx.measureJustifiedText(text[0], width);
var setting = {
    maxSpaceSize : 6,
    minSpaceSize : 0.5
}
ctx.strokeStyle = "red"
ctx.beginPath();
ctx.moveTo(left,y - size * 2);
ctx.lineTo(left, y + size * 15);
ctx.moveTo(canvas.width - left,y - size * 2);
ctx.lineTo(canvas.width - left, y + size * 15);
ctx.stroke();
ctx.textAlign = "left";
ctx.fillStyle = "red";
ctx.fillText("< 'left' aligned",left,y - size)
ctx.fillStyle = "black";
ctx.fillJustifyText(text[i++], left, y, width, setting);  // settings is remembered
ctx.fillJustifyText(text[i++], left, y+=size, width);
ctx.fillJustifyText(text[i++], left, y+=size, width);
ctx.fillJustifyText(text[i++], left, y+=size, width);
y += 2.3*size;
ctx.fillStyle = "red";
ctx.fillText("< 'end' aligned from x plus the width -------------------->",left,y - size)
ctx.fillStyle = "black";
ctx.textAlign = "end";
ctx.fillJustifyText(text[i++], left, y, width);
ctx.fillJustifyText(text[i++], left, y+=size, width);
ctx.fillJustifyText(text[i++], left, y+=size, width);
ctx.fillJustifyText(text[i++], left, y+=size, width);

y += 40;
ctx.strokeStyle = "red"
ctx.beginPath();
ctx.moveTo(center,y - size * 2);
ctx.lineTo(center, y + size * 5);
ctx.stroke();
ctx.textAlign = "center";
ctx.fillStyle = "red";
ctx.fillText("'center' aligned",center,y - size)
ctx.fillStyle = "black";
ctx.fillJustifyText(text[i++], center, y, width);
ctx.fillJustifyText(text[i++], center, y+=size, width);
ctx.fillJustifyText(text[i++], center, y+=size, width);
ctx.fillJustifyText(text[i++], center, y+=size, width);

```



## Justified paragraphs.


Renders text as justified paragraphs. **REQUIRES** the example **Justified text**

### Example render

[<img src="https://i.stack.imgur.com/xNJh8.png" alt="enter image description here" />](https://i.stack.imgur.com/xNJh8.png)
<br><sub>Top paragraph has **setting.compact = true** and bottom **false** and line spacing is **1.2** rather than the default **1.5**. Rendered by code usage example bottom of this example.</sub>

### Example code

```js
// Requires justified text extensions 
(function(){
    // code point A
    if(typeof CanvasRenderingContext2D.prototype.fillJustifyText !== "function"){
        throw new ReferenceError("Justified Paragraph extension missing requiered CanvasRenderingContext2D justified text extension");
    }
    var maxSpaceSize = 3; // Multiplier for max space size. If greater then no justificatoin applied
    var minSpaceSize = 0.5; // Multiplier for minimum space size   
    var compact = true; // if true then try and fit as many words as possible. If false then try to get the spacing as close as possible to normal
    var lineSpacing = 1.5; // space between lines
    const noJustifySetting = {  // This setting forces justified text off. Used to render last line of paragraph.
        minSpaceSize : 1,
        maxSpaceSize : 1,
    }

    // Parse vet and set settings object.
    var justifiedTextSettings = function(settings){
        var min, max;
        var vetNumber = (num, defaultNum) => {
            num = num !== null && num !== null && !isNaN(num) ? num : defaultNum;
            return num < 0 ? defaultNum : num;
        }
        if(settings === undefined || settings === null){ return; }
        compact = settings.compact === true ? true : settings.compact === false ? false : compact;
        max = vetNumber(settings.maxSpaceSize, maxSpaceSize);
        min = vetNumber(settings.minSpaceSize, minSpaceSize);
        lineSpacing = vetNumber(settings.lineSpacing, lineSpacing);
        if(min > max){ return; }
        minSpaceSize = min;
        maxSpaceSize = max;
    }        
    var getFontSize = function(font){  // get the font size. 
        var numFind = /[0-9]+/;
        var number = numFind.exec(font)[0];
        if(isNaN(number)){
            throw new ReferenceError("justifiedPar Cant find font size");
        }
        return Number(number);
    }
    function justifiedPar(ctx, text, x, y, width, settings, stroke){
        var spaceWidth, minS, maxS, words, count, lines, lineWidth, lastLineWidth, lastSize, i, renderer, fontSize, adjSpace, spaces, word, lineWords, lineFound;
        spaceWidth = ctx.measureText(" ").width;
        minS = spaceWidth * minSpaceSize;
        maxS = spaceWidth * maxSpaceSize;
        words = text.split(" ").map(word => {  // measure all words.
            var w = ctx.measureText(word).width;                
            return {
                width : w,
                word : word,
            };
        });
        // count = num words, spaces = number spaces, spaceWidth normal space size
        // adjSpace new space size >= min size. useSize Resulting space size used to render
        count = 0;
        lines = [];
        // create lines by shifting words from the words array until the spacing is optimal. If compact
        // true then will true and fit as many words as possible. Else it will try and get the spacing as
        // close as possible to the normal spacing
        while(words.length > 0){
            lastLineWidth = 0;
            lastSize = -1;
            lineFound = false;
            // each line must have at least one word.
            word = words.shift();
            lineWidth = word.width;
            lineWords = [word.word];
            count = 0;
            while(lineWidth < width && words.length > 0){ // Add words to line
                word = words.shift();
                lineWidth += word.width;
                lineWords.push(word.word);
                count += 1;
                spaces = count - 1;
                adjSpace =  (width - lineWidth) / spaces;
                if(minS > adjSpace){  // if spacing less than min remove last word and finish line
                    lineFound = true;
                    words.unshift(word);
                    lineWords.pop();
                }else{
                    if(!compact){ // if compact mode 
                        if(adjSpace < spaceWidth){ // if less than normal space width
                            if(lastSize === -1){
                                lastSize = adjSpace;
                            }
                            // check if with last word on if its closer to space width
                            if(Math.abs(spaceWidth - adjSpace) < Math.abs(spaceWidth - lastSize)){
                                lineFound = true; // yes keep it
                            }else{
                                words.unshift(word);  // no better fit if last word removes
                                lineWords.pop();
                                lineFound = true;
                            }
                        }
                    }
                }
                lastSize = adjSpace; // remember spacing 
            }
            lines.push(lineWords.join(" ")); // and the line
        }
        // lines have been worked out get font size, render, and render all the lines. last
        // line may need to be rendered as normal so it is outside the loop.
        fontSize = getFontSize(ctx.font);
        renderer = stroke === true ? ctx.strokeJustifyText.bind(ctx) : ctx.fillJustifyText.bind(ctx);
        for(i = 0; i < lines.length - 1; i ++){
            renderer(lines[i], x, y, width, settings);
            y += lineSpacing * fontSize;
        }
        if(lines.length > 0){ // last line if left or start aligned for no justify
            if(ctx.textAlign === "left" || ctx.textAlign === "start"){
                renderer(lines[lines.length - 1], x, y, width, noJustifySetting);
                ctx.measureJustifiedText("", width, settings);
            }else{
                renderer(lines[lines.length - 1], x, y, width);
            }
        }
        // return details about the paragraph.
        y += lineSpacing * fontSize;
        return {
            nextLine : y,
            fontSize : fontSize,
            lineHeight : lineSpacing * fontSize,
        };
    }
    // define fill
    var fillParagraphText = function(text, x, y, width, settings){
        justifiedTextSettings(settings);
        settings = {
            minSpaceSize : minSpaceSize,
            maxSpaceSize : maxSpaceSize,
        };
        return justifiedPar(this, text, x, y, width, settings);
    }
    // define stroke
    var strokeParagraphText = function(text, x, y, width, settings){
        justifiedTextSettings(settings);
        settings = {
            minSpaceSize : minSpaceSize,
            maxSpaceSize : maxSpaceSize,
        };
        return justifiedPar(this, text, x, y, width, settings,true);
    }
    CanvasRenderingContext2D.prototype.fillParaText = fillParagraphText;
    CanvasRenderingContext2D.prototype.strokeParaText = strokeParagraphText;
})();

```

> 
**NOTE** this extends the `CanvasRenderingContext2D` prototype. If you do not wish this to happen use the example **Justified text** to work out how to change this example to be part of the global namespace.


> 
**NOTE** Will throw a ReferenceError if this example can not find the function `CanvasRenderingContext2D.prototype.fillJustifyText`


### How to use

```html
ctx.fillParaText(text, x, y, width, [settings]);
ctx.strokeParaText(text, x, y, width, [settings]);

```

See **Justified text** for details on arguments. Arguments between `[` and `]` are optional.

The `settings` argument has two additional properties.

- **compact:** Default `true`. If true tries to pack as many words as possible per line. If false the tries to get the word spacing as close as possible to normal spacing.
- **lineSpacing** Default `1.5`. Space per line default `1.5` the distance from on line to the next in terms of font size

Properties missing from the settings object will default to their default values or to the last valid values. The properties will only be changed if the new values are valid. For `compact` valid values are only booleans `true` or `false` Truthy values are not considered valid.

### Return object

The two functions return an object containing information to help you place the next paragraph. The object contains the following properties.

- **nextLine** Position of the next line after the paragraph pixels.
- **fontSize**  Size of the font. (please note only use fonts defined in pixels eg `14px arial`)
- **lineHeight** Distance in pixels from one line to the next

This example uses a simple algorithm that works one line at to time to find the best fit for a paragraph. This does not mean that it the best fit (rather the algorithm's best) You may wish to improve the algorithm by creating a multi pass line algorithm over the generated lines. Moving words from the end of one line to the start of the next, or from the start back to the end. The best look is achieved when the spacing over the entire paragraph has the smallest variation and is the closest to the normal text spacing.

As this example is dependent on the **Justified text** example the code is very similar. You may wish to move the two into one function. Replace the function `justifiedTextSettings` in the other example with the one used in this example. Then copy all the rest of the code from this example into the anonymous function body of the **Justified text** example. You will no longer need to test for dependencies found at `// Code point A`  It can be removed.

### Usage example

```js
ctx.font = "25px arial";
ctx.textAlign = "center"

var left = 10;
var center = canvas.width / 2;
var width = canvas.width-left*2;
var y = 20;
var size = 16;
var i = 0;
ctx.fillText("Justified paragraph examples.",center,y);
y+= 30;
ctx.font = "14px arial";
ctx.textAlign = "left"
// set para settings
var setting = {
    maxSpaceSize : 6,
    minSpaceSize : 0.5,
    lineSpacing : 1.2,
    compact : true,
}
// Show the left and right bounds.
ctx.strokeStyle = "red"
ctx.beginPath();
ctx.moveTo(left,y - size * 2);
ctx.lineTo(left, y + size * 15);
ctx.moveTo(canvas.width - left,y - size * 2);
ctx.lineTo(canvas.width - left, y + size * 15);
ctx.stroke();
ctx.textAlign = "left";
ctx.fillStyle = "black";

// Draw paragraph
var line = ctx.fillParaText(para, left, y, width, setting);  // settings is remembered    

// Next paragraph
y = line.nextLine + line.lineHeight;
setting.compact = false;
ctx.fillParaText(para, left, y, width, setting);

```

> 
**Note:** For text aligned `left` or `start` the last line of tha paragraph will always have normal spacing. For all other alignments the last line is treated like all others.


> 
**Note:** You can inset the start of the paragraph with spaces. Though this may not be consistent from paragraph to paragraph. It is always a good thing to learn what a function is doing and modifying it. An exercise would be to add a setting to the settings that indents the first line by a fixed amount. Hint the while loop will need to temporarily make the first word appear larger (+ indent) `words[0].width += ?` and then when rendering lines indent the first line.


