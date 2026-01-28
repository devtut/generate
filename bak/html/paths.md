---
metaTitle: "HTML - Paths"
description: "Ellipse, Line without blurryness"
---

# Paths



## Ellipse


[<img src="http://i.stack.imgur.com/5yyE0.png" alt="enter image description here" />](http://i.stack.imgur.com/5yyE0.png)

**Note: Browsers are in the process of adding a built-in `context.ellipse` drawing command, but this command is not universally adopted (notably not in IE). The methods below work in all browsers.**

Draw an ellipse given it's desired top-left coordinate:

```js
// draws an ellipse based on x,y being top-left coordinate
function drawEllipse(x,y,width,height){
    var PI2=Math.PI*2;
    var ratio=height/width;
    var radius=Math.max(width,height)/2;
    var increment = 1 / radius;
    var cx=x+width/2;
    var cy=y+height/2;
    
    ctx.beginPath();
    var x = cx + radius * Math.cos(0);
    var y = cy - ratio * radius * Math.sin(0);
    ctx.lineTo(x,y);

    for(var radians=increment; radians<PI2; radians+=increment){ 
        var x = cx + radius * Math.cos(radians);
        var y = cy - ratio * radius * Math.sin(radians);
        ctx.lineTo(x,y);
     }

    ctx.closePath();
    ctx.stroke();
}

```

Draw an ellipse given it's desired center point coordinate:

```js
// draws an ellipse based on cx,cy being ellipse's centerpoint coordinate
function drawEllipse2(cx,cy,width,height){
    var PI2=Math.PI*2;
    var ratio=height/width;
    var radius=Math.max(width,height)/2;
    var increment = 1 / radius;

    ctx.beginPath();
    var x = cx + radius * Math.cos(0);
    var y = cy - ratio * radius * Math.sin(0);
    ctx.lineTo(x,y);

    for(var radians=increment; radians<PI2; radians+=increment){ 
        var x = cx + radius * Math.cos(radians);
        var y = cy - ratio * radius * Math.sin(radians);
        ctx.lineTo(x,y);
     }

    ctx.closePath();
    ctx.stroke();
}

```



## Line without blurryness


When Canvas draws a line it automatically adds anti-aliasing to visually heal "jaggedness". The result is a line that is less jagged but more blurry.

This function draws a line between 2 points without anti-aliasing using [Bresenham's_line algorithm](http://rosettacode.org/wiki/Bitmap/Bresenham%27s_line_algorithm#JavaScript). The result is a crisp line without the jaggedness.

****Important Note:** This pixel-by-pixel method is a much slower drawing method than `context.lineTo`.**

[<img src="http://i.stack.imgur.com/DktZn.png" alt="enter image description here" />](http://i.stack.imgur.com/DktZn.png)

```js
// Usage:
bresenhamLine(50,50,250,250);

// x,y line start
// xx,yy line end
// the pixel at line start and line end are drawn
function bresenhamLine(x, y, xx, yy){
    var oldFill = ctx.fillStyle;  // save old fill style
    ctx.fillStyle = ctx.strokeStyle; // move stroke style to fill
    xx = Math.floor(xx);
    yy = Math.floor(yy);
    x = Math.floor(x);
    y = Math.floor(y);
    // BRENSENHAM 
    var dx =  Math.abs(xx-x); 
    var sx = x < xx ? 1 : -1;
    var dy = -Math.abs(yy-y);
    var sy = y<yy ? 1 : -1; 
    var err = dx+dy;
    var errC; // error value
    var end = false;
    var x1 = x;
    var y1 = y;

    while(!end){
       ctx.fillRect(x1, y1, 1, 1); // draw each pixel as a rect
        if (x1 === xx && y1 === yy) {
            end = true;
        }else{
            errC = 2*err;
            if (errC >= dy) { 
                err += dy; 
                x1 += sx; 
            }
            if (errC <= dx) { 
                err += dx; 
                y1 += sy; 
            } 
        }
    }
    ctx.fillStyle = oldFill; // restore old fill style
}

```

