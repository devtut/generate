---
metaTitle: "HTML - Navigating along a Path"
description: "Find point on curve, Finding points along a cubic Bezier curve, Finding points along a quadratic curve, Finding points along a line, Finding points along an entire Path containing curves and lines, Length of a Quadratic Curve, Split bezier curves at position, Trim bezier curve., Length of a Cubic Bezier Curve (a close approximation), Finding extent of Quadratic Curve"
---

# Navigating along a Path



## Find point on curve


This example finds a point on a bezier or cubic curve at `position` where `position` is he unit distance on the curve 0 <= `position` <= 1. The position is clamped to the range thus if values < 0 or > 1 are passed they will be set 0,1 respectively.

Pass the function 6 coordinates for quadratic bezier or 8 for cubic.

The last optional argument is the returned vector (point). If not given it will be created.

### Example usage

```html
var p1 = {x : 10 , y : 100};
var p2 = {x : 100, y : 200};
var p3 = {x : 200, y : 0};
var p4 = {x : 300, y : 100};
var point = {x : null, y : null};

// for cubic beziers
point = getPointOnCurve(0.5, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y, point);
// or No need to set point as it is a referance and will be set
getPointOnCurve(0.5, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y, point);
// or to create a new point
var point1 = getPointOnCurve(0.5, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y);

// for quadratic beziers
point = getPointOnCurve(0.5, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, null, null, point);
// or No need to set point as it is a referance and will be set
getPointOnCurve(0.5, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, null, null, point);
// or to create a new point
var point1 = getPointOnCurve(0.5, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y);

```

### The function

**getPointOnCurve = function(position, x1, y1, x2, y2, x3, y3, [x4, y4], [vec])**

> 
**Note:** Arguments inside [x4, y4] are optional.


> 
**Note:** `x4`,`y4` if `null`, or `undefined` means that the curve is a quadratic bezier. `vec` is optional and will hold the returned point if supplied. If not it will be created.


```html
var getPointOnCurve = function(position, x1, y1, x2, y2, x3, y3, x4, y4, vec){ 
    var vec, quad;
    quad = false;
    if(vec === undefined){        
        vec = {};
    }
    
    if(x4 === undefined || x4 === null){
        quad = true;
        x4 = x3;
        y4 = y3;
    }
        
    if(position <= 0){
        vec.x = x1;
        vec.y = y1;
        return vec;
    }
    if(position >= 1){
        vec.x = x4;
        vec.y = y4;
        return vec;
    }
    c = position;
    if(quad){
        x1 += (x2 - x1) * c;
        y1 += (y2 - y1) * c;
        x2 += (x3 - x2) * c;
        y2 += (y3 - y2) * c;
        vec.x = x1 + (x2 - x1) * c;
        vec.y = y1 + (y2 - y1) * c;
        return vec;
    }
    x1 += (x2 - x1) * c;
    y1 += (y2 - y1) * c;
    x2 += (x3 - x2) * c;
    y2 += (y3 - y2) * c;
    x3 += (x4 - x3) * c;
    y3 += (y4 - y3) * c;
    x1 += (x2 - x1) * c;
    y1 += (y2 - y1) * c;
    x2 += (x3 - x2) * c;
    y2 += (y3 - y2) * c;
    vec.x = x1 + (x2 - x1) * c;
    vec.y = y1 + (y2 - y1) * c;
    return vec;     
}

```



## Finding points along a cubic Bezier curve


This example finds an array of approximately evenly spaced points along a cubic Bezier curve.

It decomposes Path segments created with `context.bezierCurveTo` into points along that curve.

```html
// Return: an array of approximately evenly spaced points along a cubic Bezier curve
//
// Attribution: Stackoverflow's @Blindman67
// Cite: http://stackoverflow.com/questions/36637211/drawing-a-curved-line-in-css-or-canvas-and-moving-circle-along-it/36827074#36827074
// As modified from the above citation
// 
// ptCount: sample this many points at interval along the curve
// pxTolerance: approximate spacing allowed between points
// Ax,Ay,Bx,By,Cx,Cy,Dx,Dy: control points defining the curve
//
function plotCBez(ptCount,pxTolerance,Ax,Ay,Bx,By,Cx,Cy,Dx,Dy){
    var deltaBAx=Bx-Ax;
    var deltaCBx=Cx-Bx;
    var deltaDCx=Dx-Cx;
    var deltaBAy=By-Ay;
    var deltaCBy=Cy-By;
    var deltaDCy=Dy-Cy;
    var ax,ay,bx,by;
    var lastX=-10000;
    var lastY=-10000;
    var pts=[{x:Ax,y:Ay}];
    for(var i=1;i<ptCount;i++){
        var t=i/ptCount;
        ax=Ax+deltaBAx*t;
        bx=Bx+deltaCBx*t;
        cx=Cx+deltaDCx*t;
        ax+=(bx-ax)*t;
        bx+=(cx-bx)*t;
        //
        ay=Ay+deltaBAy*t;
        by=By+deltaCBy*t;
        cy=Cy+deltaDCy*t;
        ay+=(by-ay)*t;
        by+=(cy-by)*t;
        var x=ax+(bx-ax)*t;
        var y=ay+(by-ay)*t;
        var dx=x-lastX;
        var dy=y-lastY;
        if(dx*dx+dy*dy>pxTolerance){
            pts.push({x:x,y:y});
            lastX=x;
            lastY=y;
        }
    }
    pts.push({x:Dx,y:Dy});
    return(pts);
}

```



## Finding points along a quadratic curve


This example finds an array of approximately evenly spaced points along a quadratic curve.

It decomposes Path segments created with `context.quadraticCurveTo` into points along that curve.

```html
// Return: an array of approximately evenly spaced points along a Quadratic curve
//
// Attribution: Stackoverflow's @Blindman67
// Cite: http://stackoverflow.com/questions/36637211/drawing-a-curved-line-in-css-or-canvas-and-moving-circle-along-it/36827074#36827074
// As modified from the above citation
//
// ptCount: sample this many points at interval along the curve
// pxTolerance: approximate spacing allowed between points
// Ax,Ay,Bx,By,Cx,Cy: control points defining the curve
//
function plotQBez(ptCount,pxTolerance,Ax,Ay,Bx,By,Cx,Cy){
    var deltaBAx=Bx-Ax;
    var deltaCBx=Cx-Bx;
    var deltaBAy=By-Ay;
    var deltaCBy=Cy-By;
    var ax,ay;
    var lastX=-10000;
    var lastY=-10000;
    var pts=[{x:Ax,y:Ay}];
    for(var i=1;i<ptCount;i++){
        var t=i/ptCount;
        ax=Ax+deltaBAx*t;
        ay=Ay+deltaBAy*t;
        var x=ax+((Bx+deltaCBx*t)-ax)*t;
        var y=ay+((By+deltaCBy*t)-ay)*t;
        var dx=x-lastX;
        var dy=y-lastY;
        if(dx*dx+dy*dy>pxTolerance){
            pts.push({x:x,y:y});
            lastX=x;
            lastY=y;
        }
    }
    pts.push({x:Cx,y:Cy});
    return(pts);
}

```



## Finding points along a line


This example finds an array of approximately evenly spaced points along a line.

It decomposes Path segments created with `context.lineTo` into points along that line.

```html
// Return: an array of approximately evenly spaced points along a line
//
// pxTolerance: approximate spacing allowed between points
// Ax,Ay,Bx,By: end points defining the line
//
function plotLine(pxTolerance,Ax,Ay,Bx,By){
    var dx=Bx-Ax;
    var dy=By-Ay;
    var ptCount=parseInt(Math.sqrt(dx*dx+dy*dy))*3;
    var lastX=-10000;
    var lastY=-10000;
    var pts=[{x:Ax,y:Ay}];
    for(var i=1;i<=ptCount;i++){
        var t=i/ptCount;
        var x=Ax+dx*t;
        var y=Ay+dy*t;
        var dx1=x-lastX;
        var dy1=y-lastY;
        if(dx1*dx1+dy1*dy1>pxTolerance){
            pts.push({x:x,y:y});
            lastX=x;
            lastY=y;
        }
    }
    pts.push({x:Bx,y:By});
    return(pts);
}

```



## Finding points along an entire Path containing curves and lines


This example finds an array of approximately evenly spaced points along an entire Path.

It decomposes all Path segments created with `context.lineTo`, `context.quadraticCurveTo` and/or `context.bezierCurveTo` into points along that Path.

**Usage**

```html
// Path related variables
var A={x:50,y:100};
var B={x:125,y:25};
var BB={x:150,y:15};
var BB2={x:150,y:185};
var C={x:175,y:200};
var D={x:300,y:150};
var n=1000;
var tolerance=1.5;
var pts;

// canvas related variables
var canvas=document.createElement("canvas");
var ctx=canvas.getContext("2d");
document.body.appendChild(canvas);
canvas.width=378;
canvas.height=256;

// Tell the Context to plot waypoint in addition to 
// drawing the path
plotPathCommands(ctx,n,tolerance);

// Path drawing commands
ctx.beginPath();
ctx.moveTo(A.x,A.y);
ctx.bezierCurveTo(B.x,B.y,C.x,C.y,D.x,D.y);
ctx.quadraticCurveTo(BB.x,BB.y,A.x,A.y);
ctx.lineTo(D.x,D.y);
ctx.strokeStyle='gray';
ctx.stroke();

// Tell the Context to stop plotting waypoints
ctx.stopPlottingPathCommands();

// Demo: Incrementally draw the path using the plotted points
ptsToRects(ctx.getPathPoints());
function ptsToRects(pts){
    ctx.fillStyle='red';
    var i=0;
    requestAnimationFrame(animate);
    function animate(){
        ctx.fillRect(pts[i].x-0.50,pts[i].y-0.50,tolerance,tolerance);
        i++;
        if(i<pts.length){ requestAnimationFrame(animate); }
    }
}

```

**An plug-in that automatically calculates points along the path**

This code modifies these Canvas Context's drawing commands so the commands not only draw the line or curve, but also create an array of points along the entire path:

- beginPath,
- moveTo,
- lineTo,
- quadraticCurveTo,
- bezierCurveTo.

**Important Note!**

This code modifies the actual drawing functions of the Context so when you are done plotting points along the path, you should call the supplied `stopPlottingPathCommands` to return the Context drawing functions to their unmodified state.

The purpose of this modified Context is to allow you to "plug-in" the points-array calculation into your existing code without having to modify your existing Path drawing commands. But, you don't need to use this modified Context -- you can separately call the individual functions that decompose a line, a quadratic curve and a cubic Bezier curve and then manually concatenate those individual point-arrays into a single point-array for the entire path.

You fetch a copy of the resulting points-array using the supplied `getPathPoints` function.

If you draw multiple Paths with the modified Context, the points-array will contain a single concatenated set of points for all the multiple Paths drawn.

If, instead, you want to get separate points-arrays, you can fetch the current array with `getPathPoints` and then clear those points from the array with the supplied `clearPathPoints` function.

```html
// Modify the Canvas' Context to calculate a set of approximately
//     evenly spaced waypoints as it draws path(s).
function plotPathCommands(ctx,sampleCount,pointSpacing){
    ctx.mySampleCount=sampleCount;
    ctx.myPointSpacing=pointSpacing;
    ctx.myTolerance=pointSpacing*pointSpacing;
    ctx.myBeginPath=ctx.beginPath;
    ctx.myMoveTo=ctx.moveTo;
    ctx.myLineTo=ctx.lineTo;
    ctx.myQuadraticCurveTo=ctx.quadraticCurveTo;
    ctx.myBezierCurveTo=ctx.bezierCurveTo;
    // don't use myPathPoints[] directly -- use "ctx.getPathPoints"
    ctx.myPathPoints=[];
    ctx.beginPath=function(){
        this.myLastX=0;
        this.myLastY=0;
        this.myBeginPath();
    }
    ctx.moveTo=function(x,y){
        this.myLastX=x;
        this.myLastY=y;
        this.myMoveTo(x,y);
    }
    ctx.lineTo=function(x,y){
        var pts=plotLine(this.myTolerance,this.myLastX,this.myLastY,x,y);
        Array.prototype.push.apply(this.myPathPoints,pts);
        this.myLastX=x;
        this.myLastY=y;
        this.myLineTo(x,y);
    }
    ctx.quadraticCurveTo=function(x0,y0,x1,y1){
        var pts=plotQBez(this.mySampleCount,this.myTolerance,this.myLastX,this.myLastY,x0,y0,x1,y1);
        Array.prototype.push.apply(this.myPathPoints,pts);
        this.myLastX=x1;
        this.myLastY=y1;
        this.myQuadraticCurveTo(x0,y0,x1,y1);
    }
    ctx.bezierCurveTo=function(x0,y0,x1,y1,x2,y2){
        var pts=plotCBez(this.mySampleCount,this.myTolerance,this.myLastX,this.myLastY,x0,y0,x1,y1,x2,y2);
        Array.prototype.push.apply(this.myPathPoints,pts);
        this.myLastX=x2;
        this.myLastY=y2;
        this.myBezierCurveTo(x0,y0,x1,y1,x2,y2);
    }
    ctx.getPathPoints=function(){
        return(this.myPathPoints.slice());
    }
    ctx.clearPathPoints=function(){
        this.myPathPoints.length=0;
    }
    ctx.stopPlottingPathCommands=function(){
        if(!this.myBeginPath){return;}
        this.beginPath=this.myBeginPath;
        this.moveTo=this.myMoveTo;
        this.lineTo=this.myLineTo;
        this.quadraticCurveto=this.myQuadraticCurveTo;
        this.bezierCurveTo=this.myBezierCurveTo;
        this.myBeginPath=undefined;
    }
}

```

**A complete Demo:**

```html
// Path related variables
var A={x:50,y:100};
var B={x:125,y:25};
var BB={x:150,y:15};
var BB2={x:150,y:185};
var C={x:175,y:200};
var D={x:300,y:150};
var n=1000;
var tolerance=1.5;
var pts;

// canvas related variables
var canvas=document.createElement("canvas");
var ctx=canvas.getContext("2d");
document.body.appendChild(canvas);
canvas.width=378;
canvas.height=256;

// Tell the Context to plot waypoint in addition to 
// drawing the path
plotPathCommands(ctx,n,tolerance);

// Path drawing commands
ctx.beginPath();
ctx.moveTo(A.x,A.y);
ctx.bezierCurveTo(B.x,B.y,C.x,C.y,D.x,D.y);
ctx.quadraticCurveTo(BB.x,BB.y,A.x,A.y);
ctx.lineTo(D.x,D.y);
ctx.strokeStyle='gray';
ctx.stroke();

// Tell the Context to stop plotting waypoints
ctx.stopPlottingPathCommands();

// Incrementally draw the path using the plotted points
ptsToRects(ctx.getPathPoints());
function ptsToRects(pts){
    ctx.fillStyle='red';
    var i=0;
    requestAnimationFrame(animate);
    function animate(){
        ctx.fillRect(pts[i].x-0.50,pts[i].y-0.50,tolerance,tolerance);
        i++;
        if(i<pts.length){ requestAnimationFrame(animate); }
    }
}


////////////////////////////////////////
// A Plug-in
////////////////////////////////////////

// Modify the Canvas' Context to calculate a set of approximately
//     evenly spaced waypoints as it draws path(s).
function plotPathCommands(ctx,sampleCount,pointSpacing){
    ctx.mySampleCount=sampleCount;
    ctx.myPointSpacing=pointSpacing;
    ctx.myTolerance=pointSpacing*pointSpacing;
    ctx.myBeginPath=ctx.beginPath;
    ctx.myMoveTo=ctx.moveTo;
    ctx.myLineTo=ctx.lineTo;
    ctx.myQuadraticCurveTo=ctx.quadraticCurveTo;
    ctx.myBezierCurveTo=ctx.bezierCurveTo;
    // don't use myPathPoints[] directly -- use "ctx.getPathPoints"
    ctx.myPathPoints=[];
    ctx.beginPath=function(){
        this.myLastX=0;
        this.myLastY=0;
        this.myBeginPath();
    }
    ctx.moveTo=function(x,y){
        this.myLastX=x;
        this.myLastY=y;
        this.myMoveTo(x,y);
    }
    ctx.lineTo=function(x,y){
        var pts=plotLine(this.myTolerance,this.myLastX,this.myLastY,x,y);
        Array.prototype.push.apply(this.myPathPoints,pts);
        this.myLastX=x;
        this.myLastY=y;
        this.myLineTo(x,y);
    }
    ctx.quadraticCurveTo=function(x0,y0,x1,y1){
        var pts=plotQBez(this.mySampleCount,this.myTolerance,this.myLastX,this.myLastY,x0,y0,x1,y1);
        Array.prototype.push.apply(this.myPathPoints,pts);
        this.myLastX=x1;
        this.myLastY=y1;
        this.myQuadraticCurveTo(x0,y0,x1,y1);
    }
    ctx.bezierCurveTo=function(x0,y0,x1,y1,x2,y2){
        var pts=plotCBez(this.mySampleCount,this.myTolerance,this.myLastX,this.myLastY,x0,y0,x1,y1,x2,y2);
        Array.prototype.push.apply(this.myPathPoints,pts);
        this.myLastX=x2;
        this.myLastY=y2;
        this.myBezierCurveTo(x0,y0,x1,y1,x2,y2);
    }
    ctx.getPathPoints=function(){
        return(this.myPathPoints.slice());
    }
    ctx.clearPathPoints=function(){
        this.myPathPoints.length=0;
    }
    ctx.stopPlottingPathCommands=function(){
        if(!this.myBeginPath){return;}
        this.beginPath=this.myBeginPath;
        this.moveTo=this.myMoveTo;
        this.lineTo=this.myLineTo;
        this.quadraticCurveto=this.myQuadraticCurveTo;
        this.bezierCurveTo=this.myBezierCurveTo;
        this.myBeginPath=undefined;
    }
}


////////////////////////////////
// Helper functions
////////////////////////////////

// Return: a set of approximately evenly spaced points along a cubic Bezier curve
//
// Attribution: Stackoverflow's @Blindman67
// Cite: http://stackoverflow.com/questions/36637211/drawing-a-curved-line-in-css-or-canvas-and-moving-circle-along-it/36827074#36827074
// As modified from the above citation
// 
// ptCount: sample this many points at interval along the curve
// pxTolerance: approximate spacing allowed between points
// Ax,Ay,Bx,By,Cx,Cy,Dx,Dy: control points defining the curve
//
function plotCBez(ptCount,pxTolerance,Ax,Ay,Bx,By,Cx,Cy,Dx,Dy){
    var deltaBAx=Bx-Ax;
    var deltaCBx=Cx-Bx;
    var deltaDCx=Dx-Cx;
    var deltaBAy=By-Ay;
    var deltaCBy=Cy-By;
    var deltaDCy=Dy-Cy;
    var ax,ay,bx,by;
    var lastX=-10000;
    var lastY=-10000;
    var pts=[{x:Ax,y:Ay}];
    for(var i=1;i<ptCount;i++){
        var t=i/ptCount;
        ax=Ax+deltaBAx*t;
        bx=Bx+deltaCBx*t;
        cx=Cx+deltaDCx*t;
        ax+=(bx-ax)*t;
        bx+=(cx-bx)*t;
        //
        ay=Ay+deltaBAy*t;
        by=By+deltaCBy*t;
        cy=Cy+deltaDCy*t;
        ay+=(by-ay)*t;
        by+=(cy-by)*t;
        var x=ax+(bx-ax)*t;
        var y=ay+(by-ay)*t;
        var dx=x-lastX;
        var dy=y-lastY;
        if(dx*dx+dy*dy>pxTolerance){
            pts.push({x:x,y:y});
            lastX=x;
            lastY=y;
        }
    }
    pts.push({x:Dx,y:Dy});
    return(pts);
}

// Return: an array of approximately evenly spaced points along a Quadratic curve
//
// Attribution: Stackoverflow's @Blindman67
// Cite: http://stackoverflow.com/questions/36637211/drawing-a-curved-line-in-css-or-canvas-and-moving-circle-along-it/36827074#36827074
// As modified from the above citation
//
// ptCount: sample this many points at interval along the curve
// pxTolerance: approximate spacing allowed between points
// Ax,Ay,Bx,By,Cx,Cy: control points defining the curve
//
function plotQBez(ptCount,pxTolerance,Ax,Ay,Bx,By,Cx,Cy){
    var deltaBAx=Bx-Ax;
    var deltaCBx=Cx-Bx;
    var deltaBAy=By-Ay;
    var deltaCBy=Cy-By;
    var ax,ay;
    var lastX=-10000;
    var lastY=-10000;
    var pts=[{x:Ax,y:Ay}];
    for(var i=1;i<ptCount;i++){
        var t=i/ptCount;
        ax=Ax+deltaBAx*t;
        ay=Ay+deltaBAy*t;
        var x=ax+((Bx+deltaCBx*t)-ax)*t;
        var y=ay+((By+deltaCBy*t)-ay)*t;
        var dx=x-lastX;
        var dy=y-lastY;
        if(dx*dx+dy*dy>pxTolerance){
            pts.push({x:x,y:y});
            lastX=x;
            lastY=y;
        }
    }
    pts.push({x:Cx,y:Cy});
    return(pts);
}

// Return: an array of approximately evenly spaced points along a line
//
// pxTolerance: approximate spacing allowed between points
// Ax,Ay,Bx,By: end points defining the line
//
function plotLine(pxTolerance,Ax,Ay,Bx,By){
    var dx=Bx-Ax;
    var dy=By-Ay;
    var ptCount=parseInt(Math.sqrt(dx*dx+dy*dy))*3;
    var lastX=-10000;
    var lastY=-10000;
    var pts=[{x:Ax,y:Ay}];
    for(var i=1;i<=ptCount;i++){
        var t=i/ptCount;
        var x=Ax+dx*t;
        var y=Ay+dy*t;
        var dx1=x-lastX;
        var dy1=y-lastY;
        if(dx1*dx1+dy1*dy1>pxTolerance){
            pts.push({x:x,y:y});
            lastX=x;
            lastY=y;
        }
    }
    pts.push({x:Bx,y:By});
    return(pts);
}

```



## Length of a Quadratic Curve


Given the 3 points of a quadratic curve the following function returns the length.

```html
function quadraticBezierLength(x1,y1,x2,y2,x3,y3)
    var a, e, c, d, u, a1, e1, c1, d1, u1, v1x, v1y;

    v1x = x2 * 2;
    v1y = y2 * 2;
    d = x1 - v1x + x3;
    d1 = y1 - v1y + y3;
    e = v1x - 2 * x1;
    e1 = v1y - 2 * y1;
    c1 = (a = 4 * (d * d + d1 * d1));
    c1 += (b = 4 * (d * e + d1 * e1));
    c1 += (c = e * e + e1 * e1);
    c1 = 2 * Math.sqrt(c1);
    a1 = 2 * a * (u = Math.sqrt(a));
    u1 = b / u;
    a = 4 * c * a - b * b;
    c = 2 * Math.sqrt(c);
    return (a1 * c1 + u * b * (c1 - c) + a * Math.log((2 * u + u1 + c1) / (u1 + c))) / (4 * a1);
} 

```

Derived from the quadratic bezier function F(t) = a * (1 - t)<sup>2</sup> + 2 * b * (1 - t) * t + c * t<sup>2</sup>



## Split bezier curves at position


This example splits cubic and bezier curves in two.

The function `splitCurveAt` splits the curve at `position` where `0.0` = start, `0.5` = middle, and `1` = end. It can split quadratic and cubic curves. The curve type is determined by the last x argument `x4`. If not `undefined` or `null` then it assumes the curve is cubic else the curve is a quadratic

### Example usage

Splitting quadratic bezier curve in two

```html
var p1 = {x : 10 , y : 100};
var p2 = {x : 100, y : 200};
var p3 = {x : 200, y : 0};
var newCurves = splitCurveAt(0.5, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y)

var i = 0;
var p = newCurves
// Draw the 2 new curves
// Assumes ctx is canvas 2d context
ctx.lineWidth = 1;
ctx.strokeStyle = "black";
ctx.beginPath();
ctx.moveTo(p[i++],p[i++]);
ctx.quadraticCurveTo(p[i++], p[i++], p[i++], p[i++]);
ctx.quadraticCurveTo(p[i++], p[i++], p[i++], p[i++]);
ctx.stroke();

```

Splitting cubic bezier curve in two

```html
var p1 = {x : 10 , y : 100};
var p2 = {x : 100, y : 200};
var p3 = {x : 200, y : 0};
var p4 = {x : 300, y : 100};
var newCurves = splitCurveAt(0.5, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y)

var i = 0;
var p = newCurves
// Draw the 2 new curves
// Assumes ctx is canvas 2d context
ctx.lineWidth = 1;
ctx.strokeStyle = "black";
ctx.beginPath();
ctx.moveTo(p[i++],p[i++]);
ctx.bezierCurveTo(p[i++], p[i++], p[i++], p[i++], p[i++], p[i++]);
ctx.bezierCurveTo(p[i++], p[i++], p[i++], p[i++], p[i++], p[i++]);
ctx.stroke();

```

### The split function

**splitCurveAt = function(position, x1, y1, x2, y2, x3, y3, [x4, y4])**

> 
**Note:** Arguments inside [x4, y4] are optional.


> 
**Note:** The function has some optional commented `/* */` code that deals with edge cases where the resulting curves may have zero length, or fall outside the start or ends of the original curve. As is attempting to split a curve outside the valid range for `position >= 0` or `position >= 1` will throw a range error. This can be removed and will work just fine, though you may have resulting curves that have zero length.


```html
// With throw RangeError if not 0 < position < 1
// x1, y1, x2, y2, x3, y3 for quadratic curves
// x1, y1, x2, y2, x3, y3, x4, y4 for cubic curves
// Returns an array of points representing 2 curves. The curves are the same type as the split curve
var splitCurveAt = function(position, x1, y1, x2, y2, x3, y3, x4, y4){
    var v1, v2, v3, v4, quad, retPoints, i, c;
    
    // =============================================================================================
    // you may remove this as the function will still work and resulting curves will still render
    // but other curve functions may not like curves with 0 length
    // =============================================================================================
    if(position <= 0 || position >= 1){
        throw RangeError("spliteCurveAt requires position > 0 && position < 1");
    }

    // =============================================================================================
    // If you remove the above range error you may use one or both of the following commented sections
    // Splitting curves position < 0 or position > 1 will still create valid curves but they will 
    // extend past the end points
    
    // =============================================================================================
    // Lock the position to split on the curve. 
    /* optional A
    position = position < 0 ? 0 : position > 1 ? 1 : position;
    optional A end */
    
    // =============================================================================================
    // the next commented section will return the original curve if the split results in 0 length curve
    // You may wish to uncomment this If you desire such functionality
    /*  optional B
    if(position <= 0 || position >= 1){
        if(x4 === undefined || x4 === null){
            return [x1, y1, x2, y2, x3, y3];
        }else{
            return [x1, y1, x2, y2, x3, y3, x4, y4];
        }
    }
    optional B end */
    
    
    retPoints = []; // array of coordinates
    i = 0;
    quad = false;  // presume cubic bezier
    v1 = {};
    v2 = {};
    v4 = {};
    v1.x = x1;
    v1.y = y1;
    v2.x = x2;
    v2.y = y2;
    if(x4 === undefined || x4 === null){
        quad = true;  // this is a quadratic bezier
        v4.x = x3;
        v4.y = y3;
    }else{
        v3 = {};
        v3.x = x3;
        v3.y = y3;
        v4.x = x4;
        v4.y = y4;
    }
    c = position;
    retPoints[i++] = v1.x;  // start point 
    retPoints[i++] = v1.y;

    if(quad){ // split quadratic bezier
        retPoints[i++] = (v1.x += (v2.x - v1.x) * c);  // new control point for first curve
        retPoints[i++] = (v1.y += (v2.y - v1.y) * c);
        v2.x += (v4.x - v2.x) * c;
        v2.y += (v4.y - v2.y) * c;
        retPoints[i++] = v1.x + (v2.x - v1.x) * c;  // new end and start of first and second curves
        retPoints[i++] = v1.y + (v2.y - v1.y) * c;
        retPoints[i++] = v2.x;  // new control point for second curve
        retPoints[i++] = v2.y;
        retPoints[i++] = v4.x;  // new endpoint of second curve
        retPoints[i++] = v4.y;
        //=======================================================
        // return array with 2 curves
        return retPoints;
    }
    retPoints[i++] = (v1.x += (v2.x - v1.x) * c); // first curve first control point                
    retPoints[i++] = (v1.y += (v2.y - v1.y) * c);
    v2.x += (v3.x - v2.x) * c;
    v2.y += (v3.y - v2.y) * c;
    v3.x += (v4.x - v3.x) * c;
    v3.y += (v4.y - v3.y) * c;
    retPoints[i++] = (v1.x += (v2.x - v1.x) * c); // first curve second control point
    retPoints[i++] = (v1.y += (v2.y - v1.y) * c);
    v2.x += (v3.x - v2.x) * c;
    v2.y += (v3.y - v2.y) * c;
    retPoints[i++] = v1.x + (v2.x - v1.x) * c; // end and start point of first second curves
    retPoints[i++] = v1.y + (v2.y - v1.y) * c;
    retPoints[i++] = v2.x;  // second curve first control point
    retPoints[i++] = v2.y;
    retPoints[i++] = v3.x;  // second curve second control point
    retPoints[i++] = v3.y;
    retPoints[i++] = v4.x;  // endpoint of second curve
    retPoints[i++] = v4.y;
    //=======================================================
    // return array with 2 curves
    return retPoints;              
}

```



## Trim bezier curve.


This example show you how to trim a bezier.

The function trimBezier trims the ends off of the curve returning the curve `fromPos` to `toPos`. `fromPos` and `toPos` are in the range 0 to 1 inclusive, It can trim quadratic and cubic curves. The curve type is determined by the last x argument `x4`. If not `undefined` or `null` then it assumes the curve is cubic else the curve is a quadratic

The trimmed curve is returned as an array of points. 6 points for quadratic curves and 8 for cubic curves.

### Example Usage

Trimming a quadratic curve.

```html
var p1 = {x : 10 , y : 100};
var p2 = {x : 100, y : 200};
var p3 = {x : 200, y : 0};
var newCurve = splitCurveAt(0.25, 0.75, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y)

var i = 0;
var p = newCurve
// Draw the trimmed curve
// Assumes ctx is canvas 2d context
ctx.lineWidth = 1;
ctx.strokeStyle = "black";
ctx.beginPath();
ctx.moveTo(p[i++],p[i++]);
ctx.quadraticCurveTo(p[i++], p[i++], p[i++], p[i++]);
ctx.stroke();

```

Trimming a cubic curve.

```html
var p1 = {x : 10 , y : 100};
var p2 = {x : 100, y : 200};
var p3 = {x : 200, y : 0};
var p4 = {x : 300, y : 100};
var newCurve = splitCurveAt(0.25, 0.75, p1.x, p1.y, p2.x, p2.y, p3.x, p3.y, p4.x, p4.y)

var i = 0;
var p = newCurve
// Draw the trimmed curve
// Assumes ctx is canvas 2d context
ctx.lineWidth = 1;
ctx.strokeStyle = "black";
ctx.beginPath();
ctx.moveTo(p[i++],p[i++]);
ctx.bezierCurveTo(p[i++], p[i++], p[i++], p[i++], p[i++], p[i++]);
ctx.stroke();

```

### Example Function

**trimBezier = function(fromPos, toPos, x1, y1, x2, y2, x3, y3, [x4, y4])**

> 
**Note:** Arguments inside [x4, y4] are optional.


> 
**Note:** This function requires the function in the example Split Bezier Curves At in this section


```html
var trimBezier = function(fromPos, toPos, x1, y1, x2, y2, x3, y3, x4, y4){
    var quad, i, s, retBez;
    quad = false;
    if(x4 === undefined || x4 === null){
        quad = true;  // this is a quadratic bezier    
    }
    if(fromPos > toPos){ // swap is from is after to
        i = fromPos;
        fromPos = toPos
        toPos = i;
    }
    // clamp to on the curve
    toPos = toPos <= 0 ? 0 : toPos >= 1 ? 1 : toPos;
    fromPos = fromPos <= 0 ? 0 : fromPos >= 1 ? 1 : fromPos;
    if(toPos === fromPos){
        s = splitBezierAt(toPos, x1, y1, x2, y2, x3, y3, x4, y4);
        i = quad ? 4 : 6;
        retBez = [s[i], s[i+1], s[i], s[i+1], s[i], s[i+1]];
        if(!quad){
            retBez.push(s[i], s[i+1]);
        }
        return retBez;
    }
    if(toPos === 1 && fromPos === 0){       // no trimming required
        retBez = [x1, y1, x2, y2, x3, y3];  // return original bezier
        if(!quad){
            retBez.push(x4, y4);
        }
        return retBez;
    }
    if(fromPos === 0){
        if(toPos < 1){
            s = splitBezierAt(toPos, x1, y1, x2, y2, x3, y3, x4, y4);
            i = 0;
            retBez = [s[i++], s[i++], s[i++], s[i++], s[i++], s[i++]];
            if(!quad){
                retBez.push(s[i++], s[i++]);
            }
        }
        return retBez;
    }
    if(toPos === 1){
        if(fromPos < 1){
            s = splitBezierAt(toPos, x1, y1, x2, y2, x3, y3, x4, y4);
            i = quad ? 4 : 6;
            retBez = [s[i++], s[i++], s[i++], s[i++], s[i++], s[i++]];
            if(!quad){
                retBez.push(s[i++], s[i++]);
            }
        }
        return retBez;
    }
    s = splitBezierAt(fromPos, x1, y1, x2, y2, x3, y3, x4, y4);
    if(quad){
        i = 4;
        toPos = (toPos - fromPos) / (1 - fromPos);
        s = splitBezierAt(toPos, s[i++], s[i++], s[i++], s[i++], s[i++], s[i++]);
        i = 0;
        retBez = [s[i++], s[i++], s[i++], s[i++], s[i++], s[i++]];
        return retBez;
        
    }
    i = 6;
    toPos = (toPos - fromPos) / (1 - fromPos);
    s = splitBezierAt(toPos, s[i++], s[i++], s[i++], s[i++], s[i++], s[i++], s[i++], s[i++]);
    i = 0;
    retBez = [s[i++], s[i++], s[i++], s[i++], s[i++], s[i++], s[i++], s[i++]];
    return retBez;
}

```



## Length of a Cubic Bezier Curve (a close approximation)


Given the 4 points of a cubic Bezier curve the following function returns its length.

**Method:** The length of a cubic Bezier curve does not have a direct mathematical calculation. This "brute force" method finds a sampling of points along the curve and calculates the total distance spanned by those points.

**Accuracy:** The approximate length is 99+% accurate using the default sampling size of 40.

```html
// Return: Close approximation of the length of a Cubic Bezier curve
//
// Ax,Ay,Bx,By,Cx,Cy,Dx,Dy: the 4 control points of the curve
// sampleCount [optional, default=40]: how many intervals to calculate
// Requires: cubicQxy (included below)
//
function cubicBezierLength(Ax,Ay,Bx,By,Cx,Cy,Dx,Dy,sampleCount){
    var ptCount=sampleCount||40;
    var totDist=0;
    var lastX=Ax;
    var lastY=Ay;
    var dx,dy;
    for(var i=1;i<ptCount;i++){
        var pt=cubicQxy(i/ptCount,Ax,Ay,Bx,By,Cx,Cy,Dx,Dy);
        dx=pt.x-lastX;
        dy=pt.y-lastY;
        totDist+=Math.sqrt(dx*dx+dy*dy);
        lastX=pt.x;
        lastY=pt.y;
    }
    dx=Dx-lastX;
    dy=Dy-lastY;
    totDist+=Math.sqrt(dx*dx+dy*dy);
    return(parseInt(totDist));
}


// Return: an [x,y] point along a cubic Bezier curve at interval T
//
// Attribution: Stackoverflow's @Blindman67
// Cite: http://stackoverflow.com/questions/36637211/drawing-a-curved-line-in-css-or-canvas-and-moving-circle-along-it/36827074#36827074
// As modified from the above citation
// 
// t: an interval along the curve (0<=t<=1)
// ax,ay,bx,by,cx,cy,dx,dy: control points defining the curve
//
function cubicQxy(t,ax,ay,bx,by,cx,cy,dx,dy) {
    ax += (bx - ax) * t;
    bx += (cx - bx) * t;
    cx += (dx - cx) * t;
    ax += (bx - ax) * t;
    bx += (cx - bx) * t;
    ay += (by - ay) * t;
    by += (cy - by) * t;
    cy += (dy - cy) * t;
    ay += (by - ay) * t;
    by += (cy - by) * t;
    return({
        x:ax +(bx - ax) * t,
        y:ay +(by - ay) * t     
    });
}

```



## Finding extent of Quadratic Curve


When you need to find the bounding rectangle of a quadratic bezier curve you can use the following performant method.

```html
// This method was discovered by Blindman67 and solves by first normalising the control point thereby reducing the algorithm complexity 
// x1,y1, x2,y2, x3,y3 Start, Control, and End coords of bezier
// [extent] is optional and if provided the extent will be added to it allowing you to use the function
//        to get the extent of many beziers.
// returns extent object (if not supplied a new extent is created)
// Extent object properties
// top, left,right,bottom,width,height
function getQuadraticCurevExtent(x1, y1, x2, y2, x3, y3, extent) {
    var brx, bx, x, bry, by, y, px, py;

    // solve quadratic for bounds by BM67 normalizing equation
    brx = x3 - x1; // get x range
    bx = x2 - x1; // get x control point offset
    x = bx / brx; // normalise control point which is used to check if maxima is in range

    // do the same for the y points
    bry = y3 - y1;
    by = y2 - y1;
    y = by / bry;

    px = x1; // set defaults in case maximas outside range
    py = y1;

    // find top/left, top/right, bottom/left, or bottom/right
    if (x < 0 || x > 1) { // check if x maxima is on the curve
        px = bx * bx / (2 * bx - brx) + x1; // get the x maxima
    }
    if (y < 0 || y > 1) { // same as x
        py = by * by / (2 * by - bry) + y1;
    }

    // create extent object and add extent
    if (extent === undefined) {
        extent = {};
        extent.left = Math.min(x1, x3, px);
        extent.top = Math.min(y1, y3, py);
        extent.right = Math.max(x1, x3, px);
        extent.bottom = Math.max(y1, y3, py);
    } else { // use spplied extent and extend it to fit this curve
        extent.left = Math.min(x1, x3, px, extent.left);
        extent.top = Math.min(y1, y3, py, extent.top);
        extent.right = Math.max(x1, x3, px, extent.right);
        extent.bottom = Math.max(y1, y3, py, extent.bottom);
    }

    extent.width = extent.right - extent.left;
    extent.height = extent.bottom - extent.top;
    return extent;
}

```

For a more detailed look at solving for extent see answer [To get extent of a quadratic bezier](http://stackoverflow.com/a/39396368/3877726) which includes runnable demos.

