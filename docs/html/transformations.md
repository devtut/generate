---
metaTitle: "HTML - Transformations"
description: "Drawing many translated, scaled, and rotated  images quickly, Rotate an Image or Path around it's centerpoint, Introduction to Transformations, A Transformation Matrix to track translated, rotated & scaled shape(s)"
---

# Transformations



## Drawing many translated, scaled, and rotated  images quickly


There are many situation where you want to draw an image that is rotated, scaled, and translated. The rotation should occur around the center of the image. This is the quickest way to do so on the 2D canvas. These functions a well suited to 2D games where the expectation is to render a few hundred even up to a 1000+ images every 60th of a second. (dependent on the hardware)

```html
// assumes that the canvas context is in ctx and in scope
function drawImageRST(image, x, y, scale, rotation){
    ctx.setTransform(scale, 0, 0, scale, x, y); // set the scale and translation
    ctx.rotate(rotation);                       // add the rotation
    ctx.drawImage(image, -image.width / 2, -image.height / 2); // draw the image offset by half its width and height
}

```

A variant can also include the alpha value which is useful for particle systems.

```html
function drawImageRST_Alpha(image, x, y, scale, rotation, alpha){
    ctx.setTransform(scale, 0, 0, scale, x, y); // set the scale and translation
    ctx.rotate(rotation);                       // add the rotation
    ctx.globalAlpha = alpha;
    ctx.drawImage(image, -image.width / 2, -image.height / 2); // draw the image offset by half its width and height
}

```

It is important to note that both functions leave the canvas context in a random state. Though the functions will not be affected other rendering my be. When you are done rendering images you may need to restore the default transform

```html
ctx.setTransform(1, 0, 0, 1, 0, 0); // set the context transform back to the default 

```

If you use the alpha version (second example) and then the standard version you will have to ensure that the global alpha state is restored

```html
ctx.globalAlpha = 1;

```

An example of using the above functions to render some particles and the a few images

```html
// assume particles to contain an array of particles
for(var i = 0; i < particles.length; i++){
    var p = particles[i];
    drawImageRST_Alpha(p.image, p.x, p.y, p.scale, p.rot, p.alpha);
    // no need to rest the alpha in the loop
}
// you need to reset the alpha as it can be any value 
ctx.globalAlpha = 1;

drawImageRST(myImage, 100, 100, 1, 0.5);  // draw an image at 100,100
// no need to reset the transform 
drawImageRST(myImage, 200, 200, 1, -0.5); // draw an image at 200,200 
ctx.setTransform(1,0,0,1,0,0);            // reset the transform

```



## Rotate an Image or Path around it's centerpoint


[<img src="http://i.stack.imgur.com/nBLvb.png" alt="enter image description here" />](http://i.stack.imgur.com/nBLvb.png)

Steps#1-5 below allow any image or path-shape to be both moved anywhere on the canvas and rotated to any angle without changing any of the image/path-shape's original point coordinates.

<li>
Move the canvas [0,0] origin to the shape's center point

```html
context.translate( shapeCenterX, shapeCenterY );

```


</li>
<li>
Rotate the canvas by the desired angle (in radians)

```html
context.rotate( radianAngle );

```


</li>
<li>
Move the canvas origin back to the top-left corner

```html
 context.translate( -shapeCenterX, -shapeCenterY );

```


</li>
<li>
Draw the image or path-shape using it's original coordinates.

```html
 context.fillRect( shapeX, shapeY, shapeWidth, shapeHeight );

```


</li>
<li>
Always clean up! Set the transformation state back to where it was before #1
</li>

<li>
**Step#5, Option#1:** Undo every transformation in the reverse order

```html
   // undo #3
   context.translate( shapeCenterX, shapeCenterY );
   // undo #2
   context.rotate( -radianAngle );
   // undo #1
   context.translate( -shapeCenterX, shapeCenterY );

```


</li>
<li>
**Step#5, Option#2:** If the canvas was in an untransformed state (the default) before beginning step#1, you can undo the effects of steps#1-3 by resetting all transformations to their default state

```html
   // set transformation to the default state (==no transformation applied)
   context.setTransform(1,0,0,1,0,0)

```


</li>

**Example code demo:**

```html
// canvas references & canvas styling
var canvas=document.createElement("canvas");
canvas.style.border='1px solid red';
document.body.appendChild(canvas);
canvas.width=378;
canvas.height=256;
var ctx=canvas.getContext("2d");
ctx.fillStyle='green';
ctx.globalAlpha=0.35;        

// define a rectangle to rotate
var rect={ x:100, y:100, width:175, height:50 };

// draw the rectangle unrotated
ctx.fillRect( rect.x, rect.y, rect.width, rect.height );

// draw the rectangle rotated by 45 degrees (==PI/4 radians)
ctx.translate( rect.x+rect.width/2, rect.y+rect.height/2 );
ctx.rotate( Math.PI/4 );
ctx.translate( -rect.x-rect.width/2, -rect.y-rect.height/2 );
ctx.fillRect( rect.x, rect.y, rect.width, rect.height );

```



## Introduction to Transformations


Transformations alter a given point's starting position by moving, rotating & scaling that point.

- **Translation:** Moves a point by a `distanceX` and `distanceY`.
- **Rotation:** Rotates a point by a `radian angle` around it's rotation point. The default rotation point in Html Canvas is the top-left origin [x=0,y=0] of the Canvas. But you can reposition the rotation point using translations.
- **Scaling:** Scales a point's position by a `scalingFactorX` and `scalingFactorY`  from it's scaling point. The default scaling point in Html Canvas is the top-left origin [x=0,y=0] of the Canvas. But you can reposition the scaling point using translations.

You can also do less common transformations, like shearing (skewing), by directly setting the transformation matrix of the canvas using `context.transform`.

**Translate (==move) a point with `context.translate(75,25)`**

[<img src="http://i.stack.imgur.com/h1jb3.png" alt="enter image description here" />](http://i.stack.imgur.com/h1jb3.png)

**Rotate a point with `context.rotate(Math.PI/8)`**

[<img src="http://i.stack.imgur.com/sf481.png" alt="enter image description here" />](http://i.stack.imgur.com/sf481.png)

**Scale a point with `context.scale(2,2)`**

[<img src="http://i.stack.imgur.com/e3Sho.png" alt="enter image description here" />](http://i.stack.imgur.com/e3Sho.png)

Canvas actually achieves transformations by altering the canvas' entire coordinate system.

- `context.translate` will move the canvas [0,0] origin from the top left corner to a new location.
- `context.rotate` will rotate the entire canvas coordinate system around the origin.
- `context.scale` will scale the entire canvas coordinate system around the origin. Think of this as increasing the size of every x,y on the canvas: `every x*=scaleX` and `every y*=scaleY`.

Canvas transformations are persistent. All New drawings will continue to be transformed until you reset the canvas' transformation back to it's default state (==totally untransformed). You can reset back to default with:

```html
// reset context transformations to the default (untransformed) state
context.setTransform(1,0,0,1,0,0);

```



## A Transformation Matrix to track translated, rotated & scaled shape(s)


Canvas allows you to `context.translate`, `context.rotate` and `context.scale` in order to draw your shape in the position & size you require.

Canvas itself uses a transformation matrix to efficiently track transformations.

- You can change Canvas's matrix with `context.transform`
- You can change Canvas's matrix with individual `translate, rotate & scale` commands
- You can completely overwrite Canvas's matrix with `context.setTransform`,
- **But you can't read Canvas's internal transformation matrix -- it's write-only.**

### **Why use a transformation matrix?**

A transformation matrix allows you to aggregate many individual translations, rotations & scalings into a single, easily reapplied matrix.

During complex animations you might apply dozens (or hundreds) of transformations to a shape. By using a transformation matrix you can (almost) instantly reapply those dozens of transformations with a single line of code.

Some Example uses:

<li>
**Test if the mouse is inside a shape that you have translated, rotated & scaled**
There is a built-in `context.isPointInPath` that tests if a point (eg the mouse) is inside a path-shape, but this built-in test is very slow compared to testing using a matrix.
Efficiently testing if the mouse is inside a shape involves taking the mouse position reported by the browser and transforming it in the same way that the shape was transformed. Then you can apply hit-testing as if the shape was not transformed.
</li>
<li>
**Redraw a shape that has been extensively translated, rotated & scaled.**
Instead of reapplying individual transformations with multiple `.translate, .rotate, .scale` you can apply all the aggregated transformations in a single line of code.
</li>
<li>
**Collision test shapes that have been translated, rotated & scaled**
You can use geometry & trigonometry to calculate the points that make up transformed shapes, but it's faster to use a transformation matrix to calculate those points.
</li>

### **A Transformation Matrix "Class"**

This code mirrors the native `context.translate`, `context.rotate`, `context.scale` transformation commands. Unlike the native canvas matrix, this matrix is readable and reusable.

Methods:

<li>
`translate`, `rotate`, `scale` mirror the context transformation commands and allow you to feed transformations into the matrix. The matrix efficiently holds the aggregated transformations.
</li>
<li>
`setContextTransform` takes a context and sets that context's matrix equal to this transformation matrix. This efficiently reapplies all transformations stored in this matrix to the context.
</li>
<li>
`resetContextTransform` resets the context's transformation to it's default state (==untransformed).
</li>
<li>
`getTransformedPoint` takes an untransformed coordinate point and converts it into a transformed point.
</li>
<li>
`getScreenPoint` takes a transformed coordinate point and converts it into an untransformed point.
</li>
<li>
`getMatrix` returns the aggregated transformations in the form of a matrix array.
</li>

**Code:**

```html
var TransformationMatrix=( function(){
    // private
    var self;
    var m=[1,0,0,1,0,0];
    var reset=function(){ var m=[1,0,0,1,0,0]; }
    var multiply=function(mat){
        var m0=m[0]*mat[0]+m[2]*mat[1];
        var m1=m[1]*mat[0]+m[3]*mat[1];
        var m2=m[0]*mat[2]+m[2]*mat[3];
        var m3=m[1]*mat[2]+m[3]*mat[3];
        var m4=m[0]*mat[4]+m[2]*mat[5]+m[4];
        var m5=m[1]*mat[4]+m[3]*mat[5]+m[5];
        m=[m0,m1,m2,m3,m4,m5];
    }
    var screenPoint=function(transformedX,transformedY){
        // invert
        var d =1/(m[0]*m[3]-m[1]*m[2]);
        im=[ m[3]*d, -m[1]*d, -m[2]*d, m[0]*d, d*(m[2]*m[5]-m[3]*m[4]), d*(m[1]*m[4]-m[0]*m[5]) ];
        // point
        return({
            x:transformedX*im[0]+transformedY*im[2]+im[4],
            y:transformedX*im[1]+transformedY*im[3]+im[5]
        });
    }
    var transformedPoint=function(screenX,screenY){
        return({
            x:screenX*m[0] + screenY*m[2] + m[4],
            y:screenX*m[1] + screenY*m[3] + m[5]
        });    
    }
    // public
    function TransformationMatrix(){
        self=this;
    }
    // shared methods
    TransformationMatrix.prototype.translate=function(x,y){
        var mat=[ 1, 0, 0, 1, x, y ];
        multiply(mat);
    };
    TransformationMatrix.prototype.rotate=function(rAngle){
        var c = Math.cos(rAngle);
        var s = Math.sin(rAngle);
        var mat=[ c, s, -s, c, 0, 0 ];    
        multiply(mat);
    };
    TransformationMatrix.prototype.scale=function(x,y){
        var mat=[ x, 0, 0, y, 0, 0 ];        
        multiply(mat);
    };
    TransformationMatrix.prototype.skew=function(radianX,radianY){
        var mat=[ 1, Math.tan(radianY), Math.tan(radianX), 1, 0, 0 ];
        multiply(mat);
    };
    TransformationMatrix.prototype.reset=function(){
        reset();
    }
    TransformationMatrix.prototype.setContextTransform=function(ctx){
        ctx.setTransform(m[0],m[1],m[2],m[3],m[4],m[5]);
    }
    TransformationMatrix.prototype.resetContextTransform=function(ctx){
        ctx.setTransform(1,0,0,1,0,0);
    }
    TransformationMatrix.prototype.getTransformedPoint=function(screenX,screenY){
        return(transformedPoint(screenX,screenY));
    }
    TransformationMatrix.prototype.getScreenPoint=function(transformedX,transformedY){
        return(screenPoint(transformedX,transformedY));
    }
    TransformationMatrix.prototype.getMatrix=function(){
        var clone=[m[0],m[1],m[2],m[3],m[4],m[5]];
        return(clone);
    }
    // return public
    return(TransformationMatrix);
})();

```

**Demo:**

This demo uses the Transformation Matrix "Class" above to:

<li>
Track (==save) a rectangle's transformation matrix.
</li>
<li>
Redraw the transformed rectangle without using context transformation commands.
</li>
<li>
Test if the mouse has clicked inside the transformed rectangle.
</li>

**Code:**

```html
<!doctype html>
<html>
<head>
<style>
    body{ background-color:white; }
    #canvas{border:1px solid red; }
</style>
<script>
window.onload=(function(){

    var canvas=document.getElementById("canvas");
    var ctx=canvas.getContext("2d");
    var cw=canvas.width;
    var ch=canvas.height;
    function reOffset(){
        var BB=canvas.getBoundingClientRect();
        offsetX=BB.left;
        offsetY=BB.top;        
    }
    var offsetX,offsetY;
    reOffset();
    window.onscroll=function(e){ reOffset(); }
    window.onresize=function(e){ reOffset(); }

    // Transformation Matrix "Class"
    
    var TransformationMatrix=( function(){
        // private
        var self;
        var m=[1,0,0,1,0,0];
        var reset=function(){ var m=[1,0,0,1,0,0]; }
        var multiply=function(mat){
            var m0=m[0]*mat[0]+m[2]*mat[1];
            var m1=m[1]*mat[0]+m[3]*mat[1];
            var m2=m[0]*mat[2]+m[2]*mat[3];
            var m3=m[1]*mat[2]+m[3]*mat[3];
            var m4=m[0]*mat[4]+m[2]*mat[5]+m[4];
            var m5=m[1]*mat[4]+m[3]*mat[5]+m[5];
            m=[m0,m1,m2,m3,m4,m5];
        }
        var screenPoint=function(transformedX,transformedY){
            // invert
            var d =1/(m[0]*m[3]-m[1]*m[2]);
            im=[ m[3]*d, -m[1]*d, -m[2]*d, m[0]*d, d*(m[2]*m[5]-m[3]*m[4]), d*(m[1]*m[4]-m[0]*m[5]) ];
            // point
            return({
                x:transformedX*im[0]+transformedY*im[2]+im[4],
                y:transformedX*im[1]+transformedY*im[3]+im[5]
            });
        }
        var transformedPoint=function(screenX,screenY){
            return({
                x:screenX*m[0] + screenY*m[2] + m[4],
                y:screenX*m[1] + screenY*m[3] + m[5]
            });    
        }
        // public
        function TransformationMatrix(){
            self=this;
        }
        // shared methods
        TransformationMatrix.prototype.translate=function(x,y){
            var mat=[ 1, 0, 0, 1, x, y ];
            multiply(mat);
        };
        TransformationMatrix.prototype.rotate=function(rAngle){
            var c = Math.cos(rAngle);
            var s = Math.sin(rAngle);
            var mat=[ c, s, -s, c, 0, 0 ];    
            multiply(mat);
        };
        TransformationMatrix.prototype.scale=function(x,y){
            var mat=[ x, 0, 0, y, 0, 0 ];        
            multiply(mat);
        };
        TransformationMatrix.prototype.skew=function(radianX,radianY){
            var mat=[ 1, Math.tan(radianY), Math.tan(radianX), 1, 0, 0 ];
            multiply(mat);
        };
        TransformationMatrix.prototype.reset=function(){
            reset();
        }
        TransformationMatrix.prototype.setContextTransform=function(ctx){
            ctx.setTransform(m[0],m[1],m[2],m[3],m[4],m[5]);
        }
        TransformationMatrix.prototype.resetContextTransform=function(ctx){
            ctx.setTransform(1,0,0,1,0,0);
        }
        TransformationMatrix.prototype.getTransformedPoint=function(screenX,screenY){
            return(transformedPoint(screenX,screenY));
        }
        TransformationMatrix.prototype.getScreenPoint=function(transformedX,transformedY){
            return(screenPoint(transformedX,transformedY));
        }
        TransformationMatrix.prototype.getMatrix=function(){
            var clone=[m[0],m[1],m[2],m[3],m[4],m[5]];
            return(clone);
        }
        // return public
        return(TransformationMatrix);
    })();

    // DEMO starts here

    // create a rect and add a transformation matrix
    // to track it's translations, rotations & scalings
    var rect={x:30,y:30,w:50,h:35,matrix:new TransformationMatrix()};

    // draw the untransformed rect in black
    ctx.strokeRect(rect.x, rect.y, rect.w, rect.h);
    // Demo: label
    ctx.font='11px arial';
    ctx.fillText('Untransformed Rect',rect.x,rect.y-10);

    // transform the canvas & draw the transformed rect in red
    ctx.translate(100,0);
    ctx.scale(2,2);
    ctx.rotate(Math.PI/12);
    // draw the transformed rect
    ctx.strokeStyle='red';
    ctx.strokeRect(rect.x, rect.y, rect.w, rect.h);
    ctx.font='6px arial';
    // Demo: label
    ctx.fillText('Same Rect: Translated, rotated & scaled',rect.x,rect.y-6);
    // reset the context to untransformed state
    ctx.setTransform(1,0,0,1,0,0);

    // record the transformations in the matrix
    var m=rect.matrix;
    m.translate(100,0);
    m.scale(2,2);
    m.rotate(Math.PI/12);

    // use the rect's saved transformation matrix to reposition, 
    //     resize & redraw the rect
    ctx.strokeStyle='blue';
    drawTransformedRect(rect);

    // Demo: instructions
    ctx.font='14px arial';
    ctx.fillText('Demo: click inside the blue rect',30,200);

    // redraw a rect based on it's saved transformation matrix
    function drawTransformedRect(r){
        // set the context transformation matrix using the rect's saved matrix
        m.setContextTransform(ctx);
        // draw the rect (no position or size changes needed!)
        ctx.strokeRect( r.x, r.y, r.w, r.h );
        // reset the context transformation to default (==untransformed);
        m.resetContextTransform(ctx);
    }

    // is the point in the transformed rectangle?
    function isPointInTransformedRect(r,transformedX,transformedY){
        var p=r.matrix.getScreenPoint(transformedX,transformedY);
        var x=p.x;
        var y=p.y;
        return(x>r.x && x<r.x+r.w && y>r.y && y<r.y+r.h);
    } 

    // listen for mousedown events
    canvas.onmousedown=handleMouseDown;
    function handleMouseDown(e){
        // tell the browser we're handling this event
        e.preventDefault();
        e.stopPropagation();
        // get mouse position
        mouseX=parseInt(e.clientX-offsetX);
        mouseY=parseInt(e.clientY-offsetY);
        // is the mouse inside the transformed rect?
        if(isPointInTransformedRect(rect,mouseX,mouseY)){
            alert('You clicked in the transformed Rect');
        }
    }

    // Demo: redraw transformed rect without using
    //       context transformation commands
    function drawTransformedRect(r,color){
        var m=r.matrix;
        var tl=m.getTransformedPoint(r.x,r.y);
        var tr=m.getTransformedPoint(r.x+r.w,r.y);
        var br=m.getTransformedPoint(r.x+r.w,r.y+r.h);
        var bl=m.getTransformedPoint(r.x,r.y+r.h);
        ctx.beginPath();
        ctx.moveTo(tl.x,tl.y);
        ctx.lineTo(tr.x,tr.y);
        ctx.lineTo(br.x,br.y);
        ctx.lineTo(bl.x,bl.y);
        ctx.closePath();
        ctx.strokeStyle=color;
        ctx.stroke();
    }

}); // end window.onload
</script>
</head>
<body>
    <canvas id="canvas" width=512 height=250></canvas>
</body>
</html>

```

