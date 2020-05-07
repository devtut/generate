---
metaTitle: "HTML - Animation"
description: "Use requestAnimationFrame() NOT setInterval() for animation loops, Animate an image across the Canvas, Simple animation with 2D context and requestAnimationFrame, Animate at a specified interval (add a new rectangle every 1 second), Animate at a specified time (an animated clock), Don't draw animations in your event handlers (a simple sketch app), Easing using Robert Penners equations, Set frame rate using requestAnimationFrame, Animate from [x0,y0] to [x1,y1]"
---

# Animation



## Use requestAnimationFrame() NOT setInterval() for animation loops


`requestAnimationFrame` is similar to setInterval, it but has these important improvements:

<li>
The animation code is synchronized with display refreshes for efficiency The clear + redraw code is scheduled, but not immediately executed. The browser will execute the clear + redraw code only when the display is ready to refresh. This synchronization with the refresh cycle increases your animation performance by giving your code the most available time in which to complete.
</li>
<li>
Every loop is always completed before another loop is allowed to start. This prevents "tearing", where the user sees an incomplete version of the drawing. The eye particularly notices tearing and is distracted when tearing occurs. So preventing tearing makes your animation appear smoother and more consistent.
</li>

- Animation automatically stops when the user switches to a different browser tab. This saves power on mobile devices because the device is not wasting power computing an animation that the user can't currently see.

Device displays will refresh about 60 times per second so requestAnimationFrame can continuously redraw at about 60 "frames" per second. The eye sees motion at 20-30 frames per second so requestAnimationFrame can easily create the illusion of motion.

Notice that requestAnimationFrame is recalled at the end of each animateCircle. This is because each 'requestAnimatonFrameonly requests a single execution of the animation function.

**Example: simple `requestAnimationFrame**

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

    // canvas related variables
    var canvas=document.getElementById("canvas");
    var ctx=canvas.getContext("2d");
    var cw=canvas.width;
    var ch=canvas.height;
           
    // start the animation
    requestAnimationFrame(animate);

    function animate(currentTime){

        // draw a full randomly circle
        var x=Math.random()*canvas.width;
        var y=Math.random()*canvas.height;
        var radius=10+Math.random()*15;
        ctx.beginPath();
        ctx.arc(x,y,radius,0,Math.PI*2);
        ctx.fillStyle='#'+Math.floor(Math.random()*16777215).toString(16);
        ctx.fill();

        // request another loop of animation
        requestAnimationFrame(animate);
    }

}); // end $(function(){});
</script>
</head>
<body>
    <canvas id="canvas" width=512 height=512></canvas>
</body>
</html>

```

To illustrate the advantages of requestAnimationFrame this [stackoverflow question has a live demo](http://stackoverflow.com/a/38709924/3877726)



## Animate an image across the Canvas


This example loads and animates and image across the Canvas

**Important Hint!** Make sure you give your image time to fully load by using `image.onload`.

**Annotated Code**

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

    // canvas related variables
    var canvas=document.getElementById("canvas");
    var ctx=canvas.getContext("2d");
    var cw=canvas.width;
    var ch=canvas.height;

    // animation related variables
    var minX=20;        // Keep the image animating 
    var maxX=250;       // between minX & maxX
    var x=minX;         // The current X-coordinate
    var speedX=1;       // The image will move at 1px per loop 
    var direction=1;    // The image direction: 1==righward, -1==leftward
    var y=20;           // The Y-coordinate

    // Load a new image
    // IMPORTANT!!! You must give the image time to load by using img.onload!
    var img=new Image();
    img.onload=start;
    img.src="https://dl.dropboxusercontent.com/u/139992952/stackoverflow/sun.png";
    function start(){
        // the image is fully loaded sostart animating
        requestAnimationFrame(animate);
    }

    function animate(time){

        // clear the canvas
        ctx.clearRect(0,0,cw,ch);

        // draw
        ctx.drawImage(img,x,y);

        // update
        x += speedX * direction;
        // keep "x" inside min & max
        if(x<minX){ x=minX; direction*=-1; }
        if(x>maxX){ x=maxX; direction*=-1; }

        // request another loop of animation
        requestAnimationFrame(animate);
    }

}); // end $(function(){});
</script>
</head>
<body>
    <canvas id="canvas" width=512 height=512></canvas>
</body>
</html>

```



## Simple animation with 2D context and requestAnimationFrame


This example will show you how to create a simple animation using the canvas and the 2D context. It is assumed you know how to create and add a canvas to the DOM and obtain the context

```js
// this example assumes ctx and canvas have been created
const textToDisplay = "This is an example that uses the canvas to animate some text.";
const textStyle     = "white";
const BGStyle       = "black";  // background style
const textSpeed     = 0.2;      // in pixels per millisecond
const textHorMargin = 8;        // have the text a little outside the canvas 


ctx.font = Math.floor(canvas.height * 0.8) + "px arial"; // size the font to 80% of canvas height
var textWidth     = ctx.measureText(textToDisplay).width; // get the text width
var totalTextSize = (canvas.width + textHorMargin * 2 + textWidth);
ctx.textBaseline  = "middle";           // not put the text in the vertical center
ctx.textAlign     = "left";             // align to the left
var textX         = canvas.width + 8;   // start with the text off screen to the right
var textOffset    = 0;                  // how far the text has moved

var startTime;
// this function is call once a frame which is approx 16.66 ms (60fps)
function update(time){              // time is passed by requestAnimationFrame
    if(startTime === undefined){    // get a reference for the start time if this is the first frame
        startTime = time;
    }
    ctx.fillStyle = BGStyle;
    ctx.fillRect(0, 0, canvas.width, canvas.height);                    // clear the canvas by drawing over it
    textOffset    = ((time - startTime) * textSpeed) % (totalTextSize); // move the text left 
    ctx.fillStyle = textStyle;                                          // set the text style
    ctx.fillText(textToDisplay, textX - textOffset, canvas.height / 2); // render the text

    requestAnimationFrame(update);// all done request the next frame
}
requestAnimationFrame(update);// to start request the first frame

```

[A demo of this example](https://jsfiddle.net/nzromo1n/) at jsfiddle



## Animate at a specified interval (add a new rectangle every 1 second)


This example adds a new rectangle to the canvas every 1 second (== a 1 second interval)

**Annotated Code:**

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

    // canvas related variables
    var canvas=document.getElementById("canvas");
    var ctx=canvas.getContext("2d");
    var cw=canvas.width;
    var ch=canvas.height;

    // animation interval variables
    var nextTime=0;      // the next animation begins at "nextTime"
    var duration=1000;   // run animation every 1000ms

    var x=20;            // the X where the next rect is drawn
    
    // start the animation
    requestAnimationFrame(animate);

    function animate(currentTime){

        // wait for nextTime to occur
        if(currentTime<nextTime){
            // request another loop of animation
            requestAnimationFrame(animate);
            // time hasn't elapsed so just return
            return;
        }
        // set nextTime
        nextTime=currentTime+duration;

        // add another rectangle every 1000ms
        ctx.fillStyle='#'+Math.floor(Math.random()*16777215).toString(16);
        ctx.fillRect(x,30,30,30);

        // update X position for next rectangle
        x+=30;

        // request another loop of animation
        requestAnimationFrame(animate);
    }

}); // end $(function(){});
</script>
</head>
<body>
    <canvas id="canvas" width=512 height=512></canvas>
</body>
</html>

```



## Animate at a specified time (an animated clock)


This example animates a clock showing the seconds as a filled wedge

**Annotated Code:**

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

    // canvas related variables
    var canvas=document.getElementById("canvas");
    var ctx=canvas.getContext("2d");
    var cw=canvas.width;
    var ch=canvas.height;
    // canvas styling for the clock
    ctx.strokeStyle='lightgray';
    ctx.fillStyle='skyblue';
    ctx.lineWidth=5;
    
    // cache often used values
    var PI=Math.PI;
    var fullCircle=PI*2;
    var sa=-PI/2;   // == the 12 o'clock angle in context.arc
    
    // start the animation
    requestAnimationFrame(animate);

    function animate(currentTime){

        // get the current seconds value from the system clock
        var date=new Date();
        var seconds=date.getSeconds();
        
        // clear the canvas
        ctx.clearRect(0,0,cw,ch);
        
        // draw a full circle (== the clock face);
        ctx.beginPath();
        ctx.moveTo(100,100);
        ctx.arc(100,100,75,0,fullCircle);
        ctx.stroke();
        // draw a wedge representing the current seconds value
        ctx.beginPath();
        ctx.moveTo(100,100);
        ctx.arc(100,100,75,sa,sa+fullCircle*seconds/60);
        ctx.fill();

        // request another loop of animation
        requestAnimationFrame(animate);
    }

}); // end $(function(){});
</script>
</head>
<body>
    <canvas id="canvas" width=512 height=512></canvas>
</body>
</html>

```



## Don't draw animations in your event handlers (a simple sketch app)


During `mousemove` you get flooded with 30 mouse events per second. You might not be able to redraw your drawings at 30 times per second. Even if you can, you're probably wasting computing power by drawing when the browser is not ready to draw (wasted == across display refresh cycles).

Therefore it makes sense to separate your users input events (like mousemove) from the drawing of your animations.

<li>
In event handlers, save all the event variables that control where drawings are positioned on the Canvas. But don't actually draw anything.
</li>
<li>
In a `requestAnimationFrame` loop, render all the drawings to the Canvas using the saved information.
</li>

By not drawing in the event handlers, you are not forcing Canvas to try to refresh complex drawings at mouse event speeds.

By doing all drawing in `requestAnimationFrame` you gain all the benefits described in here [Use 'requestanimationFrame' not 'setInterval' for animation loops](http://stackoverflow.com/documentation/html5-canvas/drafts/58696#).

**Annotated Code:**

```html
<!doctype html>
<html>
<head>
<style>
    body{ background-color: ivory; }
    #canvas{border:1px solid red; }
</style>
<script>
window.onload=(function(){

    function log(){console.log.apply(console,arguments);}

    // canvas variables
    var canvas=document.getElementById("canvas");
    var ctx=canvas.getContext("2d");
    var cw=canvas.width;
    var ch=canvas.height;
    // set canvas styling
    ctx.strokeStyle='skyblue';
    ctx.lineJoint='round';
    ctx.lineCap='round';
    ctx.lineWidth=6;

    // handle windows scrolling & resizing
    function reOffset(){
        var BB=canvas.getBoundingClientRect();
        offsetX=BB.left;
        offsetY=BB.top;        
    }
    var offsetX,offsetY;
    reOffset();
    window.onscroll=function(e){ reOffset(); }
    window.onresize=function(e){ reOffset(); }

    // vars to save points created during mousemove handling
    var points=[];
    var lastLength=0;

    // start the  animation loop
    requestAnimationFrame(draw);

    canvas.onmousemove=function(e){handleMouseMove(e);}


    function handleMouseMove(e){
        // tell the browser we're handling this event
        e.preventDefault();
        e.stopPropagation();

        // get the mouse position
        mouseX=parseInt(e.clientX-offsetX);
        mouseY=parseInt(e.clientY-offsetY);

        // save the mouse position in the points[] array
        // but don't draw anything
        points.push({x:mouseX,y:mouseY});
    }

    function draw(){
        // No additional points? Request another frame an return
        var length=points.length;
        if(length==lastLength){requestAnimationFrame(draw);return;}
        
        // draw the additional points
        var point=points[lastLength];
        ctx.beginPath();
        ctx.moveTo(point.x,point.y)
        for(var i=lastLength;i<length;i++){
            point=points[i];
            ctx.lineTo(point.x,point.y);
        }
        ctx.stroke();
        
        // request another animation loop
        requestAnimationFrame(draw);
    }

}); // end window.onload
</script>
</head>
<body>
    <h4>Move mouse over Canvas to sketch</h4>
    <canvas id="canvas" width=512 height=512></canvas>
</body>
</html>

```



## Easing using Robert Penners equations


An easing causes some **variable** to change **unevenly** over a **duration**.

**"variable"** must be able to be expressed as a number, and can represent a remarkable variety of things:

- an X-coordinate,
- a rectangle's width,
- an angle of rotation,
- the red component of an R,G,B color.
- anything that can be expressed as a number.

**"duration"** must be able to be expressed as a number and can also be a variety of things:

- a period of time,
- a distance to be travelled,
- a quantity of animation loops to be executed,
- anything that can be expressed as

**"unevenly"** means that the variable progresses from beginning to ending values unevenly:

- faster at the beginning & slower at the ending -- or visa-versa,
- overshoots the ending but backs up to the ending as the duration finishes,
- repeatedly advances/retreats elastically during the duration,
- "bounces" off the ending while coming to rest as the duration finishes.

Attribution: Robert Penner has created the "gold standard" of easing functions.

Cite: [https://github.com/danro/jquery-easing/blob/master/jquery.easing.js](https://github.com/danro/jquery-easing/blob/master/jquery.easing.js)

```js
// t: elapsed time inside duration (currentTime-startTime), 
// b: beginning value,
// c: total change from beginning value (endingValue-startingValue),
// d: total duration
var Easings={
    easeInQuad: function (t, b, c, d) {
      return c*(t/=d)*t + b;
    },
    easeOutQuad: function (t, b, c, d) {
      return -c *(t/=d)*(t-2) + b;
    },
    easeInOutQuad: function (t, b, c, d) {
      if ((t/=d/2) < 1) return c/2*t*t + b;
      return -c/2 * ((--t)*(t-2) - 1) + b;
    },
    easeInCubic: function (t, b, c, d) {
      return c*(t/=d)*t*t + b;
    },
    easeOutCubic: function (t, b, c, d) {
      return c*((t=t/d-1)*t*t + 1) + b;
    },
    easeInOutCubic: function (t, b, c, d) {
      if ((t/=d/2) < 1) return c/2*t*t*t + b;
      return c/2*((t-=2)*t*t + 2) + b;
    },
    easeInQuart: function (t, b, c, d) {
      return c*(t/=d)*t*t*t + b;
    },
    easeOutQuart: function (t, b, c, d) {
      return -c * ((t=t/d-1)*t*t*t - 1) + b;
    },
    easeInOutQuart: function (t, b, c, d) {
      if ((t/=d/2) < 1) return c/2*t*t*t*t + b;
      return -c/2 * ((t-=2)*t*t*t - 2) + b;
    },
    easeInQuint: function (t, b, c, d) {
      return c*(t/=d)*t*t*t*t + b;
    },
    easeOutQuint: function (t, b, c, d) {
      return c*((t=t/d-1)*t*t*t*t + 1) + b;
    },
    easeInOutQuint: function (t, b, c, d) {
      if ((t/=d/2) < 1) return c/2*t*t*t*t*t + b;
      return c/2*((t-=2)*t*t*t*t + 2) + b;
    },
    easeInSine: function (t, b, c, d) {
      return -c * Math.cos(t/d * (Math.PI/2)) + c + b;
    },
    easeOutSine: function (t, b, c, d) {
      return c * Math.sin(t/d * (Math.PI/2)) + b;
    },
    easeInOutSine: function (t, b, c, d) {
      return -c/2 * (Math.cos(Math.PI*t/d) - 1) + b;
    },
    easeInExpo: function (t, b, c, d) {
      return (t==0) ? b : c * Math.pow(2, 10 * (t/d - 1)) + b;
    },
    easeOutExpo: function (t, b, c, d) {
      return (t==d) ? b+c : c * (-Math.pow(2, -10 * t/d) + 1) + b;
    },
    easeInOutExpo: function (t, b, c, d) {
      if (t==0) return b;
      if (t==d) return b+c;
      if ((t/=d/2) < 1) return c/2 * Math.pow(2, 10 * (t - 1)) + b;
      return c/2 * (-Math.pow(2, -10 * --t) + 2) + b;
    },
    easeInCirc: function (t, b, c, d) { 
      return -c * (Math.sqrt(1 - (t/=d)*t) - 1) + b;
    },
    easeOutCirc: function (t, b, c, d) {
      return c * Math.sqrt(1 - (t=t/d-1)*t) + b;
    },
    easeInOutCirc: function (t, b, c, d) {
      if ((t/=d/2) < 1) return -c/2 * (Math.sqrt(1 - t*t) - 1) + b;
      return c/2 * (Math.sqrt(1 - (t-=2)*t) + 1) + b;
    },
    easeInElastic: function (t, b, c, d) {
      var s=1.70158;var p=0;var a=c;
      if (t==0) return b;  if ((t/=d)==1) return b+c;  if (!p) p=d*.3;
      if (a < Math.abs(c)) { a=c; var s=p/4; }
      else var s = p/(2*Math.PI) * Math.asin (c/a);
      return -(a*Math.pow(2,10*(t-=1)) * Math.sin( (t*d-s)*(2*Math.PI)/p )) + b;
    },
    easeOutElastic: function (t, b, c, d) {
      var s=1.70158;var p=0;var a=c;
      if (t==0) return b;  if ((t/=d)==1) return b+c;  if (!p) p=d*.3;
      if (a < Math.abs(c)) { a=c; var s=p/4; }
      else var s = p/(2*Math.PI) * Math.asin (c/a);
      return a*Math.pow(2,-10*t) * Math.sin( (t*d-s)*(2*Math.PI)/p ) + c + b;
    },
    easeInOutElastic: function (t, b, c, d) {
      var s=1.70158;var p=0;var a=c;
      if (t==0) return b;  if ((t/=d/2)==2) return b+c;  if (!p) p=d*(.3*1.5);
      if (a < Math.abs(c)) { a=c; var s=p/4; }
      else var s = p/(2*Math.PI) * Math.asin (c/a);
      if (t < 1) return -.5*(a*Math.pow(2,10*(t-=1)) * Math.sin( (t*d-s)*(2*Math.PI)/p )) + b;
      return a*Math.pow(2,-10*(t-=1)) * Math.sin( (t*d-s)*(2*Math.PI)/p )*.5 + c + b;
    },
    easeInBack: function (t, b, c, d, s) {
      if (s == undefined) s = 1.70158;
      return c*(t/=d)*t*((s+1)*t - s) + b;
    },
    easeOutBack: function (t, b, c, d, s) {
      if (s == undefined) s = 1.70158;
      return c*((t=t/d-1)*t*((s+1)*t + s) + 1) + b;
    },
    easeInOutBack: function (t, b, c, d, s) {
      if (s == undefined) s = 1.70158; 
      if ((t/=d/2) < 1) return c/2*(t*t*(((s*=(1.525))+1)*t - s)) + b;
      return c/2*((t-=2)*t*(((s*=(1.525))+1)*t + s) + 2) + b;
    },
    easeInBounce: function (t, b, c, d) {
      return c - Easings.easeOutBounce (d-t, 0, c, d) + b;
    },
    easeOutBounce: function (t, b, c, d) {
      if ((t/=d) < (1/2.75)) {
        return c*(7.5625*t*t) + b;
      } else if (t < (2/2.75)) {
        return c*(7.5625*(t-=(1.5/2.75))*t + .75) + b;
      } else if (t < (2.5/2.75)) {
        return c*(7.5625*(t-=(2.25/2.75))*t + .9375) + b;
      } else {
        return c*(7.5625*(t-=(2.625/2.75))*t + .984375) + b;
      }
    },
    easeInOutBounce: function (t, b, c, d) {
      if (t < d/2) return Easings.easeInBounce (t*2, 0, c, d) * .5 + b;
      return Easings.easeOutBounce (t*2-d, 0, c, d) * .5 + c*.5 + b;
    },
};

```

**Example Usage:**

```js
// include the Easings object from above
var Easings = ...

// Demo
var startTime;
var beginningValue=50;  // beginning x-coordinate
var endingValue=450;    // ending x-coordinate
var totalChange=endingValue-beginningValue;
var totalDuration=3000; // ms

var keys=Object.keys(Easings);
ctx.textBaseline='middle';
requestAnimationFrame(animate);

function animate(time){
    var PI2=Math.PI*2;
    if(!startTime){startTime=time;}
    var elapsedTime=Math.min(time-startTime,totalDuration);
    ctx.clearRect(0,0,cw,ch);
    ctx.beginPath();
    for(var y=0;y<keys.length;y++){
        var key=keys[y];
        var easing=Easings[key];
        var easedX=easing(
            elapsedTime,beginningValue,totalChange,totalDuration);
        if(easedX>endingValue){easedX=endingValue;}
        ctx.moveTo(easedX,y*15);
        ctx.arc(easedX,y*15+10,5,0,PI2);
        ctx.fillText(key,460,y*15+10-1);
    }
    ctx.fill();
    if(time<startTime+totalDuration){
        requestAnimationFrame(animate);
    }
}

```



## Set frame rate using requestAnimationFrame


Using requestAnimationFrame may on some systems update at more frames per second than the 60fps. 60fps is the default rate if the rendering can keep up. Some systems will run at 120fps maybe more.

If you use the following method you should only use frame rates that are integer divisions of 60 so that `(60 / FRAMES_PER_SECOND) % 1 === 0` is `true` or you will get inconsistent frame rates.

```js
const FRAMES_PER_SECOND = 30;  // Valid values are 60,30,20,15,10...
// set the mim time to render the next frame
const FRAME_MIN_TIME = (1000/60) * (60 / FRAMES_PER_SECOND) - (1000/60) * 0.5;
var lastFrameTime = 0;  // the last frame time
function update(time){
    if(time-lastFrameTime < FRAME_MIN_TIME){ //skip the frame if the call is too early
        requestAnimationFrame(update);
        return; // return as there is nothing to do
    }
    lastFrameTime = time; // remember the time of the rendered frame
    // render the frame
    requestAnimationFrame(update); // get next farme
}
requestAnimationFrame(update); // start animation

```



## Animate from [x0,y0] to [x1,y1]


Use vectors to calculate incremental [x,y] from [startX,startY] to [endX,endY]

```js
// dx is the total distance to move in the X direction
var dx = endX - startX;

// dy is the total distance to move in the Y direction
var dy = endY - startY;

// use a pct (percentage) to travel the total distances
// start at 0% which == the starting point
// end at 100% which == then ending point
var pct=0;  

// use dx & dy to calculate where the current [x,y] is at a given pct
var x = startX + dx * pct/100;
var y = startY + dx * pct/100;

```

**Example Code:**

```js
// canvas vars
var canvas=document.createElement("canvas");
document.body.appendChild(canvas);
canvas.style.border='1px solid red';
var ctx=canvas.getContext("2d");
var cw=canvas.width;
var ch=canvas.height;
// canvas styles
ctx.strokeStyle='skyblue';
ctx.fillStyle='blue';

// animating vars
var pct=101;
var startX=20;
var startY=50;
var endX=225;
var endY=100;
var dx=endX-startX;
var dy=endY-startY;

// start animation loop running
requestAnimationFrame(animate);

// listen for mouse events
window.onmousedown=(function(e){handleMouseDown(e);});
window.onmouseup=(function(e){handleMouseUp(e);});

// constantly running loop
// will animate dot from startX,startY to endX,endY 
function animate(time){
    // demo: rerun animation
    if(++pct>100){pct=0;}
    // update
    x=startX+dx*pct/100;
    y=startY+dy*pct/100;
    // draw
    ctx.clearRect(0,0,cw,ch);
    ctx.beginPath();
    ctx.moveTo(startX,startY);
    ctx.lineTo(endX,endY);
    ctx.stroke();
    ctx.beginPath();
    ctx.arc(x,y,5,0,Math.PI*2);
    ctx.fill()
    // request another animation loop
    requestAnimationFrame(animate);
}

```

