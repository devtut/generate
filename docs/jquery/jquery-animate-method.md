---
metaTitle: "jQuery - jQuery .animate() Method"
description: "Animation with callback"
---

# jQuery .animate() Method



## Animation with callback


Sometimes we need to change words position from one place to another or reduce size of the words and change the color of words automatically to improve the attraction of our website or web apps. JQuery helps a lot with this concept using `fadeIn(), hide(), slideDown()` but its functionality are limited and it only done the specific task which assign to it.

Jquery fix this problem by providing an amazing and flexible method called `.animate()`. This method allows to set custom animations which is used css properties that give permission to fly over borders. for example if we give css style property as `width:200;` and current position of the DOM element is 50, animate method reduce current position value from given css value and animate that element to 150.But we don't need to bother about this part because animation engine will handle it.

```js
<script src="https://ajax.googleapis.com/ajax/libs/jquery/3.1.1/jquery.min.js"></script>
<script>
    $("#btn1").click(function(){
        $("#box").animate({width: "200px"});
    });
</script>

<button id="btn1">Animate Width</button>
<div id="box" style="background:#98bf21;height:100px;width:100px;margin:6px;"></div>

```

**List of css style properties that allow in `.animate()` method.**

```js
backgroundPositionX, backgroundPositionY, borderWidth, borderBottomWidth, borderLeftWidth, borderRightWidth, borderTopWidth, borderSpacing, margin, marginBottom, marginLeft, marginRight, marginTop, outlineWidth, padding, paddingBottom, paddingLeft, paddingRight, paddingTop, height, width, maxHeight, maxWidth, minHeight, minWidth, fontSize, bottom, left, right, top, letterSpacing,  wordSpacing, lineHeight, textIndent, 

```

**Speed specified in `.animate()` method.**

```js
milliseconds (Ex: 100, 1000, 5000, etc.), 
"slow", 
"fast"

```

**Easing specified in `.animate()` method.**

|
|---|---|---|---|---|---|---|---|---|---
|"swing"
|"linear"

Here is some examples with complex animation options.

Eg 1:

```js
$( "#book" ).animate({
  width: [ "toggle", "swing" ],
  height: [ "toggle", "swing" ],
  opacity: "toggle"
 }, 5000, "linear", function() {
    $( this ).after( "<div>Animation complete.</div>" );
});

```

Eg 2:

```

 $("#box").animate({
     height: "300px",
     width: "300px"
     }, {
     duration: 5000,
     easing: "linear",
     complete: function(){
        $(this).after("<p>Animation is complete!</p>");
     }
  });

```



#### Syntax


1. (selector).animate({styles},{options})



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|properties|An object of CSS properties and values that the animation will move toward
|duration|(default: 400) A string or number determining how long the animation will run
|easing|(default: swing) A string indicating which easing function to use for the transition
|complete|A function to call once the animation is complete, called once per matched element.
|start|specifies a function to be executed when the animation begins.
|step|specifies a function to be executed for each step in the animation.
|queue|a Boolean value specifying whether or not to place the animation in the effects queue.
|progress|specifies a function to be executed after each step in the animation.
|done|specifies a function to be executed when the animation ends.
|fail|specifies a function to be executed if the animation fails to complete.
|specialEasing|a map of one or more CSS properties from the styles parameter, and their corresponding easing functions.
|always|specifies a function to be executed if the animation stops without completing.

