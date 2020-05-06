---
metaTitle: "jQuery - Element Visibility"
description: "Toggle possibilities, Overview"
---

# Element Visibility



## Toggle possibilities


**Simple `toggle()` case**

```js
function toggleBasic() {
  $(".target1").toggle();
}

```

**With specific **duration****

```js
function toggleDuration() {
  $(".target2").toggle("slow"); // A millisecond duration value is also acceptable
}

```

**...and **callback****

```js
function toggleCallback() {
  $(".target3").toggle("slow",function(){alert('now do something');});  
}

```

**...or with **easing** and callback.**

```js
function toggleEasingAndCallback() {
  // You may use jQueryUI as the core only supports linear and swing easings
  $(".target4").toggle("slow","linear",function(){alert('now do something');});  
}

```

**...or with a variety of **options**.**

```js
function toggleWithOptions() {
  $(".target5").toggle(
    { // See all possible options in: api.jquery.com/toggle/#toggle-options
      duration:1000, // milliseconds
      easing:"linear",
      done:function(){
        alert('now do something');
      }
    }
  );  
}

```

**It's also possible to use a **slide** as animation with `slideToggle()`**

```js
function toggleSlide() {
  $(".target6").slideToggle(); // Animates from top to bottom, instead of top corner
}

```

**...or fade in/out by changing opacity with `fadeToggle()`**

```js
function toggleFading() {
  $( ".target7" ).fadeToggle("slow")
}

```

**...or toggle a class with `toggleClass()`**

```js
function toggleClass() {
  $(".target8").toggleClass('active');
}

```

**A common case is to use `toggle()` in order to show one element while hiding the other  (same class)**

```js
function toggleX() {
  $(".targetX").toggle("slow");  
}

```

All the above examples can be found [here](https://codepen.io/anon/pen/GERVJe)



## Overview


```js
$(element).hide()            // sets display: none
$(element).show()            // sets display to original value
$(element).toggle()          // toggles between the two
$(element).is(':visible')    // returns true or false
$('element:visible')         // matches all elements that are visible
$('element:hidden')          // matches all elements that are hidden

$('element').fadeIn();          // display the element
$('element').fadeOut();          // hide the element

$('element').fadeIn(1000);          // display the element using timer
$('element').fadeOut(1000);          // hide the element using timer

// display the element using timer and a callback function
$('element').fadeIn(1000, function(){
 // code to execute
});          

// hide the element using timer and a callback function
$('element').fadeOut(1000, function(){
   // code to execute
});       

```



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|Duration|When passed, the effects of `.hide()`, `.show()` and `.toggle()` are animated; the element(s) will gradually fade in or out.

