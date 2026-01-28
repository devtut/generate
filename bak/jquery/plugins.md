---
metaTitle: "jQuery - Plugins"
description: "Plugins - Getting Started, jQuery.fn.extend() method"
---

# Plugins



## Plugins - Getting Started


The jQuery API may be extended by adding to its prototype. For example, the existing API already has many functions available such as `.hide()`, `.fadeIn()`, `.hasClass()`, etc.

The jQuery prototype is exposed through `$.fn`, the source code contains the line

```js
jQuery.fn = jQuery.prototype

```

Adding functions to this prototype will allow those functions to be available to be called from any constructed jQuery object (which is done implicitly with each call to jQuery, or each call to $ if you prefer).

A constructed jQuery object will hold an internal array of elements based on the selector passed to it. For example, `$('.active')` will construct a jQuery object that holds elements with the active class, at the time of calling (as in, this is not a live set of elements).

The `this` value inside of the plugin function will refer to the constructed jQuery object. As a result, `this` is used to represent the matched set.

**Basic Plugin**:

```js
$.fn.highlight = function() {
    this.css({ background: "yellow" });
};

// Use example:
$("span").highlight();

```

[jsFiddle example](https://jsfiddle.net/chdtkgex/)

**Chainability & Reusability**

**Unlike the example above**, jQuery Plugins are expected to be **Chainable**.<br />
What this means is the possibility to chain multiple Methods to a same Collection of Elements like `$(".warn").append("WARNING! ").css({color:"red"})` (see how we used the `.css()` method after the `.append()`, both methods apply on the same `.warn` Collection)

Allowing one to use the same plugin on different Collections passing different customization options plays an important role in **Customization / Reusability**

```js
(function($) {
  $.fn.highlight = function( custom ) {

    // Default settings
    var settings = $.extend({
        color : "",              // Default to current text color
        background : "yellow"    // Default to yellow background
    }, custom);

    return this.css({     // `return this` maintains method chainability
        color : settings.color,
        backgroundColor : settings.background
    });

  };
}( jQuery ));


// Use Default settings
$("span").highlight();    // you can chain other methods

// Use Custom settings
$("span").highlight({
    background: "#f00",
    color: "white"
});

```

[jsFiddle demo](https://jsfiddle.net/chdtkgex/5/)

**Freedom**

The above examples are in the scope of understanding basic Plugin creation. Keep in mind  to not restrict a user to a limited set of customization options.

Say for example you want to build a `.highlight()` Plugin where you can pass a desired **text** String that will be highlighted and allow maximal freedom regarding styles:

```js
//...
// Default settings
var settings = $.extend({
   text : "",          // text to highlight
   class : "highlight" // reference to CSS class
}, custom);

return this.each(function() {
   // your word highlighting logic here
});
//...

```

the user can now pass a desired **text** and have complete control over the added styles by using a custom CSS class:

```js
$("#content").highlight({
    text : "hello",
    class : "makeYellowBig"
});

```

[jsFiddle example](https://jsfiddle.net/chdtkgex/3/)



## jQuery.fn.extend() method


This method extends the jQuery prototype ($.fn) object to provide new custom methods that can be chained to the jQuery() function.

For example:

```js
<div>Hello</div>
<div>World!</div>
 
<script>
jQuery.fn.extend({
  // returns combination of div texts as a result
  getMessage: function() {
    var result;
    // this keyword corresponds result array of jquery selection
    this.each(function() {
        // $(this) corresponds each div element in this loop
        result = result + " " + $(this).val();
    });
    return result;
  }
});
 
// newly created .getMessage() method
var message = $("div").getMessage();

// message = Hello World!
console.log(message); 
</script>

```

