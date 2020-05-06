---
metaTitle: "jQuery - CSS Manipulation"
description: "CSS – Getters and Setters, Increment/Decrement Numeric Properties, Set CSS property, Get CSS property"
---

# CSS Manipulation



## CSS – Getters and Setters


### CSS Getter

The `.css()` **getter** function can be applied to every DOM element on the page like the following:

```js
// Rendered width in px as a string. ex: `150px`
// Notice the `as a string` designation - if you require a true integer, 
// refer to `$.width()` method
$("body").css("width"); 

```

This line will return the **computed width** of the specified element, each CSS property you provide in the parentheses will yield the value of the property for this `$("selector")` DOM element, if you ask for CSS attribute that doesn't exist you will get `undefined` as a response.

You also can call the **CSS getter** with an array of attributes:

```js
$("body").css(["animation","width"]);

```

this will return an object of all the attributes with their values:

```js
Object {animation: "none 0s ease 0s 1 normal none running", width: "529px"}

```

### CSS Setter

The `.css()` **setter** method can also be applied to every DOM element on the page.

```js
$("selector").css("width", 500);

```

This statement set the `width` of the `$("selector")` to `500px` and return the jQuery object so you can chain more methods to the specified selector.

The `.css()` **setter** can also be used passing an Object of CSS properties and values like:

```js
$("body").css({"height": "100px", width:100, "padding-top":40, paddingBottom:"2em"});

```

All the changes the setter made are appended to the DOM element `style` property thus affecting the elements' styles (unless that style property value is already defined as `!important` somewhere else in styles).



## Increment/Decrement Numeric Properties


Numeric CSS properties can be incremented and decremented with the `+=` and `-=` syntax, respectively, using the `.css()` method:

```

 // Increment using the += syntax
  $("#target-element").css("font-size", "+=10");
  
  // You can also specify the unit to increment by
  $("#target-element").css("width", "+=100pt");
  $("#target-element").css("top", "+=30px");
  $("#target-element").css("left", "+=3em");
  
  // Decrementing is done by using the -= syntax
  $("#target-element").css("height", "-=50pt");

```



## Set CSS property


Setting only one style:

```js
$('#target-element').css('color', '#000000');

```

Setting multiple styles at the same time:

```js
$('#target-element').css({
    'color': '#000000',
    'font-size': '12pt',
    'float': 'left',
});

```



## Get CSS property


To get an element's CSS property you can use the `.css(propertyName)` method:

```js
var color    = $('#element').css('color');
var fontSize = $('#element').css('font-size');

```



#### Syntax


- .css( cssProperty )    // Get the rendered CSS property value
- .css( [cssProperty , ...] ) // Get values from Array of cssProperties
- .css( cssProperty, value ) // Set value
- .css( {cssProperty:value, ...} ) // Set properties and values
- .css( cssProperty, function ) // Expose the cssProperty to a callback function



#### Remarks


**Rendered values**

If a responsive unit is used (like `"auto"`, `"%"`, `"vw"` etc.), `.css()` will return the actual rendered value in `px`

```js
.myElement{ width: 20%; }

```

```js
var width = $(".myElement").css("width"); // "123px" 

```

**Formatting properties and values**

**Properties** can be defined using **standard CSS formatting as String**  or using **camelCase**

```js
"margin-bottom"
marginBottom

```

**Values** should be expressed in String. Numeric values are treated as `px` units internally by jQuery

```js
.css(fontSize: "1em")
.css(fontSize: "16px")
.css(fontSize: 16)      // px will be used

```

**As of jQuery 3 avoid using `.show()` and `.hide()`**

According to [this jQuery Blog post](http://blog.jquery.com/2015/07/13/jquery-3-0-and-jquery-compat-3-0-alpha-versions-released/), due to overhead and performance issues, you should no longer be using `.show()` or `.hide()`.

> 
<p>If you have elements in a stylesheet that are set to `display: none`,
the `.show()` method will no longer override that. So the most
important rule for moving to jQuery 3.0 is this: Don’t use a
stylesheet to set the default of `display: none` and then try to use
`.show()` – or any method that shows elements, such as `.slideDown()`
and `.fadeIn()` – to make it visible.
If you need an element to be hidden by default, the best way is to add
a class name like “hidden” to the element and define that class to be
`display: none` in a stylesheet. Then you can add or remove that class
using jQuery’s `.addClass()` and `.removeClass()` methods to control
visibility. Alternately, you can have a `.ready()` handler call
`.hide()` on the elements before they are displayed on the page. Or,
if you really must retain the stylesheet default, you can use
`.css("display", "block")` (or the appropriate display value) to
override the stylesheet.</p>


