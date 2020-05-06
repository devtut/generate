---
metaTitle: "jQuery - document-ready event"
description: "What is document-ready and how should I use it?, jQuery 2.2.3 and earlier, jQuery 3.0, Difference between $(document).ready() and $(window).load(), Attaching events and manipulating the DOM inside ready(), Difference between jQuery(fn) and executing your code before </body>"
---

# document-ready event



## What is document-ready and how should I use it?


jQuery code is often wrapped in `jQuery(function($) { ... });` so that it only runs after the DOM has finished loading.

```js
<script type="text/javascript"> 
  jQuery(function($) {
    // this will set the div's text to "Hello".
    $("#myDiv").text("Hello");
  });
</script>

<div id="myDiv">Text</div>

```

This is important because jQuery (and JavaScript generally) cannot select a DOM element that has not been rendered to the page.

```js
<script type="text/javascript">
  // no element with id="myDiv" exists at this point, so $("#myDiv") is an
  // empty selection, and this will have no effect
  $("#myDiv").text("Hello");
</script>

<div id="myDiv">Text</div>

```

Note that you can alias the jQuery namespace by passing a custom handler into the `.ready()` method. This is useful for cases when another JS library is using the same shortened `$` alias as  **jQuery**, which create a conflict.
To avoid this conflict, you must call `$.noConflict();` - This forcing you to use only the default **jQuery** namespace (Instead of the short `$` alias).<br />
By passing a custom handler to the `.ready()` handler, you will be able to choose the alias name to use **jQuery**.

```js
$.noConflict();

jQuery( document ).ready(function( $ ) {
   // Here we can use '$' as jQuery alias without it conflicting with other 
   // libraries that use the same namespace 
   $('body').append('<div>Hello</div>')
});

jQuery( document ).ready(function( jq ) {
   // Here we use a custom jQuery alias 'jq' 
   jq('body').append('<div>Hello</div>')
});

```

Rather than simply putting your jQuery code at the bottom of the page, using the `$(document).ready` function ensures that all HTML elements have been rendered and the entire Document Object Model (DOM) is ready for JavaScript code to execute.



## jQuery 2.2.3 and earlier


These are all equivalent, the code inside the blocks will run when the document is ready:

```js
$(function() {
  // code
});

$().ready(function() {
  // code
});

$(document).ready(function() {
  // code
});

```

Because these are equivalent the first is the recommended form, the following is a version of that with the `jQuery` keyword instead of the `$` which produce the same results:

```js
jQuery(function() {
  // code
});

```



## jQuery 3.0


### Notation

As of jQuery 3.0, only this form is recommended:

```js
jQuery(function($) {
  // Run when document is ready
  // $ (first argument) will be internal reference to jQuery
  // Never rely on $ being a reference to jQuery in the global namespace
});

```

All other document-ready handlers [are deprecated in jQuery 3.0.](https://jquery.com/upgrade-guide/3.0/#deprecated-document-ready-handlers-other-than-jquery-function)

### Asynchronous

As of jQuery 3.0, the ready handler [will always be called asynchronously](https://jquery.com/upgrade-guide/3.0/#breaking-change-document-ready-handlers-are-now-asynchronous). This means that in the code below, the log 'outside handler' will always be displayed first, regardless whether the document was ready at the point of execution.

```js
$(function() {
  console.log("inside handler");
});
console.log("outside handler");

```

> 
<p>> outside handler<br />
> inside handler</p>




## Difference between $(document).ready() and $(window).load()


`$(window).load()` was **deprecated in jQuery version 1.8 (and completely removed from [jQuery 3.0](https://jquery.com/upgrade-guide/3.0/#breaking-change-load-unload-and-error-removed))** and as such should not be used anymore. The reasons for the deprecation are noted on the [jQuery page about this event](http://api.jquery.com/load-event/)

> 
Caveats of the load event when used with images
A common challenge developers attempt to solve using the `.load()` shortcut is to execute a function when an image (or collection of images) have completely loaded. There are several known caveats with this that should be noted. These are:
<ul>
- It doesn't work consistently nor reliably cross-browser
- It doesn't fire correctly in WebKit if the image `src` is set to the same `src` as before
- It doesn't correctly bubble up the DOM tree
- Can cease to fire for images that already live in the browser's cache
</ul>


If you still wish to use `load()` it is documented below:

`$(document).ready()` waits until the full DOM is availble -- all the elements in the HTML have been parsed and are in the document. However, resources such as images may not have fully loaded at this point. If it is important to wait until all resources are loaded, `$(window).load()` **and you're aware of the significant limitations of this event** then the below can be used instead:

```js
$(document).ready(function() {
  console.log($("#my_large_image").height()); // may be 0 because the image isn't available
});

$(window).load(function() {
  console.log($("#my_large_image").height()); // will be correct
});

```



## Attaching events and manipulating the DOM inside ready()


Example uses of `$(document).ready()`:

<li>**Attaching event handlers**<br />
Attach jQuery event handlers</li>

```js
$(document).ready(function() {
  $("button").click(function() {
    // Code for the click function
  });
});

```


1. **Run jQuery code after the page structure is created**

```js
jQuery(function($) {
// set the value of an element.
   $("#myElement").val("Hello");
});

```


<li>**Manipulate the loaded DOM structure**<br />
For example: hide a `div` when the page loads for the first time
and show it on the click event of a button</li>

```js
$(document).ready(function() {
  $("#toggleDiv").hide();
  $("button").click(function() {
    $("#toggleDiv").show();
  });
});

```



## Difference between jQuery(fn) and executing your code before </body>


Using the document-ready event can have small [performance drawbacks](http://stackoverflow.com/q/9557846/938297), with delayed execution of up to ~300ms. Sometimes the same behavior can be achieved by execution of code just before the closing `</body>` tag:

```js
<body>
  <span id="greeting"></span> world!
  <script>
    $("#greeting").text("Hello");
  </script>
</body>

```

will produce similar behavior but perform sooner than as it does not wait for the document ready event trigger as it does in:

```js
<head>
  <script>
    jQuery(function($) {
      $("#greeting").text("Hello");
    });
  </script>
</head>
<body>
  <span id="greeting"></span> world!
</body>

```

Emphasis on the fact that first example relies upon your knowledge of your page and placement of the script just prior to the closing `</body>` tag and specifically after the `span` tag.

