---
metaTitle: "jQuery - Getting started with jQuery"
description: "Getting Started, Avoiding namespace collisions, jQuery Namespace (jQuery and $), Loading jQuery via console on a page that does not have it., Include script tag in head of HTML page, The jQuery Object, Loading namespaced jQuery plugins"
---

# Getting started with jQuery



## Getting Started


Create a file `hello.html` with the following content:

```js
<!DOCTYPE html>
<html>
<head>
    <title>Hello, World!</title>
</head>
<body>
    <div>
        <p id="hello">Some random text</p>
    </div>
    <script src="https://code.jquery.com/jquery-2.2.4.min.js"></script>
    <script>
        $(document).ready(function() {
            $('#hello').text('Hello, World!');
        });
    </script>
</body>
</html>

```

[Live Demo on JSBin](http://jsbin.com/sipuwoxono/1/edit?html,output)

Open this file in a web browser. As a result you will see a page with the text: `Hello, World!`

### Explanation of code

<li>
Loads the jQuery library from the jQuery [CDN](https://en.wikipedia.org/wiki/Content_delivery_network):

```js
<script src="https://code.jquery.com/jquery-2.2.4.min.js"></script>

```


This introduces the `$` global variable, an alias for the `jQuery` function and namespace.
<blockquote>
<p><em>Be aware that one of the most common mistakes made when including
jQuery is failing to load the library BEFORE any other scripts or
libraries that may depend on or make use of it.</em></p>
</blockquote>
</li>

<li>
Defers a function to be executed when the DOM ([Document Object Model](http://stackoverflow.com/documentation/dom/2584/introduction-to-dom#t=201607251515142666553)) is detected to be ["ready"](https://learn.jquery.com/using-jquery-core/document-ready/) by jQuery:

```js
// When the `document` is `ready`, execute this function `...`
$(document).ready(function() { ... });
 
// A commonly used shorthand version (behaves the same as the above)
$(function() { ... });

```


</li>

<li>
Once the DOM is ready, jQuery executes the callback function shown above. Inside of our function, there is only one call which does 2 main things:
<ol>
<li>
Gets the element with the `id` attribute equal to `hello` (our [selector](http://stackoverflow.com/documentation/jquery/389/selectors#t=201607222002365660416) `#hello`). Using a selector as the passed argument is the core of jQuery's functionality and naming; the entire library essentially evolved from extending [document.querySelectorAll<sup>MDN</sup>](https://developer.mozilla.org/en-US/docs/Web/API/Document/querySelectorAll).
</li>
<li>
Set the [`text()`](http://api.jquery.com/text/) inside the selected element to `Hello, World!`.

```js
#    ↓ - Pass a `selector` to `$` jQuery, returns our element
$('#hello').text('Hello, World!');
#             ↑ - Set the Text on the element

```


</li>

For more refer to the [jQuery - Documentation](http://api.jquery.com/) page.



## Avoiding namespace collisions


Libraries other than jQuery may also use `$` as an alias. This can cause interference between those libraries and jQuery.

To release `$` for use with other libraries:

```js
jQuery.noConflict();

```

After calling this function, `$` is no longer an alias for `jQuery`. However, you can still use the variable `jQuery` itself to access jQuery functions:

```js
jQuery('#hello').text('Hello, World!');

```

Optionally, you can assign a different variable as an alias for jQuery:

```js
var jqy = jQuery.noConflict();
jqy('#hello').text('Hello, World!');

```

Conversely, to prevent other libraries from interfering with jQuery, you can wrap your jQuery code in an [immediately invoked function expression (IIFE)](http://stackoverflow.com/documentation/javascript/186/functions/843/immediately-invoked-function-expressions#t=201607152249399289216) and pass in `jQuery` as the argument:

```js
(function($) {
    $(document).ready(function() {
        $('#hello').text('Hello, World!');
    });
})(jQuery);

```

Inside this IIFE, `$` is an alias for jQuery only.

Another simple way to **secure jQuery's `$` alias and make sure DOM is ready**:

```js
jQuery(function( $ ) { // DOM is ready
   // You're now free to use $ alias
   $('#hello').text('Hello, World!');
});

```

To summarize,

- `jQuery.noConflict()` : `$` no longer refers to jQuery, while the variable `jQuery` does.
- `var jQuery2 = jQuery.noConflict()` -  `$` no longer refers to jQuery, while the variable `jQuery` does and so does the variable `jQuery2`.

Now, there exists a third scenario - What if we want jQuery to be available **only in `jQuery2`**? Use,

`var jQuery2 = jQuery.noConflict(true)`

This results in neither `$` nor `jQuery` referring to jQuery.

This is useful when multiple versions of jQuery are to be loaded onto the same page.

```js
<script src='https://code.jquery.com/jquery-1.12.4.min.js'></script>
<script>
    var jQuery1 = jQuery.noConflict(true);
</script>
<script src='https://code.jquery.com/jquery-3.1.0.min.js'></script>
<script>
    // Here, jQuery1 refers to jQuery 1.12.4 while, $ and jQuery refers to jQuery 3.1.0.
</script>

```

[https://learn.jquery.com/using-jquery-core/avoid-conflicts-other-libraries/](https://learn.jquery.com/using-jquery-core/avoid-conflicts-other-libraries/)



## jQuery Namespace ("jQuery" and "$")


`jQuery` is the starting point for writing any jQuery code. It can be used as a function `jQuery(...)` or a variable `jQuery.foo`.

`$` is an alias for `jQuery` and the two can usually be interchanged for each other (except where `jQuery.noConflict();` has been used - see [Avoiding namespace collisions](http://stackoverflow.com/documentation/jquery/211/getting-started-with-jquery/1960/avoiding-namespace-collisions)).

Assuming we have this snippet of HTML -

```js
<div id="demo_div" class="demo"></div>

```

We might want to use jQuery to add some text content to this div. To do this we could use the jQuery [`text()`](https://api.jquery.com/text/#text2) function.  This could be written using either `jQuery` or `$`. i.e. -

```js
jQuery("#demo_div").text("Demo Text!");

```

Or -

```js
$("#demo_div").text("Demo Text!");

```

Both will result in the same final HTML -

```js
<div id="demo_div" class="demo">Demo Text!</div>

```

As `$` is more concise than `jQuery` it is the generally the preferred method of writing jQuery code.

jQuery uses CSS selectors and in the example above an ID selector was used.  For more information on selectors in jQuery see [types of selectors](http://stackoverflow.com/documentation/jquery/389/selectors/2115/types-of-selectors).



## Loading jQuery via console on a page that does not have it.


Sometimes one has to work with pages that are not using `jQuery` while most developers are used to have `jQuery` handy.

In such situations one can use `Chrome Developer Tools` console (<kbd>F12</kbd>) to manually add `jQuery` on a loaded page by running following:

```js
var j = document.createElement('script');
j.onload = function(){ jQuery.noConflict(); };
j.src = "https://ajax.googleapis.com/ajax/libs/jquery/1.12.4/jquery.min.js";
document.getElementsByTagName('head')[0].appendChild(j);

```

Version you want might differ from above(`1.12.4`) you can get the link for [one you need here](https://developers.google.com/speed/libraries/#jquery).



## Include script tag in head of HTML page


To load **jQuery** from the official [CDN](https://wikipedia.org/wiki/Content_delivery_network), go to the jQuery [website](https://code.jquery.com/). You'll see a list of different versions and formats available.

[<img src="https://i.stack.imgur.com/eOt5z.jpg" alt="jQuery CDN page stating versions of jQuery available" />](https://i.stack.imgur.com/eOt5z.jpg)

Now, copy the source of the version of jQuery, you want to load. Suppose, you want to load **jQuery 2.X**, click **uncompressed** or **minified** tag which will show you something like this:

[<img src="https://i.stack.imgur.com/E8zjB.jpg" alt="Script tag popped up with jQuery version is selected" />](https://i.stack.imgur.com/E8zjB.jpg)

Copy the full code (or click on the copy icon) and paste it in the `<head>` or `<body>` of your html.

The best practice is to load any external JavaScript libraries at the head tag with the `async` attribute. Here is a demonstration:

```js
<!DOCTYPE html>
    <html>
      <head>
         <title>Loading jquery-2.2.4</title>
         <script src="https://code.jquery.com/jquery-2.2.4.min.js" async></script>
      </head>
      <body>
          <p>This page is loaded with jquery.</p>
      </body>
   </html>

```

When using `async` attribute be conscious as the javascript libraries are then asynchronously loaded and executed as soon as available. If two libraries are included where second library is dependent on the first library is this case if second library is loaded and executed before first library then it may throw an error and application may break.



## The jQuery Object


Every time jQuery is called, by using `$()` or `jQuery()`, internally it is creating a `new` instance of `jQuery`. This is the [source code](http://code.jquery.com/jquery-2.2.4.js) which shows the new instance:

```js
// Define a local copy of jQuery
jQuery = function( selector, context ) {

    // The jQuery object is actually just the init constructor 'enhanced'
    // Need init if jQuery is called (just allow error to be thrown if not included)
    return new jQuery.fn.init( selector, context );
}

```

Internally jQuery refers to its prototype as `.fn`, and the style used here of internally instantiating a jQuery object allows for that prototype to be exposed without the explicit use of `new` by the caller.

In addition to setting up an instance (which is how the jQuery API, such as `.each`, `children`,`filter`, etc. is exposed), internally jQuery will also create an array-like structure to match the result of the selector (provided that something other than nothing, `undefined`, `null`, or similar was passed as the argument). In the case of a single item, this array-like structure will hold only that item.

A simple demonstration would be to find an element with an id, and then access the jQuery object to return the underlying DOM element (this will also work when multiple elements are matched or present).

```js
var $div = $("#myDiv");//populate the jQuery object with the result of the id selector
var div = $div[0];//access array-like structure of jQuery object to get the DOM Element

```



## Loading namespaced jQuery plugins


Typically when loading plugins, make sure to always include the plugin **after** jQuery.

```js
<script src="https://code.jquery.com/jquery-3.1.1.min.js"></script>
<script src="some-plugin.min.js"></script>

```

If you **must** use more than one version of jQuery, then make sure to load the plugin(s) **after** the required version of jQuery followed by code to set [`jQuery.noConflict(true)`](http://api.jquery.com/jQuery.noConflict/); then load the next version of jQuery and its associated plugin(s):

```js
<script src="https://code.jquery.com/jquery-1.7.0.min.js"></script>
<script src="plugin-needs-1.7.min.js"></script>
<script>
// save reference to jQuery v1.7.0
var $oldjq = jQuery.noConflict(true);
</script>
<script src="https://code.jquery.com/jquery-3.1.1.min.js"></script>
<script src="newer-plugin.min.js"></script>

```

Now when initializing the plugins, you'll need to use the associated jQuery version

```js
<script>
// newer jQuery document ready
jQuery(function($){
  // "$" refers to the newer version of jQuery
  // inside of this function

  // initialize newer plugin
  $('#new').newerPlugin();
});

// older jQuery document ready
$oldjq(function($){
  // "$" refers to the older version of jQuery
  // inside of this function

  // initialize plugin needing older jQuery
  $('#old').olderPlugin();
});
</script>

```

It is possible to only use one document ready function to initialize both plugins, but to avoid confusion and problems with any extra jQuery code inside the document ready function, it would be better to keep the references separate.



#### Remarks


jQuery is a JavaScript library which simplifies DOM operations, event handling, AJAX, and animations. It also takes care of many browser compatibility issues in underlying DOM and javascript engines.

Each version of jQuery can be downloaded from [https://code.jquery.com/jquery/](https://code.jquery.com/jquery/) in both compressed (minified) and uncompressed formats.

