---
metaTitle: "jQuery - DOM Traversing"
description: "Select children of element, Iterating over list of jQuery elements, Selecting siblings, closest() method, Get next element, Get previous element, Filter a selection, find() method"
---

# DOM Traversing



## Select children of element


To select the children of an element you can use the [`children()`](https://api.jquery.com/children/) method.

```js
<div class="parent">
    <h2>A headline</h2>
    <p>Lorem ipsum dolor sit amet...</p>
    <p>Praesent quis dolor turpis...</p>
</div>

```

Change the color of **all** the children of the `.parent` element:

```js
$('.parent').children().css("color", "green");

```

The method accepts an optional `selector` argument that can be used to filter the elements that are returned.

```js
// Only get "p" children
$('.parent').children("p").css("color", "green");

```



## Iterating over list of jQuery elements


When you need to iterate over the list of jQuery elements.

Consider this DOM structure:

```js
<div class="container">
    <div class="red one">RED 1 Info</div>
    <div class="red two">RED 2 Info</div>
    <div class="red three">RED 3 Info</div>
</div>

```

To print the text present in all the `div` elements with a class of `red`:

```js
$(".red").each(function(key, ele){
    var text = $(ele).text();
    console.log(text);
});

```

**Tip:** `key` is the index of the `div.red` element we're currently iterating over, within its parent. `ele` is the HTML element, so we can create a jQuery object from it using `$()` or `jQuery()`, like so: `$(ele)`. After, we can call any jQuery method on the object, like `css()` or `hide()` etc. In this example, we just pull the text of the object.



## Selecting siblings


To select siblings of an item you can use the [`.siblings()`](https://api.jquery.com/siblings/) method.

A typical example where you want to modify the siblings of an item is in a menu:

```js
<ul class="menu">
    <li class="selected">Home</li>
    <li>Blog</li>
    <li>About</li>
</ul>

```

When the user clicks on a menu item the `selected` class should be added to the clicked element and removed from its **siblings**:

```js
$(".menu").on("click", "li", function () {
    $(this).addClass("selected");
    $(this).siblings().removeClass("selected");
});

```

The method takes an optional `selector` argument, which can be used if you need to narrow down the kinds of siblings you want to select:

```js
$(this).siblings("li").removeClass("selected");

```



## closest() method


Returns the first element that matches the selector starting at the element and traversing up the DOM tree.

HTML

```js
<div id="abc" class="row">
    <div id="xyz" class="row">
    </div>
    <p id="origin">
      Hello
    </p>
</div>

```

jQuery

```js
var target = $('#origin').closest('.row');
console.log("Closest row:", target.attr('id') );

var target2 = $('#origin').closest('p');
console.log("Closest p:", target2.attr('id') );

```

OUTPUT

```js
"Closest row: abc"
"Closest p: origin"

```

**first() method :**
The first method returns the first element from the matched set of elements.

HTML

```js
<div class='.firstExample'>
   <p>This is first paragraph in a div.</p>
   <p>This is second paragraph in a div.</p>
   <p>This is third paragraph in a div.</p>
   <p>This is fourth paragraph in a div.</p>
   <p>This is fifth paragraph in a div.</p>
</div>

```

JQuery

```js
var firstParagraph = $("div p").first();
console.log("First paragraph:", firstParagraph.text());

```

Output:

```js
First paragraph: This is first paragraph in a div.

```



## Get next element


To get the next element you can use the [`.next()`](https://api.jquery.com/next/) method.

```js
<ul>
    <li>Mark</li>
    <li class="anna">Anna</li>
    <li>Paul</li>
</ul>

```

If you are standing on the "Anna" element and you want to get the next element, "Paul", the `.next()` method will allow you to do that.

```js
// "Paul" now has green text
$(".anna").next().css("color", "green");

```

The method takes an optional `selector` argument, which can be used if the next element must be a certain kind of element.

```js
// Next element is a "li", "Paul" now has green text
$(".anna").next("li").css("color", "green");

```

If the next element is not of the type `selector` then an empty set is returned, and the modifications will not do anything.

```js
// Next element is not a ".mark", nothing will be done in this case
$(".anna").next(".mark").css("color", "green");

```



## Get previous element


To get the previous element you can use the [`.prev()`](https://api.jquery.com/prev/) method.

```js
<ul>
    <li>Mark</li>
    <li class="anna">Anna</li>
    <li>Paul</li>
</ul>

```

If you are standing on the "Anna" element and you want to get the previous element, "Mark", the `.prev()` method will allow you to do that.

```js
// "Mark" now has green text
$(".anna").prev().css("color", "green");

```

The method takes an optional `selector` argument, which can be used if the previous element must be a certain kind of element.

```js
// Previous element is a "li", "Mark" now has green text
$(".anna").prev("li").css("color", "green");

```

If the previous element is not of the type `selector` then an empty set is returned, and the modifications will not do anything.

```js
// Previous element is not a ".paul", nothing will be done in this case
$(".anna").prev(".paul").css("color", "green");

```



## Filter a selection


To filter a selection you can use the [`.filter()`](https://api.jquery.com/filter/) method.

The method is called on a selection and returns a new selection. If the filter matches an element then it is added to the returned selection, otherwise it is ignored. If no element is matched then an empty selection is returned.

### The HTML

This is the HTML we will be using.

```js
<ul>
    <li class="zero">Zero</li>
    <li class="one">One</li>
    <li class="two">Two</li>
    <li class="three">Three</li>
</ul>

```

### Selector

Filtering using [selectors](http://stackoverflow.com/documentation/jquery/389/selectors#t=201607222002365660416) is one of the simpler ways to filter a selection.

```js
$("li").filter(":even").css("color", "green"); // Color even elements green
$("li").filter(".one").css("font-weight", "bold"); // Make ".one" bold

```

### Function

Filtering a selection using a [function](http://stackoverflow.com/documentation/javascript/186/functions#t=201607222035397089186) is useful if it is not possible to use selectors.

The function is called for each element in the selection. If it returns a `true` value then the element will be added to the returned selection.

```js
var selection = $("li").filter(function (index, element) {
    // "index" is the position of the element
    // "element" is the same as "this"
    return $(this).hasClass("two");
});
selection.css("color", "green"); // ".two" will be colored green

```

### Elements

You can filter by DOM elements. If the DOM elements are in the selection then they will be included in the returned selection.

```js
var three = document.getElementsByClassName("three");
$("li").filter(three).css("color", "green");

```

### Selection

You can also filter a selection by another selection. If an element is in both selections then it will be included in the returned selection.

```js
var elems = $(".one, .three");
$("li").filter(elems).css("color", "green");

```



## find() method


.find() method allows us to search through the descendants of these elements in the DOM tree and construct a new jQuery object from the matching elements.

HTML

```

    <div class="parent">
        <div class="children" name="first">
            <ul>
                <li>A1</li>
                <li>A2</li>
                <li>A3</li>
            </ul>
        </div>
        <div class="children" name="second">
            <ul>
                <li>B1</li>
                <li>B2</li>
                <li>B3</li>
            </ul>
        </div>
       </div>

```

jQuery

```

$('.parent').find('.children[name="second"] ul li').css('font-weight','bold');

```

Output

    - A1
    - A2
    - A3

    - **B1**
    - **B2**
    - **B3**

