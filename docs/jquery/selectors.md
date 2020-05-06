---
metaTitle: "jQuery - Selectors"
description: "Types of Selectors, Overview, Caching Selectors, Combining selectors, DOM Elements as selectors, HTML strings as selectors"
---

# Selectors


A jQuery selectors selects or finds a DOM (document object model) element in an HTML document. It is used to select HTML elements based on id, name, types, attributes, class and etc. It is based on existing CSS selectors.



## Types of Selectors


In jQuery you can select elements in a page using many various properties of the element, including:

- Type
- Class
- ID
- Possession of Attribute
- Attribute Value
- Indexed Selector
- [Pseudo-state](https://developer.mozilla.org/en-US/docs/Web/CSS/Pseudo-classes)

If you know [CSS selectors](http://stackoverflow.com/documentation/css/611/selectors#t=201607220821542728451) you will notice selectors in jQuery are the same (with minor exceptions).

Take the following HTML for example:

```js
<a href="index.html"></a>                  <!-- 1 -->
<a id="second-link"></a>                   <!-- 2 -->
<a class="example"></a>                    <!-- 3 -->
<a class="example" href="about.html"></a>  <!-- 4 -->
<span class="example"></span>              <!-- 5 -->

```

**Selecting by Type:**

The following jQuery selector will select all `<a>` elements, including 1, 2, 3 and 4.

```js
$("a")

```

**Selecting by Class**

The following jQuery selector will select all elements of class `example` (including non-a elements), which are 3, 4 and 5.

```js
$(".example")

```

**Selecting by ID**

The following jQuery selector will select the element with the given ID, which is 2.

```js
$("#second-link")

```

**Selecting by Possession of Attribute**

The following jQuery selector will select all elements with a defined `href` attribute, including 1 and 4.

```js
$("[href]")

```

**Selecting by Attribute Value**

The following jQuery selector will select all elements where the `href` attribute exists with a value of `index.html`, which is just 1.

```js
$("[href='index.html']")

```

**Selecting by Indexed Position (**Indexed Selector**)**

The following jQuery selector will select only 1, the second `<a>` ie. the `second-link` because index supplied is `1` like `eq(1)` (Note that the index starts at `0` hence the second got selected here!).

```js
$("a:eq(1)")

```

**Selecting with Indexed Exclusion**

To exclude an element by using its index `:not(:eq())`

The following selects `<a>` elements, except that with the class `example`, which is 1

```js
$("a").not(":eq(0)")

```

**Selecting with Exclusion**

To exclude an element from a selection, use `:not()`

The following selects `<a>` elements, except those with the class `example`, which are 1 and 2.

```js
$("a:not(.example)")

```

**Selecting by Pseudo-state**

You can also select in jQuery using pseudo-states, including `:first-child`, `:last-child`, `:first-of-type`, `:last-of-type`, etc.

The following jQuery selector will only select the first `<a>` element: number 1.

```js
$("a:first-of-type")

```

**Combining jQuery selectors**

You can also increase your specificity by combining multiple jQuery selectors; you can combine any number of them or combine all of them. You can also select multiple classes, attributes and states at the same time.

```js
$("a.class1.class2.class3#someID[attr1][attr2='something'][attr3='something']:first-of-type:first-child")

```

This would select an `<a>` element that:

- Has the following classes: `class1, class2, and class3`
- Has the following ID: `someID`
- Has the following Attribute: `attr1`
- Has the following Attributes and values: `attr2` with value `something`, `attr3` with value `something`
- Has the following states: `first-child` and `first-of-type`

You can also separate different selectors with a comma:

```js
$("a, .class1, #someID")  

```

This would select:

- All `<a>` elements
- All elements that have the class `class1`
- An element with the id `#someID`

**Child and Sibling selection**

jQuery selectors generally conform to the same conventions as CSS, which allows you to select children and siblings in the same way.

- To select a non-direct child, use a space
- To select a direct child, use a `>`
- To select an adjacent sibling following the first, use a `+`
- To select a non-adjacent sibling following the first, use a `~`

**Wildcard selection**

There might be cases when we want to select all elements but there is not a common property to select upon (class, attribute etc). In that case we can use the `*` selector that simply selects all the elements:

```js
$('#wrapper *')    // Select all elements inside #wrapper element

```



## Overview


Elements can be selected by jQuery using [jQuery Selectors](https://api.jquery.com/category/selectors/). The function returns either an element or a list of elements.

### Basic selectors

```js
$("*")                       // All elements
$("div")                     // All <div> elements
$(".blue")                   // All elements with class=blue
$(".blue.red")               // All elements with class=blue AND class=red
$(".blue,.red")              // All elements with class=blue OR class=red
$("#headline")               // The (first) element with id=headline
$("[href]")                  // All elements with an href attribute
$("[href='example.com']")    // All elements with href=example.com

```

### Relational operators

```js
$("div span")          // All <span>s that are descendants of a <div>
$("div > span")        // All <span>s that are a direct child of a <div>
$("a ~ span")          // All <span>s that are siblings following an <a>
$("a + span")          // All <span>s that are immediately after an <a>

```



## Caching Selectors


Each time you use a selector in jQuery the DOM is searched for elements that match your query. Doing this too often or repeatedly will decrease performance. If you refer to a specific selector more than once you should add it to the cache by assigning it to a variable:

```js
var nav = $('#navigation');
nav.show();

```

This would replace:

```js
$('#navigation').show();

```

Caching this selector could prove helpful if your website needs to show/hide this element often. If there are multiple elements with the same selector the variable will become an array of these elements:

```js
<div class="parent">
    <div class="child">Child 1</div>
    <div class="child">Child 2</div>
</div>

<script>
    var children = $('.child');
    var firstChildText = children[0].text();
    console.log(firstChildText); 
    
    // output: "Child 1"
</script>

```

**NOTE:** The element has to exist in the DOM at the time of its assignment to a variable. If there is no element in the DOM with a class called `child` you will be storing an empty array in that variable.

```js
<div class="parent"></div>

<script>
    var parent   = $('.parent');
    var children = $('.child');
    console.log(children);

    // output: []

    parent.append('<div class="child">Child 1</div>');
    children = $('.child');
    console.log(children[0].text());

    // output: "Child 1"
</script>

```

Remember to reassign the selector to the variable after adding/removing elements in the DOM with that selector.

**Note**: When caching selectors, many developers will start the variable name with a `$` to denote that the variable is a jQuery object like so:

```js
var $nav = $('#navigation');
$nav.show();

```



## Combining selectors


Consider following DOM Structure

```js
<ul class="parentUl">
    <li> Level 1
        <ul class="childUl">
            <li>Level 1-1 <span> Item - 1 </span></li>
            <li>Level 1-1 <span> Item - 2 </span></li>
        </ul>
    </li>
    <li> Level 2
        <ul class="childUl">
            <li>Level 2-1 <span> Item - 1 </span></li>
            <li>Level 2-1 <span> Item - 1 </span></li>
        </ul>
    </li>
</ul>

```

### Descendant and child selectors

Given a parent `<ul>` - `parentUl` find its descendants (`<li>`),

<li>
Simple **`$('parent child')`**
>> `$('ul.parentUl li')`
This gets all matching descendants of the specified ancestor **all levels down**.
</li>
<li>
`>` - **`$('parent > child')`**
>> `$('ul.parentUl > li')`
This finds all matching children (**only 1st level down**).
</li>
<li>
Context based selector - **`$('child','parent')`**
>> `$('li','ul.parentUl')`
This works same as 1. above.
</li>
<li>
`find()` - **`$('parent').find('child')`**
>> `$('ul.parentUl').find('li')`
This works same as 1. above.
</li>
<li>
`children()` - **`$('parent').find('child')`**
>> `$('ul.parentUl').children('li')`
This works same as 2. above.
</li>

### Other combinators

### Group Selector : ","

Select all `<ul>` elements AND all `<li>` elements AND all `<span>` elements :

```js
$('ul, li, span')

```

### Multiples selector : "" (no character)

Select all `<ul>` elements with class `parentUl` :

```js
$('ul.parentUl')

```

### Adjacent Sibling Selector : "+"

Select all `<li>` elements that are placed immediately after another `<li>` element:

```js
$('li + li')

```

### General Sibling Selector : "~"

Select all `<li>` elements that are siblings of other `<li>` elements:

```js
$('li ~ li')

```



## DOM Elements as selectors


jQuery accepts a wide variety of parameters, and one of them is an actual DOM element. Passing a DOM element to jQuery will cause the underlying array-like structure of the [jQuery object](http://stackoverflow.com/documentation/jquery/211/getting-started-with-jquery/23283/the-jquery-object) to hold that element.

jQuery will detect that the argument is a DOM element by inspecting its nodeType.

The most common use of a DOM element is in callbacks, where the current element is passed to the jQuery constructor in order to gain access to the jQuery API.

Such as in the `each` callback (note: each is an iterator function).

```js
$(".elements").each(function(){
    //the current element is bound to `this` internally by jQuery when using each
    var currentElement = this;

    //at this point, currentElement (or this) has access to the Native API
    
    //construct a jQuery object with the currentElement(this)
    var $currentElement = $(this);

    //now $currentElement has access to the jQuery API
});

```



## HTML strings as selectors


jQuery accepts a wide variety of parameters as "selectors", and one of them is an HTML string. Passing an HTML string to jQuery will cause the underlying array-like structure of the [jQuery object](http://stackoverflow.com/documentation/jquery/211/getting-started-with-jquery/23283/the-jquery-object) to hold the resulting constructed HTML.

jQuery uses regex to determine if the string being passed to the constructor is an HTMLstring, and also that it **must** start with `<`. That regex is defined as `rquickExpr = /^(?:\s*(<[\w\W]+>)[^>]*|#([\w-]*))$/` ([explanation at regex101.com](https://regex101.com/r/wF0rZ0/1)).

The most common use of an HTML string as a selector is when sets of DOM elements need to be created in code only, often this is used by libraries for things like Modal popouts.

For example, a function which returned an anchor tag wrapped in a div as a template

```js
function template(href,text){
    return $("<div><a href='" + href + "'>" + text + "</a></div>");
}

```

Would return a jQuery object holding

```js
<div>
    <a href="google.com">Google</a>
</div>

```

if called as `template("google.com","Google")`.



#### Syntax


- Tag: No marker, use the tag directly
- Id: `#id`
- Class: `.className`
- Attribute: `[attributeName]`
- Attribute with value: `[attributeName ='value']`
- Attribute starts with value `^=`: `[attributeName ^= 'value']`
- Attribute ends with value `$=`: `[attributeName $='value']`
- Attribute contains value `*=` : `[attributeName *= 'value']`
- Pseudo-selector: `:pseudo-selector`
- Any descendant: Space between selectors
- Direct children: `>` between selectors
- Adjacent sibling following the first: `+`
- Non-adjacent sibling following the first: `~`
- Or: `,` (comma) between selector



#### Remarks


When writing `selectors` for `class` or `id` or `attribute` which contains some special characters like

`!` `"` `#` `$` `%` `&` `'` `(` `)` `*` `+` `,` `.` `/` `:` `;` `<` `=` `>` `?` `@` `[` `\` `]` `^`  `{` `|` `}` `~`

the characters need to be escaped using two backslashes `\\` .

eg.

```js
<span id="temp.foobar"><span>

```

the selectors will be framed like,

```js
$('#temp\\.foobar')

```

