---
metaTitle: "HTML - Classes and IDs"
description: "Giving an element a class, Giving an element an ID, Acceptable Values, Problems related to duplicated IDs"
---

# Classes and IDs


Classes and IDs make referencing HTML elements from scripts and stylesheets easier. The class attribute can be used on one or more tags and is used by CSS for styling. IDs however are intended to refer to a single element, meaning the same ID should never be used twice. IDs are generally used with JavaScript and internal document links, and are discouraged in CSS. This topic contains helpful explanations and examples regarding proper usage of class and ID attributes in HTML.



## Giving an element a class


Classes are identifiers for the elements that they are assigned to. Use the `class` attribute to assign a class to an element.

```html
<div class="example-class"></div>

```

To assign multiple classes to an element, separate the class names with spaces.

```html
<div class="class1 class2"></div>

```

### Using classes in CSS

Classes can be used for styling certain elements without changing all elements of that kind. For example, these two `span` elements can have completely different stylings:

```html
<span></span>
<span class="special"></span>

```

Classes of the same name can be given to any number of elements on a page and they will all receive the styling associated with that class. This will always be true unless you specify the element within the CSS.

For example, we have two elements, both with the class `highlight`:

```html
<div class="highlight">Lorem ipsum</div>
<span class="highlight">Lorem ipsum</span>

```

If our CSS is as below, then the color green will be applied to the text within both elements:

```html
.highlight { color: green; }

```

However, if we only want to target `div`'s with the class `highlight` then we can add specificity like below:

```html
div.highlight { color: green; }

```

Nevertheless, when styling with CSS, it is generally recommended that only classes (e.g. `.highlight`) be used rather than elements with classes (e.g. `div.highlight`).

As with any other selector, classes can can be nested:

```html
.main .highlight { color: red; } /* Descendant combinator */ 
.footer > .highlight { color: blue; } /* Child combinator */ 

```

You can also chain the class selector to only select elements that have a combination of several classes. For example, if this is our HTML:

```html
<div class="special left menu">This text will be pink</div>

```

And we want to colour this specific piece of text pink, we can do the following in our CSS:

```html
.special.left.menu { color: pink; } 

```



## Giving an element an ID


The ID attribute of an element is an identifier which must be unique in the whole document. Its purpose is to uniquely identify the element when linking (using an anchor), scripting, or styling (with CSS).

```html
<div id="example-id"></div>

```

You should not have two elements with the same ID in the same document, even if the attributes are attached to two different kinds of elements. For example, the following code is incorrect:

```html
<div id="example-id"></div>
<span id="example-id"></span>

```

Browsers will do their best to render this code, but unexpected behavior may occur when styling with CSS or adding functionality with JavaScript.

To reference elements by their ID in CSS, prefix the ID with `#`.

```html
#example-id { color: green; }

```

To jump to an element with an ID on a given page, append `#` with the element name in the URL.

```html
http://example.com/about#example-id

```

This feature is supported in most browsers and does not require additional JavaScript or CSS to work.



## Acceptable Values


### For an ID

The only restrictions on the value of an `id` are:

1. it must be unique in the document
1. it must not contain any space characters
1. it must contain at least one character

So the value can be all digits, just one digit, just punctuation characters, include special characters, whatever. Just no whitespace.

So these are valid:

```html
<div id="container"> ... </div>
<div id="999"> ... </div>
<div id="#%LV-||"> ... </div>
<div id="____V"> ... </div>
<div id="⌘⌥"> ... </div>
<div id="♥"> ... </div>
<div id="{}"> ... </div>
<div id="©"> ... </div>
<div id="♤₩¤☆€~¥"> ... </div>

```

This is invalid:

```html
<div id=" "> ... </div>

```

This is also invalid, when included in the same document:

```html
<div id="results"> ... </div>
<div id="results"> ... </div>

```

An `id` value must begin with a letter, which can then be followed only by:

- letters (A-Z/a-z)
- digits (0-9)
- hyphens ("-")
- underscores ("_")
- colons (":")
- periods (".")

Referring to the first group of examples in the HTML5 section above, only one is valid:

```html
<div id="container"> ... </div>

```

These are also valid:

```html
<div id="sampletext"> ... </div>
<div id="sample-text"> ... </div>
<div id="sample_text"> ... </div>
<div id="sample:text"> ... </div>
<div id="sample.text"> ... </div>

```

Again, if it doesn't start with a letter (uppercase or lowercase), it's not valid.

### For a Class

The rules for classes are essentially the same as for an `id`. The difference is that `class` values **do not** need to be unique in the document.

Referring to the examples above, although this is not valid in the same document:

```html
<div id="results"> ... </div>
<div id="results"> ... </div>

```

This is perfectly okay:

```html
<div class="results"> ... </div>
<div class="results"> ... </div>

```

### **Important Note: How ID and Class values are treated outside of HTML**

Keep in mind that the rules and examples above apply within the context of HTML.

Using numbers, punctuation or special characters in the value of an `id` or a `class` may cause trouble in other contexts, such as CSS, JavaScript and regular expressions.

For example, although the following `id` is valid in HTML5:

```html
<div id="9lions"> ... </div>

```

... it is invalid in CSS:

> 
[**4.1.3 Characters and case**](https://www.w3.org/TR/CSS21/syndata.html#characters)
<p>In CSS, **identifiers** (including element names, classes, and IDs in
selectors) can contain only the characters [a-zA-Z0-9] and ISO 10646
characters U+00A0 and higher, plus the hyphen (-) and the underscore
(_); <strong><em>they cannot start with a digit, two hyphens, or a hyphen
followed by a digit</em></strong>. <sub>(emphasis added)</sub></p>


In most cases you may be able to escape characters in contexts where they have restrictions or special meaning.

### **W3C References**

- [3.2.5.1 The `id` attribute](http://www.w3.org/TR/html5/dom.html#the-id-attribute)
- [3.2.5.7 The `class` attribute](http://www.w3.org/TR/html5/dom.html#classes)
- [6.2 SGML basic types](http://www.w3.org/TR/html4/types.html#type-id)



## Problems related to duplicated IDs


Having more than one element with the same ID is a hard to troubleshoot problem. The HTML parser will usually try to render the page in any case. Usually no error occurs. But the pace could end up in a mis-behaving web page.

In this example:

```html
<div id="aDiv">a</div>
<div id="aDiv">b</div>

```

CSS selectors still work

```html
#aDiv {
    color: red;
}

```

But JavaScript fails to handle both elements:

```html
var html = document.getElementById("aDiv").innerHTML;

```

In this case`html` variable bears only the first `div` content `("a")`.



#### Syntax


- class="class1 class2 class3"
- id="uniqueid"



#### Parameters


|Parameter|Details
|---|---|---|---|---|---|---|---|---|---
|class|Indicates the Class of the element (non-unique)
|id|Indicates the ID of the element (unique in the same context)



#### Remarks


- Both `class` and `id` are global attributes, and may therefore be assigned to any HTML element.
- Class names must begin with a letter (A-Z or a-z) and can be followed by letters, digits , hyphens and underscores.
- In `HTML5`, the class and id attributes can be used on any element. In HTML 4.0.1, they were off-limits to the `<base>`, `<head>`, `<html>`, `<meta>`, `<param>`, `<script>`, `<style>` and `<title>` tags.
- An element can have one or more classes. Classes are separated by spaces and cannot contain spaces themselves.
- An element can have only one ID and it must be unique within its context (i.e. a webpage). IDs also cannot contain spaces themselves.

