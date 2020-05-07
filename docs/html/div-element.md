---
metaTitle: "HTML - Div Element"
description: "Basic usage, Nesting"
---

# Div Element


The div element in HTML is a container element that encapsulates other elements and can be used to group and separate parts of a webpage. A div by itself does not inherently represent anything but is a powerful tool in web design. This topic covers the purpose and applications of the div element.



## Basic usage


The `<div>` element usually has no specific semantic meaning by itself, simply representing a division, and is typically used for grouping and encapsulating other elements within an HTML document and separating those from other groups of content. As such, each `<div>` is best described by its contents.

```html
<div>
  <p>Hello! This is a paragraph.</p>
</div>

```

The `div` element is typically a [block-level element](http://stackoverflow.com/documentation/css/1473/layout-control/4804/the-display-property#t=201607220740232806457), meaning that it separates a block of an HTML document and occupying the maximum width of the page. Browsers typically have the following default CSS rule:

```css
div {
  display: block;
}

```

> 
<p>It's strongly encouraged by the **The World Wide Web Consortium (W3C)** to view the div element as an element of last resort, for when no other element is suitable.
The use of more appropriate elements instead of the div element leads to better accessibility for readers and easier maintainability for authors.</p>


For example, a blog post would be marked up using `<article>`, a chapter using `<section>`, a page's navigation aids using `<nav>`, and a group of form controls using `<fieldset>`.

div elements can be useful for stylistic purposes or to wrap multiple paragraphs within a section that are all to be annotated in a similar way.



## Nesting


It is a common practice to place multiple `<div>` inside another `<div>`. This is usually referred to as "nesting" elements and allows for further dividing elements into subsections or aid developers with CSS styling.

The `<div class="outer-div">` is used to group together two `<div class="inner-div">` elements; each containing a `<p>` element.

```html
<div class="outer-div">
  <div class="inner-div">
    <p>This is a paragraph</p>
  </div>
  <div class="inner-div">
    <p>This is another paragraph</p>
  </div>
</div>

```

This will yield the following result (CSS styles applied for clarity):

[<img src="https://i.stack.imgur.com/T58z4.png" alt="enter image description here" />](https://i.stack.imgur.com/T58z4.png)

**Nesting inline and block elements**
While nesting elements you should keep in mind, that there are inline and block elements.
while block elements "add a line break in the background", what means, other nested elements are shown in the next line automatically, inline elements can be positioned next to each other by default

**Avoid deep `<div>` nesting**

A deep and oftenly used nested container layouts shows a bad coding style.

Rounded corners or some similar functions often create such an HTML code. For most of the last generation browsers there are CSS3 counterparts. Try to use as little as possible HTML elements to increase the content to tag ratio and reduce page load, resulting in a better ranking in search engines.

`div` section Element should be not nested deeper than 6 layers.



#### Syntax


- `<div>example div</div>`

