---
metaTitle: "Grid"
description: "Basic Example"
---

# Grid

Grid layout is a new and powerful CSS layout system that allows to divide a web page content into rows and columns in an easy way.

## Basic Example

| Property | Possible Values    |
| -------- | ------------------ |
| display  | grid / inline-grid |

The CSS Grid is defined as a display property. It applies to a parent element and its immediate children only.

Consider the following markup:

```html
<section class="container">
  <div class="item1">item1</div>
  <div class="item2">item2</div>
  <div class="item3">item3</div>
  <div class="item4">item4</div>
</section>
```

The easiest way to define the markup structure above as a grid is to simply set its `display` property to `grid`:

```css
.container {
  display: grid;
}
```

However, doing this will invariably cause all the child elements to collapse on top of one another. This is because the children do not currently know how to position themselves within the grid. But we can explicitly tell them.

First we need to tell the grid element `.container` how many rows and columns will make up its structure and we can do this using the `grid-columns` and `grid-rows` properties (note the pluralisation):

```css
.container {
  display: grid;
  grid-columns: 50px 50px 50px;
  grid-rows: 50px 50px;
}
```

However, that still doesn't help us much because we need to give an order to each child element. We can do this by specifying the `grid-row` and `grid-column` values which will tell it where it sits in the grid:

```css
.container .item1 {
  grid-column: 1;
  grid-row: 1;
}
.container .item2 {
  grid-column: 2;
  grid-row: 1;
}
.container .item3 {
  grid-column: 1;
  grid-row: 2;
}
.container .item4 {
  grid-column: 2;
  grid-row: 2;
}
```

By giving each item a column and row value it identifies the items order within the container.

View a working example on [JSFiddle](https://jsfiddle.net/fexfwkkv/3/). You'll need to view this in IE10, IE11 or Edge for it to work as these are currently the only browsers supporting Grid Layout (with vendor prefix `-ms-`) or enable a flag in Chrome, Opera and Firefox according to [caniuse](http://caniuse.com/#feat=css-grid) in order to test with them.

#### Remarks

[CSS Grid Layout Module Level 1](https://www.w3.org/TR/css-grid-1/) is, as of 9 September 2016, a W3C Candidate Recommendation. It is considered to be in the Testing stage ([https://www.w3.org/Style/CSS/current-work)](https://www.w3.org/Style/CSS/current-work)).

As of 3 July 2017, Microsoft's Internet Explorer 10 and 11 and Edge browsers only support an older version of the specification using a vendor prefix.
