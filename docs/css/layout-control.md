---
metaTitle: "CSS - Layout Control"
description: "The display property, To get old table structure using div"
---

# Layout Control



## The display property


The `display` CSS property is fundamental for controlling the layout and flow of an HTML document. Most elements have a default `display` value of either `block` or `inline` (though some elements have other default values).

### Inline

An `inline` element occupies only as much width as necessary. It stacks horizontally with other elements of the same type and may not contain other non-inline elements.

```html
<span>This is some <b>bolded</b> text!</span>

```

[<img src="https://i.stack.imgur.com/tv9k8.png" alt="" />](https://i.stack.imgur.com/tv9k8.png)

As demonstrated above, two `inline` elements, `<span>` and `<b>`, are in-line (hence the name) and do not break the flow of the text.

### Block

A `block` element occupies the maximum available width of its' parent element. It starts with a new line and, in contrast to `inline` elements, it does not restrict the type of elements it may contain.

```html
<div>Hello world!</div><div>This is an example!</div>

```

[<img src="https://i.stack.imgur.com/MCTnB.png" alt="" />](https://i.stack.imgur.com/MCTnB.png)

The `div` element is block-level by default, and as shown above, the two `block` elements are vertically stacked and, unlike the `inline` elements, the flow of the text breaks.

### Inline Block

The `inline-block` value gives us the best of both worlds: it blends the element in with the flow of the text while allowing us to use `padding`, `margin`, `height` and similar properties which have no visible effect on `inline` elements.

Elements with this display value act as if they were regular text and as a result are affected by rules controlling the flow of text such as `text-align`. By default they are also shrunk to the the smallest size possible to accommodate their content.

```html
<!--Inline: unordered list-->
<style>
li {
    display : inline;
    background : lightblue;
    padding:10px;

    border-width:2px;
    border-color:black;
    border-style:solid;
    }
</style>    

<ul>
<li>First Element </li>
<li>Second Element </li>
<li>Third Element </li>
</ul>

```

[<img src="https://i.stack.imgur.com/eTy8E.png" alt="Display:inline" />](https://i.stack.imgur.com/eTy8E.png)

```html
<!--block: unordered list-->
<style>
li {
    display : block;
    background : lightblue;
    padding:10px;

    border-width:2px;
    border-color:black;
    border-style:solid;
    }
</style>    

<ul>
<li>First Element </li>
<li>Second Element </li>
<li>Third Element </li>
</ul>

```

[<img src="https://i.stack.imgur.com/fJErb.png" alt="Display:block" />](https://i.stack.imgur.com/fJErb.png)

```html
<!--Inline-block: unordered list-->
<style>
li {
    display : inline-block;
    background : lightblue;
    padding:10px; 

    border-width:2px;
    border-color:black;
    border-style:solid;
    }

</style>    

<ul>
<li>First Element </li>
<li>Second Element </li>
<li>Third Element </li>
</ul>

```

[<img src="https://i.stack.imgur.com/RNjHH.png" alt="Display:inline-block" />](https://i.stack.imgur.com/RNjHH.png)

### none

An element that is given the none value to its display property will not be displayed at all.

For example let's create a div-element that has an id of `myDiv`:

```html
<div id="myDiv"></div>

```

This can now be marked as not being displayed by the following CSS rule:

```css
#myDiv {
    display: none;
}

```

When an element has been set to be `display:none;` the browser ignores every other layout property for that specific element (both `position` and `float`). No box will be rendered for that element and its existence in html does not affect the position of following elements.

Note that this is different from setting the `visibility` property to `hidden`. Setting `visibility: hidden;` for an element would not display the element on the page but the element would still take up the space in the rendering process as if it would be visible. This will therefore affect how following elements are displayed on the page.

The `none` value for the display property is commonly used along with JavaScript to show or hide elements at will, eliminating the need to actually delete and re-create them.



## To get old table structure using div


This is the normal HTML table structure

```html
<style>
    table {
        width: 100%;
    }
</style>

<table>
  <tr>
    <td>
        I'm a table
    </td>
  </tr>
</table>

```

You can do same implementation like this

```html
<style>
    .table-div {
        display: table;
    }
    .table-row-div {
        display: table-row;
    }
    .table-cell-div {
        display: table-cell;
    }
</style>

<div class="table-div>
  <div class="table-row-div>
    <div class="table-cell-div>
      I behave like a table now
    </div>
  </div>
</div>

```



#### Syntax


- display: none | inline | block | list-item | inline-list-item | inline-block | inline-table | table | table-cell | table-column | table-column-group | table-footer-group | table-header-group | table-row | table-row-group | flex | inline-flex | grid | inline-grid | run-in | ruby | ruby-base | ruby-text | ruby-base-container | ruby-text-container | contents;



#### Parameters


|Value|Effect
|---|---|---|---
|`none`</td>|Hide the element and prevent it from occupying space.
|`block`</td>|Block element, occupy 100% of the available width, break after element.
|`inline`</td>|Inline element, occupy no width, no break after element.
|`inline-block`</td>|Taking special properties from both inline and block elements, no break, but can have width.
|`inline-flex`</td>|Displays an element as an inline-level flex container.
|`inline-table`</td>|The element is displayed as an inline-level table.
|`grid`</td>|Behaves like a block element and lays out its content according to the grid model.
|`flex`</td>|Behaves like a block element and lays out its content according to the flexbox model.
|`inherit`</td>|Inherit the value from the parent element.
|`initial`</td>|Reset the value to the default value taken from behaviors described in the HTML specifications or from the browser/user default stylesheet.
|`table`</td>|Behaves like the HTML `table` element.
|`table-cell`</td>|Let the element behave like a `<td>` element
|`table-column`</td>|Let the element behave like a `<col>` element
|`table-row`</td>|Let the element behave like a `<tr>` element
|`list-item`</td>|Let the element behave like a `<li>` element.

