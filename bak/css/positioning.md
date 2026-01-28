---
metaTitle: "CSS - Positioning"
description: "Overlapping Elements with z-index, Absolute Position, Fixed position, Relative Position, Static positioning"
---

# Positioning

## Overlapping Elements with z-index

To change the default [stack order](https://developer.mozilla.org/en-US/docs/Web/CSS/CSS_Positioning/Understanding_z_index/The_stacking_context) positioned elements (`position` property set to `relative`, `absolute` or `fixed`), use the `z-index` property.

The higher the z-index, the higher up in the stacking context (on the z-axis) it is placed.

### Example

In the example below, a z-index value of 3 puts green on top, a z-index of 2 puts red just under it, and a z-index of 1 puts blue under that.

### HTML

```html
<div id="div1"></div>
<div id="div2"></div>
<div id="div3"></div>
```

### CSS

```css
div {
  position: absolute;
  height: 200px;
  width: 200px;
}
div#div1 {
  z-index: 1;
  left: 0px;
  top: 0px;
  background-color: blue;
}
div#div2 {
  z-index: 3;
  left: 100px;
  top: 100px;
  background-color: green;
}
div#div3 {
  z-index: 2;
  left: 50px;
  top: 150px;
  background-color: red;
}
```

This creates the following effect:

<img src="https://i.imgur.com/rhzQmfd.png" alt="green on top of red, red on top of blue" />

See a working example at [JSFiddle](https://jsfiddle.net/esnc10tq/).

### Syntax

```css
z-index: [ number ] | auto;
```

| Parameter | Details                                                                                                                    |
| --------- | -------------------------------------------------------------------------------------------------------------------------- |
| `number`  | An integer value. A higher number is higher on the `z-index` stack. `0` is the default value. Negative values are allowed. |
| `auto`    | Gives the element the same stacking context as its parent. (**Default**)                                                   |

### Remarks

All elements are laid out in a 3D axis in CSS, including a depth axis, measured by the `z-index` property. `z-index` only works on positioned elements: (see: [Why does z-index need a defined position to work?](https://www.sitepoint.com/community/t/why-does-z-index-need-a-defined-position-to-work/46115)). The only value where it is ignored is the default value, `static`.

Read about the z-index property and Stacking Contexts in the [CSS Specification](https://drafts.csswg.org/css-position/#layered-presentation) on layered presentation and at the [Mozilla Developer Network](https://developer.mozilla.org/en-US/docs/Web/CSS/z-index).

## Absolute Position

When absolute positioning is used the box of the desired element is taken out of the **Normal Flow** and it no longer affects the position of the other elements on the page. Offset properties:

1. top
1. left
1. right
1. bottom

specify the element should appear in relation to its next non-static containing element.

```css
.abspos {
  position: absolute;
  top: 0px;
  left: 500px;
}
```

This code will move the box containing element with attribute `class="abspos"` down 0px and right 500px relative to its containing element.

## Fixed position

Defining position as fixed we can remove an element from the document flow and set its position relatively to the browser window. One obvious use is when we want something to be visible when we scroll to the bottom of a long page.

```css
#stickyDiv {
  position: fixed;
  top: 10px;
  left: 10px;
}
```

## Relative Position

Relative positioning moves the element in relation to where it would have been in **normal flow** .Offset properties:

1. top
1. left
1. right
1. bottom

are used to indicate how far to move the element from where it would have been in normal flow.

```css
.relpos {
  position: relative;
  top: 20px;
  left: 30px;
}
```

This code will move the box containing element with attribute class="relpos" 20px down and 30px to the right from where it would have been in normal flow.

## Static positioning

The default position of an element is `static`. To quote [MDN](https://developer.mozilla.org/en-US/docs/Web/CSS/position#values):

> This keyword lets the element use the normal behavior, that is it is laid out in its current position in the flow. The top, right, bottom, left and z-index properties do not apply.

```css
.element {
  position: static;
}
```

#### Syntax

- position: static|absolute|fixed|relative|sticky|initial|inherit|unset;
- z-index: auto|**number**|initial|inherit;

#### Parameters

| Parameter | Details                                                                                                                                                  |
| --------- | -------------------------------------------------------------------------------------------------------------------------------------------------------- |
| static    | Default value. Elements render in order, as they appear in the document flow. The top, right, bottom, left and z-index properties do not apply.          |
| relative  | The element is positioned relative to its normal position, so `left:20px` adds 20 pixels to the element's LEFT position                                  |
| fixed     | The element is positioned relative to the browser window                                                                                                 |
| absolute  | The element is positioned relative to its first positioned (not static) ancestor element                                                                 |
| initial   | Sets this property to its default value.                                                                                                                 |
| inherit   | Inherits this property from its parent element.                                                                                                          |
| sticky    | Experimental feature. It behaves like `position: static` within its parent until a given offset threshold is reached, then it acts as `position: fixed`. |
| unset     | Combination of initial and inherit. More info [here](https://developer.mozilla.org/en-US/docs/Web/CSS/unset).                                            |

#### Remarks

**Normal Flow** is the flow of elements if the position of element is **static**.

1. defining **width** is beneficial because in some cases it prevents overlapping of element's content.
