---
metaTitle: "Outlines"
description: "Overview, outline-style"
---

# Outlines

## Overview

Outline is a line that goes around the element, outside of the border. In contrast to `border`, outlines do not take any space in the box model. So adding an outline to an element does not affect the position of the element or other elements.

In addition, outlines can be non-rectangular in some browsers. This can happen if `outline` is applied on a `span` element that has text with different `font-size` properties inside it. Unlike borders, outlines **cannot** have rounded corners.

The essential parts of `outline` are `outline-color`, `outline-style` and `outline-width`.

The definition of an outline is equivalent to the definition of a border:

> An outline is a line around an element. It is displayed around the margin of the element. However, it is different from the border property.

```css
outline: 1px solid black;
```

## outline-style

The `outline-style` property is used to set the style of the outline of an element.

```css
p {
  border: 1px solid black;
  outline-color: blue;
  line-height: 30px;
}
.p1 {
  outline-style: dotted;
}
.p2 {
  outline-style: dashed;
}
.p3 {
  outline-style: solid;
}
.p4 {
  outline-style: double;
}
.p5 {
  outline-style: groove;
}
.p6 {
  outline-style: ridge;
}
.p7 {
  outline-style: inset;
}
.p8 {
  outline-style: outset;
}
```

**HTML**

```css
<p class="p1">A dotted outline</p>
<p class="p2">A dashed outline</p>
<p class="p3">A solid outline</p>
<p class="p4">A double outline</p>
<p class="p5">A groove outline</p>
<p class="p6">A ridge outline</p>
<p class="p7">An inset outline</p>
<p class="p8">An outset outline</p>

```

[<img src="https://i.stack.imgur.com/KHAoO.png" alt="enter image description here" />](https://i.stack.imgur.com/KHAoO.png)

#### Syntax

- outline: outline-color outline-style outline-width | initial | inherit;
- outline-width: medium | thin | thick | length | initial | inherit;
- outline-style: none | hidden | dotted | dashed | solid | double | groove | ridge | inset | outset | initial | inherit;

#### Parameters

| Parameter | Details                                                |
| --------- | ------------------------------------------------------ |
| dotted    | dotted outline                                         |
| dashed    | dashed outline                                         |
| solid     | solid outline                                          |
| double    | double outline                                         |
| groove    | 3D grooved outline, depends on the outline-color value |
| ridge     | 3D ridged outline, depends on the outline-color value  |
| inset     | 3D inset outline, depends on the outline-color value   |
| outset    | 3D outset outline, depends on the outline-color value  |
| none      | no outline                                             |
| hidden    | hidden outline                                         |

#### Remarks

`outline` is now described in [Basic UI](https://www.w3.org/TR/css-ui-3/#outline-props), a CSS Module Level 3 (it was already described in REC CSS2.1)

Outline property is defined by default in browsers for focusable elements in **`:focus`** state.<br />
It shouldn't be removed, see [http://outlinenone.com](http://outlinenone.com) which states:

> **What does the outline property do?**
> It provides visual feedback for links that have "focus" when navigating a web document using the TAB key (or equivalent). This is especially useful for folks who can't use a mouse or have a visual impairment. If you remove the outline you are making your site inaccessible for these people. (…)

Interesting related examples on Stack Overflow:

- [How to remove the border highlight on an input text element](http://stackoverflow.com/questions/1457849/how-to-remove-the-border-highlight-on-an-input-text-element?rq=1)
- [How to remove Firefox&#39;s dotted outline on BUTTONS as well as links?](http://stackoverflow.com/questions/71074/how-to-remove-firefoxs-dotted-outline-on-buttons-as-well-as-links?rq=1)
