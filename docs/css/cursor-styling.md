---
metaTitle: "Cursor Styling"
description: "Changing cursor type, pointer-events, caret-color"
---

# Cursor Styling

## Changing cursor type

```css
cursor: value;
```

[<img src="http://i.stack.imgur.com/E76ws.png" alt="visualization" />](http://i.stack.imgur.com/E76ws.png)

**Examples:**

| Value   | Description                                   |
| ------- | --------------------------------------------- |
| none    | No cursor is rendered for the element         |
| auto    | Default. The browser sets a cursor            |
| help    | The cursor indicates that help is available   |
| wait    | The cursor indicates that the program is busy |
| move    | The cursor indicates something is to be moved |
| pointer | The cursor is a pointer and indicates a link  |

## pointer-events

The pointer-events property allows for control over how HTML elements respond to mouse/touch events.

```css
.disabled {
  pointer-events: none;
}
```

In this example,

>

<p>'none' prevents all click, state and cursor options on the specified HTML
element [[1]]</p>

Other valid values for HTMl elements are:

- auto;
- inherit.

1. [https://css-tricks.com/almanac/properties/p/pointer-events/](https://css-tricks.com/almanac/properties/p/pointer-events/)

Other resources:

<li>
[https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events](https://developer.mozilla.org/en-US/docs/Web/CSS/pointer-events)
</li>
<li>
[https://davidwalsh.name/pointer-events](https://davidwalsh.name/pointer-events)
</li>

## caret-color

The caret-color CSS property specifies the color of the caret, the visible indicator of the insertion point in an element where text and other content is inserted by the user's typing or editing.

HTML

```css
<input id="example" />

```

CSS

```css
#example {
  caret-color: red;
}
```

Resources:

- [https://developer.mozilla.org/en-US/docs/Web/CSS/caret-color](https://developer.mozilla.org/en-US/docs/Web/CSS/caret-color)

#### Syntax

- cursor: auto | default | none | context-menu | help | pointer | progress | wait | cell | crosshair | text | vertical-text | alias | copy | move | no-drop | not-allowed | e-resize | n-resize | ne-resize | nw-resize | s-resize | se-resize | sw-resize | w-resize | ew-resize | ns-resize | nesw-resize | nwse-resize | col-resize | row-resize | all-scroll | zoom-in | zoom-out | grab | grabbing;
