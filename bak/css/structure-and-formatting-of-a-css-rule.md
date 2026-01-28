---
metaTitle: "CSS - Structure and Formatting of a CSS Rule"
description: "Property Lists, Rules, Selectors, and Declaration Blocks, Multiple Selectors"
---

# Structure and Formatting of a CSS Rule



## Property Lists


Some properties can take multiple values, collectively known as a **property list**.

```css
/* Two values in this property list */
span {
    text-shadow: yellow 0 0 3px, green 4px 4px 10px;
}

/* Alternate Formatting */
span {
    text-shadow:
        yellow 0 0 3px,
        green 4px 4px 10px;
}

```



## Rules, Selectors, and Declaration Blocks


A CSS **rule** consists of a **selector** (e.g. `h1`) and **declaration block** (`{}`).

```css
h1 {}

```



## Multiple Selectors


When you group CSS selectors, you apply the same styles to several different elements without repeating the styles in your style sheet. Use a comma to separate multiple grouped selectors.

```css
div, p { color: blue }

```

So the blue color applies to all `<div>` elements and all `<p>` elements. Without the comma only `<p>` elements that are a child of a `<div>` would be red.

This also applies to all types of selectors.

```css
p, .blue, #first, div span{ color : blue }

```

This rule applies to:

- `<p>`
- elements of the `blue` class
- element with the ID `first`
- every `<span>` inside of a `<div>`



#### Remarks


For ease of readability, keep all declarations indented one level from their selector, and the closing curly brace on its own line. Add a single space after selectors and colons, and always place a semicolon after the final declaration.

### Good

```css
p {
    color: maroon;
    font-size: 16px;
}

```

### Bad

```css
p{
 color: maroon;
font-size:16px }

```

### One-Liner

If there are only one or two declarations, you **might** get away with this one. Not recommended for most cases. Always be consistent when possible.

```css
p { color: maroon; font-size: 16px; }

```

