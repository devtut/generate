---
metaTitle: "Inline-Block Layout"
description: "Justified navigation bar"
---

# Inline-Block Layout



## Justified navigation bar


The horizontally justified navigation (menu) bar has some number of items that should be justified. The first (left) item has no left margin within the container, the last (right) item has no right margin within the container. The distance between items is equal, independent on the individual item width.

### HTML

```css
<nav>
    <ul>
        <li>abc</li>
        <li>abcdefghijkl</li>
        <li>abcdef</li>
    </ul>
</nav>

```

### CSS

```css
nav {
    width: 100%;
    line-height: 1.4em;
}
ul {
    list-style: none;
    display: block;
    width: 100%;
    margin: 0;
    padding: 0;
    text-align: justify;
    margin-bottom: -1.4em;
}
ul:after {
    content: "";
    display: inline-block;
    width: 100%;
}
li {
    display: inline-block;
}

```

### Notes

- The `nav`, `ul` and `li` tags were chosen for their semantic meaning of 'a list of navigation (menu) items'. Other tags may also be used of course.
- The `:after` pseudo-element causes an extra 'line' in the `ul` and thus an extra, empty height of this block, pushing other content down. This is solved by the negative `margin-bottom`, which has to have the same magnitude as the `line-height` (but negative).
- If the page becomes too narrow for all the items to fit, the items will break to a new line (starting from the right) and be justified on this line. The total height of the menu will grow as needed.

