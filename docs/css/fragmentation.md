---
metaTitle: "Fragmentation"
description: "Media print page-break"
---

# Fragmentation



## Media print page-break


```css
@media print {
  p {
    page-break-inside: avoid;
  }
  h1 { 
    page-break-before: always;
  }
  h2 {
    page-break-after: avoid;
  }
}

```

This code does 3 things:

- it prevents a page break inside any p tags, meaning a paragraph will never be broken in two pages, if possible.
- it forces a page-break-before in all h1 headings, meaning that before every h1 occurrence, there will be a page break.
- it prevents page-breaks right after any h2



#### Syntax


<li>page-break-after: auto | always | avoid | left | right | initial |
inherit;</li>
<li>page-break-before: auto | always | avoid | left | right | initial |
inherit;</li>
- page-break-inside: auto | avoid | initial | inherit;



#### Parameters


|Value|Description
|------
|auto|Default. Automatic page breaks
|always|Always insert a page break
|avoid|Avoid page break (if possible)
|left|Insert page breaks so that the next page is formatted as a left page
|right|Insert page breaks so that the next page is formatted as a right page
|initial|Sets this property to its default value.
|inherit|Inherits this property from its parent element.



#### Remarks


There is no page-break property in CSS. Only the 3 properties (**page-break-before**, **page-break-after**, **page-break-inside**).

Related: `orphans`, `widows`.

