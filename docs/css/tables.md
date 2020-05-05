---
metaTitle: "Tables"
description: "table-layout, empty-cells, border-collapse, border-spacing, caption-side"
---

# Tables

## table-layout

The `table-layout` property changes the algorithm that is used for the layout of a table.

Below an example of two tables both set to `width: 150px`:

[<img src="https://i.stack.imgur.com/wkJpO.png" alt="enter image description here" />](https://i.stack.imgur.com/wkJpO.png)

The table on the left has `table-layout: auto` while the one on the right has `table-layout: fixed`. The former is wider than the specified width (210px instead of 150px) but the contents fit. The latter takes the defined width of 150px, regardless if the contents overflow or not.

| Value    | Description                                                                                                                                                                                                 |
| -------- | ----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------- |
| **auto** | This is the default value. It defines the layout of the table to be determined by the contents of its' cells.                                                                                               |
| fixed    | This value sets the table layout to be determined by the width property provided to the table. If the content of a cell exceeds this width, the cell will not resize but instead, let the content overflow. |

## empty-cells

The `empty-cells` property determines if cells with no content should be displayed or not. This has no effect unless `border-collapse` is set to `separate`.

Below an example with two tables with different values set to the `empty-cells` property:

[<img src="http://i.stack.imgur.com/2G54T.png" alt="enter image description here" />](http://i.stack.imgur.com/2G54T.png)

The table on the left has `empty-cells: show` while the one on the right has `empty-cells: hide`. The former does display the empty cells whereas the latter does not.

| Value    | Description                                                              |
| -------- | ------------------------------------------------------------------------ |
| **show** | This is the default value. It shows cells even if they are empty.        |
| hide     | This value hides a cell altogether if there are no contents in the cell. |

More Information:

- [https://www.w3.org/TR/CSS21/tables.html#empty-cells](https://www.w3.org/TR/CSS21/tables.html#empty-cells)
- [https://developer.mozilla.org/en-US/docs/Web/CSS/empty-cells](https://developer.mozilla.org/en-US/docs/Web/CSS/empty-cells)
- [http://codepen.io/SitePoint/pen/yfhtq](http://codepen.io/SitePoint/pen/yfhtq)
- [https://css-tricks.com/almanac/properties/e/empty-cells/](https://css-tricks.com/almanac/properties/e/empty-cells/)

## border-collapse

The `border-collapse` property determines if a tables' borders should be separated or merged.

Below an example of two tables with different values to the `border-collapse` property:

[<img src="https://i.stack.imgur.com/awGlj.png" alt="enter image description here" />](https://i.stack.imgur.com/awGlj.png)

The table on the left has `border-collapse: separate` while the one on the right has `border-collapse: collapse`.

| Value    | Description                                                                             |
| -------- | --------------------------------------------------------------------------------------- |
| separate | This is the default value. It makes the borders of the table separate from each other.  |
| collapse | This value sets the borders of the table to merge together, rather than being distinct. |

## border-spacing

The `border-spacing` property determines the spacing between cells. This has no effect unless `border-collapse` is set to `separate`.

Below an example of two tables with different values to the `border-spacing` property:

[<img src="http://i.stack.imgur.com/KlVh0.png" alt="enter image description here" />](http://i.stack.imgur.com/KlVh0.png)

The table on the left has `border-spacing: 2px` (default) while the one on the right has `border-spacing: 8px`.

| Value             | Description                                                                         |
| ----------------- | ----------------------------------------------------------------------------------- |
| **<length>**      | This is the default behavior, though the exact value can vary between browsers.     |
| <length> <length> | This syntax allows specifying separate horizontal and vertical values respectively. |

## caption-side

The `caption-side` property determines the vertical positioning of the `<caption>` element within a table. This has no effect if such element does not exist.

Below an example with two tables with different values set to the `caption-side` property:

[<img src="http://i.stack.imgur.com/nmLkG.png" alt="enter image description here" />](http://i.stack.imgur.com/nmLkG.png)

The table on the left has `caption-side: top` while the one on the right has `caption-side: bottom`.

| Value   | Description                                                       |
| ------- | ----------------------------------------------------------------- |
| **top** | This is the default value. It places the caption above the table. |
| bottom  | This value places the caption below the table.                    |

#### Syntax

- table-layout: **auto** | fixed;
- border-collapse: **separate** | collapse;
- border-spacing: <length> | <length> <length>;
- empty-cells: **show** | hide;
- caption-side: **top** | bottom;

#### Remarks

These properties apply to both `<table>` elements (\*) and HTML elements displayed as `display: table` or `display: inline-table`

(\*) `<table>` elements are obviously natively styled by UA/browsers as `display: table`

HTML tables are semantically valid for tabular data. It is not recommended to use tables for layout. Instead, use CSS.
