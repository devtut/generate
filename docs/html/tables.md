---
metaTitle: "HTML - Tables"
description: "Simple Table, Spanning columns or rows, Column Groups, Table with thead, tbody, tfoot, and caption, Heading scope"
---

# Tables


The HTML `<table>` element allows web authors to display tabular data (such as text, images, links, other tables, etc.) in a two dimensional table with rows and columns of cells.



## Simple Table


```html
<table>
  <tr>
    <th>Heading 1/Column 1</th>
    <th>Heading 2/Column 2</th>
  </tr>
  <tr>
    <td>Row 1 Data Column 1</td>
    <td>Row 1 Data Column 2</td>
  </tr>
  <tr>
    <td>Row 2 Data Column 1</td>
    <td>Row 2 Data Column 2</td>
  </tr>
</table>

```

This will render a `<table>` consisting of three total rows (`<tr>`): one row of header cells (`<th>`) and two rows of content cells (`<td>`). `<th>` elements are **tabular headers** and `<td>` elements are **tabular data**. You can put whatever you want inside a `<td>` or `<th>`.
    |Heading 1/Column 1    |Heading 2/Column 2  
    |Row 1 Data Column 1    |Row 1 Data Column 2  
    |Row 2 Data Column 1    |Row 2 Data Column 2  



## Spanning columns or rows


Table cells can span multiple columns or rows using the `colspan` and `rowspan` attributes. These attributes can be applied to `<th>` and `<td>` elements.

```html
<table>
    <tr>
        <td>row 1 col 1</td>
        <td>row 1 col 2</td>
        <td>row 1 col 3</td>
    </tr>
    <tr>
        <td colspan="3">This second row spans all three columns</td>
    </tr>
    <tr>
        <td rowspan="2">This cell spans two rows</td>
        <td>row 3 col 2</td>
        <td>row 3 col 3</td>
    </tr>
    <tr>
        <td>row 4 col 2</td>
        <td>row 4 col 3</td>
    </tr>

</table>

```

Will result in<br />
[<img src="http://i.stack.imgur.com/voMBJ.jpg" alt="enter image description here" />](http://i.stack.imgur.com/voMBJ.jpg)

Note that you should not design a table where both rows and columns overlap as this is invalid HTML and the result is handled differently by different web browsers.

`rowspan` = A non-negative integer that specifies the number of rows spanned by a cell.  The default value of this attribute is one (`1`).  A value of zero (`0`) means that the cell will extend from the current row until the last row of the table (`<thead>`, `<tbody>`, or `<tfoot>`).

`colspan` = A non-negative integer that specifies the number of columns spanned by the current cell. The default value of this attribute is one (`1`). A value of zero (`0`) means that the cell will extend from the current to the last column of the column group `<colgroup>` in which the cell is defined.



## Column Groups


Sometimes you may want to apply styling to a column or group of columns.  Or for semantic purposes, you may want to group columns together.  To do this, use `<colgroup>` and `<col>` elements.

The optional `<colgroup>` tag allows you to group columns together.  `<colgroup>` elements must be child elements of a `<table>` and must come after any `<caption>` elements and before any table content (e.g., `<tr>`, `<thead>`, `<tbody>`, etc.).

```html
<table>
    <colgroup span="2"></colgroup>
    <colgroup span="2"></colgroup>
    ...
</table>

```

The optional `<col>` tag allows you to reference individual columns or a range of columns without applying a logical grouping.  `<col>` elements are optional, but if present, they must be inside a `<colgroup>` element.

```html
<table>
    <colgroup>
        <col id="MySpecialColumn" />
        <col />
    </colgroup>
    <colgroup>
        <col class="CoolColumn" />
        <col class="NeatColumn" span="2" />
    </colgroup>
    ...
</table>

```

The following CSS styles can be applied to `<colgroup>` and `<col>` elements:

<li>
`border`
</li>
<li>
`background`
</li>
<li>
`width`
</li>
<li>
`visibility`
</li>
<li>
`display` (as in `display: none`)
<ul>
- `display: none;` will actually remove the columns from the display, causing the table to render as if those cells don't exist

For more information, see [HTML5 Tabular data](https://www.w3.org/TR/html5/tabular-data.html).



## Table with thead, tbody, tfoot, and caption


HTML also provides the tables with the `<thead>`, `<tbody>`, `<tfoot>`, and `<caption>` elements. These additional elements are useful for adding semantic value to your tables and for providing a place for separate CSS styling.

When printing out a table that doesn't fit onto one (paper) page, most browsers repeat the contents of `<thead>` on every page.

There's a specific order that must be adhered to, and we should be aware that not every element falls into place as one would expect. The following example demonstrates how our 4 elements should be placed.

```html
<table>
 <caption>Table Title</caption> <!--| caption is the first child of table |-->
  <thead> <!--======================| thead is after caption |-->
    <tr>
      <th>Header content 1</th> 
      <th>Header content 2</th>
    </tr>
  </thead>

  <tbody> <!--======================| tbody is after thead |-->
    <tr>
      <td>Body content 1</td>
      <td>Body content 2</td>
    </tr>
  </tbody>
  
  <tfoot><!--| tfoot can be placed before or after tbody, but not in a group of tbody. |-->         <!--| Regardless where tfoot is in markup, it's rendered at the bottom. |-->                                                                                                                                                                                                                                                                                               
    <tr>
      <td>Footer content 1</td>
      <td>Footer content 2</td>
    </tr>
  </tfoot>

</table>

```

The following example's results are demonstrated twice--the first table lacks any styles, the second table has a few CSS properties applied: `background-color`, `color`, and `border`*. The styles are provided as a visual guide and is not an essential aspect of the topic at hand.

[<img src="http://i.stack.imgur.com/vXlAo.png" alt="Table Example" />](http://i.stack.imgur.com/vXlAo.png)

[<img src="http://i.stack.imgur.com/75a9z.png" alt="Table Example" />](http://i.stack.imgur.com/75a9z.png)

|Element|Styles Applies
|---|---|---|---|---|---|---|---|---|---
|`<caption>`|Yellow text on black background.
|`<thead>`|Bold text on purple background.
|`<tbody>`|Text on blue background.
|`<tfoot>`|Text on green background.
|`<th>`|Orange borders.
|`<td>`|Red borders.



## Heading scope


`th` elements are very commonly used to indicate headings for table rows and columns, like so:

```html
<table>
    <thead>
        <tr>
            <td></td>
            <th>Column Heading 1</th>
            <th>Column Heading 2</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <th>Row Heading 1</th>
            <td></td>
            <td></td>
        </tr>
        <tr>
            <th>Row Heading 2</th>
            <td></td>
            <td></td>
        </tr>
    </tbody>
</table>

```

This can be improved for accessibility by the use of the `scope` attribute. The above example would be amended as follows:

```html
<table>
    <thead>
        <tr>
            <td></td>
            <th scope="col">Column Heading 1</th>
            <th scope="col">Column Heading 2</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <th scope="row">Row Heading 1</th>
            <td></td>
            <td></td>
        </tr>
        <tr>
            <th scope="row">Row Heading 1</th>
            <td></td>
            <td></td>
        </tr>
    </tbody>
</table>

```

`scope` is known as an **enumerated attribute**, meaning that it can have a value from a specific set of possible values. This set includes:

- `col`
- `row`
- `colgroup`
- `rowgroup`

References:

- [https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th#attr-scope](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th#attr-scope)
- [https://www.w3.org/TR/WCAG20-TECHS/H63.html](https://www.w3.org/TR/WCAG20-TECHS/H63.html)



#### Syntax


- `<table></table>`
- `<thead></thead>`
- `<tbody></tbody>`
- `<tfoot></tfoot>`
- `<tr></tr>`
- `<th></th>`
- `<td></td>`



#### Remarks


The various table elements and their content attributes together define the table model. The `<table>` element is the container element for table models/tabular data. Tables have rows, columns, and cells given by their descendants. The rows and columns form a grid; a table's cells must completely cover that grid without overlap. The list below describes the various elements in the table model:

- `<table>` - The container element for table models/tabular data. `<table>` represents data with more than one dimension in the form of a table.
- `<caption>` - Table caption or title (Like a `figcaption` to a `figure`)
- `<col>` - A column (a no-content element)
- `<colgroup>` - A grouping of columns
- `<thead>` - Table header (only one)
- `<tbody>` - Table body / content (multiple are okay)
- `<tfoot>` - Table footer (only one)
- `<tr>` - Table row
- `<th>` - Table header cell
- `<td>` - Table data cell

Semantically, tables are meant for holding tabular data. You can think of it as a way to  display and describe data that would make sense in a spreadsheet (columns and rows).

Using tables for layout is not recommended. Instead, use CSS rules for layout and formatting, including `display: table`.

One notable exception typically displayed in the industry regarding using `<table>` layout is in regards to HTML email: some email clients, including Outlook, rolled back to older rendering engines after Microsoft lose their monopoly case vs. the EU. In order for Microsoft to make IE not part of the OS, they simply rolled back Outlook's rendering engine to an earlier version of Trident. This rollback simply doesn't support modern web technologies, so using `<table>` based layouts for HTML email is the only way to ensure cross-browser/platform/client compatibility.

