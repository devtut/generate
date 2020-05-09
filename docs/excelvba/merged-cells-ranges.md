---
metaTitle: "Excel VBA - Merged Cells / Ranges"
description: "Think twice before using Merged Cells/Ranges"
---

# Merged Cells / Ranges



## Think twice before using Merged Cells/Ranges


First of all, Merged Cells are there only to improve the look of your sheets.

So it is literally the last thing that you should do, once your sheet and workbook are totally functional!

### Where is the data in a Merged Range?

When you merge a Range, you'll only display one block.

The data will be in the very **first cell of that Range**, and the **others will be empty cells**!

One good point about it : no need to fill all the cells or the range once merged, just fill the first cell! ;)

The other aspects of this merged ranged are globally negative :

<li>
If you use a [method for finding last row or column](http://stackoverflow.com/documentation/excel-vba/918/methods-for-finding-the-last-used-row-or-column-in-a-worksheet#t=201610071525133554127), you'll risk some errors
</li>
<li>
If you loop through rows and you have merged some ranges for a better readability, you'll encounter empty cells  and not the value displayed by the merged range
</li>

