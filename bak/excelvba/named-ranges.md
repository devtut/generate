---
metaTitle: "Excel VBA - Named Ranges"
description: "Define A Named Range, Using Named Ranges in VBA, Manage Named Range(s) using Name Manager, Named Range Arrays"
---

# Named Ranges




## Define A Named Range


Using named ranges allows you to describe the meaning of a cell(s) contents and use this defined name in place of an actual cell address.

For example, formula `=A5*B5` can be replaced with `=Width*Height` to make the formula much easier to read and understand.

To define a new named range, select cell or cells to name and then type new name into the Name Box next to the formula bar.

[<img src="https://i.stack.imgur.com/KqLbf.gif" alt="enter image description here" />](https://i.stack.imgur.com/KqLbf.gif)

> 
<p>Note: Named Ranges default to global scope meaning that they can be
accessed from anywhere within the workbook.  Older versions of Excel allow for duplicate names so care must be taken to
prevent duplicate names of global scope otherwise results will be
unpredictable.  Use Name Manager from Formulas tab to change scope.</p>




## Using Named Ranges in VBA


**Create** new named range called ‘MyRange’ assigned to cell `A1`

```vb
ThisWorkbook.Names.Add Name:="MyRange", _
    RefersTo:=Worksheets("Sheet1").Range("A1")

```

**Delete** defined named range by name

```vb
ThisWorkbook.Names("MyRange").Delete

```

**Access Named Range** by name

```vb
Dim rng As Range
Set rng = ThisWorkbook.Worksheets("Sheet1").Range("MyRange")
Call MsgBox("Width = " & rng.Value)

```

**Access a Named Range with a Shortcut**

[Just like any other range](http://www.informit.com/articles/article.aspx?p=2021718&seqNum=4), named ranges can be accessed directly with through a shortcut notation that does not require a `Range` object to be created. The three lines from the code excerpt above can be replaced by a single line:

```vb
Call MsgBox("Width = " & [MyRange])

```

> 
Note: The default property for a Range is its Value, so `[MyRange]` is the same as `[MyRange].Value`


You can also call methods on the range. The following selects `MyRange`:

```vb
[MyRange].Select

```

> 
<p>Note: One caveat is that the shortcut notation does not work with words
that are used elsewhere in the VBA library. For example, a range named
`Width` would not be accessible as `[Width]` but would work as expected
if accessed through `ThisWorkbook.Worksheets("Sheet1").Range("Width")`</p>




## Manage Named Range(s) using Name Manager


Formulas tab > Defined Names group > Name Manager button

Named Manager allows you to:

1. Create or change name
1. Create or change cell reference
1. Create or change scope
1. Delete existing named range

[<img src="https://i.stack.imgur.com/62GIj.jpg" alt="enter image description here" />](https://i.stack.imgur.com/62GIj.jpg)

Named Manager provides a useful quick look for broken links.

[<img src="https://i.stack.imgur.com/wx6B0.jpg" alt="enter image description here" />](https://i.stack.imgur.com/wx6B0.jpg)



## Named Range Arrays


Example sheet

[<img src="https://i.stack.imgur.com/Q7YIB.png" alt="enter image description here" />](https://i.stack.imgur.com/Q7YIB.png)

**Code**

```vb
Sub Example()
    Dim wks As Worksheet
    Set wks = ThisWorkbook.Worksheets("Sheet1")
    
    Dim units As Range
    Set units = ThisWorkbook.Names("Units").RefersToRange
    
    Worksheets("Sheet1").Range("Year_Max").Value = WorksheetFunction.Max(units)
    Worksheets("Sheet1").Range("Year_Min").Value = WorksheetFunction.Min(units)
End Sub

```

**Result**

[<img src="https://i.stack.imgur.com/RYAKu.png" alt="enter image description here" />](https://i.stack.imgur.com/RYAKu.png)

