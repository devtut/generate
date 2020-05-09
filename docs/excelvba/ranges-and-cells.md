---
metaTitle: "Excel VBA - Ranges and Cells"
description: "Ways to refer to a single cell, Creating a Range, Offset Property, Saving a reference to a cell in a variable, How to Transpose Ranges (Horizontal to Vertical & vice versa)"
---

# Ranges and Cells




## Ways to refer to a single cell


The simplest way to refer to a single cell on the current Excel worksheet is simply to enclose the A1 form of its reference in square brackets:

```vb
[a3] = "Hello!"

```

Note that square brackets are just convenient [syntactic sugar](https://en.wikipedia.org/wiki/Syntactic_sugar) for the `Evaluate` method of the `Application` object, so technically, this is identical to the following code:

```vb
Application.Evaluate("a3") = "Hello!"

```

You could also call the `Cells` method which takes a row and a column and returns a cell reference.

```vb
Cells(3, 1).Formula = "=A1+A2"

```

Remember that whenever you pass a row and a column to Excel from VBA, the row is always first, followed by the column, which is confusing because it is the opposite of the common `A1` notation where the column appears first.

In both of these examples, we did not specify a worksheet, so Excel will use the active sheet (the sheet that is in front in the user interface). You can specify the active sheet explicitly:

```vb
ActiveSheet.Cells(3, 1).Formula = "=SUM(A1:A2)"

```

Or you can provide the name of a particular sheet:

```vb
Sheets("Sheet2").Cells(3, 1).Formula = "=SUM(A1:A2)"

```

There are a wide variety of methods that can be used to get from one range to another. For example, the `Rows` method can be used to get to the individual rows of any range, and the `Cells` method can be used to get to individual cells of a row or column, so the following code refers to cell C1:

```vb
ActiveSheet.Rows(1).Cells(3).Formula = "hi!"

```



## Creating a Range


A [Range](https://msdn.microsoft.com/en-us/library/office/ff838238.aspx) cannot be created or populated the same way a string would:

```vb
Sub RangeTest()
    Dim s As String
    Dim r As Range 'Specific Type of Object, with members like Address, WrapText, AutoFill, etc.
    
    ' This is how we fill a String:
    s = "Hello World!"

    ' But we cannot do this for a Range:
    r = Range("A1") '//Run. Err.:  91 Object variable or With block variable not set//

    ' We have to use the Object approach, using keyword Set:
    Set r = Range("A1")
End Sub

```

It is considered best practice to [qualify your references](http://stackoverflow.com/documentation/excel-vba/1107/vba-best-practices/11274/always-define-and-set-references-to-all-workbooks-and-sheets#t=201607261348479429911), so from now on we will use the same approach here.<br />
More about [Creating Object Variables (e.g. Range) on MSDN](https://msdn.microsoft.com/en-us/library/office/gg251791.aspx) . More about [Set Statement on MSDN](https://msdn.microsoft.com/en-us/library/office/gg251642.aspx).

There are different ways to create the same Range:

```vb
Sub SetRangeVariable()
    Dim ws As Worksheet
    Dim r As Range

    Set ws = ThisWorkbook.Worksheets(1) ' The first Worksheet in Workbook with this code in it
    
    ' These are all equivalent:
    Set r = ws.Range("A2")
    Set r = ws.Range("A" & 2)
    Set r = ws.Cells(2, 1) ' The cell in row number 2, column number 1
    Set r = ws.[A2] 'Shorthand notation of Range.
    Set r = Range("NamedRangeInA2") 'If the cell A2 is named NamedRangeInA2. Note, that this is Sheet independent.
    Set r = ws.Range("A1").Offset(1, 0) ' The cell that is 1 row and 0 columns away from A1
    Set r = ws.Range("A1").Cells(2,1) ' Similar to Offset. You can "go outside" the original Range.

    Set r = ws.Range("A1:A5").Cells(2) 'Second cell in bigger Range.
    Set r = ws.Range("A1:A5").Item(2) 'Second cell in bigger Range.
    Set r = ws.Range("A1:A5")(2) 'Second cell in bigger Range.
End Sub

```

Note in the example that Cells(2, 1) is equivalent to Range("A2"). This is because Cells returns a Range object.<br />
Some sources: [Chip Pearson-Cells Within Ranges](http://www.cpearson.com/Excel/cells.htm); [MSDN-Range Object](https://msdn.microsoft.com/en-us/library/office/ff838238.aspx); [John Walkenback-Referring To Ranges In Your VBA Code](http://spreadsheetpage.com/index.php/tip/referring_to_ranges_in_your_vba_code/).

Also note that in any instance where a number is used in the declaration of the range, and the number itself is outside of quotation marks, such as Range("A" & 2), you can swap that number for a variable that contains an integer/long. For example:

```vb
Sub RangeIteration()
    Dim wb As Workbook, ws As Worksheet
    Dim r As Range

    Set wb = ThisWorkbook
    Set ws = wb.Worksheets(1)

    For i = 1 To 10
        Set r = ws.Range("A" & i)
        ' When i = 1, the result will be Range("A1")
        ' When i = 2, the result will be Range("A2")
        ' etc.
        ' Proof:
        Debug.Print r.Address
    Next i
End Sub

```

If you are using double loops, Cells is better:

```vb
Sub RangeIteration2()
    Dim wb As Workbook, ws As Worksheet
    Dim r As Range

    Set wb = ThisWorkbook
    Set ws = wb.Worksheets(1)

    For i = 1 To 10
        For j = 1 To 10
            Set r = ws.Cells(i, j)
            ' When i = 1 and j = 1, the result will be Range("A1")
            ' When i = 2 and j = 1, the result will be Range("A2")
            ' When i = 1 and j = 2, the result will be Range("B1")
            ' etc.
            ' Proof:
            Debug.Print r.Address
        Next j
    Next i
End Sub

```



## Offset Property


- **Offset(Rows, Columns)** - The operator used to statically reference another point from the current cell. Often used in loops. It should be understood that positive numbers in the rows section moves right, wheres as negatives move left. With the columns section positives move down and negatives move up.

i.e

```vb
Private Sub this()
    ThisWorkbook.Sheets("Sheet1").Range("A1").Offset(1, 1).Select
    ThisWorkbook.Sheets("Sheet1").Range("A1").Offset(1, 1).Value = "New Value"
    ActiveCell.Offset(-1, -1).Value = ActiveCell.Value
    ActiveCell.Value = vbNullString
End Sub

```

This code selects B2, puts a new string there, then moves that string back to A1 afterwards clearing out B2.



## Saving a reference to a cell in a variable


To save a reference to a cell in a variable, you must use the `Set` syntax, for example:

```vb
Dim R as Range
Set R = ActiveSheet.Cells(3, 1)

```

**later...**

```vb
R.Font.Color = RGB(255, 0, 0)

```

Why is the `Set` keyword required? `Set` tells Visual Basic that the value on the right hand side of the `=` is meant to be an object.



## How to Transpose Ranges (Horizontal to Vertical & vice versa)


```vb
Sub TransposeRangeValues()
    Dim TmpArray() As Variant, FromRange as Range, ToRange as Range

    set FromRange = Sheets("Sheet1").Range("a1:a12")         'Worksheets(1).Range("a1:p1")
    set ToRange = ThisWorkbook.Sheets("Sheet1").Range("a1")  'ThisWorkbook.Sheets("Sheet1").Range("a1")

    TmpArray = Application.Transpose(FromRange.Value)
    FromRange.Clear
    ToRange.Resize(FromRange.Columns.Count,FromRange.Rows.Count).Value2 = TmpArray
End Sub

```

Note: Copy/PasteSpecial also has a Paste Transpose option which updates the transposed cells' formulas as well.



#### Syntax


- **Set** - The operator used to set a reference to an object, such as a Range
- **For Each** - The operator used to loop through every item in a collection



#### Remarks


Note that the variable names `r`, `cell` and others can be named however you like but should be named appropriately so the code is easier to understand for you and others.

